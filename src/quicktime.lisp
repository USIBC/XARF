;;; quicktime.lisp -- QT report subcomponents of XARF
;;; D. Racine, 20070511

(in-package :xarf)

(defun get-qt-instance (id)
  "Returns the QT instance object corresponding to the specified id string"
  (find-if
   #'(lambda (x) (string= id (princ-to-string (qt-instance-id x)))) *qt-instances*))


(defun qt-client-name (id)
  "Returns client name corresponding to the specified id string"
  (princ-to-string (or (qt-instance-client (get-qt-instance id)) "")))


(defun qt-ids (type)
  "Return the specified subset of QT IDs as a list of strings."
  (if (eq type 'all)
      (mapcar #'(lambda (x) (princ-to-string (qt-instance-id x))) *qt-instances*)
      (let ((s (remove type *qt-instances* :test-not #'eq :key #'qt-instance-type)))
        (mapcar #'(lambda (x) (princ-to-string (qt-instance-id x))) s))))


(defun qt-nodes (webfarm)
  "Returns the list of hostnames in the specified QT webfarm."
  (if (eq webfarm 'all)
      (mapcar #'qt-node-hostname *qt-nodes*)
      (mapcar #'qt-node-hostname
              (remove webfarm *qt-nodes* :test-not #'eq :key #'qt-node-webfarm))))


(defun get-qt-hosts (id)
  "Returns list of webfarm hosts on which id(str) is deployed per qt-config"
  (let ((qti (get-qt-instance id)))
    (when qti (qt-nodes (qt-instance-webfarm qti)))))
   

(defun make-qt-url (id)
  "Returns the URL for the specified QT instance ID."
  (let ((template (cdr (assoc (qt-instance-webfarm (get-qt-instance id)) *qt-webfarms*))))
    (regex-replace-all "<ID>" template id)))


(make-uri-dispatcher-and-handler qstatus
  (session-check qstatus)
  (make-xarf-html-screen
      (:title "QT App-tier Status Summary" :footmenu *footer-menu*)
    (:pre (fmt "~a" (remote-slurp (scat *qt-ssh-user* "@" *qstatus-host*) "bin/qstatus z")))))


;;===< begin qt-dbstatus >===

(defun test-qt-url (id)
  "Issues an HTTP GET to id's login screen, returns 'OK' or 'failed'"
  (let ((lpg (with-output-to-string (s)
               (process-close
                (run-program "wget" `("-T 10" "-qO-" ,(make-qt-url id))
                             :search t :output s :error nil)))))
    (if (search "Application Login" lpg) "OK" "failed")))


(defun test-qt-db (id)
  "Returns output of testdb or 'timeout'"
  (remote-slurp (scat *qt-ssh-user* "@" (car (get-qt-hosts id)))
                (scat "bin/testdb " id) :timeout 10))


(defun start-qt-test-thread (id)
  "Returns a thread running test-qt-db and test-qt-url on id"
  (make-thread (lambda () (with-xarf-pkg (list (test-qt-db id) (test-qt-url id))))))


(defun test-all-qt (idlist)
  "Returns a list of (dbtest urltest) result pairs, one per id"
  (let ((test-threads (mapcar #'start-qt-test-thread idlist)))
    (mapcar #'(lambda (x) (join-thread x :default nil)) test-threads)))


(make-uri-dispatcher-and-handler qt-dbstatus
  (session-check qt-dbstatus)
  (let* ((failpat "failed|timeout")
         (ids (qt-ids 'prod))
         (statrows (mapcar #'(lambda (i r) (cons i r)) ids (test-all-qt ids))))
    (make-xarf-html-screen
        (:title "Quicktime DB Status" :footmenu *footer-menu*)
      (:h3 "Production Quicktime status as of " (str (timestr)))
      ((:table :class "qtdir")
       ((:tr) (:th "ID - Client") (:th "Database") (:th "Login Screen"))
       (dolist (i statrows)
         (htm ((:tr)
               ((:td) (applink qt (car i)))
               (dolist (j (cdr i))
                 (if (or (scan failpat j) (string= j ""))
                     (htm ((:td :class "fail") (str j)))
                     (htm ((:td :class "pass") (str j))))))))))))

;;===< end qt-dbstatus >===

;;===< begin qt-directory >===

(defun remote-qt-detail (qtnode)
  "Returns list of all cluster member info from app tier host"
  (let* ((sshproc (run-remote-cmd
                   (scat *qt-ssh-user* "@" (qt-node-hostname qtnode)) "bin/xarf-qt-info"))
         (result (read (process-output sshproc) nil)))
    (process-close (process-wait sshproc t)) result))


(defun start-remote-qt-detail-thread (qtnode)
  "Starts & returns a thread running 'remote-qt-detail' on qtnode"
  (make-thread (lambda () (with-xarf-pkg (remote-qt-detail qtnode)))))


(defun all-remote-qt-detail (list-of-nodes)
  "Returns a list of the nodes' info lists, aka 'crop'"
  (let ((cthreads (mapcar #'start-remote-qt-detail-thread list-of-nodes)))
    (mapcar #'(lambda (x) (join-thread x :default nil)) cthreads)))


(defun qt-i-dyn (i crop)
  "Returns i-dyn, a list of all of instance i's cluster details from crop"
  (remove (qt-instance-id i) (apply #'append crop) :test-not #'eq :key #'car))


(defun qt-dyn-prop (prop i-dyn)
  "Returns a string representing one of i's dynamically obtained properties"
  (if i-dyn
      (flet ((reconcile-prop (selector idyn)
               (let ((x (eq+ (mapcar selector idyn))))
                 (if x (string-downcase (princ-to-string x)) "*discrepancy*"))))
        (case prop
          (version (reconcile-prop #'third i-dyn))
          (schema (reconcile-prop #'fourth i-dyn))
          (db (reconcile-prop #'fifth i-dyn))
          (deployed-on
           (string-trim "()" (string-downcase (princ-to-string (mapcar #'second i-dyn)))))
          (running-on
           (let ((r (remove-if #'null
                               (mapcar #'(lambda (x) (if (sixth x) (second x))) i-dyn))))
             (if r (string-trim "()" (string-downcase (princ-to-string r))) "")))))
      ""))


(defun qt-directory-row (i crop)
  "Returns attributes of a QT instance as a list of strings"
  (let ((dyn (qt-i-dyn i crop)))
    (list (princ-to-string (qt-instance-id i))
          (case (qt-instance-type i)
            (dev "Development")
            (trn-dmo "Training/Demo")
            (test-nr-auto "NxtRel Auto Test")
            (test-nr-func "NxtRel Func Test")
            (test-em-auto "Emerg Auto Test")
            (test-em-func "Emerg Func Test")
            (prod "Production")
            (para "Parallel")
            (otherwise "unknown"))
          (qt-dyn-prop 'version dyn)
          (qt-dyn-prop 'schema dyn)
          (qt-dyn-prop 'db dyn)
          (qt-dyn-prop 'deployed-on dyn)
          (qt-dyn-prop 'running-on dyn))))


(make-uri-dispatcher-and-handler qt-directory
  (session-check qt-directory)
  (let* ((crop (all-remote-qt-detail *qt-nodes*))
         (allrows (mapcar #'(lambda (x) (qt-directory-row x crop)) *qt-instances*)))
    (make-xarf-html-screen
        (:title "QT Instance Directory" :footmenu *footer-menu*)
      (:h4 "Generated " (str (timestr)))
      ((:table :class "qtdir")
       ((:tr)
        (:th "ID - Client") (:th "Type") (:th "Version") (:th "Schema")
        (:th "DB") (:th "Deployed On") (:th "Running On"))
       (dolist (i allrows)
         (htm ((:tr)
               ((:td) (applink qt (car i)))
               (dolist (j (cdr i))
                 (htm ((:td) (fmt "~a" j)))))))))))

;;===< end qt-directory >===

;;===< begin qt-load-graphs >===

(defparameter *qtjs* "<!--
  function idnav()
  {var w = document.qtidform.id.selectedIndex;
  var url_add = document.qtidform.id.options[w].value;
  window.location.href = url_add;}
  //-->")


(make-uri-dispatcher-and-handler qtgraph
  (session-check qtgraph)
  (no-cache)
  (setf (content-type*) "image/png")
  (let ((gt (sanitize (get-parameter "type")))
        (id (sanitize (get-parameter "id")))
        (start (sanitize (get-parameter "start")))
        (len (sanitize (get-parameter "len")))
        (bin (scat *xarf-home* "bin/graph")))
    (flexi-streams:with-output-to-sequence (s)
      (process-close
       (run-program bin (list "qt" gt id start len) :output s)))))


(defun qt-id-form (uri id start len ids)
  (with-html-output (*standard-output*)
    ((:form :method "get" :name "qtidform")
     ((:table)
      ((:tr)
       ((:td)
        "Instance ID: "
        ((:select :name "id" :onchange "idnav()")
         (dolist (i ids)
           (htm ((:option
                  :value (concatenate
                          'string uri "?id=" i "&start=" start "&len=" len)
                  :selected (string= i id))
                 (str i) " - " (str (qt-client-name i))))))))))))


(make-uri-dispatcher-and-handler qt-load-graphs
  (session-check qt-load-graphs)
  (let* ((id (sanitize (or (get-parameter "id") (car (qt-ids 'prod)))))
         (24h (sanitize (get-parameter "24h")))
         (start (if 24h nil (sanitize (get-parameter "start"))))
         (len (if 24h nil (sanitize (get-parameter "len"))))
         (ids (qt-ids 'prod)))
    (make-xarf-html-screen
        (:title "QT Load & Performance" :footmenu *footer-menu* :js *qtjs*)
      (qt-id-form "/qt-load-graphs" id start len ids)
      ((:form :method "get" :name "rangeform")
       ((:table)
        ((:tr)
         ((:td) "Start (YYYYMMDD[.hh.mm]): "
          (:input :name "start" :size 12 :value start))
         ((:td) "Length (# of days): "
          (:input :name "len" :size 2 :value len))
         ((:td) (:input :name "24h" :type "checkbox" :value "1") "Past 24 hours")
         ((:td)
          (:input :name "id" :type "hidden" :value id)
          (:input :type "submit" :value "Apply")))))
      (when id
        (htm (:img :alt "graph_a"
                   :src (scat "/qtgraph?type=a&id=" id "&start=" start "&len=" len))
             (:br)
             (:img :alt "graph_b"
                   :src (scat "/qtgraph?type=b&id=" id "&start=" start "&len=" len)))))))

;;===< end qt-load-graphs >===

;;===< begin qtappcfg >===

(make-uri-dispatcher-and-handler getqtcfg
  (session-check getqtcfg)
  (no-cache)
  (setf (content-type*) "text/plain")
  (let* ((id (sanitize (get-parameter "id")))
         (host (car (get-qt-hosts id))))
    (when (and host id)
      (remote-slurp (scat *qt-ssh-user* "@" host)
                    (scat "cat /qt/" id "/web/app_cfg.jpl") :timeout 15))))
   

(make-uri-dispatcher-and-handler qtappcfg
  (session-check qtappcfg)
  (let ((id (sanitize (get-parameter "id"))))
    (make-xarf-html-screen
        (:title "QT app_cfg viewer" :footmenu *footer-menu*)
      ((:form :method "get" :name "idform")
       ((:table)
        ((:tr) ((:td)
                "App Instance ID: "
                ((:select :name "id")
                 (dolist (i (qt-ids 'all))
                   (htm ((:option :value i :selected (string= i id))
                         (str i) " - " (str (qt-client-name i)))))))
         ((:td) (:input :type "submit" :value "Get app_cfg.jpl")))))
      ((:object
        :data (scat "/getqtcfg?id=" id) :type "text/plain" :width "100%" :height "700")))))

;;===< end qtappcfg >===


(make-uri-dispatcher-and-handler webtrans
  (session-check webtrans)
  (let* ((appid (sanitize (get-parameter "s")))
         (type (sanitize (get-parameter "t")))
         (ids (append (qt-ids 'test-nr-auto)
                      (qt-ids 'test-nr-func)
                      (qt-ids 'test-em-func)
                      (qt-ids 'prod)
                      (qt-ids 'test-em-auto)))
         (rcmd (scat "bin/xarf-trans " appid " " type))
         (result ""))
    (when (and appid type)
      (setq result (remote-slurp (scat *qt-ssh-user* "@" (car (qt-nodes 'test))) rcmd)))
    (make-xarf-html-screen
        (:title "QT Transmission Data" :footmenu *footer-menu*)
      (:h3 "Transmission/Amendment File Extractor")
      ((:form :method "get" :name "transfileform")
       ((:table)
        ((:tr)
         ((:td)
          "App Instance ID: "
          ((:select :name "s")
           (dolist (i ids)
             (htm ((:option :selected (string= appid i)) (str i))))))
         ((:td)
          (:input :type "radio" :name "t" :value "trans"
                  :checked (or (string= type "trans") (null type)))
          "Transmission "
          (:input :type "radio" :name "t" :value "amend"
                  :checked (and (not (null type)) (string/= type "trans")))
          "Amendment")
         ((:td) (:input :type "submit" :value "Submit Query")))))
      (when (and appid type)
        (htm ((:h4 :style "text-align:left")
              (str (if (string= type "trans") "Transmission " "Amendment "))
              "data generated in app instance " (str appid) " today:")
             ((:pre) (fmt "~a" result)))))))


(make-uri-dispatcher-and-handler ts-status
  (session-check ts-status)
  (let* ((pp (sanitize (get-parameter "pp")))
         (yr (sanitize (get-parameter "yr")))
         (rcmd (scat "bin/timesheet-status " pp " " yr)))
    (make-xarf-html-screen
        (:title "QT Timesheet Status" :footmenu *footer-menu*)
      ((:form :method "get" :name "pp-year")
       "Pay Period: "
       (:input :name "pp" :size 3 :value pp)
       " Year (optional): "
       (:input :name "yr" :size 5 :value yr) " "
       (:input :type "submit" :value "Get Timesheet Status"))
      (when pp
        (htm (:pre
              (fmt "~a" (remote-slurp (scat *qt-ssh-user* "@" (car (qt-nodes 'prod))) rcmd))))))))


(make-uri-dispatcher-and-handler listimports
  (session-check listimports)
  (let ((id (sanitize (get-parameter "s")))
        (ids (qt-ids 'prod))
        (result ""))
    (when id
      (setq result (remote-slurp (scat *qt-ssh-user* "@" (car (get-qt-hosts id)))
                                 (scat "bin/listimports " id))))
    (make-xarf-html-screen (:title "QT Data Imports" :footmenu *footer-menu*)
      ((:form :method "get" :name "impform")
       ((:table)
        ((:tr)
         ((:td) "Instance ID: "
          ((:select :name "s")
           (dolist (i ids)
             (htm ((:option :value i :selected (string= id i))
                   (str i) " - " (str (qt-client-name i)))))))
         ((:td) (:input :type "submit" :value "Submit Query")))))
      (when id (htm ((:pre) (fmt "~a" result)))))))
