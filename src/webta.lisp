;;; webta.lisp -- WebTA report subcomponents of XARF
;;; D. Racine, 20140528

(in-package :xarf)

(defun get-wt-instance (id)
  "Returns the WebTA instance object corresponding to the specified id string"
  (find-if
   #'(lambda (x) (string= id (princ-to-string (wt-instance-id x)))) *wt-instances*))


(defun wt-client-name (id)
  "Returns client name corresponding to the specified id string"
  (princ-to-string (or (wt-instance-client (get-wt-instance id)) "")))


(defun wt-ids (type)
  "Return the specified subset of WebTA IDs as a list of strings."
  (if (eq type 'all)
      (mapcar #'(lambda (x) (princ-to-string (wt-instance-id x))) *wt-instances*)
      (let ((s (remove type *wt-instances* :test-not #'eq :key #'wt-instance-type)))
        (mapcar #'(lambda (x) (princ-to-string (wt-instance-id x))) s))))


(defun wt-nodes (webfarm)
  "Returns the list of hostnames for the specified WebTA webfarm."
  (if (eq webfarm 'all)
      (mapcar #'wt-node-hostname *wt-nodes*)
      (mapcar #'wt-node-hostname
              (remove webfarm *wt-nodes* :test-not #'eq :key #'wt-node-webfarm))))


(defun make-wt-url (id)
  "Returns the URL for the specified WebTA instance ID."
  (let ((template (cdr (assoc (wt-instance-webfarm (get-wt-instance id)) *wt-webfarms*))))
    (regex-replace-all "<ID>" template id)))


(make-uri-dispatcher-and-handler wtusercount
  (session-check wtusercount)
  (make-xarf-html-screen
      (:title "WebTA Active User Counts" :footmenu *footer-menu*)
    (:pre (fmt "~a" (remote-slurp (scat *wt-ssh-user* "@" (car (wt-nodes 'prod)))
                                  "bin/active-user-counts")))))


;;===< begin wt-dbstatus >===

(defun test-wt-url (id)
  "Issues an HTTP GET to id's login screen, returns 'OK' or 'failed'"
  (let ((lpg (with-output-to-string (s)
               (process-close
                (run-program "wget" `("-T 10" "-qO-" ,(make-wt-url id))
                             :search t :output s :error nil)))))
    (if (search "login_form" lpg) "OK" "failed")))


(defun test-wt-db (id)
  "Returns output of testdb or 'timeout'"
  (remote-slurp (scat *wt-ssh-user* "@" (car (wt-nodes 'prod)))
                (scat "bin/testdb " id) :timeout 10))


(defun start-wt-test-thread (id)
  "Returns a thread running test-wt-db and test-wt-url on id"
  (make-thread (lambda () (with-xarf-pkg (list (test-wt-db id) (test-wt-url id))))))


(defun test-all-wt (idlist)
  "Returns a list of (dbtest urltest) result pairs, one per id"
  (let ((test-threads (mapcar #'start-wt-test-thread idlist)))
    (mapcar #'(lambda (x) (join-thread x :default nil)) test-threads)))


(make-uri-dispatcher-and-handler wt-dbstatus
  (session-check wt-dbstatus)
  (let* ((failpat "failed|timeout")
         (ids (wt-ids 'prod))
         (statrows (mapcar #'(lambda (i r) (cons i r)) ids (test-all-wt ids))))
    (make-xarf-html-screen
        (:title "WebTA DB Status" :footmenu *footer-menu*)
      (:h3 "Production WebTA status as of " (str (timestr)))
      ((:table :class "qtdir")
       ((:tr) (:th "ID - Client") (:th "Database") (:th "Login Screen"))
       (dolist (i statrows)
         (htm ((:tr)
               ((:td) (applink wt (car i)))
               (dolist (j (cdr i))
                 (if (or (scan failpat j) (string= j ""))
                     (htm ((:td :class "fail") (str j)))
                     (htm ((:td :class "pass") (str j))))))))))))
        
;;===< end wt-dbstatus >===

;;===< begin wt-directory >===

(defun remote-wt-detail (wtnode)
  "Returns list of all cluster member info from app tier host"
  (let* ((sshproc (run-remote-cmd
                   (scat *wt-ssh-user* "@" (wt-node-hostname wtnode))
                   "bin/xarf-wt-info"))
         (result (read (process-output sshproc) nil)))
    (process-close (process-wait sshproc t))
    result))


(defun start-remote-wt-detail-thread (wtnode)
  "Starts & returns a thread running 'remote-wt-detail' on wtnode"
  (make-thread (lambda () (with-xarf-pkg (remote-wt-detail wtnode)))))


(defun all-remote-wt-detail (list-of-nodes)
  "Returns a list of the nodes' info lists, aka 'crop'"
  (let ((cthreads (mapcar #'start-remote-wt-detail-thread list-of-nodes)))
    (mapcar #'(lambda (x) (join-thread x :default nil)) cthreads)))


(defun wt-i-dyn (i crop)
  "Returns i-dyn, a list of all of instance i's cluster details from crop"
  (remove (wt-instance-id i) (apply #'append crop) :test-not #'eq :key #'car))


(defun wt-dyn-prop (prop i-dyn)
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


(defun wt-directory-row (i crop)
  "Returns attributes of a WebTA instance as a list of strings"
  (let ((dyn (wt-i-dyn i crop)))
    (list (princ-to-string (wt-instance-id i))
          (case (wt-instance-type i)
            (trn-dmo "Training/Demo")
            (test-nr-auto "NxtRel Auto Test")
            (test-nr-func "NxtRel Func Test")
            (test-em-auto "Emerg Auto Test")
            (test-em-func "Emerg Func Test")
            (prod "Production")
            (para "Parallel")
            (otherwise "unknown"))
          (wt-dyn-prop 'version dyn)
          (wt-dyn-prop 'schema dyn)
          (wt-dyn-prop 'db dyn)
          (wt-dyn-prop 'deployed-on dyn)
          (wt-dyn-prop 'running-on dyn))))


(make-uri-dispatcher-and-handler wt-directory
  (session-check wt-directory)
  (let* ((crop (all-remote-wt-detail *wt-nodes*))
         (allrows (mapcar #'(lambda (x) (wt-directory-row x crop)) *wt-instances*)))
    (make-xarf-html-screen
        (:title "WebTA Instance Directory" :footmenu *footer-menu*)
      (:h4 "Generated " (str (timestr)))
      ((:table :class "qtdir")
       ((:tr)
        (:th "ID - Client") (:th "Type") (:th "Version") (:th "Schema")
        (:th "DB") (:th "Deployed On") (:th "Running On"))
       (dolist (i allrows)
         (htm ((:tr)
               ((:td) (applink wt (car i)))
               (dolist (j (cdr i))
                 (htm ((:td) (fmt "~a" j)))))))))))

;;===< end wt-directory >===

;;===< begin wt-load-graphs >===

(defparameter *wtjs* "<!--
  function idnav()
  {var w = document.wtidform.id.selectedIndex;
  var url_add = document.wtidform.id.options[w].value;
  window.location.href = url_add;}
  //-->")


(make-uri-dispatcher-and-handler wtgraph
  (session-check wtgraph)
  (no-cache)
  (setf (content-type*) "image/png")
  (let ((gt (sanitize (get-parameter "type")))
        (id (sanitize (get-parameter "id")))
        (start (sanitize (get-parameter "start")))
        (len (sanitize (get-parameter "len")))
        (bin (scat *xarf-home* "bin/graph")))
    (flexi-streams:with-output-to-sequence (s)
      (process-close
       (run-program bin (list "wt" gt id start len) :output s)))))


(defun wt-id-form (uri id start len ids)
  (with-html-output (*standard-output*)
    ((:form :method "get" :name "wtidform")
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
                 (str i) " - " (str (wt-client-name i))))))))))))


(make-uri-dispatcher-and-handler wt-load-graphs
  (session-check wt-load-graphs)
  (let* ((id (sanitize (or (get-parameter "id") (car (wt-ids 'prod)))))
         (24h (sanitize (get-parameter "24h")))
         (start (if 24h nil (sanitize (get-parameter "start"))))
         (len (if 24h nil (sanitize (get-parameter "len"))))
         (ids (wt-ids 'prod)))
    (make-xarf-html-screen
        (:title "WebTA Load & Performance" :footmenu *footer-menu* :js *wtjs*)
      (wt-id-form "/wt-load-graphs" id start len ids)
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
                   :src (scat "/wtgraph?type=a&id=" id "&start=" start "&len=" len))
             (:br)
             (:img :alt "graph_b"
                   :src (scat "/wtgraph?type=b&id=" id "&start=" start "&len=" len)))))))

;;===< end wt-load-graphs >===

;;===< begin bulkgroovy >===

(make-uri-dispatcher-and-handler groovit
  (session-check groovit)
  (no-cache)
  (let* ((s (parse-integer (sanitize (get-parameter "s")) :junk-allowed t))
         (i (sanitize (get-parameter "i")))
         (script (when (integerp s) (nth (abs s) *groovy*)))
         (ids (case (parse-integer i :junk-allowed t)
                (6 (wt-ids 'test-nr-func))
                (14 (wt-ids 'test-em-func))
                (16 (wt-ids 'prod))))
         (*print-pretty* nil)
         (rcmd (scat "bin/xarf-rg groovy/" script ".groovy "
                     (string-trim "()" (princ-to-string ids)))))
    (when (and script ids)
      (setf (content-type*) "application/zip")
      (setf (header-out :Content-Disposition) (scat "attachment; filename=" i "xx-" script ".zip"))
      (remote-slurp (scat *wt-ssh-user* "@" (car (wt-nodes 'prod))) rcmd :binary t :timeout 240))))


(make-uri-dispatcher-and-handler bulkgroovy
  (session-check bulkgroovy)
  (make-xarf-html-screen
      (:title "WebTA Bulk Groovy Reports" :footmenu *footer-menu*)
    ((:p)
     "Applies the selected groovy script to a subset of WebTA app instances
     and returns a single zip file containing the script output documents.")
    ((:p)
     "Script execution will take at least several seconds before the output is
     returned. Processing time depends on the complexity of the requested script.")
    ((:form :method "get" :name "bulkform" :action "/groovit")
     ((:p) "Run script "
      ((:select :name "s")
       (dotimes (j (length *groovy*))
         (htm ((:option :value (princ-to-string j)) (str (nth j *groovy*))))))
      " on the "
      ((:select :name "i")
       ((:option :value "6") "6-series")
       ((:option :value "14") "14-series")
       ((:option :value "16") "16-series")) " app instances.")
     ((:p) (:input :type "submit" :value "Submit Request")))))

;;===< end bulkgroovy >===

;;===< begin wt-ts-status >===

(defun get-ts-status (yr pp)
  "Returns a list of all timesheet status data from xarf-ts-status"
  (let* ((ids (wt-ids 'prod))
         (*print-pretty* nil)
         (sshproc (run-remote-cmd
                   (scat *wt-ssh-user* "@" (car (wt-nodes 'prod)))
                   (scat "bin/xarf-ts-status " yr "-" pp " "
                         (string-trim "()" (princ-to-string ids)))))
         (result (read (process-output sshproc) nil)))
    (process-close (process-wait sshproc t))
    result))


(make-uri-dispatcher-and-handler wt-ts-status
  (session-check wt-ts-status)
  (let ((pp (sanitize (get-parameter "pp")))
        (yr (sanitize (get-parameter "yr")))
        (status nil))
    (make-xarf-html-screen
        (:title "WebTA Timesheet Status" :footmenu *footer-menu*)
      ((:form :method "get" :name "yearpp")
       "Pay Period: "
       (:input :name "pp" :size 3 :value pp)
       " Year (optional): "
       (:input :name "yr" :size 5 :value yr) " "
       (:input :type "submit" :value "Get Timesheet Status"))
      (when (and (scan "^[0-9]{1,2}$" pp)
                 (or (scan "^[0-9]{4}$" yr) (string= yr "") (eq yr nil)))
        (when (or (string= yr "") (eq yr nil)) (setq yr (subseq (timestr) 4 8)))
        (setq status (get-ts-status yr pp))
        (htm
         (:h4 "PP " (str yr) "-" (str pp) " timesheet status as of " (str (timestr)))
         ((:table :class "qtdir")
          ((:tr)
           (:th "ID - Client") (dolist (j *wt-ts-states*) (htm (:th (str j)))))
          (dolist (i status)
            (htm
             ((:tr) ((:td) (applink wt (princ-to-string (car i))))
              (dolist (j *wt-ts-states*)
                (let ((ct (second (find-if #'(lambda (x) (string= (car x) j)) (cdr i)))))
                  (htm ((:td :class "n") (if ct (fmt "~a" ct) ""))))))))))))))

;;===< end wt-ts-status >===
