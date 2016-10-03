;;; Calculates time-series % utilization of all jservers of one
;;; cluster member of a Quicktime app instance based on its web.log.
;;; D. Racine 20150515

(in-package :xarf)


(defun ts-to-sec (tstr)
  "Converts string HH:MM:SS to number of seconds since midnight"
  (let ((h (parse-integer tstr :start 0 :end 2))
        (m (parse-integer tstr :start 3 :end 5))
        (s (parse-integer tstr :start 6)))
    (+ (* h 3600) (* m 60) s)))


(defun parse-record (r)
  "Returns (jserver-PID timestamp busytime-ms) given a record from web.log"
  (let ((pid (parse-integer (scan-to-strings "\\t *[0-9]* [0-9]{2}:" r) :junk-allowed t))
        (ts (ts-to-sec (scan-to-strings "[0-9]{2}:[0-9]{2}:[0-9]{2}" r)))
        (bt (parse-integer
             (scan-to-strings
              "[0-9]*$"
              (scan-to-strings "^[0-9]*\\t[0-9]*\\t[0-9]*" r)) :junk-allowed t)))
    (list pid ts bt)))


(defun hash-record (pid ts bt accumulator)
  "For 1 weblog record, adds busytime to jserver-timestamp in accumulator hashtable"
  (if (eq 'dne (gethash pid accumulator 'dne))
      (progn (setf (gethash pid accumulator) (make-hash-table))
             (setf (gethash ts (gethash pid accumulator)) bt))
      (setf (gethash ts (gethash pid accumulator))
            (+ (gethash ts (gethash pid accumulator) 0) bt))))


(defun sorted-keys (ht)
  "Assumes hashtable keys are numbers, i.e. jserver PIDs or timestamps"
  (sort (let (r) (maphash #'(lambda (&rest x) (push (car x) r)) ht) r) #'<))


(defun mk-util-tbl (bttbl)
  "Returns a tstamp:%utilization hashtable given a tstamp:busy-ms hashtable"
  (let (ts-prev (result (make-hash-table)))
    (dolist (ts (sorted-keys bttbl))
      (when ts-prev
        (setf (gethash ts result) (/ (gethash ts bttbl) (* 10.0 (- ts ts-prev)))))
      (setq ts-prev ts))
    result))


(defun calc-all (accumulator)
  "Returns a hashtable of PIDs:%utilization_tables for all jservers
   in the specified accumulator table"
  (let ((result (make-hash-table)))
    (maphash
     #'(lambda (k v) (setf (gethash k result) (mk-util-tbl v))) accumulator)
    result))


(defun scan-weblog (host qtid &optional date)
  "Returns all-util, a hashtable of PIDs:%utilization_tables, for all jservers
   referenced in host-qtid's web.log"
  (let* ((accumulator (make-hash-table))
         (logdmp (if date
                     (scat "tar xOf /qt/" qtid "/log_archive/"
                           host "-" qtid "-" date ".tar.gz "
                           host "-" qtid "-" date "/web/web.log")
                     (scat "cat /qt/" qtid "/web/web.log")))
         (sshproc (run-remote-cmd (scat *qt-ssh-user* "@" host) logdmp)))
    (with-open-stream (s (process-output sshproc))
      (do ((line (read-line s nil) (read-line s nil)))
          ((null line))
        (when (scan "^[0-9]*\\t[0-9]*\\t[0-9]*" line)
          (apply #'hash-record (append (parse-record line) `(,accumulator))))))
    (process-close (process-wait sshproc t))
    (calc-all accumulator)))


(defun sample (from to utbl)
  "Returns list of utilization percentages from utbl where [from < tstamp <= to]"
  (let (r)
    (do ((k (1+ from) (1+ k))) ((> k to))
      (let ((v (gethash k utbl))) (when v (push v r))))
    (if r r '(nd))))


(defun load-summary (all-util)
  "Returns a hashtable containing the 2-minute moving average of all-util
   computed at 30s intervals. <ts>:<2m_avg_%utilization_of_all_jservers>"
  (let ((range 120)
        (step 30)
        (startsec 18000)                ;05:00
        (endsec 61200)                  ;17:00
        (result (make-hash-table)))
    (do ((i startsec (+ i step))) ((> i endsec))
      (let (s n)
        (maphash #'(lambda (&rest x)
                     (setq s (append s (sample (- i range) i (cadr x)))))
                 all-util)
        (setq n (length (remove 'nd s)))
        (when (> n 0) (setf (gethash i result) (/ (apply #'+ (remove 'nd s)) n)))))
    result))


(defun ts-to-str (sec)
  "Converts seconds since midnight to HH:MM string"
  (multiple-value-bind (h r) (floor sec 3600)
    (with-output-to-string (z) (format z "~2,'0d:~2,'0d" h (round r 60)))))


(defun graph-rpt (host qtid stream &optional date)
  "Writes a graph in PNG format to stream based on qtid's web.log on host"
  (with-chart (:line 1400 400)
    (add-series
     (scat host "-" qtid ": All jservers referenced in " (if date date "today's") " web.log")
     (let (r) (maphash #'(lambda (k v) (push (list k v) r))
                       (if date (load-summary (scan-weblog host qtid date))
                           (load-summary (scan-weblog host qtid)))) r)
     :color '(.2 .6 .9))
    (set-axis :y "% Utilization"
              :draw-gridlines-p t
              :label-formatter "~,1,,,f")
    (set-axis :x nil
              :draw-gridlines-p t
              :label-formatter #'(lambda (k) (ts-to-str k))
              :data-interval 1800)
    (save-stream stream)))


(make-uri-dispatcher-and-handler jserver-graph
  (session-check jserver-graph)
  (no-cache)
  (setf (content-type*) "image/png")
  (let ((host (sanitize (get-parameter "h")))
        (id (sanitize (get-parameter "i")))
        (date (sanitize (get-parameter "d"))))
    (when (and (scan *qt-hostname-regex* host) (scan "^[0-9]{4}$" id))
      (flexi-streams:with-output-to-sequence (s)
        (if (and (stringp date) (scan "^[0-9]{8}$" date))
            (graph-rpt host id s date)
            (graph-rpt host id s))))))


(defun last30days ()
  (let (r (now (get-universal-time)))
    (dotimes (i 30)
      (multiple-value-bind (s m h day month year)
          (decode-universal-time (- now (* (1+ i) 86400)))
        (declare (ignore s m h))
        (push (with-output-to-string (s)
                (format s "~d~2,'0d~2,'0d" year month day)) r)))
    (cons "Today" (reverse r))))


(make-uri-dispatcher-and-handler jserver-utilization
  (session-check jserver-utilization)
  (let ((host (sanitize (get-parameter "h")))
        (id (sanitize (get-parameter "i")))
        (date (sanitize (get-parameter "d"))))
    (make-xarf-html-screen
        (:title "QT Jserver Utilization" :footmenu *footer-menu*)
      ((:form :method "get" :name "hidform")
       ((:table)
        ((:tr)
         ((:td) "Host: "
          ((:select :name "h")
           (dolist (i (qt-nodes 'prod))
             (htm ((:option :value i :selected (string= i host)) (str i))))))
         ((:td) "App Instance ID: "
          ((:select :name "i")
           (dolist (i (qt-ids 'prod))
             (htm ((:option :value i :selected (string= i id))
                   (str i) " - " (str (qt-client-name i)))))))
         ((:td) "Date: "
          ((:select :name "d")
           (dolist (i (last30days))
             (htm ((:option :value i :selected (string= i date)) (str i))))))
         ((:td) (:input :type "submit" :value "Generate Report")))))
      (when (and host id)
        (htm (:img :alt "web.log retrieval and analysis may take several seconds"
                   :src (scat "/jserver-graph?h=" host "&i=" id "&d=" date)))))))
