;;; primitives.lisp -- low-level convenience functions & macros for XARF
;;; D. Racine, 20070511

(in-package :xarf)

(defun str2suid (uidstr) (intern (string-upcase uidstr)))

(defun suid2str (suid) (string-downcase (symbol-name suid)))

(defmacro s+ (&rest args) `(concatenate 'string ,@args))

(defmacro lst2str (ctrlstr arglist)
  `(apply #'format (append (list nil ,ctrlstr) ,arglist)))


(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))


(defun xarf-menu ()
  `(("XARF Menu" "/xarf" ,(append *reports-menu* '(("---" "/xarf")) *meta-menu*))))


(defun find-menu-item (key menu)
  "Searches menu and returns the first item whose label matches key. A menu item
  has the form (label url [menu]). A menu is a list of one or more menu items."
  (or (find key menu :test #'string= :key #'car)
      (loop for i in (mapcar #'third menu)
         when (find-menu-item key i) return it)))


(defun html-menu (cssid menu)
  "Writes an html menu as a nested <ul id='cssid'> to stdout.
  menu should be of the form ((label url [menu]) ...)"
  (when menu
    (with-html-output (*standard-output*)
      (htm ((:ul :id cssid)
            (dolist (i menu)
              (htm ((:li) ((:a :href (second i)) (str (car i)))
                    (html-menu cssid (third i))))))))))


(defun html-footer (fmenu)
  "Writes an html footer containing fmenu to stdout.
  fmuenu should be of the form ((label url) ...)"
  (when fmenu
    (with-html-output (*standard-output*)
      ((:h5)
       (dolist (i fmenu)
         (htm ((:a :href (second i)) (str (car i)))))
       (:p "You are logged in as '"
           (fmt "~a" (suid2str (session-value 'uid))) "'")))))


(defmacro make-xarf-html-screen
    ((&key title no-title-menu footmenu (js "") redir-uri) &body content)
  (let ((tmenu (if no-title-menu nil '(html-menu "nav" (xarf-menu)))))
    `(progn
       (no-cache)
       (with-html
         (:html
          (:head
           (:title (fmt "~a" ,title))
           (:link :rel "stylesheet" :type "text/css" :href "/static/xarf.css")
           ((:script :type "text/javascript") (fmt "~a" ,js))
           ,(if redir-uri `(:meta
                            :http-equiv "refresh"
                            :content (s+ "0;url=" ,redir-uri))))
          (:body
           (:h1 ,tmenu (fmt "~a" ,title))
           ((:div :class "main")
            ,@content)
           (html-footer ,footmenu)))))))


(defmacro make-uri-dispatcher-and-handler (uri &body handler-fn-body)
  `(progn
     (defun ,uri ()
       ,@handler-fn-body)
     (push (create-prefix-dispatcher ,(format nil "/~(~a~)" uri) ',uri) *dispatch-table*)))


(defun pull-static-file (file)
  "Pull the contents of a file into a string."
  (with-open-file (s file)
    (let ((seq (make-string (file-length s))))
      (read-sequence seq s)
      seq)))


(defun sha256hash (str)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array str))))


(defun sanitize (str)
  (regex-replace-all "[\;\:\"\'\(\)\`\#\,\|\<\>]" str ""))


(defun tz-dst-map (tz dst-p)
  (let ((prefix (cdr (assoc tz '((5 . "E") (6 . "C") (7 . "M") (8 . "P")))))
        (suffix (if dst-p "DT" "ST")))
    (if prefix
        (s+ prefix suffix)
        (s+ "TZ" (princ-to-string tz) suffix))))


(defun stringify-universal-time (universal-time)
  "Decode a universal time to a local date/time string"
  (multiple-value-bind (sec min hour day month year weekday dst-p tz)
      (decode-universal-time universal-time)
    (with-output-to-string (s)
      (format s "~a ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
              (nth weekday '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
              year month day hour min sec
              (tz-dst-map tz dst-p)))))


(defun timestr ()
  "Return the current system date/time as a string."
  (stringify-universal-time (get-universal-time)))


(defun passbuffer (latest old-passwds)
  "Appends latest to the end of old-passwds list, drops the oldest if needed."
  (if (< (length old-passwds) *max-previous-passwords*)
      (append old-passwds (list latest))
      (append (nthcdr (1+ (- (length old-passwds) *max-previous-passwords*))
                      old-passwds)
              (list latest))))


(defun run-remote-cmd (usr@host rcmd &key (output :stream))
  "Returns a process structure running 'rcmd' as usr on host via ssh"
  (run-program
   "ssh" `("-nq" "-o ConnectTimeout=10" "-o BatchMode=yes" ,usr@host ,rcmd)
   :search t :wait nil :output output :error *message-log* :if-error-exists :append))


(defmacro with-pkg (package-keyword &body body)
  `(let ((*package* (find-package ,package-keyword)))
     ,@body))


(defmacro remote-slurp (usr@host rcmd &key (timeout 30) binary (pkg *package*))
  "Writes code to invoke rcmd as usr on host and return its output as a string or octet sequence"
  (let ((fn (if binary 'flexi-streams:with-output-to-sequence 'with-output-to-string))
        (tv (if binary #() "timeout")))
    `(join-thread
      (make-thread
       (lambda () (with-pkg ,pkg
                    (,fn (s) (process-close
                              (process-wait (run-remote-cmd ,usr@host ,rcmd :output s) t))))))
      :default ,tv :timeout ,timeout)))


(defun hoverlink (txt url)
  (with-html-output (*standard-output*)
    ((:a :class "qturl" :href url)
     (str txt)
     ((:span) (fmt "~a" url)))))
