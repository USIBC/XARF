;;; server.lisp -- XARF web server, URIs, & dispatchers
;;; D. Racine, 20070511


;; Ensure hunchentoot worker threads inherit their caller's package binding
;; ref. https://stackoverflow.com/a/18244557
(in-package :hunchentoot)
(defmethod start-thread ((taskmaster one-thread-per-connection-taskmaster)
                         thunk &key name)
  (let* ((package-name (package-name *package*)) ;calling thread's current package
         (initial-bindings `((*package* . (find-package ,package-name)))))
    (bt:make-thread thunk :name name :initial-bindings initial-bindings)))


(in-package :xarf)

(defparameter *ssl-cert-file* (s+ *xarf-home* "ssl/cert.pem"))

(defparameter *ssl-privkey-file* (s+ *xarf-home* "ssl/privatekey.pem"))


(defun create-authenticated-folder-dispatcher-and-handler
    (uri-prefix base-path &optional content-type)
  "This is a knock-off of hunchentoot's create-folder-dispatcher-and-handler
   that does not serve content unless a session exists."
  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (parameter-error "~S must be string ending with a slash." uri-prefix))
  (unless (fad:directory-pathname-p base-path)
    (parameter-error "~S is supposed to denote a directory." base-path))
  (flet ((handler ()
           (session-check handler)
           (let ((request-path (request-pathname *request* uri-prefix)))
             (when (null request-path)
               (setf (return-code*) +http-forbidden+)
               (abort-request-handler))
             (handle-static-file (merge-pathnames request-path base-path) content-type))))
    (create-prefix-dispatcher uri-prefix #'handler)))


(push (create-folder-dispatcher-and-handler "/static/" *static-dir*) *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/favicon.ico" (s+ *xarf-home* "static/images/favicon.ico")
       "image/x-icon") *dispatch-table*)

(push (create-authenticated-folder-dispatcher-and-handler
       "/authstat/" (format nil "~Aauthstat/" *xarf-home*)) *dispatch-table*)

(defparameter *xarf-acceptor* nil)

(defun start-xarf ()
  (setq *xarf-acceptor*
        (start (make-instance 'easy-ssl-acceptor
                              :port (parse-integer (posix-getenv "XARF_WEB_PORT"))
                              :ssl-certificate-file *ssl-cert-file*
                              :ssl-privatekey-file *ssl-privkey-file*
                              :access-log-destination *access-log*
                              :message-log-destination *message-log*))))

(defun clean-quit ()
  (stop *xarf-acceptor* :soft t) (sleep 1) (lmdb:close-environment *lmdb-env*) (exit))
