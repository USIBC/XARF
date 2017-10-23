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

(defparameter *static-uri* "/static/")

(defparameter *ssl-cert-file* (scat *xarf-home* "ssl/cert.pem"))

(defparameter *ssl-privkey-file* (scat *xarf-home* "ssl/privatekey.pem"))

(push (create-folder-dispatcher-and-handler *static-uri* *static-dir*) *dispatch-table*)

(push (create-static-file-dispatcher-and-handler
       "/favicon.ico" (scat *xarf-home* "static/images/favicon.ico")
       "image/x-icon") *dispatch-table*)

(defparameter *xarf-acceptor*
  (start (make-instance 'easy-ssl-acceptor
                        :port (parse-integer (posix-getenv "XARF_WEB_PORT"))
                        :ssl-certificate-file *ssl-cert-file*
                        :ssl-privatekey-file *ssl-privkey-file*
                        :access-log-destination *access-log*
                        :message-log-destination *message-log*)))

(defun clean-quit () (stop *xarf-acceptor* :soft t) (sleep 1) (exit))
