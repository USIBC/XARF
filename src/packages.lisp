(in-package :cl-user)

(defpackage :xarf
  (:use :cl :hunchentoot :cl-who :cl-ppcre :sb-ext :sb-thread :cl-smtp :cl-pass)
  (:export :*xarf-home*
           :*static-dir*
           :*reports-menu*
           :*footer-menu*
           :s+
           :lst2str
           :find-menu-item
           :html-menu
           :make-xarf-html-screen
           :make-uri-dispatcher-and-handler
           :pull-static-file
           :sanitize
           :stringify-universal-time
           :timestr
           :run-remote-cmd
           :with-pkg
           :remote-slurp
           :hoverlink
           :session-check
           :role-check
           :clean-quit))
