;;; config.lisp -- Top-level config items for XARF
;;; D. Racine, 20070511

(in-package :xarf)

;; Absolute path to the directory where XARF is installed. Include trailing '/':
(defparameter *xarf-home* (format nil "~A/" (posix-getenv "XARF_HOME")))


;; Base URL of XARF for emailed links. https://name[:port] (exclude trailing '/')
(defparameter *base-url* "https://localhost:19999")


;; Log files:
(defparameter *access-log* (format nil "~Alog/access" *xarf-home*))
(defparameter *message-log* (format nil "~Alog/messages" *xarf-home*))


;; Directory where XARF's static content resides:
(defparameter *static-dir* (format nil "~Astatic/" *xarf-home*))


;; Sessions:
(defparameter *use-remote-addr-for-sessions* nil)
(defparameter *use-user-agent-for-sessions* t)
(defparameter *session-max-time* 14400) ;seconds


;; Passwords:
(defparameter *max-password-age* 60)    ;days
(defparameter *password-warn* 7)        ;days
(defparameter *max-previous-passwords* 8)
(defparameter *min-password-length* 8)


;; Email sent by XARF:
(defparameter *relay-host* "relay")
(defparameter *from-address* "XARF_noreply@nospam.gov")


;; Menu definitions (uri name [submenu]):
(defparameter *xarf-menu*
  '(("/" "Main Menu"
     (("/"
       "Quicktime"
       (("/qtappcfg" "app_cfg Viewer")
        ("/qstatus" "App-tier Status Summary")
        ("/indirect?u=/qt-dbstatus&t=Quicktime+DB+Status" "Database Status")
        ("/indirect?u=/qt-directory&t=QT+Instance+Directory" "Instance Directory")
        ("/jserver-utilization" "Jserver Utilization")
        ("/listimports" "List Import Data")
        ("/qt-load-graphs" "Load & Performance")
        ("/ts-status" "Timesheet Status")
        ("/webtrans" "Trans/Amend Extractor")))
      ("/streports" "StarTeam")
      ("/"
       "WebTA"
       (("/wtusercount" "Active User Counts")
        ("/wtstatus" "App-tier Status Summary")
        ("/bulkgroovy" "Bulk Groovy Reports")
        ("/indirect?u=/wt-dbstatus&t=WebTA+DB+Status" "Database Status")
        ("/indirect?u=/wt-directory&t=WebTA+Instance+Directory" "Instance Directory")
        ("/wt-load-graphs" "Load & Performance")
        ("/wt-ts-status" "Timesheet Status")))
      ("/passwd" "Change Password")
      ("/admin" "Administration")
      ("/about" "About the XARF")
      ("/logout" "Log Out")))))


(defparameter *footer-menu* '(("#" "Top") ("/" "Main Menu") ("/logout" "Log Out")))


;; Login screen footer:
(defparameter *helpmsg* "For assistance contact the help desk at 555-1234 or <a href='mailto:help_addr@nospam.gov'>help_addr@nospam.gov</a>")

;; :html5, :xml, or :sgml
(setf (html-mode) :html5)
