;;; config.lisp -- Top-level config items for XARF
;;; D. Racine, 20070511

(in-package :xarf)

;; Absolute path to the directory where XARF is installed. Include trailing '/':
(defparameter *xarf-home* (format nil "~A/" (posix-getenv "XARF_HOME")))


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

;; Base URL of XARF -- used to construct emailed URLs.
;; Exclude trailing '/':  https://name.domain[:port]
(defparameter *base-url* "https://localhost:19999")


;; Menu definitions ((label url [submenu]) ...)
(defparameter *xarf-menu*
  '(("XARF Menu"
     "/xarf"
     (("PPSD Dashboard" "/")
      ("Quicktime"
       "/menu?k=Quicktime"
       (("Active User Counts" "/qtusercount")
        ("app_cfg Viewer" "/qtappcfg")
        ("App-tier Status Summary" "/qstatus")
        ("Database Status" "/indirect?u=/qt-dbstatus&t=Quicktime+DB+Status")
        ("Instance Directory" "/indirect?u=/qt-directory&t=QT+Instance+Directory")
        ("Jserver Utilization" "/jserver-utilization")
        ("List Import Data" "/listimports")
        ("Load & Performance" "/qt-load-graphs")
        ("Timesheet Status" "/ts-status")
        ("Trans/Amend Extractor" "/webtrans")))
      ("StarTeam" "/streports")
      ("WebTA"
       "/menu?k=WebTA"
       (("Active User Counts" "/wtusercount")
        ("App-tier Status Summary" "/wtstatus")
        ("Bulk Groovy Reports" "/bulkgroovy")
        ("Database Status" "/indirect?u=/wt-dbstatus&t=WebTA+DB+Status")
        ("Instance Directory" "/indirect?u=/wt-directory&t=WebTA+Instance+Directory")
        ("Load & Performance" "/wt-load-graphs")
        ("Timesheet Status" "/wt-ts-status")))
      ("Change Password" "/passwd")
      ("Administration" "/admin")
      ("About the XARF" "/about")
      ("Log Out" "/logout")))))


(defparameter *footer-menu*
  '(("Top" "#") ("XARF Menu" "/xarf") ("PPSD Dashboard" "/") ("Log Out" "/logout")))


;; Login screen footer:
(defparameter *helpmsg* "For assistance contact the help desk at 555-1234 or <a href='mailto:help_addr@nospam.gov'>help_addr@nospam.gov</a>")

;; :html5, :xml, or :sgml
(setf (html-mode) :html5)
