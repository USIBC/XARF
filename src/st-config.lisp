;;; st-config.lisp -- configuration definitions for use by
;;; XARF's StarTeam-backed report browser in 'starteam.lisp'.
;;; D. Racine 20160127

(in-package :xarf)


;; *comment-map* defines strings that should be displayed next to file names in
;; the list of html files displayed by /streports. Format:
;; ( (regex-pattern . comment-string) ... )

(defparameter *comment-map*
  '(("Estimated_Hours"
     . "CCB Separated Est Hrs Report")

    ("FPPS-QT"
     . "TimeInStatusReports\\OpenTasks.html")

    ("WebTA_Open_SARs"
     . "TimeInStatusReports\\OpenSARs-WebTA.html")

    ("WebTA_Open_Tasks"
     . "TimeInStatusReports\\OpenTasks-WebTA.html")))
