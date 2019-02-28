;;; reports/dashboard/conf.lisp -- configuration definitions for use
;;; by the PPSD dashboard report in dashboard.lisp.
;;; D. Racine 20170908

(in-package :xarf-dashboard)

;; This is the link in the XARF menu to the PPSD Dashboard site:
(defparameter *menu* '(("PPSD Dashboard" "/")))


;; PPSD Dashboard menu ((label url [submenu]) ...)
(defparameter *dash-menu*
  '(("PPSD Dashboard Menu"
     "/"
     (("Miscellaneous"
       "/dm?k=Miscellaneous"
       (("Something" "https://something-url.gov/")
        ("Something Else" "https://something-else-url.gov/")))
      ("XARF" "/xarf")))))
