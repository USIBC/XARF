;;; dash-config.lisp -- configuration definitions for use by the
;;; the PPSD dashboard report in dashboard.lisp.
;;; D. Racine 20170908

(in-package :xarf)


;; PPSD Dashboard menu ((label url [submenu]) ...)
(defparameter *dash-menu*
  '(("PPSD Dashboard Menu"
     "/"
     (("Quicktime"
       "/dm?k=Quicktime"
       (("Test" "/qttest-d")
        ("Production" "/qtprod-d")))
      ("WebTA"
       "/dm?k=WebTA"
       (("Emergency Test" "/wtemtest-d")
        ("Next Release Test" "/wtnrtest-d")
        ("Production" "/wtprod-d")
        ("Training" "/wttrn-d")))
      ("Miscellaneous"
       "/dm?k=Miscellaneous"
       (("Something" "https://something-url.gov/")
        ("Something Else" "https://something-else-url.gov/")))
      ("XARF" "/xarf")))))
