;;; dashboard.lisp -- 'PPSD Dashboard' website
;;; D. Racine 20170908

(in-package :xarf-dashboard)


(setq *reports-menu* (stable-sort (append *reports-menu* *menu*)
                                  #'string-lessp :key #'car))


(define-easy-handler (index :uri "/") nil
  (make-xarf-html-screen (:title "PPSD Dashboard" :no-title-menu t)
    (html-menu "nav" *dash-menu*)
    (:div :class "flatmenu" (html-menu "flat" *dash-menu*))))


(make-uri-dispatcher-and-handler dm
  (let* ((key (sanitize (get-parameter "k")))
         (title-prefix (sanitize (get-parameter "p")))
         (i (find-menu-item  key *dash-menu*))
         (m (if i (list i) *dash-menu*)))
    (make-xarf-html-screen (:title "PPSD Dashboard" :no-title-menu t)
      (html-menu "nav" *dash-menu*)
      (:h4 (str title-prefix) " " (str key) " Submenu")
      (:div :class "flatmenu" (html-menu "flat" m)))))
