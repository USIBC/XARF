;;; dashboard.lisp -- 'PPSD Dashboard' website
;;; D. Racine 20170908

(in-package :xarf)

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


(defmacro ddir (title uri qt-or-wt id-select-fn typegetter-fn instancegetter-fn types)
  `(make-uri-dispatcher-and-handler ,uri
     (let ((ids (apply #'append (mapcar ,id-select-fn ,types))))
       (make-xarf-html-screen (:title ,title :no-title-menu t)
         (html-menu "nav" *dash-menu*)
         ((:table :class "qtdir")
          ((:tr) (:th "ID - Client") (:th "Type"))
          (dolist (i (sort ids (lambda (x y) (<= (parse-integer x) (parse-integer y)))))
            (let ((type (case (funcall ,typegetter-fn (funcall ,instancegetter-fn i))
                          (dev "Development")
                          (trn-dmo "Training/Demo")
                          (test-nr-auto "NxtRel Auto Test")
                          (test-nr-func "NxtRel Func Test")
                          (test-em-auto "Emerg Auto Test")
                          (test-em-func "Emerg Func Test")
                          (prod "Production")
                          (para "Parallel")
                          (otherwise "unknown"))))
              (htm ((:tr)
                    ((:td) (applink ,qt-or-wt i))
                    ((:td) (str type)))))))))))


(ddir "Quicktime Test" qttest-d
      qt #'qt-ids #'qt-instance-type #'get-qt-instance
      '(test-nr-auto test-nr-func test-em-auto test-em-func dev))


(ddir "Quicktime Production" qtprod-d
      qt #'qt-ids #'qt-instance-type #'get-qt-instance
      '(prod para trn-dmo))


(ddir "WebTA Emergency Test" wtemtest-d
      wt #'wt-ids #'wt-instance-type #'get-wt-instance
      '(test-em-auto test-em-func))


(ddir "WebTA Next Release Test" wtnrtest-d
      wt #'wt-ids #'wt-instance-type #'get-wt-instance
      '(test-nr-auto test-nr-func))


(ddir "WebTA Production" wtprod-d
      wt #'wt-ids #'wt-instance-type #'get-wt-instance
      '(prod para))


(ddir "WebTA Training" wttrn-d
      wt #'wt-ids #'wt-instance-type #'get-wt-instance
      '(trn-dmo))
