;;; starteam.lisp
;;; Front-end to browse/view the output of '$XARF_HOME/STreports/run'
;;; D. Racine 20151217

(in-package :xarf)

(defparameter *streports-outdir* (scat *xarf-home* "STreports/out/"))


(defun getcomment (filename)
  "Returns the first comment-string in *comment-map* whose pattern matches filename"
  (cdr (find-if #'(lambda (r) (scan r filename)) *comment-map* :key #'car)))


(defun pathtail (namestr)
  "Returns the last field of a slash-delimited string"
  (car (last (split "/" namestr))))


(make-uri-dispatcher-and-handler streports
  (session-check streports)
  (let ((date (sanitize (get-parameter "d")))
        (rpt (sanitize (get-parameter "r")))
        outdirs rptfiles)
    (cond
      ((and date rpt (scan "^[0-9]{8}$" date))
       (setf (header-out :Content-Disposition) (scat "inline; filename=" rpt))
       (handle-static-file (scat *streports-outdir* date "/" rpt)))
      ((and date (null rpt) (scan "^[0-9]{8}$" date))
       (setq rptfiles
             (mapcar #'pathtail
                     (mapcar #'namestring
                             (cl-fad:list-directory (scat *streports-outdir* date))))))
      (t (setq outdirs
               (sort (mapcar #'pathtail
                             (mapcar #'namestring (cl-fad:list-directory *streports-outdir*)))
                     #'string-greaterp))))
    (when (or outdirs rptfiles)
      (make-xarf-html-screen
          (:title "StarTeam Reports" :footmenu *footer-menu*)
        (if rptfiles
            (htm                        ;list of report file links
             ((:h4) "Reports extracted from StarTeam on " (str date) ":")
             ((:table :class "qtdir")
              (dolist (i rptfiles)
                (htm ((:tr)
                      ((:td)
                       ((:a :class "qturl"
                            :href (scat "/streports?d=" date "&r=" i)) (str i)))
                      ((:td :style "font:10pt sans-serif") (str (getcomment i))))))))
            (htm                        ;list of date directory links
             ((:table :class "qtdir")
              (dolist (i outdirs)
                (htm ((:tr)
                      ((:td)
                       ((:a :class "qturl" :href (scat "/streports?d=" i)) (str i)))))))))))))
