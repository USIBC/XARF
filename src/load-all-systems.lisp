;;; D. Racine 20190305

(in-package :cl-user)

(require :asdf)

(asdf:initialize-source-registry
 (merge-pathnames (concatenate 'string (sb-ext:posix-getenv "XARF_HOME") "/asdf.conf")))

(defun de-tail (seq n) (subseq seq 0 (- (length seq) n)))

;; Find and load ASDF systems in src/reports/:
(let* ((paths (directory (concatenate 'string
                                      (sb-ext:posix-getenv "XARF_HOME")
                                      "/src/reports/*/*.asd")))
       (names (mapcar (lambda (x) (de-tail (file-namestring x) 4)) paths))
       (keywords (mapcar (lambda (x) (intern (string-upcase x) :keyword)) names)))
  (dolist (i keywords) (require i)))
