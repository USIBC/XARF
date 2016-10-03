(in-package :asdf-user)

(defsystem :xarf
  :depends-on (:cl-who :cl-ppcre :hunchentoot :ironclad :adw-charting-vecto :cl-smtp :cl-pass)
  :components ((:file "packages")
               (:file "config" :depends-on ("packages"))
               (:file "primitives" :depends-on ("config"))
               (:file "db" :depends-on ("primitives"))
               (:file "main" :depends-on ("db"))
               (:file "admin" :depends-on ("main"))
               (:file "qt-config" :depends-on ("main"))
               (:file "quicktime" :depends-on ("qt-config"))
               (:file "jserver-util" :depends-on ("quicktime"))
               (:file "wt-config" :depends-on ("main"))
               (:file "webta" :depends-on ("wt-config"))
               (:file "st-config" :depends-on ("main"))
               (:file "starteam" :depends-on ("st-config"))
               (:file "server" :depends-on ("admin" "jserver-util" "webta" "starteam"))))
