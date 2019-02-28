(in-package :asdf-user)

(defsystem :xarf
  :depends-on (:cl-who
               :cl-ppcre
               :hunchentoot
               :ironclad
               :cl-smtp
               :cl-pass
               :lmdb)
  :serial t
  :components
  ((:file "packages")
   (:file "conf")
   (:file "primitives")
   (:file "db")
   (:file "main")
   (:file "admin")
   (:file "server")))
