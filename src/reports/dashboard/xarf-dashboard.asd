(in-package :asdf-user)

(defsystem :xarf-dashboard
  :depends-on (:xarf)
  :serial t
  :components
  ((:file "packages")
   (:file "conf")
   (:file "dashboard")))
