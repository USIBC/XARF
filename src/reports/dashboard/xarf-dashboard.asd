(in-package :asdf-user)

(defsystem :xarf-dashboard
  :depends-on (:xarf :xarf-quicktime :xarf-webta)
  :serial t
  :components
  ((:file "packages")
   (:file "conf")
   (:file "dashboard")))
