;;; admin.lisp -- XARF admin screens
;;; D. Racine, 20070523

(in-package :xarf)

(make-uri-dispatcher-and-handler admin
  (session-check admin)
  (role-check admin :required-role-list '(admin) :refusal-destination "/xarf?msg=10")
  (let ((fn (sanitize (post-parameter "fn")))
        (name (sanitize (post-parameter "name")))
        (eml (sanitize (post-parameter "eml")))
        (uid (sanitize (post-parameter "uid")))
        (pass (post-parameter "pass"))
        (pass2 (post-parameter "pass2"))
        (msg (sanitize (get-parameter "msg"))))
    (when fn
      (cond
        ((and (string= fn "adduser") (> (length name) 0) (> (length uid) 0)
              (> (length pass) 0) (string= pass pass2)
              (not (find (str2suid uid) (get-all-users) :key #'user-id :test #'eq)))
         (add-user :name name :uid (str2suid uid) :pass (hash pass) :email eml)
         (setq msg "6"))
        ((string= fn "rmusers")
         (let ((rm-uids
                (mapcar #'cdr
                        (remove "ruser" (post-parameters*) :test-not #'string= :key #'car))))
           (cond
             ((> (length rm-uids) 0)
              (remove-users (mapcar #'str2suid rm-uids)) (setq msg "7"))
             (t (setq msg "8")))))
        (t (setq msg "9"))))
    (make-xarf-html-screen
        (:title "XARF Administration" :footmenu *footer-menu*)
      (and msg (htm (:div :class "msg" (fmt "~a" (get-msg msg)))))
      ((:table)
       ((:tr :height "10%")
        ((:td) ((:form :method "post" :action "/admin")
                ((:table)
                 ((:tr) (:th :colspan "2" "Add New User"))
                 ((:tr) (:td "Full Name:")
                  (:td (:input :type "text" :name "name" :size 22)))
                 ((:tr) (:td "Email Address:")
                  (:td (:input :type "text" :name "eml" :size 30)))
                 ((:tr) (:td "User ID:")
                  (:td (:input :type "text" :name "uid" :size 14)))
                 ((:tr) (:td "Initial password:")
                  (:td (:input :type "password" :name "pass" :size 14)))
                 ((:tr) (:td "Initial password (again):")
                  (:td (:input :type "password" :name "pass2" :size 14)))
                 ((:tr) (:td (:input :type "hidden" :name "fn" :value "adduser"))
                  (:td (:input :type "submit" :value "Add User"))))))
        ((:td :rowspan "2")
         ((:form :method "post" :action "/admin")
          ((:table)
           ((:tr) (:th "Remove Existing Users"))
           (dolist (u (get-all-users))
             (htm ((:tr) (:td (:input :type "checkbox" :name "ruser"
                                      :value (suid2str (user-id u)))
                              (fmt "~a (~a)" (suid2str (user-id u)) (user-name u))))))
           ((:tr) (:td (:input :type "hidden" :name "fn" :value "rmusers")
                       (:input :type "submit" :value "Remove Selected Users")))))))
       ((:tr)
        ((:td) ((:form :method "post" :action "/edit-user")
                ((:table)
                 ((:tr) (:th :colspan "3" "View/Modify existing user data"))
                 ((:tr) (:td "Select a user:")
                  (:td
                   ((:select :name "uid")
                    (dolist (u (get-all-users))
                      (htm ((:option :value (suid2str (user-id u)))
                            (str (suid2str (user-id u))))))))
                  (:td (:input :type "submit" :value "View/Modify User")))))))))))


(defun role-checkboxes (uid)
  (with-html-output (*standard-output*)
    ((:table :class "layout")
     ((:tr) (dolist (r *xarf-user-roles*)
              (htm ((:td)
                    (:input
                     :type "checkbox"
                     :name "role"
                     :value (princ-to-string r)
                     :checked (find r (user-roles (get-user-record uid))))
                    (str r))))))))


(make-uri-dispatcher-and-handler edit-user
  (session-check edit-user)
  (role-check edit-user :required-role-list '(admin) :refusal-destination "/xarf?msg=10")
  (let* ((uid (sanitize (post-parameter "uid")))
         (newname (sanitize (post-parameter "newname")))
         (neweml (sanitize (post-parameter "neweml")))
         (npass (post-parameter "npass"))
         (npass2 (post-parameter "npass2"))
         (msg (sanitize (get-parameter "msg")))
         (user (if uid
                   (get-user-record (str2suid uid))
                   (progn (redirect "/admin") (return-from edit-user))))
         (newroles
          (mapcar #'intern
                  (mapcar #'cdr
                          (remove "role" (post-parameters*) :test-not #'string= :key #'car)))))
    (when (post-parameter "cancel") (redirect "/admin") (return-from edit-user))
    (when (and user (post-parameter "apply"))
      (when (string/= newname (user-name user))
        (modify-user (user-id user) :name newname)
        (setq msg "11"))
      (when (string/= neweml (user-email user))
        (modify-user (user-id user) :email neweml)
        (setq msg "11"))
      (when (not (equal newroles (user-roles user)))
        (modify-user (user-id user) :roles newroles)
        (setq msg "11"))
      (when (and (> (length npass) 0) (string= npass npass2))
        (modify-user (user-id user) :pass (hash npass) :pass-ctime 0)
        (setq msg "11")))
    (make-xarf-html-screen
        (:title "Edit User Data" :footmenu *footer-menu*)
      (and msg (htm (:div :class "msg" (fmt "~a" (get-msg msg)))))
      ((:form :method "post" :action "/edit-user")
       ((:table)
        ((:tr)(:th :colspan "2" "User data for login ID '" (fmt "~a" uid) "'"))
        ((:tr) (:td "Full name:")
         (:td (:input :type "text" :name "newname" :size 22
                      :value (if user (user-name user) ""))))
        ((:tr) (:td "Email Address:")
         (:td (:input :type "text" :name "neweml" :size 30
                      :value (if user (user-email user) ""))))
        ((:tr) (:td "Roles:")
         (:td (if user (role-checkboxes (user-id user)) "")))
        ((:tr) (:td "New password:")
         (:td (:input :type "password" :name "npass" :size 14)))
        ((:tr) (:td "New password (again):")
         (:td (:input :type "password" :name "npass2" :size 14)))  
        ((:tr) (:td (:input :type "hidden" :name "uid" :value uid)
                    (:input :type "submit" :name "apply" :value "Apply Changes"))
         (:td (:input :type "submit" :name "cancel" :value "Return to main admin screen"))))))))
