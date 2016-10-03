;;; main.lisp -- Main XARF screens & related components
;;; D. Racine, 20070511

(in-package :xarf)


(defparameter *reset-tbl* (make-hash-table :test 'equal))


(defun password-age (uid)
  "Returns the # of days since the user's password was changed."
  (let ((last-change-time (user-pass-change-time (get-user-record uid)))
        (current-time (get-universal-time))
        (seconds-per-day 86400))
    (/ (- current-time last-change-time) seconds-per-day)))


(defmacro session-check (blockname &key skip-passwd-redirect)
  (if skip-passwd-redirect
      `(when (null *session*)
         (redirect "/login?msg=1")
         (return-from ,blockname))
      `(cond
         ((null *session*)
          (redirect "/login?msg=1")
          (return-from ,blockname))
         ((> (password-age (session-value 'uid)) *max-password-age*)
          (redirect "/passwd?msg=15")
          (return-from ,blockname))
         (t nil))))


(defmacro role-check (blockname &key required-role-list (refusal-destination "/"))
  `(unless (subsetp ,required-role-list
                    (user-roles (get-user-record (session-value 'uid))))
     (redirect ,refusal-destination)
     (return-from ,blockname)))


(defun check-credentials (id pass)
  "Returns user struct if valid, nil otherwise."
  (let ((target (get-user-record id)))
    (cond
      ((null target) nil)
      ((check-password pass (user-pass target)) target)
      (t nil))))


(define-easy-handler (index :uri "/") (msg)
  (session-check index)
  (make-xarf-html-screen (:title "XARF Main Menu" :footmenu *footer-menu*)
    (and msg (htm (:div :class "msg" (fmt "~a" (get-msg (sanitize msg))))))
    (:div :class "flatmenu" (menu "flat" *xarf-menu*))))


(make-uri-dispatcher-and-handler about
  (session-check about)
  (make-xarf-html-screen
      (:title "About the XARF" :footmenu *footer-menu*)
    (fmt "~a" (pull-static-file (scat *xarf-home* "static/about.html")))
    ((:h3 :style "text-align:left") "Change Log:")
    ((:pre)
     (fmt "~a" (pull-static-file (scat *xarf-home* "static/CHANGELOG"))))))


(make-uri-dispatcher-and-handler login
  (let ((uid (sanitize (post-parameter "uid")))
        (pass (post-parameter "pass"))
        (msg (sanitize (get-parameter "msg"))))
    (when (and uid pass)
      (setf uid (str2suid uid))
      (cond
        ((check-credentials uid pass)
         (start-session) (setf (session-value 'uid) uid)
         (log-message* :info "~a login from ~a" uid (real-remote-addr))
         (if (< (- *max-password-age* *password-warn*) (password-age uid) *max-password-age*)
             (redirect "/?msg=14")
             (redirect "/"))
         (return-from login))
        (t (setq msg "2"))))
    (make-xarf-html-screen
        (:title "XARF Login" :no-title-menu t)
      (and msg (htm (:div :class "msg" (fmt "~a" (get-msg msg)))))
      ((:form :id "login" :method "post" :action "/login")
       ((:label) "User ID") (:input :type "text" :name "uid" :size 20) (:br)
       ((:label) "Password") (:input :type "password" :name "pass" :size 20) (:br)
       (:input :type "submit" :value "Log In"))
      ((:div :class "msg2") ((:a :href "/reqreset") "Request a password reset email"))
      ((:div :class "msg2") (fmt "~a" *helpmsg*)))))


(make-uri-dispatcher-and-handler reset
  (let ((sid (sanitize (get-parameter "i"))) req)
    (cond
      ((not (setq req (gethash sid *reset-tbl*)))
       (redirect "/") (return-from reset))
      ((> (- (get-universal-time) (cadr req)) 7200)
       (remhash sid *reset-tbl*) (redirect "/login?msg=22") (return-from reset))
      (t
       (modify-user (car req) :pass-ctime 0)
       (start-session) (setf (session-value 'uid) (car req)) (setf (session-value 'reset) t)
       (log-message* :info "~a password reset login from ~a" (car req) (real-remote-addr))
       (remhash sid *reset-tbl*)
       (redirect "/passwd?msg=15")))))


(make-uri-dispatcher-and-handler reqreset
  (let ((uid (sanitize (post-parameter "uid")))
        (msg (sanitize (get-parameter "msg")))
        urec sid)
    (when uid
      (setq uid (str2suid uid))
      (cond
        ((not (setq urec (get-user-record uid)))
         (redirect "/reqreset?msg=20"))
        ((not (scan "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+" (user-email urec)))
         (redirect "/reqreset?msg=21"))
        (t
         (setq sid (sha256hash (scat (format nil "~x" (random 10000000)) (user-email urec))))
         (setf (gethash sid *reset-tbl*) (list uid (get-universal-time)))
         (send-email *relay-host* *from-address* (user-email urec)
                     "XARF password reset URL" (scat *base-url* "/reset?i=" sid))
         (redirect "/reqreset?msg=19")))
      (return-from reqreset))
    (make-xarf-html-screen
        (:title "XARF Password Reset" :no-title-menu t)
      (and msg (htm (:div :class "msg" (fmt "~a" (get-msg msg)))))
      ((:form :id "login" :method "post" :action "/reqreset")
       ((:label) "User ID") (:input :type "text" :name "uid" :size 20) (:br)
       (:input :type "submit" :value "Request reset email")))))


(make-uri-dispatcher-and-handler logout
  (and *session* (remove-session *session*))
  (redirect "/login?msg=3"))


(make-uri-dispatcher-and-handler passwd
  (session-check passwd :skip-passwd-redirect t)
  (let* ((uid (session-value 'uid))
         (cpass (post-parameter "cpass"))
         (npass (post-parameter "npass"))
         (npass2 (post-parameter "npass2"))
         (msg (sanitize (get-parameter "msg")))
         (urec (get-user-record uid)))
    (when (and npass npass2)
      (cond
        ((< (length npass) *min-password-length*)
         (setq msg "16"))
        ((not (string= npass npass2))
         (setq msg "17"))
        ((or (check-password npass (user-pass urec))
             (find-if #'(lambda (x) (check-password npass x)) (user-previous-passwords urec)))
         (setq msg "18"))
        ((or (session-value 'reset) (check-credentials uid cpass))
         (modify-user uid :pass (hash npass) :pass-ctime (get-universal-time)
                      :prev-pws (passbuffer (user-pass urec) (user-previous-passwords urec)))
         (setf (session-value 'reset) nil)
         (redirect "/?msg=4")
         (return-from passwd))
        (t
         (setq msg "5"))))
    (make-xarf-html-screen
        (:title "Change Password" :footmenu *footer-menu*)
      (and msg (htm (:div :class "msg" (fmt "~a" (get-msg msg)))))
      ((:form :id "login" :method "post" :action "/passwd")
       ((:label) "Changing password of '" (fmt "~a" (suid2str uid)) "':") (:br)
       (unless (session-value 'reset)
         (htm ((:label) "Current password")
              (:input :type "password" :name "cpass" :size 20) (:br)))
       ((:label) "New password") (:input :type "password" :name "npass" :size 20) (:br)
       ((:label) "New password (again)") (:input :type "password" :name "npass2" :size 20) (:br)
       (:input :type "submit" :value "Change Password")))))


(make-uri-dispatcher-and-handler indirect
  (session-check indirect)
  (let ((uri (sanitize (get-parameter "u")))
        (title (sanitize (get-parameter "t"))))
    (if (or (not uri) (scan "indirect" uri))
        (redirect "/")
        (make-xarf-html-screen
            (:title title :footmenu *footer-menu* :redir-uri uri)
          (:h4 (fmt "~a" "Gathering report data..."))
          (:h4 (:img :src "/static/images/wait.gif"))))))
