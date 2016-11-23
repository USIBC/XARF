;;; db.lisp -- XARF "database"
;;; D. Racine, 20070720

(in-package :xarf)

;; ====< Users >====

(defstruct user id name email roles pass pass-change-time previous-passwords)

(defparameter *xarf-user-roles* '(admin executive))

(defparameter *userdb-mutex* (make-mutex :name "userdb-lock"))

(defparameter *xarf-users*
  (with-open-file (s (scat *xarf-home* "data/users") :direction :input)
    (read s nil)))

;; C:
(defun add-user (&key name uid pass roles email)
  (when (and name uid pass)
    (with-mutex (*userdb-mutex*)
      (setq *xarf-users*
            (sort (cons (make-user :id uid :name name :email email
                                   :roles roles :pass pass :pass-change-time 0)
                        *xarf-users*)
                  #'string-lessp :key #'(lambda (x) (symbol-name (user-id x)))))
      (with-open-file (s (scat *xarf-home* "data/users")
                         :direction :output :if-exists :supersede)
        (prin1 *xarf-users* s)))))


;; R:
(defun get-all-users ()
  "Returns a copy of the current user list."
  (with-mutex (*userdb-mutex*) *xarf-users*))

(defun get-user-record (uid)
  "Returns a copy of uid's user structure"
  (find uid (get-all-users) :key #'user-id :test #'eq))


;; U:
(defun modify-user (uid &key name email (roles nil roles-supplied) pass pass-ctime prev-pws)
  "Replaces uid's record in *xarf-users* with updates to the specified fields."
  (let ((target (get-user-record uid)))
    (cond
      ((null target) nil)
      (t
       (when name (setf (user-name target) name))
       (when email (setf (user-email target) email))
       (when roles-supplied (setf (user-roles target) roles))
       (when pass (setf (user-pass target) pass))
       (when pass-ctime (setf (user-pass-change-time target) pass-ctime))
       (when prev-pws (setf (user-previous-passwords target) prev-pws))
       (with-mutex (*userdb-mutex*)
         (setq *xarf-users*
               (sort (cons target (remove uid *xarf-users* :key #'user-id :test #'eq))
                     #'string-lessp :key #'(lambda (x) (symbol-name (user-id x)))))
         (with-open-file (s (scat *xarf-home* "data/users")
                            :direction :output :if-exists :supersede)
           (prin1 *xarf-users* s)))))))


;; D:
(defun remove-users (idlist)
  (with-mutex (*userdb-mutex*)
    (dolist (i idlist)
      (setq *xarf-users* (remove i *xarf-users* :key #'user-id :test #'eq)))
    (with-open-file (s (scat *xarf-home* "data/users")
                       :direction :output :if-exists :supersede)
      (prin1 *xarf-users* s))))

;; ====< End Users >====


;; ====< Messages >====

(defparameter *xarf-messages*
  (with-open-file (s (scat *xarf-home* "data/messages") :direction :input)
    (read s nil)))

(defun get-msg (msg-id)
  (or (cdr (assoc msg-id *xarf-messages* :test #'string=)) ""))

;; ====< End Messages >====
