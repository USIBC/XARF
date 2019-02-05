;;; db.lisp -- XARF "database"
;;; D. Racine, 20070720

(in-package :xarf)

;; ====< Users in LMDB >====

(defstruct user id name email roles pass pass-change-time previous-passwords)

(defun str2vec (s) (trivial-utf-8:string-to-utf-8-bytes s))

(defun vec2str (v) (when v (trivial-utf-8:utf-8-bytes-to-string v)))

(defparameter *lmdb-env*
  (lmdb:open-environment (lmdb:make-environment (scat *xarf-home* "data/lmdb/"))))

(defmacro with-userdb ((d) &body body)
  "Perform a user DB transaction in *lmdb-env*"
  (alexandria:with-gensyms (txn)
    `(let ((,txn (lmdb:make-transaction *lmdb-env*)))
       (lmdb:begin-transaction ,txn)
       (let ((,d (lmdb:make-database ,txn "users")))
         (lmdb:with-database (,d)
           (prog1 (progn ,@body) (lmdb:commit-transaction ,txn)))))))

(defun userdb-put (keystr valstr)
  (when (plusp (length keystr))
    (with-userdb (x) (lmdb:put x (str2vec keystr) (str2vec valstr)))))

(defun userdb-get (keystr)
  (when (plusp (length keystr))
    (vec2str (with-userdb (x) (lmdb:get x (str2vec keystr))))))

(defun userdb-del (keystr)
  (when (plusp (length keystr))
    (with-userdb (x) (lmdb:del x (str2vec keystr)))))

(defun userdb-dump ()
  (let (r)
    (with-userdb (x) (lmdb:do-pairs (x k v)
                       (pushnew (cons (vec2str k) (vec2str v)) r :test #'equal)))
    (nreverse r)))

;; C:
(defun add-user (&key name uid pass roles email)
  (when (and name uid pass)
    (userdb-put
     (suid2str uid)
     (prin1-to-string
      (make-user :id uid :name name :email email :roles roles :pass pass :pass-change-time 0)))))
      
;; R:
(defun get-all-users ()
  "Reads user DB content into a list of user structures"
  (mapcar #'read-from-string (mapcar #'cdr (userdb-dump))))

(defun get-user-record (uid)
  "Returns uid's user structure"
  (let ((s (userdb-get (suid2str uid)))) (when s (read-from-string s))))

;; U:
(defun modify-user (uid &key
                          (name nil name-sup)
                          (email nil email-sup)
                          (roles nil roles-sup)
                          (pass nil pass-sup)
                          (pass-ctime nil pass-ctime-sup)
                          (prev-pws nil prev-pws-sup))
  "Replaces uid's record with updates to the specified fields."
  (let ((u (get-user-record uid)))
    (cond ((null u) nil)
          (t (when name-sup (setf (user-name u) name))
             (when email-sup (setf (user-email u) email))
             (when roles-sup (setf (user-roles u) roles))
             (when pass-sup (setf (user-pass u) pass))
             (when pass-ctime-sup (setf (user-pass-change-time u) pass-ctime))
             (when prev-pws-sup (setf (user-previous-passwords u) prev-pws))
             (userdb-put (suid2str uid) (prin1-to-string u))))))

;; D:
(defun remove-users (idlist)
  (dolist (i idlist) (userdb-del (suid2str i))))

;; ====< End Users >====


;; ====< Messages >====

(defparameter *xarf-messages*
  (with-open-file (s (scat *xarf-home* "data/messages") :direction :input)
    (read s nil)))

(defun get-msg (msg-id)
  (or (cdr (assoc msg-id *xarf-messages* :test #'string=)) ""))

;; ====< End Messages >====
