;;; db.lisp -- XARF "database"
;;; D. Racine, 20070720

(in-package :xarf)

;; ====< Users in LMDB >====

(defstruct user id name email roles pass pass-change-time previous-passwords)

(defun str2vec (s) (trivial-utf-8:string-to-utf-8-bytes s))

(defun vec2str (v) (when v (trivial-utf-8:utf-8-bytes-to-string v)))

(defmacro with-userdb ((d) &body body)
  "See http://borretti.me/article/lmdb-from-common-lisp"
  (alexandria:with-gensyms (env txn)
    `(let ((,env (lmdb:make-environment (scat *xarf-home* "data/lmdb/"))))
       (lmdb:with-environment (,env)
         (let ((,txn (lmdb:make-transaction ,env)))
           (lmdb:begin-transaction ,txn)
           (let ((,d (lmdb:make-database ,txn "users")))
             (lmdb:with-database (,d)
               (prog1 (progn ,@body) (lmdb:commit-transaction ,txn)))))))))

(defun userdb-put (keystr valstr)
  (with-userdb (x) (lmdb:put x (str2vec keystr) (str2vec valstr))))

(defun userdb-get (keystr)
  (vec2str (with-userdb (x) (lmdb:get x (str2vec keystr)))))

(defun userdb-del (keystr)
  (with-userdb (x) (lmdb:del x (str2vec keystr))))

(defun userdb-dump ()
  (let (res)
    (with-userdb (x) (lmdb:do-pairs (x k v)
                       (pushnew (cons (vec2str k) (vec2str v)) res :test #'equal)))
    res))

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
  (sort (mapcar #'read-from-string (mapcar #'cdr (userdb-dump)))
        #'string-lessp :key #'(lambda (x) (symbol-name (user-id x)))))

(defun get-user-record (uid)
  "Returns uid's user structure"
  (let ((s (userdb-get (suid2str uid)))) (when s (read-from-string s))))

;; U:
(defun modify-user (uid &key name email (roles nil roles-supplied) pass pass-ctime prev-pws)
  "Replaces uid's record with updates to the specified fields."
  (let ((target (get-user-record uid)))
    (cond
      ((null target) nil)
      (t (when name (setf (user-name target) name))
         (when email (setf (user-email target) email))
         (when roles-supplied (setf (user-roles target) roles))
         (when pass (setf (user-pass target) pass))
         (when pass-ctime (setf (user-pass-change-time target) pass-ctime))
         (when prev-pws (setf (user-previous-passwords target) prev-pws))
         (userdb-put (suid2str uid) (prin1-to-string target))))))

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
