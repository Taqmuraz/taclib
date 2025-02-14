(in-package #:taclib)

(defmacro forhash (params hash &body body)
  `(maphash (lambda ,params ,@body) ,hash)
)

(defgeneric with-vals (v &rest kvs))

(defmethod with-vals ((v vector) &rest kvs)
  (let ((c (copy-seq v)))
    (loop for (k v) on kvs by #'cddr do (setf (aref c k) v))
    c
  )
)

(defmethod with-vals ((h hash-table) &rest kvs)
  (let ((r (make-hash-table)))
    (forhash (k v) h (setf (gethash k r) v))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun hash->str (h)
  (with-output-to-string (r)
    (format r "{")
    (forhash (k v) h (format r " ~A ~A" k v))
    (format r " }")
  )
)

(defun make-hash (&rest kvs)
  (let ((r (make-hash-table)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun merge-hash (&rest hs)
  (let ((r (make-hash-table)))
    (dolist (h hs) (forhash (k v) h (setf (gethash k r) v)))
    r
  )
)

(defun copy-hash (h)
  (let ((r (make-hash-table)))
    (forhash (k v) h (setf (gethash k r) v))
    r
  )
)

(defun hash-keys (h)
  (let ((r nil))
    (forhash (k v) h (declare (ignore v)) (setf r (nconc r (list k))))
    r
  )
)

(defun hash-vals (h)
  (let ((r nil))
    (forhash (k v) h (declare (ignore k)) (setf r (nconc r (list v))))
    r
  )
)

(defun hash->assoc (h)
  (let ((r nil))
    (forhash (k v) h (setf r (nconc r (list (cons k v)))))
    r
  )
)

(defun assoc->hash (a)
  (loop with r = (make-hash-table) for (k . v) in a
    do (setf (gethash k r) v)
    finally (return r)
  )
)

(defgeneric update (map func &rest keys))

(defmethod update ((v vector) func &rest keys)
  (let ((c (copy-seq v)))
    (dolist (k keys) (setf (aref c k) (funcall func (aref v k))))
    c
  )
)

(defmethod update ((h hash-table) func &rest keys)
  (loop for k in keys
    with r = (copy-hash h)
    do (multiple-value-bind (v p) (gethash k r)
      (if p
        (setf (gethash k r) (funcall func v))
      )
    )
    finally (return r)
  )
)

(defmacro defvec (name &rest fields)
  (let (
      (len (length fields))
      (accessors (mapcar (mpartial symbol-of name '-) fields))
      (ctor name)
    )
    `(progn
      (defun ,ctor ,fields (vector ,@fields))
      ,@(mapcan (lambda (a i)
        `(
          (defmacro ,a (v) (list 'aref v ,i))
          (defun ,(symbol-of 'f a) (v) (declare (type (simple-vector ,len) v)) (aref v ,i))
        )
      ) accessors (list-range len))
      t
    )
  )
)

(defun hash-accessor (key)
  (lambda (table) (declare (type hash-table table)) (gethash key table))
)

(defgeneric map-key (map key &optional not-found))

(defmethod map-key ((map hash-table) key &optional not-found)
  (multiple-value-bind (v h) (gethash key map) (if h v not-found))
)

(defmethod map-key ((map vector) key &optional not-found)
  (let ((l (length map))) (if (and (>= l 0) (< key l)) (aref map key) not-found))
)

(defmethod print-object ((h hash-table) stream)
  (format stream "~A" (hash->assoc h))
)
