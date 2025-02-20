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
  (let ((r (hash)))
    (forhash (k v) h (setf (gethash k r) v))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defmethod with-vals ((c list) &rest kvs)
  (let ((r (hash)))
    (loop for (k . v) in c do (setf (gethash k r) v))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    (hash->assoc r)
  )
)

(defun make-hash (&rest kvs)
  (let ((r (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun make-assoc (&rest kvs)
  (hash->assoc (apply #'make-hash kvs))
)

(defmacro hash (&rest kvs)
  `(make-hash ,@kvs)
)

(defun merge-into (type &rest ms)
  (lets (
      r (hash)
    )
    (loop for m in ms do
      (typecase m
        (list (loop for (k . v) in m do (setf (gethash k r) v)))
        (vector (loop for i from 0 below (length m) do (setf (gethash i r) (aref m i))))
        (hash-table (forhash (k v) m (setf (gethash k r) v)))
        (t (error (format nil "Type ~A is not a map type~%" (type-of m))))
      )
    )
    (cases type
      'list (hash->assoc r)
      'vector (hash->vector r)
      'hash-table r
      t (error (format nil "Cannot merge into ~A~%" type))
    )
  )
)

(defun copy-hash (h)
  (let ((r (hash)))
    (forhash (k v) h (setf (gethash k r) v))
    r
  )
)

(defgeneric keys (map))
(defgeneric vals (map))

(defmethod keys ((h hash-table))
  (let ((r nil))
    (forhash (k v) h (declare (ignore v)) (setf r (nconc r (list k))))
    r
  )
)

(defmethod vals ((h hash-table))
  (let ((r nil))
    (forhash (k v) h (declare (ignore k)) (setf r (nconc r (list v))))
    r
  )
)

(defmethod keys ((v vector))
  (loop for i from 0 below (length v) collect i)
)

(defmethod vals ((v vector))
  (coerce v 'list)
)

(defmethod keys ((al list))
  (loop for (k . v) in al collect k)
)

(defmethod vals ((al list))
  (loop for (k . v) in al collect v)
)

(defun hash->assoc (h)
  (let ((r nil))
    (forhash (k v) h (setf r (nconc r (list (cons k v)))))
    r
  )
)

(defun assoc->hash (a)
  (loop with r = (hash) for (k . v) in a
    do (setf (gethash k r) v)
    finally (return r)
  )
)

(defun hash->vector (h)
  (lets (
      ks (keys h)
      l (+ 1 (apply #'max ks))
      r (make-array l)
    )
    (loop for k in ks do (setf (aref r k) (gethash k h)))
    r
  )
)

(defun vector->hash (v)
  (loop with r = (hash)
    for i from 0 below (length v)
    do (setf (gethash i r) (aref v i))
    finally (return r)
  )
)

(defun func->hash (f &rest keys)
  (loop for k in keys with r = (hash)
    do (setf (gethash k r) (funcall f k))
    finally (return r)
  )
)

(defun hash->func (h)
  (sfun k map-key h k)
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

(defmethod update ((map list) func &rest keys)
  (hash->assoc (apply #'update (assoc->hash map) func keys))
)

(defgeneric update-keys (map func))

(defmethod update-keys ((map hash-table) func)
  (lets (r (hash))
    (forhash (k v) map (setf (gethash (funcall func k) r) v))
    r
  )
)

(defmethod update-keys ((map list) func)
  (loop for (k . v) in map collect (cons (funcall func k) v))
)

(defgeneric update-vals (map func))

(defmethod update-vals ((map hash-table) func)
  (lets (r (hash)) (forhash (k v) map (setf (gethash k r) (funcall func v))) r)
)

(defmethod update-vals ((map list) func)
  (loop for (k . v) in map collect (cons k (funcall func v)))
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

(defun assoc-accessor (key)
  (lambda (map) (let ((a (assoc key map :test #'equal))) (if a (cdr a) nil)))
)

(defgeneric map-key (map key &optional not-found))

(defmethod map-key ((map hash-table) key &optional not-found)
  (multiple-value-bind (v h) (gethash key map) (if h v not-found))
)

(defmethod map-key ((map vector) key &optional not-found)
  (let ((l (length map))) (if (and (>= l 0) (< key l)) (aref map key) not-found))
)

(defmethod map-key ((map list) key &optional not-found)
  (let ((a (assoc key map :test #'equal))) (if a (cdr a) not-found))
)

(defmethod map-key ((map function) key &optional not-found)
  (lets (r (funcall map key)) (if r r not-found))
)

(defmethod print-object ((h hash-table) stream)
  (format stream "#<HASH-TABLE ~A>" (prin1-to-string (keys h)))
)

(defgeneric select-keys (map &rest keys))

(defmethod select-keys ((map list) &rest keys)
  (apply #'make-assoc (loop for k in keys append (list k (map-key map k))))
)

(defmethod select-keys ((map vector) &rest keys)
  (map 'vector (mpart map-key map) keys)
)

(defmethod select-keys ((map hash-table) &rest keys)
  (apply #'make-hash (loop for k in keys append (list k (map-key map k))))
)
