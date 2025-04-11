(in-package #:taclib)

(defmacro hash (&rest kvs)
  `(make-hash ,@kvs)
)

(defmacro forhash (params hash &body body)
  `(maphash (lambda ,params ,@body) ,hash)
)

(defmacro forvec ((i v) vec &body body)
  (once (vec)
    `(if (vectorp ,vec)
      (loop for ,i from 0 below (length ,vec) do
        (lets (,v (aref ,vec ,i)) ,@body)
      )
      (error (format nil "Expected vector, but got ~A~%" ,vec))
    )
  )
)

(defmacro for-maps-list (keys maps &body body)
  (lets (m (gensym))
    (once (maps)
      `(loop for ,m in ,maps do (with-map-keys ,keys ,m ,@body)))))

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
  (loop
    with r = (loop for (k . v) in c collect (cons k v))
    with acc = nil
    for (k v) on kvs by #'cddr
    for met = nil
    do (progn
      (loop for p in r do
        (when (equal k (car p))
          (setf (cdr p) v)
          (setf met t)
          (return nil)
        )
      )
      (when (null met) (push (cons k v) acc))
    )
    finally (return (append r acc))
  )
)

(defun make-hash (&rest kvs)
  (let ((r (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k r) v))
    r
  )
)

(defun make-assoc (&rest kvs)
  (loop for (k v) on kvs by #'cddr collect
    (cons k v)
  )
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
  (lets (
      ks (apply #'hash-set keys)
    )
    (loop for (k . v) in map collect
      (multiple-value-bind (val present) (gethash k ks)
        (cons k (if present (funcall func v) v))
      )
    )
  )
)

(defgeneric update-or-put (map func &rest keys))

(defmethod update-or-put ((h hash-table) func &rest keys)
  (loop for k in keys
    with r = (copy-hash h)
    do (setf (gethash k r) (funcall func k (gethash k r)))
    finally (return r)
  )
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

(defun select-vals (map &rest keys)
  (loop for k in keys collect (map-key map k))
)

(defgeneric has-key-p (map key))

(defmethod has-key-p ((map hash-table) key)
  (multiple-value-bind (k has) (gethash key map) has)
)

(defun make-set (&rest items)
  (loop with h = (hash) for i in items
    do (setf (gethash i h) i)
    finally (return h)
  )
)

(defgeneric map-pairs (func map))

(defmethod map-pairs (func (map vector))
  (loop for i from 0 below (length map) collect
    (funcall func i (aref map i))))

(defmethod map-pairs (func (map hash-table))
  (lets (r nil)
    (forhash (k v) map (push (funcall func k v) r)) (nreverse r)))

(defmethod map-pairs (func (map list))
  (loop for (k . v) in map collect (funcall func k v)))

(defmacro on-map ((k v) map &body body)
  `(map-pairs (lambda (,k ,v) ,@body) ,map)
)

(defun map-by-key (type key map)
  (map type (sfun m map-key m key) map)
)

(defmacro hash-once (key hash-table &body body)
  (once (key hash-table)
    `(when (not (has-key-p ,hash-table ,key))
      (setf (gethash ,key ,hash-table) (progn ,@body))
    )
  )
)
