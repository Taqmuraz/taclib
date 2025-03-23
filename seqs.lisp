(in-package #:taclib)

(defun list-from-to (from to)
  (loop for i from from below to collect i)
)

(defun vector-from-to (from to)
  (if (< from to)
    (loop
      with r = (make-array (- to from))
      for i from from below to
      do (setf (aref r (- i from)) i)
      finally (return r)
    )
    #()
  )
)

(defun list-range (n)
  (loop for i from 0 below n collect i)
)

(defun vector-range (n)
  (loop
    with r = (make-array n)
    for i from 0 below n
    do (setf (aref r i) i)
    finally (return r)
  )
)

(defgeneric connect (sep seq))

(defmethod connect (sep (seq list))
  (when seq
    (cons (first seq) (loop for e in (rest seq) append (list sep e)))
  )
)

(defun take (seq n &optional result-type)
  (lets (r (last-> seq length (min n) (max 0) (subseq seq 0)))
    (if result-type (coerce r result-type) r)
  )
)

(defun skip (seq n &optional result-type)
  (lets (r (last-> seq length (min n) (max 0) (subseq seq)))
    (if result-type (coerce r result-type) r)
  )
)

(defun group-by (seq func)
  (loop for v in (coerce seq 'list)
    with r = (hash)
    for k = (funcall func v)
    do (setf (gethash k r) (cons v (gethash k r)))
    finally (return r)))

(defun cycled (l) (lets (c (copy-list l)) (nconc c c)))

(defun concat (type cols)
  (apply #'concatenate type
    (coerce cols 'list)))

(defun hash-set (&rest elements)
  (lets (r (hash))
    (loop for e in elements do (setf (gethash e r) e))
    r
  )
)

(defun into-vector (seq) (coerce seq 'vector))

(defun into-list (seq) (coerce seq 'list))

(defun into-string (val)
  (typecase val
    (number (format nil "~A" val))
    (t (coerce val 'string))
  )
)

(defgeneric at-least-one (seq pred))

(defmethod at-least-one ((seq list) pred)
  (loop for e in seq do (when (funcall pred e) (return-from at-least-one t)))
)

(defmethod at-least-one ((seq vector) pred)
  (loop for e across seq do (when (funcall pred e) (return-from at-least-one t)))
)

(defmacro push-when (pred item list)
  (lets (i (gensym))
    `(lets (,i ,item) (when ,i (push ,i ,list)))
  )
)

(defun all-possible-pairs (list)
  (mapcon (sfun l mapcar (sfun e list (car l) e) (cdr l)) list)
)
