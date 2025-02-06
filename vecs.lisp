(in-package #:taclib)

(defmacro defop (name params fbody mbody)
  (list 'progn
    `(defun ,name ,params ,fbody)
    `(defmacro ,(symbol-of 'm name) ,params ,mbody)
  )
)

(defmacro def-vec-op (type name op)
  `(defop ,name (&rest vs)
    (apply #'map ,type ,op vs)
    (append (list 'map ',type ',op) vs)
  )
)

(defmacro def-vec-type (type &rest ops)
  (cons 'progn (loop for (name op) in ops collect
    `(def-vec-op ,type ,name ,op)
  ))
)

(def-vec-type 'vector (v+ #'+) (v- #'-) (v* #'*) (v/ #'/))
(def-vec-type 'list (l+ #'+) (l- #'-) (l* #'*) (l/ #'/))

(defun vdot (a b)
  (loop
    with al = (length a) and bl = (length b) and r = 0
    for i from 0 below (min al bl)
    do (incf r (* (aref a i) (aref b i)))
    finally (return r)
  )
)

(defun ldot (a b)
  (reduce #'+ (l* a b))
)

(defun mat-identity (n)
  (loop for i from 0 below n
    with r = (make-array n)
    and c = (make-array n)
    do (setf (aref r i) (with-vals c i 1))
    finally (return r)
  )
)

(defun transponed (m)
  (apply #'map 'vector #'vector (map 'list #'identity m))
)

(defun mul-mat (a b)
  (let* (
      (at (transponed a))
      (bl (length b))
      (atl (length at))
      (r (apply #'vector (loop repeat bl collect (make-array atl))))
    )
    (loop for i from 0 below atl
      do (loop
        for j from 0 below bl do
        (setf (aref (aref r j) i) (vdot (aref at i) (aref b j)))
      )
      finally (return r)
    )
  )
)

(defmacro mmul-mat (a b am an bm bn)
  (labels (
      (mvdot (a b l) (cons '+
        (loop for i from 0 below l collect (list '* (funcall a i) (funcall b i)))
      ))
      (str (x) (format nil "~A" x))
      (rel (i j) (mvdot
        (lambda (x) (symbol-of 'a_ (str i) '_ (str x)))
        (lambda (x) (symbol-of 'b_ (str x) '_ (str j))) am)
      )
      (rcol (j) (cons 'vector (loop for i from 0 below am collect (rel i j))))
    )
    `(let*
      ,(append
        (list `(a ,a) `(b ,b))
        (loop for j from 0 below an append
          (cons
            (list (symbol-of 'a_ (str j)) `(aref a ,j))
            (loop for i from 0 below am collect
              (list (symbol-of 'a_ (str i) '_ (str j)) `(aref ,(symbol-of 'a_ (str j)) ,i))
            )
          )
        )
        (loop for j from 0 below bn append
          (cons
            (list (symbol-of 'b_ (str j)) `(aref b ,j))
            (loop for i from 0 below bm collect
              (list (symbol-of 'b_ (str i) '_ (str j)) `(aref ,(symbol-of 'b_ (str j)) ,i))
            )
          )
        )
      )
      ,@(append
        (loop for i from 0 below am append
          (loop for j from 0 below an collect
            `(declare (type fixnum ,(symbol-of 'a_ (str i) '_ (str j))))
          )
        )
        (loop for i from 0 below bm append
          (loop for j from 0 below bn collect
            `(declare (type fixnum ,(symbol-of 'b_ (str i) '_ (str j))))
          )
        )
        (list (cons 'vector
          (loop for j from 0 below bn collect
            (rcol j)
          )
        ))
      )
    )
  )
)

(defun mul-mats (&rest ms)
  (reduce #'mul-mat ms)
)
