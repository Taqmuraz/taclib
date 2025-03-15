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

(defun repeat (type times value)
  (cases type
    vector (make-array times :initial-element value)
    list (loop repeat times collect value)
    t (error (format nil "Cannot repeat value into ~A" type))
  )
)

(defun mat-identity (n)
  (loop for i from 0 below n
    with r = (make-array n)
    and c = (make-array n :initial-element 0)
    do (setf (aref r i) (with-vals c i 1))
    finally (return r)
  )
)

(defun transponed (m)
  (apply #'map 'vector #'vector (coerce m 'list))
)

(defmacro mtransponed (m s)
  (lets (v (gensym))
    `(lets (,v ,m)
      (vector ,@(loop for i from 0 below s collect
        (cons 'vector (loop for j from 0 below s collect
          `(aref (aref ,v ,j) ,i)
        ))
      ))
    )
  )
)

(defun transponed-4x4 (m) (mtransponed m 4))

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
            `(declare (type number ,(symbol-of 'a_ (str i) '_ (str j))))
          )
        )
        (loop for i from 0 below bm append
          (loop for j from 0 below bn collect
            `(declare (type number ,(symbol-of 'b_ (str i) '_ (str j))))
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

(defun mul-mat-4x4 (a b)
  (mmul-mat a b 4 4 4 4)
)

(defun mul-mats-4x4 (&rest ms)
  (reduce #'mul-mat-4x4 ms)
)

(defun transform-point-4x4 (m p)
  (subseq
    (aref (mmul-mat m (vector (vector (aref p 0) (aref p 1) (aref p 2) 1)) 4 4 4 1) 0)
      0 3)
)

(defun vec-16->mat-4x4 (v)
  (vector
    (subseq v 0 4)
    (subseq v 4 8)
    (subseq v 8 12)
    (subseq v 12 16)
  )
)

(defun mat-4x4->vec-16 (m)
  (concatenate 'vector
    (aref m 0)
    (aref m 1)
    (aref m 2)
    (aref m 3)
  )
)

(defun mat-copy (m)
  (map 'vector (sfun r copy-seq r) m)
)

(defun mat-scale-4x4 (x y z)
  (vector
    (vector x 0 0 0)
    (vector 0 y 0 0)
    (vector 0 0 z 0)
    (vector 0 0 0 1)
  )
)

(defmacro classic-matrix (&rest rows)
  (last-> rows
    (apply #'mapcar #'list)
    (mapcar (mpart cons 'vector))
    (cons 'vector)
  )
)

(defun mat-perspective (aspect fov near far)
  (lets (
      tan (tan (/ fov 2))
      r (make-array 16 :initial-element 0)
    )
    (setf (aref r 0) (/ (* aspect tan)))
    (setf (aref r 5) (/ tan))
    (setf (aref r 10) (/ (+ far near) (- far near)))
    (setf (aref r 11) 1)
    (setf (aref r 14) (/ (* -2 far near) (- far near)))
    (setf (aref r 15) 0)
    (vec-16->mat-4x4 r)
  )
)

(defun mat-translation (x y z)
  (classic-matrix
    (1 0 0 x)
    (0 1 0 y)
    (0 0 1 z)
    (0 0 0 1)
  )
)

(defun mat-rotation-x (rad)
  (lets (
      s (sin rad)
      c (cos rad)
    )
    (classic-matrix
      (1 0 0 0)
      (0 c (- s) 0)
      (0 s c 0)
      (0 0 0 1)
    )
  )
)

(defun mat-rotation-y (rad)
  (lets (
      s (sin rad)
      c (cos rad)
    )
    (classic-matrix
      (c 0 s 0)
      (0 1 0 0)
      ((- s) 0 c 0)
      (0 0 0 1)
    )
  )
)

(defun mat-rotation-z (rad)
  (lets (
      s (sin rad)
      c (cos rad)
    )
    (classic-matrix
      (c (- s) 0 0)
      (s c 0 0)
      (0 0 1 0)
      (0 0 0 1)
    )
  )
)

(defun mat-rotation (x y z)
  (mul-mats-4x4
    (mat-rotation-z z)
    (mat-rotation-y y)
    (mat-rotation-x x)
  )
)
