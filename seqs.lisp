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

