(in-package #:taclib)

(defmacro mpart (name &rest args)
  `(lambda (&rest a) (apply #',name ,@args a))
)

(defun part (func &rest pre-args)
  (lambda (&rest args) (apply func (append pre-args args)))
)

(defun comp (func &rest funcs)
  (lambda (&rest args)
    (loop for f in funcs
      with r = (apply func args)
      do (setf r (funcall f r))
      finally (return r)
    )
  )
)

(defmacro mcomp (func &rest funcs)
  (lets (args (gensym))
    `(lambda (&rest ,args) ,
      (loop for f in funcs
        with r = `(apply ,func ,args)
        do (setf r `(funcall ,f ,r))
        finally (return r)
      )
    )
  )
)

(defmacro sfun (params &rest body)
  `(lambda ,(if (listp params) params (list params)) (,@body))
)
