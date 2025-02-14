(in-package #:taclib)

(defmacro mpart (name &rest args)
  `(lambda (&rest a) (apply #',name ,@args a))
)

(defun part (func &rest pre-args)
  (lambda (&rest args) (apply func (append pre-args args)))
)
