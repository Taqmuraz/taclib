(in-package #:taclib)

(defmacro mpartial (name &rest args)
  `(lambda (&rest a) (apply #',name ,@args a))
)

(defun partial (func &rest pre-args)
  (lambda (&rest args) (apply func (append pre-args args)))
)
