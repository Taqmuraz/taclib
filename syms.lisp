(in-package #:taclib)

(defun symbol-of (&rest vals)
  (intern (apply #'concatenate 'string (mapcar #'string vals)))
)
