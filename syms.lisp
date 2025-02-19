(in-package #:taclib)

(defun symbol-of (&rest vals)
  (intern (apply #'concatenate 'string (mapcar #'string vals)))
)

(defun keyword-of (&rest vals)
  (intern (apply #'concatenate 'string (mapcar #'string vals)) "KEYWORD")
)
