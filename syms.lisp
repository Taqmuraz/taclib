(in-package #:taclib)

(defun symbol-of (&rest vals)
  (intern (apply #'concatenate 'string (mapcar #'into-string vals)))
)

(defun keyword-of (&rest vals)
  (intern (apply #'concatenate 'string (mapcar #'into-string vals)) "KEYWORD")
)
