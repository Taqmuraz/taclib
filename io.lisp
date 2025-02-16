(in-package #:taclib)

(defun file-parent (file)
  (last-> file truename pathname-directory rest
    (connect "/")
    (cons "/")
    (apply #'concatenate 'string)
  )
)

(defun concat-path (&rest paths)
  (last-> paths (connect "/") (apply #'concatenate 'string))
)
