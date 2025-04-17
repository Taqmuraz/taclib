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

(defun read-file (file)
  (with-open-file (f file :direction :input)
    (read f)
  )
)
