(in-package #:taclib)

(defmacro def-> (name params list-case non-list-case)
  `(defmacro ,name (&body exprs)
    (reduce
      (lambda ,params
        (if (listp ,(second params))
          ,list-case
          ,non-list-case
        )
      )
      exprs
    )
  )
)

(def-> -> (acc e)
  (apply #'list (first e) acc (rest e))
  (list e acc)
)

(def-> last-> (acc e)
  (append e (list acc))
  (list e acc)
)

(defmacro macfun (macro &rest params)
  `(lambda ,params (,macro ,@params))
)

(defmacro lets (bindings &rest exprs)
  (if (evenp (length bindings))
    `(let* ,(loop for (a b) on bindings by #'cddr collect (list a b)) ,@exprs)
    `(error (format nil "Number of forms for lets macro bindings must be even : ~%~A" ,bindings))
  )
)

(defmacro conds (&rest forms)
  (if (evenp (length forms))
    `(cond ,@(loop for (a b) on forms by #'cddr collect (list a b)))
    `(error (format nil "Number of forms for conds macro must be even : ~%~A") ,forms)
  )
)

(defmacro cases (expr &rest forms)
  (if (evenp (length forms))
    `(case ,expr ,@(loop for (a b) on forms by #'cddr collect (list a b)))
    `(error (format nil "Number of forms for cases macro must be even : ~%~A") ,forms)
  )
)

(defmacro with-map-keys (keys map &body forms)
  (lets (
      m (gensym)
      ks (loop for k in keys
        collect (if (listp k) k (list k (intern (symbol-name k) "KEYWORD")))
      )
    )
    `(let* ((,m ,map) ,@(loop for (p k) in ks
      collect (list p (list 'map-key m k)))) ,@forms)
  )
)
