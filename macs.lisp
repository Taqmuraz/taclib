(in-package #:taclib)

(defun symbol-expr-pair-p (p)
  (and (listp p) (symbolp (car p)) (cdr p) p)
)

(defmacro once (vars &body body)
  (let ((syms (loop for v in vars collect
    (typecase v
      (symbol (list (gensym "ONCE") v v))
      ((and list (satisfies symbol-expr-pair-p)) (cons (gensym "ONCE") v))
      (t (error (format nil "Expected symbol or (symbol expr), but got ~A~%" v)))))))
    `(let ,(loop for (g v e) in syms collect (list g '(gensym)))
      `(let (,,@(loop for (g v e) in syms collect ``(,,g ,,e)))
        ,(let ,(loop for (g v) in syms collect (list v g)) ,@body))))
)

(defmacro def-> (name params list-case keyword-case non-list-case)
  `(defmacro ,name (&body exprs)
    (reduce
      (lambda ,params
        (typecase ,(second params)
          (list ,list-case)
          (keyword ,keyword-case)
          (t ,non-list-case)
        )
      )
      exprs
    )
  )
)

(def-> -> (acc e)
  (apply #'list (first e) acc (rest e))
  (list 'map-key acc e)
  (list e acc)
)

(def-> last-> (acc e)
  (append e (list acc))
  (list 'map-key acc e)
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

(defmacro with-maps-keys (kms &body forms)
  (reduce (lambda (km fs) `(with-map-keys ,(car km) ,(cadr km) ,fs))
    kms
    :initial-value `(progn ,@forms)
    :from-end t
  )
)

(defmacro applyv (func vec-args)
  `(apply ,func (coerce ,vec-args 'list))
)

(defmacro pipe (expr &body forms)
  (lets (e (gensym))
    `(lets (
        ,e ,expr
      )
      ,@(loop for f in forms collect (typecase f
        (list (cons (car f) (cons e (cdr f))))
        (keyword (list 'map-key e f))
        (t (list f e))))
      ,e
    )
  )
)

(defmacro defvec (name &rest fields)
  (let (
      (len (length fields))
      (accessors (mapcar (mpartial symbol-of name '-) fields))
      (ctor name)
    )
    `(progn
      (defun ,ctor ,fields (vector ,@fields))
      ,@(mapcan (lambda (a i)
        `(
          (defmacro ,a (v) (list 'aref v ,i))
          (defun ,(symbol-of 'f a) (v) (declare (type (simple-vector ,len) v)) (aref v ,i))
        )
      ) accessors (list-range len))
      t
    )
  )
)

(defmacro if-let (cond var yes &optional no)
  (when (-> var symbolp null)
    (error
      (format nil "Error expanding if-let, var must be a symbol : (if-let ~A ~A ~A ~A)~%"
        cond var yes no)))
  `(let ((,var ,cond)) (if ,var ,yes ,no))
)

(defmacro with-items (vars l &body body)
  (lets (r (gensym))
    `(let* ((,r ,l)(,(car vars) (car ,r)) ,@(loop for v in (cdr vars) append `((,r (cdr ,r))(,v (car ,r))))) ,@body)
  )
)

(defmacro with-assoc-items (vars l &body body)
  (lets (r (gensym))
    `(let* ((,r ,l)(,(car vars) (cdr (car ,r))) ,@(loop for v in (cdr vars) append `((,r (cdr ,r))(,v (cdr (car ,r)))))) ,@body)
  )
)

(defmacro with-vector-items (vars v &body body)
  (once (v)
    `(let ,(mapcar #'list vars (last-> vars length list-range (mapcar (sfun e list 'aref v e))))
      ,@body
    )
  )
)

(defmacro cases-equal (val &body forms)
  (once (val)
    `(cond ,@(loop for (test expr) on forms by #'cddr collect `((equal ,val ,test) ,expr)))
  )
)

(defmacro format-vars (stream &body vars)
  `(format
    ,stream
    ,(concat 'string (loop for v in vars append (list (princ-to-string v) " ~A~%")))
    ,@vars
  )
)
