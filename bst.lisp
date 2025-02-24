(in-package #:taclib)

(defun key< (<)
  (lambda (a b) (funcall < (car a) (car b)))
)

(defun key= (=)
  (lambda (a b) (funcall = (car a) (car b)))
)

(defstruct
  (node
    (:constructor make-node
      (&key val l r &aux
        (height (+ 1 (max (if l (node-height l) 0) (if r (node-height r) 0))))
      )
    )
  )
  val l r height
)

(defun tree-height (tree)
  (if tree (node-height tree) 0)
)

(defun tree-root (tree) (node-val tree))

(defun tree-l (tree) (node-l tree))

(defun tree-r (tree) (node-r tree))

(defun node-balance (n)
  (if n
    (- (tree-height (node-r n)) (tree-height (node-l n)))
    0
  )
)

(defun rotate-left (tree)
  (let* (
      (r (node-r tree))
      (l (node-l tree))
      (rl (node-l r))
      (rr (node-r r))
    )
    (make-node
      :val (node-val r)
      :l (make-node :val (node-val tree) :r rl :l l)
      :r rr
    )
  )
)

(defun rotate-right (tree)
  (let* (
      (r (node-r tree))
      (l (node-l tree))
      (ll (node-l l))
      (lr (node-r l))
    )
    (make-node
      :val (node-val l)
      :l ll
      :r (make-node :val (node-val tree) :r r :l lr)
    )
  )
)

(defun balance-tree (tree)
  (if tree
    (case (node-balance tree)
      (2 (rotate-left tree))
      (-2 (rotate-right tree))
      (t tree)
    )
  )
)

(defun print-node (n stream &optional (indent 0))
  (if n
    (progn
      (format stream "~&~v@T|~A ~A" (* 2 indent) (node-val n) (node-balance n))
      (print-node (node-l n) stream (+ 1 indent))
      (print-node (node-r n) stream (+ 1 indent))
    )
    (format stream "~&~v@T|~A" (* 2 indent) nil)
  )
)

(defmethod print-object ((n node) stream)
  (print-node n stream)
)

(defun insert-node (tree node < =)
  (if tree
    (let (
        (nv (node-val node))
        (tv (node-val tree))
        (tl (node-l tree))
        (tr (node-r tree))
      )
      (balance-tree (cond
        ((funcall = nv tv) tree)
        ((funcall < nv tv) (make-node :val tv :l (insert-node tl node < =) :r tr))
        (t (make-node :val tv :l tl :r (insert-node tr node < =)))
      ))
    )
    node
  )
)

(defun insert-value (tree key val < =)
  (insert-node tree (make-node :val (cons key val)) < =)
)

(defun tree-min (tree)
  (cond
    ((node-l tree) (tree-min (node-l tree)))
    (t tree)
  )
)

(defun tree-max (tree)
  (cond
    ((node-r tree) (tree-max (node-r tree)))
    (t tree)
  )
)

(defun tree-content (tree &key key)
  (if tree
    (append
      (tree-content (node-l tree) :key key)
      (list (funcall (if key key #'cdr) (node-val tree)))
      (tree-content (node-r tree) :key key)
    )
  )
)

(defun search-tree (tree item < = &key key)
  (let (
      (kf (if key key #'car))
      (n tree)
    )
    (loop while n do
      (let (
          (k (funcall kf (node-val n)))
        )
        (setf n
          (cond
            ((funcall < item k) (node-l n))
            ((funcall = item k) (return-from search-tree n))
            (t (node-r n))
          )
        )
      )
    )
  )
)

(defun find-bounds (tree item < &key key)
  (lets (
      n tree
      kf (if key key #'car)
      min (-> tree tree-min node-val car)
      max (-> tree tree-max node-val car)
    )
    (loop while n do
      (lets (
          k (last-> n node-val (funcall kf))
        )
        (setf n (cond
          ((funcall < item k)
            (when (funcall < k max) (setf max k))
            (node-l n))
          (t
            (when (funcall < min k) (setf min k))
            (node-r n))
        ))
      )
    )
    (list min max)
  )
)

(defun assoc->tree (pairs < =)
  (reduce
    (lambda (r e) (insert-value r (car e) (cdr e) (key< <) (key= =)))
    pairs
    :initial-value nil
  )
)

(defun make-tree (< = &rest kvs)
  (loop
    with r = nil
    for (k v) on kvs by #'cddr
    do (setf r (insert-value r k v (key< <) (key= =)))
    finally (return r)
  )
)
