;;;; package.lisp

(defpackage #:taclib
  (:use #:cl)
  (:export
    #:forhash
    #:with-vals
    #:make-hash
    #:make-assoc
    #:hash
    #:merge-hash
    #:copy-hash
    #:hash-keys
    #:hash-vals
    #:hash->assoc
    #:assoc->hash
    #:update
    #:defvec
    #:hash-accessor
    #:assoc-accessor
    #:map-key
    #:select-keys
    #:->
    #:last->
    #:macfun
    #:lets
    #:conds
    #:cases
    #:with-map-keys
    #:vdot
    #:ldot
    #:mat-identity
    #:transponed
    #:mul-mat
    #:mul-mat-4x4
    #:transform-point-4x4
    #:mul-mats
    #:mmul-mat
    #:vec-16->mat-4x4
    #:mat-4x4->vec-16
    #:mat-scale-4x4
    #:classic-matrix
    #:mat-perspective
    #:mat-translation
    #:v+ #:v- #:v* #:v/
    #:l+ #:l- #:l* #:l/
    #:part
    #:mpart
    #:comp
    #:mcomp
    #:file-parent
    #:concat-path
    #:connect
    #:symbol-of
  )
)
