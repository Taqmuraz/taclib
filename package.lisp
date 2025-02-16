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
    #:mul-mats
    #:mmul-mat
    #:v+ #:v- #:v* #:v/
    #:l+ #:l- #:l* #:l/
    #:part
    #:mpart
    #:comp
    #:mcomp
    #:file-parent
    #:concat-path
    #:connect
  )
)
