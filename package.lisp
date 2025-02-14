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
    #:map-key
    #:->
    #:last->
    #:macfun
    #:lets
    #:conds
    #:cases
    #:vdot
    #:ldot
    #:mat-identity
    #:transponed
    #:mul-mat
    #:mul-mats
    #:mmul-mat
    #:v+ #:v- #:v* #:v/
    #:l+ #:l- #:l* #:l/
  )
)
