;;;; package.lisp

(defpackage #:taclib
  (:use #:cl)
  (:export
    #:forhash
    #:with-vals
    #:hash->str
    #:make-hash
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
    #:lets
  )
)
