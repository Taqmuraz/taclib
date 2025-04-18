;;;; package.lisp

(defpackage #:taclib
  (:use #:cl)
  (:export
    #:forhash
    #:forvec
    #:for-maps-list
    #:with-vals
    #:make-hash
    #:make-assoc
    #:hash
    #:merge-into
    #:keys
    #:vals
    #:copy-hash
    #:hash->assoc
    #:assoc->hash
    #:hash->vector
    #:vector->hash
    #:func->hash
    #:hash->func
    #:update
    #:update-keys
    #:update-vals
    #:defvec
    #:hash-accessor
    #:assoc-accessor
    #:map-key
    #:select-keys
    #:select-vals
    #:has-key-p
    #:make-set
    #:map-pairs
    #:on-map
    #:map-by-key
    #:hash-once
    #:take
    #:skip
    #:group-by
    #:cycled
    #:concat
    #:connect
    #:hash-set
    #:into-vector
    #:into-list
    #:into-string
    #:at-least-one
    #:push-when
    #:all-possible-pairs
    #:partitions
    #:list-from-to
    #:vector-from-to
    #:list-range
    #:vector-range
    #:once
    #:->
    #:last->
    #:applyv
    #:pipe
    #:macfun
    #:lets
    #:conds
    #:cases
    #:with-map-keys
    #:with-maps-keys
    #:dot
    #:len
    #:norm
    #:mdotl3
    #:mlenl3
    #:mnorml3
    #:clamp-length
    #:mclamp
    #:closest
    #:repeat
    #:ll #:lll #:llll ll* lll* llll*
    #:cross
    #:mat-identity
    #:transponed
    #:mtransponed
    #:transponed-4x4
    #:mul-mat
    #:mul-mat-4x4
    #:mul-mats-4x4
    #:transform-point
    #:transform-vector
    #:mul-mats
    #:mmul-mat
    #:mmul-mat-4x4
    #:vec-16->mat-4x4
    #:mat-4x4->vec-16
    #:mat-scale-4x4
    #:classic-matrix
    #:mat-perspective
    #:mat-translation
    #:mat-rotation-x
    #:mat-rotation-y
    #:mat-rotation-z
    #:mat-rotation
    #:mat-pos-rot
    #:mat-pos-rot-inversed
    #:xyz->rotation
    #:xyz->translation
    #:xyz->x0z
    #:clock->xy
    #:xy->clock
    #:look->rotation
    #:blst
    #:ulst
    #:inlst
    #:blst3
    #:ulst3
    #:inlst3
    #:l+ #:l- #:l* #:l/ #:lmin #:lmax
    #:ml2+ ml2- ml2* ml2/ ml2min ml2max
    #:ml3+ ml3- ml3* ml3/ ml3min ml3max
    #:ml2*n ml3*n
    #:part
    #:mpart
    #:comp
    #:mcomp
    #:sfun
    #:if-let
    #:with-items
    #:with-assoc-items
    #:with-vector-items
    #:cases-equal
    #:format-vars
    #:with-coerced
    #:with-doubles
    #:with-floats
    #:file-parent
    #:concat-path
    #:read-file
    #:symbol-of
    #:keyword-of
    #:search-tree
    #:assoc->tree
    #:tree-content
    #:tree-min
    #:tree-max
    #:make-tree
    #:tree-root
    #:tree-height
    #:tree-l
    #:tree-r
    #:find-bounds
  )
)
