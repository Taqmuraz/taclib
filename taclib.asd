;;;; taclib.asd

(asdf:defsystem #:taclib
  :description "Describe taclib here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components (
    (:file "package")
    (:file "taclib")
    (:file "funcs")
    (:file "maps")
    (:file "macs")
    (:file "seqs")
    (:file "syms")
    (:file "vecs")
  )
)
