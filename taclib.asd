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
    (:file "syms")
    (:file "funcs")
    (:file "macs")
    (:file "bst")
    (:file "maps")
    (:file "seqs")
    (:file "vecs")
    (:file "files")
  )
)
