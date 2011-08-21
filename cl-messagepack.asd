;;;; cl-messagepack.asd

(asdf:defsystem #:cl-messagepack
  :serial t
  :depends-on (:flexi-streams :babel)
  :components ((:file "package")
               (:file "cl-messagepack")))

