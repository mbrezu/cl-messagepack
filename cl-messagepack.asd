;;;; cl-messagepack.asd

(asdf:defsystem #:cl-messagepack
  :serial t
  :depends-on (:flexi-streams :babel :cl-json)
  :components ((:file "package")
               (:file "cl-messagepack")
               (:file "benchmark")))

