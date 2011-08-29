;;;; cl-messagepack.asd

(asdf:defsystem #:cl-messagepack
  :serial t
  :depends-on (:flexi-streams :babel :cl-json :fiveam)
  :components ((:file "package")
               (:file "cl-messagepack")
               (:file "benchmark")
               (:file "tests")))

