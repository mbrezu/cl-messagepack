;;;; package.lisp

(defpackage #:messagepack
  (:use #:cl)
  (:nicknames #:mpk)
  (:export encode encode-stream
           decode decode-stream
           write-hex
           *use-extensions*
           *symbol->int*
           *int->symbol*
           get-symbol-int-table
           with-symbol-int-table
           *decoder-prefers-lists*))

(defpackage #:messagepack-tests
  (:use #:cl #:fiveam)
  (:nicknames #:mpk-tests))


