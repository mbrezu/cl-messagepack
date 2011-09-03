
(in-package :mpk-tests)

(def-suite cl-messagepack-tests
    :description "Tests for cl-messagepack.")

(in-suite cl-messagepack-tests)

(test integers
  "Test encoding of integers."
  (is (equalp #(0) (mpk:encode 0)))
  (is (equalp #(127) (mpk:encode 127)))
  (is (equalp #(#xCC 128) (mpk:encode 128)))
  (is (equalp #(255) (mpk:encode -1)))
  (is (equalp #(#xE0) (mpk:encode -32)))
  (is (equalp #(#xD0 #xDF) (mpk:encode -33)))
  (is (equalp #(#xCD #x01 #x00) (mpk:encode 256)))
  (is (equalp #(#xCD #xFF #xFF) (mpk:encode 65535)))
  (is (equalp #(#xCE #x00 #x01 #x00 #x00) (mpk:encode 65536)))
  (is (equalp #(#xCE #xFF #xFF #xFF #xFF) (mpk:encode (1- (expt 2 32)))))
  (is (equalp #(#xCF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF) (mpk:encode (1- (expt 2 64)))))
  (is (equalp #(#xD0 #x80) (mpk:encode -128)))
  (is (equalp #(#xD1 #x80 #x00) (mpk:encode (- (expt 2 15)))))
  (is (equalp #(#xD2 #x80 #x00 #x00 #x00) (mpk:encode (- (expt 2 31)))))
  (is (equalp #(#xD3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00) (mpk:encode (- (expt 2 63))))))

(test nil-true-false
  "Test encoding of NIL and T."
  (is (equalp #(#xc0) (mpk:encode nil)))
  (is (equalp #(#xc3) (mpk:encode t))))

(test floating-point
  "Test encoding of single and double precision floating point
numbers."
  #+sbcl (is (equalp #(#xCA #x3F #x80 #x00 #x00) (mpk:encode 1.0s0)))
  (is (equalp #(#xCB #x3F #xF0 #x00 #x00 #x00 #x00 #x00 #x00) (mpk:encode 1.0d0))))

(test strings
  "Test encoding of strings (raw bytes in msgpack parlance, strings
since I am interested in using msgpack as a binary JSON). Use strings
of various lengths to make sure string length is encoded properly."
  (is (equalp #(#xAB #x54 #x65 #x73 #x74 #x20 #x73 #x74 #x72 #x69 #x6E #x67)
              (mpk:encode "Test string")))
  (is (equalp #(#xDA #x00 #x3C #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73
                #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E
                #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72
                #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69 #x6E #x67)
              (mpk:encode (with-output-to-string (str)
                            (loop repeat 10 do (princ "string" str))))))
  (is (equalp #(#xDB #x00 #x05 #x7E #x40 #x73 #x74 #x72 #x69 #x6E #x67 #x73 #x74 #x72 #x69)
              (subseq  (mpk:encode (let ((str (make-string 360000)))
                                     (loop for i from 1 to 60000
                                        do (setf (subseq str (* 6 (1- i)) (* 6 i)) "string"))
                                     str))
                       0 15))))

(test arrays
  "Test encoding of arrays of various sizes. Make sure that arrays
encode properly."
  (is (equalp #(#x9F #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D #x0E #x0F)
              (mpk:encode '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))
  (is (equalp #(#xDC #x00 #x10 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x0C #x0D
                #x0E #x0F #x10)
              (mpk:encode (loop for i from 1 to 16 collect i))))
  (is (equalp #(#xDD #x00 #x01 #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A)
              (subseq (mpk:encode (loop for i from 1 to 65536 collect i))
                      0
                      15))))

(test maps
  "Test maps of various sizes, make sure that lengths are encoded
  properly."
  (labels ((make-map (size)
             (let ((result (make-hash-table)))
               (loop
                  for i from 1 to size
                  do (setf (gethash i result) (- i)))
               result)))
    (is (equalp #(#x8A)
                (subseq (mpk:encode (make-map 10)) 0 1)))
    (is (equalp #(#xDE #x00 #x10)
                (subseq  (mpk:encode (make-map 16)) 0 3)))
    (is (equalp #(#xDF #x00 #x01 #x00 #x00)
                (subseq (mpk:encode (make-map 65536))
                        0 5)))))

