;; Copyright (c) 2012, Miron Brezuleanu
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage #:messagepack-tests
  (:use #:cl #:fiveam)
  (:nicknames #:mpk-tests))

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
  #+sbcl (is (equalp #(#xCA #xBF #x80 #x00 #x00) (mpk:encode -1.0s0)))
  (is (equalp #(#xCB #x3F #xF0 #x00 #x00 #x00 #x00 #x00 #x00) (mpk:encode 1.0d0)))
  (is (equalp #(#xCB #xBF #xF0 #x00 #x00 #x00 #x00 #x00 #x00) (mpk:encode -1.0d0))))

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

(test alists
  "Test alists can be encoded into either map or lists of lists depending on
*encode-alist-as-hash*."
  (let ((alist '((1 11 12) ("b" "bc"))))
    (is (equalp #(#x82 #x01 #x92 #x0B #x0C #xA1 #x62 #x91 #xA2 #x62 #x63)
                (mpk:encode alist)))
    (is (equalp #(#x92 #x93 #x01 #x0B #x0C #x92 #xA1 #x62 #xA2 #x62 #x63)
                (let ((mpk:*encode-alist-as-map*)) (mpk:encode alist))))))

(test maps
  "Test maps of various sizes, make sure that lengths are encoded
  properly."
  (labels ((make-map (size)
             (let ((result (make-hash-table)))
               (loop
                  for i from 1 to size
                  do (setf (gethash i result) (- i)))
               result))
	   (make-alist (size)
	     (let ((result ()))
	       (loop
		  for i from 1 to size
		  do (push (cons i (- i)) result))
	       result))
	   (make-plist (size)
	     (loop
		for i from 1 to size
		append (list (intern (write-to-string i) :keyword) (- i)))))
    (is (equalp #(#x8A)
                (subseq (mpk:encode (make-map 10)) 0 1)))
    (is (equalp #(#x8A)
                (subseq (mpk:encode (make-alist 10)) 0 1)))
    (is (equalp #(#x8A)
                (subseq (mpk:encode (make-plist 10)) 0 1)))
    (is (equalp #(#xDE #x00 #x10)
                (subseq  (mpk:encode (make-map 16)) 0 3)))
    (is (equalp #(#xDE #x00 #x10)
                (subseq  (mpk:encode (make-alist 16)) 0 3)))
    (is (equalp #(#xDE #x00 #x10)
                (subseq  (mpk:encode (make-plist 16)) 0 3)))
    (is (equalp #(#xDF #x00 #x01 #x00 #x00)
                (subseq (mpk:encode (make-map 65536))
                        0 5)))
    (is (equalp #(#xDF #x00 #x01 #x00 #x00)
                (subseq (mpk:encode (make-alist 65536))
                        0 5)))
    (is (equalp #(#xDF #x00 #x01 #x00 #x00)
                (subseq (mpk:encode (make-plist 65536))
                        0 5)))))

(test extension-types
  "Test that extended types are encoded properly."
  (let ((mpk:*extended-types*
          (mpk:define-extension-types
            '(7   type1))))
    (is (equalp #(#xC7 #x01 #x07 #x09)
                (mpk:encode (make-instance 'type1 'messagepack-sym:id 9))))))

(test decoding-integers
  "Test that (equalp (decode (encode data)) data) for integers (that
  can be encoded as Message Pack integers."
  (is (eql 100 (mpk:decode (mpk:encode 100))))
  (is (eql -30 (mpk:decode (mpk:encode -30))))
  (is (eql 12345 (mpk:decode (mpk:encode 12345))))
  (is (eql 2390493 (mpk:decode (mpk:encode 2390493))))
  (is (eql 2390493000 (mpk:decode (mpk:encode 2390493000)))))

(test decoding-bools
  "Test that (equalp (decode (encode data)) data) for bools."
  (is (eql t (mpk:decode (mpk:encode t))))
  (is (eql nil (mpk:decode (mpk:encode nil)))))

(test decoding-floats
  "Test that (equalp (decode (encode data)) data) for floats."
  #+ (or sbcl ccl) (is (eql  100d0 (mpk:decode (mpk:encode  100d0))))
  #+ (or sbcl ccl) (is (eql -100d0 (mpk:decode (mpk:encode -100d0))))
  #+ (or sbcl ccl) (is (eql -1.2345678901234567e19
                            (mpk:decode (mpk:encode -1.2345678901234567e19))))
  #+ sbcl (is (eql  102s0 (mpk:decode (mpk:encode  102s0))))
  #+ sbcl (is (eql -102s0 (mpk:decode (mpk:encode -102s0)))))

(test decoding-strings
  "Test that (equalp (decode (encode data)) data) holds for strings."
  (let ((*print-pretty* nil))
    (let ((short-string "test")
          (medium-string (with-output-to-string (str)
                           (loop repeat 10 do (princ "test" str))))
          (long-string (with-output-to-string (str)
                         (loop repeat (expt 10 5) do (princ "test" str)))))
      (is (string= short-string (mpk:decode (mpk:encode short-string))))
      (is (string= medium-string (mpk:decode (mpk:encode medium-string))))
      (is (string= long-string (mpk:decode (mpk:encode long-string)))))))

(test decoding-binary
  "Test decoding of binary type."
  (is (equalp #() (mpk:decode #(#xc4 #x00))))
  (is (equalp #() (mpk:decode #(#xc5 #x00 #x00))))
  (is (equalp #() (mpk:decode #(#xc6 #x00 #x00 #x00 #x00))))
  (flet ((to-b8array (&rest contents)
           (apply #'concatenate '(vector (unsigned-byte 8)) contents)))
    (let ((short-array #(1 2 3))
          (medium-array (make-array 1000 :initial-element #xd1))
          (long-array (make-array 70000 :initial-element #xd1)))
      (is (equalp short-array  (mpk:decode (to-b8array #(#xc4 #x03) short-array))))
      (is (equalp short-array  (mpk:decode (to-b8array #(#xc5 #x00 #x03) short-array))))
      (is (equalp medium-array (mpk:decode (to-b8array #(#xc5 #x03 #xe8) medium-array))))
      (is (equalp short-array  (mpk:decode (to-b8array #(#xc6 #x00 #x00 #x00 #x03) short-array))))
      (is (equalp medium-array (mpk:decode (to-b8array #(#xc6 #x00 #x00 #x03 #xe8) medium-array))))
      (is (equalp long-array   (mpk:decode (to-b8array #(#xc6 #x00 #x01 #x11 #x70) long-array)))))
    (let ((mpk:*decode-bin-as-string* T)
          (short-string "tes")
          (medium-string (with-output-to-string (str)
                           (loop repeat (/ 1000 5) do (princ "tests" str))))
          (long-string (with-output-to-string (str)
                         (loop repeat (/ 70000 5) do (princ "tests" str)))))
      (is (string= short-string  (mpk:decode (to-b8array #(#xc4 #x03) (babel:string-to-octets short-string)))))
      (is (string= short-string  (mpk:decode (to-b8array #(#xc5 #x00 #x03) (babel:string-to-octets short-string)))))
      (is (string= medium-string (mpk:decode (to-b8array #(#xc5 #x03 #xe8) (babel:string-to-octets medium-string)))))
      (is (string= short-string  (mpk:decode (to-b8array #(#xc6 #x00 #x00 #x00 #x03) (babel:string-to-octets short-string)))))
      (is (string= medium-string (mpk:decode (to-b8array #(#xc6 #x00 #x00 #x03 #xe8) (babel:string-to-octets medium-string)))))
      (is (string= long-string   (mpk:decode (to-b8array #(#xc6 #x00 #x01 #x11 #x70) (babel:string-to-octets long-string))))))))

(test decoding-arrays
  "Test that (equalp (decode (encode data)) data) holds for arrays."
  (let ((short-array #(1 2 3))
        (medium-array (make-array 1000 :initial-element 10))
        (long-array (make-array 70000 :initial-element 10)))
    (is (equalp short-array (mpk:decode (mpk:encode short-array))))
    (is (equalp medium-array (mpk:decode (mpk:encode medium-array))))
    (is (equalp long-array (mpk:decode (mpk:encode long-array))))))

(test decoding-lists
  "Test that (equalp (decode (encode data)) data) holds for lists,
  with the proper options."
  (labels ((mk-list (size)
             (let (result)
               (dotimes (i size)
                 (push i result))
               result)))
    (let ((mpk:*decoder-prefers-lists* t))
      (let ((short-list (mk-list 10))
            (medium-list (mk-list 1000))
            (long-list (mk-list 70000)))
        (is (equalp short-list (mpk:decode (mpk:encode short-list))))
        (is (equalp medium-list (mpk:decode (mpk:encode medium-list))))
        (is (equalp long-list (mpk:decode (mpk:encode long-list))))))))

(defun make-map (size)
  (let ((result (make-hash-table :test #'equalp)))
    (loop
       for i from 1 to size
       do (setf (gethash i result) (- i)))
    result))

(test decoding-maps
  "Test that (equalp (decode (encode data)) data) holds for hash
tables that have #'equalp as test."
  (let ((small-map (make-map 10))
        (medium-map (make-map 1000))
        (big-map (make-map 70000)))
    (is (equalp small-map (mpk:decode (mpk:encode small-map))))
    (is (equalp medium-map (mpk:decode (mpk:encode medium-map))))
    (is (equalp big-map (mpk:decode (mpk:encode big-map))))))

(test extension-types-decoding
  "Test that extended types are decoded properly."
  (dolist (test '((:byte-array #(100 200 250 5) #(100 14 1 200))
                  (:numeric 27 100)))
    (destructuring-bind (encoding-type id1 id2) test
      (let ((mpk:*extended-types*
              (mpk:define-extension-types
                `(,encoding-type 120 typea typeb)))
            (mpk:*lookup-table* (mpk:make-lookup-table)))
        (let* ((a1 (make-instance 'typea 'messagepack-sym:id id1))
               (a2 (make-instance 'typea 'messagepack-sym:id id2))
               (b1 (make-instance 'typeb 'messagepack-sym:id id1)))
          (is (eq (mpk:decode (mpk:encode a1)) a1))
          (is (eq (mpk:decode (mpk:encode a2)) a2))
          (is (eq (mpk:decode (mpk:encode b1)) b1))
          (is (eq (mpk:decode (mpk:encode a1)) (mpk:decode (mpk:encode a1))))
          (is (eq (mpk:decode (mpk:encode a2)) (mpk:decode (mpk:encode a2))))
          (is (not (eq (mpk:decode (mpk:encode a1)) (mpk:decode (mpk:encode a2)))))
          (is (not (eq (mpk:decode (mpk:encode b1)) (mpk:decode (mpk:encode a1)))))
          (is (not (eq (mpk:decode (mpk:encode b1)) (mpk:decode (mpk:encode a2))))))))))

(test decoding-maps-to-alists
  "Tests that decoding maps to alists works as expected."
  (let ((mpk:*decoder-prefers-alists* t))
    (is (equal '((1 . -1) (2 . -2) (3 . -3))
               (mpk:decode (mpk:encode (make-map 3)))))))
