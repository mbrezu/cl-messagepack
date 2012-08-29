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

;;;; cl-messagepack.lisp

(in-package #:messagepack)

(declaim (optimize (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (format nil "~{~a~}" args))
  (defun mksymb (&rest args)
    (intern (apply #'mkstr args)))
  (defun speed-for-size (size)
    (if (< size (integer-length most-positive-fixnum))
        3
        0)))

(defmacro signed-unsigned-convertors (size)
  `(progn
     (defun ,(mksymb 'sb size '-> 'ub size) (sb)
       (declare (optimize (debug 0) (safety 0) (speed ,(speed-for-size size)))
                (type (integer ,(- (expt 2 (1- size))) ,(1- (expt 2 (1- size)))) sb))
       (if (< sb 0)
           (ldb (byte ,size 0) sb)
           sb))
     (defun ,(mksymb 'ub size '-> 'sb size) (sb)
       (declare (optimize (debug 0) (safety 0) (speed ,(speed-for-size size)))
                (type (mod ,(expt 2 size)) sb))
       (if (logbitp (1- ,size) sb)
           (- (1+ (logxor (1- (expt 2 ,size)) sb)))
           sb))))

(signed-unsigned-convertors 8)
(signed-unsigned-convertors 16)
(signed-unsigned-convertors 32)
(signed-unsigned-convertors 64)

(defun write-hex (data)
  (let (line)
    (loop
       for i from 0 to (1- (length data))
       do (push (elt data i) line)
       when (= (length line) 16)
       do
         (format t "~{#x~2,'0x ~}~%" (nreverse line))
         (setf line nil))
    (when line
      (format t "~{#x~2,'0x ~}~%" (nreverse line)))))

(defun encode (data)
  (flexi-streams:with-output-to-sequence (stream)
    (encode-stream data stream)))

(defun make-hash (data)
  (let ((result (make-hash-table)))
    (dolist (kv data)
      (cond ((consp (cdr kv))
             (setf (gethash (first kv) result) (second kv)))
            (t
             (setf (gethash (car kv) result) (cdr kv)))))
    result))

(defun is-byte-array (data-type)
  (and (vectorp data-type)
       (equal '(unsigned-byte 8) (array-element-type data-type))))

(defmacro store-big-endian (number stream byte-count)
  (let ((g-number (gensym "number"))
        (g-stream (gensym "stream")))
    `(let ((,g-number ,number)
           (,g-stream ,stream))
       ,@(loop for i from (1- byte-count) downto 0
            collect `(write-byte (ldb (byte 8 ,(* 8 i)) ,g-number) ,g-stream)))))

(defvar *use-extensions* nil)
(defvar *symbol->int* nil)
(defvar *int->symbol* nil)
(defvar *symbol->int* nil)
(defvar *decoder-prefers-lists* nil)
(defvar *decoder-prefers-alists* nil)

(defmacro with-symbol-int-table (tables &body body)
  `(let ((*symbol->int* (first ,tables))
         (*int->symbol* (second ,tables)))
     ,@body))

(defun get-symbol-int-table (table)
  (let ((symbol->int (make-hash-table))
        (int->symbol (make-hash-table)))
    (dolist (item table)
      (when (not (symbolp (first item)))
        (error (format nil "First element in pair ~a must be a symbol." item)))
      (when (not (integerp (second item)))
        (error (format nil "Second element in pair ~a must be an integer." item)))
      (when (not (= 2 (length item)))
        (error (format nil "Table pairs must have two elements ~a." item)))
      (when (gethash (first item) symbol->int)
        (error (format nil "Symbol found twice ~a." item)))
      (when (gethash (second item) int->symbol)
        (error (format nil "Integer used twice ~a." item)))
      (setf (gethash (first item) symbol->int) (second item))
      (setf (gethash (second item) int->symbol) (first item)))
    (list symbol->int int->symbol)))

(defun pure-cons (data)
  (and (consp data)
       (not (consp (cdr data)))))

(defun encode-stream (data stream)
  (cond ((integerp data) (encode-integer data stream))
        ((floatp data) (encode-float data stream))
        ((null data) (write-byte #xc0 stream))
        ((eq data t) (write-byte #xc3 stream))
        ((and *use-extensions* (pure-cons data))
         (encode-cons data stream))
        ((stringp data)
         (encode-string data stream))
        ((is-byte-array data)
         (encode-raw-bytes data stream))
        ((or (consp data) (vectorp data))
         (encode-array data stream))
        ((hash-table-p data)
         (encode-hash data stream))
        ((symbolp data)
         (encode-symbol data stream))
        ((and *use-extensions* (rationalp data))
         (encode-rational data stream))
        (t (error
            (format nil
                    "Cannot encode data ~a (maybe you should set *use-extensions* to t?)." data)))))

(defun encode-rational (data stream)
  (labels ((encode-bignum (data)
             (cond ((encodable-as-integer data)
                    (encode-integer data stream))
                   (t
                    (encode-string (write-to-string data) stream)))))
    (write-byte #xc7 stream)
    (encode-bignum (numerator data))
    (encode-bignum (denominator data))))

(defun encode-cons (data stream)
  (write-byte #xc4 stream)
  (encode-stream (car data) stream)
  (encode-stream (cdr data) stream))

(defun encode-symbol (data stream)
  (cond (*use-extensions*
         (let (code)
           (cond ((and *symbol->int* (setf code (gethash data *symbol->int*)))
                  (write-byte #xc6 stream)
                  (encode-integer code stream))
                 (t
                  (write-byte #xc5 stream)
                  (encode-string (package-name (symbol-package data)) stream)
                  (encode-string (symbol-name data) stream)))))
        (t
         (encode-string (symbol-name data) stream))))

(defun encode-string (data stream)
  (encode-raw-bytes (babel:string-to-octets data) stream))

#+sbcl (defun sbcl-encode-float (data stream)
         (cond ((equal (type-of data) 'single-float)
                (write-byte #xca stream)
                (store-big-endian (sb-kernel:single-float-bits data) stream 4))
               ((equal (type-of data) 'double-float)
                (write-byte #xcb stream)
                (store-big-endian (sb-kernel:double-float-high-bits data) stream 4)
                (store-big-endian (sb-kernel:double-float-low-bits data) stream 4))))

#+ccl (defun ccl-encode-double-float (data stream)
        (cond ((equal (type-of data) 'single-float)
               (error "No cl-messagepack support for single precision floats in CCL."))
              ((equal (type-of data) 'double-float)
               (write-byte #xcb stream)
               (multiple-value-bind (hi lo)
                   (ccl::double-float-bits data)
                 (store-big-endian hi stream 4)
                 (store-big-endian lo stream 4)))))

(defun encode-float (data stream)
  (or #+sbcl (sbcl-encode-float data stream)
      #+ccl (ccl-encode-double-float data stream)
      #-(or sbcl ccl) (error "No floating point support yet.")))

(defun encode-each (data stream)
  (cond ((hash-table-p data)
         (maphash (lambda (key value)
                    (encode-stream key stream)
                    (encode-stream value stream))
                  data))
        ((vectorp data)
         (dotimes (i (length data))
           (encode-stream (aref data i) stream)))
        ((consp data)
         (dolist (subdata data)
           (encode-stream subdata stream)))
        (t (error "Not sequence or hash table."))))

(defun encode-sequence-length (data stream
                               short-prefix short-length
                               typecode-16 typecode-32)
  (let ((len (if (hash-table-p data)
                 (hash-table-count data)
                 (length data))))
    (cond ((<= 0 len short-length)
           (write-byte (+ short-prefix len) stream))
          ((<= 0 len 65535)
           (write-byte typecode-16 stream)
           (store-big-endian len stream 2))
          ((<= 0 len (1- (expt 2 32)))
           (write-byte typecode-32 stream)
           (store-big-endian len stream 4)))))

(defun encode-hash (data stream)
  (encode-sequence-length data stream #x80 15 #xde #xdf)
  (encode-each data stream))

(defun encode-array (data stream)
  (encode-sequence-length data stream #x90 15 #xdc #xdd)
  (encode-each data stream))

(defun encode-raw-bytes (data stream)
  (encode-sequence-length data stream #xa0 31 #xda #xdb)
  (write-sequence data stream))

(defun encode-integer (data stream)
  ;; (declare (type fixnum data))
  (cond ((<= 0 data 127) (write-byte data stream))
        ((<= -32 data -1) (write-byte (sb8->ub8 data) stream))
        ((<= 0 data 255)
         (write-byte #xcc stream)
         (write-byte data stream))
        ((<= 0 data 65535)
         (write-byte #xcd stream)
         (store-big-endian data stream 2))
        ((<= 0 data (1- (expt 2 32)))
         (write-byte #xce stream)
         (store-big-endian data stream 4))
        ((<= 0 data (1- (expt 2 64)))
         (write-byte #xcf stream)
         (store-big-endian data stream 8))
        ((<= -128 data 127)
         (write-byte #xd0 stream)
         (write-byte (ldb (byte 8 0) data) stream))
        ((<= -32768 data 32767)
         (write-byte #xd1 stream)
         (store-big-endian (ldb (byte 16 0) data) stream 2))
        ((<= (- (expt 2 31)) data (1- (expt 2 31)))
         (write-byte #xd2 stream)
         (store-big-endian (ldb (byte 32 0) data) stream 4))
        ((<= (- (expt 2 63)) data (1- (expt 2 63)))
         (write-byte #xd3 stream)
         (store-big-endian (ldb (byte 64 0) data) stream 8))
        (t (error "Integer too large or too small."))))

(defun encodable-as-integer (data)
  (or (<= 0 data (1- (expt 2 64)))
      (<= (- (expt 2 63)) data (1- (expt 2 63)))))

(defmacro load-big-endian (stream byte-count)
  (let ((g-stream (gensym "stream")))
    `(let ((,g-stream ,stream)
           (result 0))
       ,@(loop
            repeat byte-count
            collect `(setf result (+ (ash result 8)
                                     (read-byte ,g-stream))))
       result)))

(defun decode (byte-array)
  (flexi-streams:with-input-from-sequence (stream byte-array)
    (decode-stream stream)))

(defun decode-stream (stream)
  (let ((byte (read-byte stream)))
    (cond ((= 0 (ldb (byte 1 7) byte))
           byte)
          ((= 7 (ldb (byte 3 5) byte))
           (ub8->sb8 byte))
          ((= #xcc byte)
           (read-byte stream))
          ((= #xcd byte)
           (load-big-endian stream 2))
          ((= #xce byte)
           (load-big-endian stream 4))
          ((= #xcf byte)
           (load-big-endian stream 8))
          ((= #xd0 byte)
           (ub8->sb8 (read-byte stream)))
          ((= #xd1 byte)
           (ub16->sb16 (load-big-endian stream 2)))
          ((= #xd2 byte)
           (ub32->sb32 (load-big-endian stream 4)))
          ((= #xd3 byte)
           (ub64->sb64 (load-big-endian stream 8)))
          ((= #xc0 byte)
           nil)
          ((= #xc3 byte)
           t)
          ((= #xc2 byte)
           nil)
          ((= #xca byte)
           (or #+sbcl (sb-kernel:make-single-float (load-big-endian stream 4))
               #-(or sbcl) (error "No floating point support yet.")))
          ((= #xcb byte)
           (or #+sbcl (sb-kernel:make-double-float (load-big-endian stream 4)
                                                   (load-big-endian stream 4))
               #+ccl (ccl::double-float-from-bits (load-big-endian stream 4)
                                                  (load-big-endian stream 4))
               #-(or sbcl ccl) (error "No floating point support yet.")))
          ((= 5 (ldb (byte 3 5) byte))
           (decode-string (ldb (byte 5 0) byte) stream))
          ((= #xda byte)
           (decode-string (load-big-endian stream 2) stream))
          ((= #xdb byte)
           (decode-string (load-big-endian stream 4) stream))
          ((= 9 (ldb (byte 4 4) byte))
           (decode-array (- byte #x90) stream))
          ((= #xdc byte)
           (decode-array (load-big-endian stream 2) stream))
          ((= #xdd byte)
           (decode-array (load-big-endian stream 4) stream))
          ((= 8 (ldb (byte 4 4) byte))
           (decode-map (- byte #x80) stream))
          ((= #xde byte)
           (decode-map (load-big-endian stream 2) stream))
          ((= #xdf byte)
           (decode-map (load-big-endian stream 4) stream))
          ((and *use-extensions* (= #xc4 byte))
           (decode-cons stream))
          ((and *use-extensions* (= #xc5 byte))
           (decode-symbol stream))
          ((and *use-extensions* (= #xc6 byte))
           (decode-symbol-as-number stream))
          ((and *use-extensions* (= #xc7 byte))
           (decode-rational stream))
          (t (error
              (format nil
                      "Cannot decode ~a (maybe you should set *use-extensions* to t?)" byte))))))

(defun decode-rational (stream)
  (let ((numerator (decode-stream stream))
        (denominator (decode-stream stream)))
    (when (stringp numerator)
      (setf numerator (parse-integer numerator)))
    (when (stringp denominator)
      (setf denominator (parse-integer denominator)))
    (/ numerator denominator)))

(defun decode-symbol-as-number (stream)
  (when (null *int->symbol*)
    (error "No int->symbol table defined."))
  (let ((code (decode-stream stream)))
    (when (not (integerp code))
      (error "Code for symbol (extension #xC6) not an integer."))
    (let ((symbol (gethash code *int->symbol*)))
      (when (not symbol)
        (error (format nil "Integer ~a not found in int->symbol table." code)))
      symbol)))

(defun decode-symbol (stream)
  (let ((package-name (decode-stream stream))
        (symbol-name (decode-stream stream)))
    (intern symbol-name (find-package package-name))))

(defun decode-cons (stream)
  (cons (decode-stream stream)
        (decode-stream stream)))

(defun decode-map (length stream)
  (if *decoder-prefers-alists*
      (loop
         repeat length
         collect (cons (decode-stream stream)
                       (decode-stream stream)))
      (let ((hash-table (make-hash-table :test #'equalp)))
        (loop repeat length
           do (let ((key (decode-stream stream))
                    (value (decode-stream stream)))
                (setf (gethash key hash-table) value)))
        hash-table)))

(defun decode-array (length stream)
  (if *decoder-prefers-lists*
      (let (result)
        (dotimes (i length)
          (push (decode-stream stream) result))
        (reverse result))
      (let ((array (make-array length)))
        (dotimes (i length)
          (setf (aref array i) (decode-stream stream)))
        array)))

(defun decode-string (length stream)
  (let ((seq (make-array length :element-type '(mod 256))))
    (read-sequence seq stream)
    (babel:octets-to-string seq)))

