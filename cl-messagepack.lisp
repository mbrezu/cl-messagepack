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

(defun alistp (l)
  "Alist predicate"
  (and (consp l) (consp (car l)) (atom (caar l))))

(defun plistp (l)
  "Plist predicate."
  (and (consp l) (keywordp (car l)) (consp (cdr l))))

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

(defvar *use-false* nil)
(defvar *symbol->int* nil)
(defvar *int->symbol* nil)
(defvar *symbol->int* nil)
(defvar *encode-alist-as-map* T)
(defvar *decoder-prefers-lists* nil)
(defvar *decoder-prefers-alists* nil)
(defvar *decode-bin-as-string* nil)

(defvar *extended-types* nil)
(defvar *lookup-table* nil)

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
        ((eq data :false) (write-byte #xc2 stream))
        ((eq data t) (write-byte #xc3 stream))
        ((stringp data)
         (encode-string data stream))
        ((is-byte-array data)
         (encode-raw-bytes data stream))
        ((vectorp data)
         (encode-array data stream))
        ((consp data)
         (if (or (and *encode-alist-as-map* (alistp data)) (plistp data))
             (encode-hash data stream)
             (encode-array data stream)))
        ((hash-table-p data)
         (encode-hash data stream))
        ((symbolp data)
         (encode-symbol data stream))
        ((and *extended-types*
              (typep data 'extension-type)
              (try-encode-ext-type data stream))
         t)
        (t (error
            (format nil
                    "Cannot encode data ~a (maybe you should bind *extended-types*?)." data)))))

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
  ;; TODO: bind *package* to :keywords, so that the package is included as well?
  (encode-string (symbol-name data) stream))

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
        ((and *encode-alist-as-map* (alistp data))
         (dolist (pair data)
           (encode-stream (car pair) stream)
           (encode-stream (cdr pair) stream)))
        ((plistp data)
         (loop
           for lst on data by #'cddr
           do (progn (encode-stream (car  lst) stream)
                     (encode-stream (cadr lst) stream))))
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
  (let ((len (cond ((hash-table-p data) (hash-table-count data))
		   ((plistp data) (let ((ln (length data)))
				    (if (evenp ln)
					(/ ln 2)
					(error "Malformed plist ~s. Length should be even." data))))
		   (t (length data)))))
    (cond ((and (<= 0 len short-length) (plusp short-length))
           (write-byte (+ short-prefix len) stream))
          ((and (<= 0 len #xff) (zerop short-length))
           (write-byte short-prefix stream)
           (write-byte len stream)
           (store-big-endian len stream 1))
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

(defun parse-big-endian (byte-array)
  ;; TODO: do words at once?
  (loop with result = 0
        for b across byte-array
        do (setf result (+ (ash result 8)
                           b))
        finally (return result)))

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
          ((<= #xd4 byte #xd8) ; fixext1: type, data
           (let ((len (ash 1 (- byte #xd4))))
             (typed-data (read-byte stream)
                         (decode-byte-array len stream))))
          ((= #xc7 byte)
           (let ((len (read-byte stream)))
             (typed-data (read-byte stream)
                         (decode-byte-array len stream))))
          ((= #xc8 byte)
           (let ((len (load-big-endian stream 2)))
             (typed-data (read-byte stream)
                         (decode-byte-array len stream))))
          ((= #xc9 byte)
           (let ((len (load-big-endian stream 4)))
             (typed-data (read-byte stream)
                         (decode-byte-array len stream))))
          ((= #xc0 byte)
           nil)
          ((= #xc3 byte)
           t)
          ((= #xc2 byte)
           (if *use-false* :false nil))
          ((= #xca byte)
           (or #+sbcl (sb-kernel:make-single-float (ub32->sb32 (load-big-endian stream 4)))
               #-(or sbcl) (error "No floating point support yet.")))
          ((= #xcb byte)
           (or #+sbcl (sb-kernel:make-double-float (ub32->sb32 (load-big-endian stream 4))
                                                   (load-big-endian stream 4))
               #+ccl (ccl::double-float-from-bits (load-big-endian stream 4)
                                                  (load-big-endian stream 4))
               #-(or sbcl ccl) (error "No floating point support yet.")))
          ((= 5 (ldb (byte 3 5) byte))
           (decode-string (ldb (byte 5 0) byte) stream))
          ((= #xd9 byte)
           (decode-string (read-byte stream) stream))
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
          ((= #xc4 byte)
           (funcall (if *decode-bin-as-string* #'decode-string #'decode-byte-array) (read-byte stream) stream))
          ((= #xc5 byte)
           (funcall (if *decode-bin-as-string* #'decode-string #'decode-byte-array) (load-big-endian stream 2) stream))
          ((= #xc6 byte)
           (funcall (if *decode-bin-as-string* #'decode-string #'decode-byte-array) (load-big-endian stream 4) stream))
          (t (error
              (format nil
                      "Cannot decode ~a (maybe you should bind *extended-types*?)" byte))))))

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

(defun decode-byte-array (length stream)
  (let ((seq (make-array length :element-type '(mod 256))))
    (read-sequence seq stream)
    seq))

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



;; How to get type-num for the types?
;; A class would have a :allocation :class ...
;; A pointer to the e-t-d would be longer than the int itself.
(defclass extension-type ()
  ((id :initform (error "need an ID")
       :initarg id
       :reader extension-type-id
       :writer (setf extension-type-id)
       :type (or integer (array (unsigned-byte 8) *))))
  (:documentation
    "Base type for Ext-Types."))

(defmethod print-object ((obj extension-type) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "~a" (extension-type-id obj))))


(defclass extension-type-description ()
  #. (mapcar (lambda (d)
               (destructuring-bind (name init &rest rest) d
                 `(,name :initform ,init
                         :initarg ,(intern (symbol-name name) :keyword)
                         :reader ,name
                         :writer (setf ,name)
                         ,@ rest)))
             '((type-number     nil    :type (integer 0 127))
               (encode-with     nil    :type function)
               (decode-with     nil    :type function)
               (as-numeric      nil    :type (member t nil))
               (reg-class       nil)
               )))

(defmethod print-object ((obj extension-type-description) stream)
  (print-unreadable-object (obj stream :type T :identity T)
    (format stream "~a ~d"
            (class-name (reg-class obj))
            (type-number obj))))


(defun symbol-to-extension-type (num sym decode-as)
  (assert (member decode-as '(:numeric :byte-array)))
  (let ((num? (eq decode-as :numeric)))
    (unless (find-class sym nil)
      (closer-mop:ensure-class
        sym
        :direct-superclasses '(extension-type)))
    (flet
      ((maybe-cache (obj id)
         (if *lookup-table*
           (or
             (lookup-table-find num id)
             (lookup-table-insert num id obj))
           obj)))
      (make-instance 'extension-type-description
                     :type-number  num
                     :reg-class    (find-class sym)
                     :encode-with  (lambda (obj)
                                     ;; TODO: better use EXTENSION-TYPE-ID?
                                     (let ((id (slot-value obj 'id)))
                                       ;; store outgoing objects...
                                       (maybe-cache obj id)
                                       id))
                     :decode-with  (lambda (id)
                                     ;; TODO: (if num? ( ... ) x)?
                                     (let ((obj (make-instance sym
                                                               'id id)))
                                       ;; store incoming objects...
                                       ;; TODO: what if that object already exists?
                                       (or
                                         (maybe-cache obj id)
                                         obj)))
                     :as-numeric   num?))))


(defun typed-data (type-num bytes)
  (let ((ext-type (find type-num *extended-types*
                        :test #'eql
                        :key #'type-number)))
    ;; TODO: better throw or error?
    (assert ext-type)
    (funcall (decode-with ext-type)
             (if (as-numeric ext-type)
               (parse-big-endian bytes)
               bytes))))

(defun try-encode-ext-type (obj stream)
  (let ((ext-type (find (class-of obj) *extended-types*
                        :test #'eq
                        :key #'reg-class)))
    ;; doesn't run ENCODE-WITH function?!
    (when ext-type
      (let* ((id (funcall (encode-with ext-type) obj))
             (bytes (if (numberp id)
                      (flexi-streams:with-output-to-sequence (s)
                        (encode-integer id s))
                      id))
             (len (length bytes)))
        ;; TODO: in theory the ID might be longer than 256 bytes...
        ;; (encode-sequence-length bytes stream #xc7 0 #xc8 #xc9)
        ;; but we need the type inbetween.
        (assert (<= 0 len #xff))
        (write-byte #xc7 stream)
        (write-byte len stream)
        (write-byte (type-number ext-type) stream)
        (write-sequence bytes stream))
      T)))


(defun define-extension-types (args)
  "This function defines types for the MessagePack extension type system
   (#xD4 to #xD8, and #xC7 to #xC9), and returns a list of them
   that can be bound to *EXTENSION-TYPES*.
   128 different types can be available simultaneously at any one time.

   This function takes integers, flags, and/or closures as arguments;
   these get used as items for the next arguments.
   * Integers define which type number to use next.
   * Flags for decoding:
       :BYTE-ARRAY    - return the bytes as array. Default.
       :NUMERIC       - return value in DATA as a number. Only for fixextN.
   * A symbol associates the current type number to this type;
       this type should be derived from MESSAGEPACK-EXT-TYPE, as
       to have a correct MESSAGEPACK:ID slot.

   Example:
   (defvar *my-extension-types*
     (define-extension-types :numeric
                             5 'buffer 'block
                             8 'cursor))
   Eg., the type 6 would then return (MAKE-BLOCK 'ID <content>)."
  (loop with type-num = 0
        with decode-as = :byte-array
        ; with encode
        for el in args
        append (cond
                 ((numberp el)
                  (if (<= 0 el 127)
                    (setf type-num el)
                    (error "Integer ~a out of range." el))
                  nil)
                 ((member el '(:byte-array :numeric))
                  (setf decode-as el)
                  nil)
                 ((keywordp el)
                  (error "Keywords ~s not in use." el))
                 ((symbolp el)
                  (prog1
                    (list (symbol-to-extension-type type-num el decode-as))
                    (incf type-num)))
                 (T
                  (error "~s not understood." el))
                 )))


(defun make-lookup-table ()
  "Returns something that can be used for *LOOKUP-TABLE*."
  (make-hash-table :test #'equalp))

(defun lookup-table-insert (type id obj)
  (setf (gethash (cons type id) *lookup-table*) obj))

(defun lookup-table-find (type id)
  (gethash (cons type id) *lookup-table*))
