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

(in-package :mpk-tests)

(defvar *test1*)
(defvar *test2*)
(defvar *test-size*)

(setf *test-size* 2000)

(setf *test1* (loop for i from 1 to *test-size* collect i))

(setf *test2* (loop for i from 1 to *test-size* collect
                   `(('a ,i)
                     ('b ,(format nil "~a %" (* 100 (/ i *test-size*))))
                     ('c ,(- i)))))

(defun benchmark (&optional (test *test1*))
  (let (encoded-json encoded-msgpack)
    (time (setf encoded-json (with-output-to-string (str)
                               (json:encode-json test str))))
    (time (length (with-input-from-string (str encoded-json)
                    (json:decode-json str))))
    (time (length (with-input-from-string (str (with-output-to-string (str)
                                                 (json:encode-json test str)))
                    (json:decode-json str))))

    (time (setf encoded-msgpack (encode test)))
    (time (length (decode encoded-msgpack)))
    (time (length (decode (encode test))))

    (values)))
