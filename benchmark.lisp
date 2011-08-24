
(in-package :mpk)

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