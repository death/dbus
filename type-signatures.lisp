;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Type Signatures

(defun sigexp (object)
  "Return the signature expression corresponding to the object passed.
If the object is a string, it is assumed to be a signature string,
otherwise it is assumed to be a signature expression and is returned
as-is."
  (if (stringp object)
      (with-input-from-string (in object)
        (parse-signature-from-stream in))
      object))

(defun parse-signature-from-stream (stream &optional end-of-stream num-elements)
  "Parse a signature string from a character stream and return the
corresponding signature expression.

The value of END-OF-STREAM determines when to stop parsing.  If it is
NIL (the default), parsing is stopped when there are no more
characters left to read from the stream.  If it is a character,
parsing is stopped when the same character is read from the stream.

The value of NUM-ELEMENTS determines how many elements (types) should
be read before parsing is stopped.  If it is NIL (the default), there
is no bound on the number of elements to be read."
  (loop with result = '()
        with element-no = 0
        for char = (read-char stream nil nil)
        until (or (eql char end-of-stream)
                  (null char)) do
        (flet ((add (type)
                 (push type result)
                 (when (eql (incf element-no) num-elements)
                   (loop-finish))))
          (ecase char
            (#\y (add :byte))
            (#\b (add :boolean))
            (#\n (add :int16))
            (#\q (add :uint16))
            (#\i (add :int32))
            (#\u (add :uint32))
            (#\x (add :int64))
            (#\t (add :uint64))
            (#\d (add :double))
            (#\s (add :string))
            (#\o (add :object-path))
            (#\g (add :signature))
            (#\a (add (cons :array (parse-signature-from-stream stream nil 1))))
            (#\( (add (cons :struct (parse-signature-from-stream stream #\)))))
            (#\v (add :variant))
            (#\{ (add (cons :dict-entry (parse-signature-from-stream stream #\}))))))
        finally (return (nreverse result))))

(defun signature (object)
  "Return the signature string corresponding to the object passed.
If the object is a string, it is assumed to be a signature string and
is returned as-is, otherwise it is assumed to be a signature
expression."
  (if (stringp object)
      object
      (with-output-to-string (out)
        (format-sigexp-to-stream object out))))

(defun format-sigexp-to-stream (sigexp stream)
  "Format a signature expression as a signature string into a
character stream."
  (flet ((out (char) (write-char char stream)))
    (dolist (type sigexp)
      (etypecase type
        ((eql :byte) (out #\y))
        ((eql :boolean) (out #\b))
        ((eql :int16) (out #\n))
        ((eql :uint16) (out #\q))
        ((eql :int32) (out #\i))
        ((eql :uint32) (out #\u))
        ((eql :int64) (out #\x))
        ((eql :uint64) (out #\t))
        ((eql :double) (out #\d))
        ((eql :string) (out #\s))
        ((eql :object-path) (out #\o))
        ((eql :signature) (out #\g))
        ((cons (eql :array)) (out #\a) (format-sigexp-to-stream (cdr type) stream))
        ((cons (eql :struct)) (out #\() (format-sigexp-to-stream (cdr type) stream) (out #\)))
        ((eql :variant) (out #\v))
        ((cons (eql :dict-entry)) (out #\{) (format-sigexp-to-stream (cdr type) stream) (out #\}))))))
