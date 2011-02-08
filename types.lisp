;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Defining DBUS types

(defclass dbus-type ()
  ((name :initarg :name :reader dbus-type-name)
   (signature :initarg :signature :reader dbus-type-signature)
   (sigexp-formatter :initarg :sigexp-formatter :reader dbus-type-sigexp-formatter)
   (signature-parser :initarg :signature-parser :reader dbus-type-signature-parser)
   (alignment :initarg :alignment :reader dbus-type-alignment)
   (packer :initarg :packer :reader dbus-type-packer)
   (unpacker :initarg :unpacker :reader dbus-type-unpacker)))

(defmethod print-object ((type dbus-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~S" (dbus-type-name type)))
  type)

(defclass dbus-type-table ()
  ((by-name :initform (make-hash-table) :reader dbus-type-table-by-name)
   (by-signature :initform (make-hash-table) :reader dbus-type-table-by-signature)))

(defvar *dbus-type-table*
  (make-instance 'dbus-type-table))

(defun find-dbus-type (designator &optional (table *dbus-type-table*))
  (etypecase designator
    (dbus-type (values designator '()))
    (symbol
     (values
      (or (gethash designator (dbus-type-table-by-name table))
          (error "Can't find DBUS type with name ~S." designator))
      '()))
    (character
     (values
      (or (gethash designator (dbus-type-table-by-signature table))
          (error "Can't find DBUS type with signature ~S." designator))
      '()))
    ((cons symbol)
     (values (find-dbus-type (first designator) table) (rest designator)))))

(defun register-dbus-type (type &optional (table *dbus-type-table*))
  (setf (gethash (dbus-type-name type) (dbus-type-table-by-name table)) type)
  (setf (gethash (dbus-type-signature type) (dbus-type-table-by-signature table)) type)
  table)

(defun formatter/parser-for-type (name signature composite)
  (etypecase composite
    ((eql nil)
     (values `(lambda (stream element-types)
                (declare (ignore element-types))
                (write-char ',signature stream))
             `(lambda (stream)
                (declare (ignore stream))
                ',name)))
    ((eql t)
     (values `(lambda (stream element-types)
                (write-char ',signature stream)
                (format-sigexp-to-stream element-types stream))
             `(lambda (stream)
                (cons ',name (parse-signature-from-stream stream nil 1)))))
    (character
     (values `(lambda (stream element-types)
                (write-char ',signature stream)
                (format-sigexp-to-stream element-types stream)
                (write-char ',composite stream))
             `(lambda (stream)
                (prog1 (cons ',name (parse-signature-from-stream stream ',composite))
                  (read-char stream)))))))

(defmacro define-dbus-type (name &key signature composite alignment pack unpack)
  (multiple-value-bind (formatter parser)
      (formatter/parser-for-type name signature composite)
    `(progn
       (register-dbus-type
        (make-instance 'dbus-type
                       :name ',name
                       :signature ',signature
                       :sigexp-formatter ,formatter
                       :signature-parser ,parser
                       :alignment ',alignment
                       :packer (lambda (stream endianness element-types value)
                                 (declare (ignorable element-types value))
                                 (with-binary-writers (stream endianness)
                                   (align ',alignment)
                                   ,pack))
                       :unpacker (lambda (stream endianness element-types)
                                   (declare (ignorable element-types))
                                   (with-binary-readers (stream endianness)
                                     (align ',alignment)
                                     ,unpack))))
       ',name)))

(defun pack-1 (stream endianness type value)
  "Pack a single DBUS value into stream."
  (multiple-value-bind (type element-types) (find-dbus-type type)
    (funcall (dbus-type-packer type) stream endianness element-types value)))

(defun unpack-1 (stream endianness type)
  "Unpack a single DBUS value from stream."
  (multiple-value-bind (type element-types) (find-dbus-type type)
    (funcall (dbus-type-unpacker type) stream endianness element-types)))

(defun pack-seq (stream endianness types values)
  "Pack a sequence of values into stream."
  (map nil (lambda (type value) (pack-1 stream endianness type value)) types values))

(defun unpack-seq (stream endianness types)
  "Unpack a sequence of DBUS values from stream."
  (map 'list (lambda (type) (unpack-1 stream endianness type)) types))

(defun pack-string (stream endianness value length-size)
  "Pack DBUS string into stream."
  (with-binary-writers (stream endianness)
    (let ((octets (babel:string-to-octets value :encoding :utf-8)))
      (ecase length-size
        (8 (u8 (length octets)))
        (32 (u32 (length octets))))
      (map nil #'u8 octets)
      (u8 0))))

(defun unpack-string (stream endianness length)
  "Unpack DBUS string from stream."
  (with-binary-readers (stream endianness)
    (prog1 (babel:octets-to-string
            (map-into (make-octet-vector length) #'u8)
            :encoding :utf-8)
      (u8))))

(defun pack-array (stream endianness element-type value)
  "Pack DBUS array into stream."
  (with-binary-writers (stream endianness)
    (let ((length-position (file-position stream)))
      (u32 0)
      (align (alignment element-type))
      (let ((start-position (file-position stream)))
        (pack-seq stream endianness (circular-list element-type) value)
        (let ((end-position (file-position stream)))
          (file-position stream length-position)
          (u32 (- end-position start-position))
          (file-position stream end-position))))))

(defun unpack-array (stream endianness element-type length)
  "Unpack DBUS array from stream."
  (with-binary-readers (stream endianness)
    (align (alignment element-type))
    (loop with start = (stream-read-position stream)
          with end = (+ start length)
          until (= end (stream-read-position stream))
          collect (unpack-1 stream endianness element-type))))

(defun pack-variant (stream endianness element-types value)
  "Pack DBUS variant into stream."
  (pack-1 stream endianness :signature element-types)
  (pack-1 stream endianness (first element-types) value))

(defun unpack-variant (stream endianness)
  "Unpack DBUS variant from stream."
  (with-binary-readers (stream endianness)
    (unpack-1 stream endianness
              (first (sigexp (unpack-string stream endianness (u8)))))))

(defun alignment (type)
  "Return the number of octets to which elements of the supplied type
should be aligned."
  (dbus-type-alignment (find-dbus-type type)))

(defun parse-signature-from-stream (stream &optional terminator-char num-elements)
  "Parse a signature string from a character stream and return the
corresponding signature expression.

The value of TERMINATOR-CHAR determines when to stop parsing.  If it
is NIL (the default), parsing is stopped when there are no more
characters left to read from the stream.  If it is a character,
parsing is stopped when the same character is read from the stream.

The value of NUM-ELEMENTS determines how many elements (types) should
be read before parsing is stopped.  If it is NIL (the default), there
is no bound on the number of elements to be read."
  (loop for num-read from 0
        for char = (peek-char nil stream nil nil)
        until (or (null char) (eql char terminator-char) (eql num-read num-elements))
        collect (let ((type (find-dbus-type char)))
                  (read-char stream)
                  (funcall (dbus-type-signature-parser type) stream))))

(defun format-sigexp-to-stream (sigexp stream)
  "Format a signature expression as a signature string into a
character stream."
  (dolist (subexp sigexp)
    (multiple-value-bind (type element-types) (find-dbus-type subexp)
      (funcall (dbus-type-sigexp-formatter type) stream element-types))))


;;;; Operators related to DBUS types

(defun sigexp (object)
  "Return the signature expression corresponding to the object passed.
If the object is a string, it is assumed to be a signature string,
otherwise it is assumed to be a signature expression and is returned
as-is."
  (if (stringp object)
      (with-input-from-string (in object)
        (parse-signature-from-stream in))
      object))

(defun signature (object)
  "Return the signature string corresponding to the object passed.
If the object is a string, it is assumed to be a signature string and
is returned as-is, otherwise it is assumed to be a signature
expression."
  (if (stringp object)
      object
      (with-output-to-string (out)
        (format-sigexp-to-stream object out))))

(defun pack (stream endianness sigexp &rest values)
  "Pack values according to the signature expression and endianness
into stream."
  (pack-seq stream endianness (sigexp sigexp) values))

(defun unpack (stream endianness sigexp)
  "Unpack values from stream according to endianness and the signature
expression and return them as a list."
  (unpack-seq stream endianness (sigexp sigexp)))
