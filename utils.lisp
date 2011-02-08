;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Utilities

(defun make-octet-vector (size &rest array-options)
  "Return a fresh vector whose element type is (unsigned-byte 8)."
  (apply #'make-array size :element-type '(unsigned-byte 8) array-options))

(define-condition inexistent-entry (error)
  ((designator :initarg :designator :reader inexistent-entry-designator))
  (:report (lambda (condition stream)
             (format stream "An inexistent entry was sought using ~S as designator."
                     (inexistent-entry-designator condition)))))

(defun inexistent-entry (designator if-does-not-exist)
  "Called when an inexistent entry was sought using DESIGNATOR, and
acts according to the value of IF-DOES-NOT-EXIST:

  :ERROR

    Signal an INEXISTENT-ENTRY error with a USE-VALUE restart.

  NIL

    Return NIL."
  (ecase if-does-not-exist
    (:error
     (restart-case (error 'inexistent-entry :designator designator)
       (use-value (new-value)
         :report "Use a value as entry."
         :interactive prompt-for-value
         new-value)))
    ((nil) nil)))

(defun prompt-for-value ()
  "Interactively prompt for a value.  An expression is read and
evaluated, and its value is returned."
  (format t "Enter an expression to yield a value: ")
  (multiple-value-list (eval (read))))

(define-condition entry-replacement-attempt (error)
  ((old :initarg :old :reader entry-replacement-attempt-old)
   (new :initarg :new :reader entry-replacement-attempt-new))
  (:report (lambda (condition stream)
             (format stream "Attempted to replace ~S by ~S."
                     (entry-replacement-attempt-old condition)
                     (entry-replacement-attempt-new condition)))))

(defun replace-entry-p (old new if-exists)
  "Return true if the new entry should replace the old one.  IF-EXISTS
determines how to find out:

  :ERROR

    Signal an ENTRY-REPLACEMENT-ATTEMPT error with a CONTINUE restart
    to replace the entry, and an ABORT restart to not replace it.

  :WARN

    Replace the entry after signaling a warning.

  :DONT-REPLACE

    Don't replace entry.

  :REPLACE

    Replace entry."
  (flet ((replace-it () (return-from replace-entry-p t))
         (dont-replace-it () (return-from replace-entry-p nil)))
    (ecase if-exists
      (:error
       (restart-case (error 'entry-replacement-attempt :old old :new new)
         (continue ()
           :report "Replace old entry."
           (replace-it))
         (abort ()
           :report "Don't replace old entry."
           (dont-replace-it))))
      (:warn
       (warn "Replacing existing entry ~S with ~S." old new)
       (replace-it))
      (:dont-replace
       (dont-replace-it))
      (:replace
       (replace-it)))))

(defun encode-hex-string (data &key (start 0) end)
  "Encode a string composed of hexadecimal digit character pairs, each
representing an octet.  The input is either an octet vector, or a
UTF-8 string that will be converted to one.

START and END are bounding index designators for the data."
  (etypecase data
    (string
     (encode-hex-string
      (string-to-octets data :encoding :utf-8 :start start :end end)))
    (vector
     (with-output-to-string (out)
       (loop for index from start below (or end (length data))
             for octet = (aref data index) do 
             (write-char (char-downcase (digit-char (ash octet -4) 16)) out)
             (write-char (char-downcase (digit-char (logand octet #x0F) 16)) out))))))

(defun decode-hex-string (string &key (start 0) end)
  "Decode a string composed of hexadecimal digit character pairs, each
representing an octet, to an octet vector with the corresponding
octets.

START and END are bounding index designators for the string."
  (when (null end)
    (setf end (length string)))
  (assert (evenp (- end start)))
  (let ((octets (make-octet-vector (/ (- end start) 2) :fill-pointer 0)))
    (with-input-from-string (in string :start start :end end)
      (loop for hi = (read-char in nil nil)
            for lo = (read-char in nil nil)
            until (null hi)
            do (vector-push (logior (ash (digit-char-p hi 16) 4)
                                    (digit-char-p lo 16))
                            octets)))
    octets))

(defun call-with-if-failed-handler (if-failed function)
  "Call FUNCTION in a context according to IF-FAILED:

  :ERROR

    Signal an error on failure.

  NIL

    Return NIL on failure."
  (ecase if-failed
    (:error (funcall function))
    ((nil) (ignore-errors (funcall function)))))

(defmacro with-if-failed-handler (if-failed-form &body forms)
  "Sugar for CALL-WITH-IF-FAILED-HANDLER."
  `(call-with-if-failed-handler ,if-failed-form (lambda () ,@forms)))

(defun current-username ()
  "Return the current user's name."
  (nth-value 0 (getpwuid (getuid))))

(defmacro with-binary-writers ((stream endianness &key prefix) &body forms)
  "Evaluate forms with functions to write binary data to the stream in
a given endianness.

  STREAM

    A form evaluating to a binary output stream with a file position.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

  PREFIX

    Either NIL (the default) or a string designator.  In the latter
    case, the following function names will be symbols interned in the
    current package, with <PREFIX>-<NAME> names, e.g., OUTPUT-U8 if
    the prefix is OUTPUT.

Local functions:

  ALIGN

    A function that takes an integer and ensures the stream's file
    position is aligned to it.  It does so by writing the appropriate
    number of 0 octets.

  U8, U16, U32, U64

    Functions that take 8-, 16-, 32-, and 64-bit unsigned byte values,
    respectively, and write these values to the stream, in the
    appropriate endianness.  The values are always naturally aligned
    before written."
  (destructuring-bind (align u8 u16 u32 u64)
      (mapcar (lambda (symbol)
                (if (null prefix)
                    symbol
                    (intern (format nil "~A-~A" prefix symbol))))
              '(align u8 u16 u32 u64))
    (once-only (stream)
      (with-gensyms (body-function-name u8-var u16-var u32-var u64-var)
        `(flet ((,body-function-name (,u8-var ,u16-var ,u32-var ,u64-var)
                  (labels ((,align (n)
                             (loop until (zerop (mod (file-position ,stream) n)) do (,u8 0)))
                           (,u8 (value)
                             (funcall ,u8-var value))
                           (,u16 (value)
                             (,align 2)
                             (funcall ,u16-var value))
                           (,u32 (value)
                             (,align 4)
                             (funcall ,u32-var value))
                           (,u64 (value)
                             (,align 8)
                             (funcall ,u64-var value)))
                    (declare (inline ,align ,u8 ,u16 ,u32 ,u64))
                    (declare (ignorable #',align #',u8 #',u16 #',u32 #',u64))
                    ,@forms)))
           (ecase ,endianness
             (:little-endian
              (macrolet ((u (size)
                           `(lambda (value)
                              ,@(loop for i from 0 below size by 8
                                      collect `(write-byte (ldb (byte 8 ,i) value) ,',stream)))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))
             (:big-endian
              (macrolet ((u (size)
                           `(lambda (value)
                              ,@(loop for i from (- size 8) downto 0 by 8
                                      collect `(write-byte (ldb (byte 8 ,i) value) ,',stream)))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))))))))

(defvar *stream-read-positions*
  (make-weakly-keyed-hash-table)
  "A mapping from a stream (weakly referenced) to a read position.")

(defun stream-read-position (stream)
  "Return the stream's read position (zero by default)."
  (gethash stream *stream-read-positions* 0))

(defun (setf stream-read-position) (new-read-position stream)
  "Set the stream's read position to a new value."
  (setf (gethash stream *stream-read-positions*) new-read-position))

(defmacro with-binary-readers ((stream endianness &key prefix) &body forms)
  "Evaluate forms with functions to read binary data from the stream
in a given endianness.

  STREAM

    A form evaluating to a binary input stream.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

  PREFIX

    Either NIL (the default) or a string designator.  In the latter
    case, the following function names will be symbols interned in the
    current package, with <PREFIX>-<NAME> names, e.g., INPUT-U8 if
    the prefix is INPUT.

Local functions:

  ALIGN

    A function that takes an integer and ensures the stream's read
    position is aligned to it.  It does so by reading and ignoring the
    appropriate number of octets.

  U8, U16, U32, U64

    Functions that read 8-, 16-, 32-, and 64-bit unsigned byte values,
    respectively, from the stream, in the appropriate endianness.  The
    read position is ensured to be naturally aligned before reading
    the value."
  (destructuring-bind (align u8 u16 u32 u64)
      (mapcar (lambda (symbol)
                (if (null prefix)
                    symbol
                    (intern (format nil "~A-~A" prefix symbol))))
              '(align u8 u16 u32 u64))
    (once-only (stream)
      (with-gensyms (body-function-name u8-var u16-var u32-var u64-var)
        `(flet ((,body-function-name (,u8-var ,u16-var ,u32-var ,u64-var)
                  (labels ((,align (n)
                             (loop until (zerop (mod (stream-read-position ,stream) n)) do (,u8)))
                           (,u8 ()
                             (funcall ,u8-var))
                           (,u16 ()
                             (,align 2)
                             (funcall ,u16-var))
                           (,u32 ()
                             (,align 4)
                             (funcall ,u32-var))
                           (,u64 ()
                             (,align 8)
                             (funcall ,u64-var)))
                    (declare (inline ,align ,u8 ,u16 ,u32 ,u64))
                    (declare (ignorable #',align #',u8 #',u16 #',u32 #',u64))
                    ,@forms)))
           (ecase ,endianness
             (:little-endian
              (macrolet ((u (size)
                           `(lambda ()
                              (let ((value 0))
                                ,@(loop for i from 0 below size by 8
                                        collect `(setf (ldb (byte 8 ,i) value)
                                                       (read-byte ,',stream)))
                                (incf (stream-read-position ,',stream) ,(floor size 8))
                                value))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))
             (:big-endian
              (macrolet ((u (size)
                           `(lambda ()
                              (let ((value 0))
                                ,@(loop for i from (- size 8) downto 0 by 8
                                        collect `(setf (ldb (byte 8 ,i) value)
                                                       (read-byte ,',stream)))
                                (incf (stream-read-position ,',stream) ,(floor size 8))
                                value))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))))))))
  
(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given
size."
  (ldb (byte size 0) value))

(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given
size."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defmacro define-name-class-mapping (&key class map find)
  "Define an interface for mapping names (strings) to classes (or
class names)."
  (let ((map-docstring (format nil "Map names to ~A classes or class names." class))
        (find-docstring (format nil "Return the ~A class (or class name) corresponding to NAME." class))
        (find-setf-docstring (format nil "Associate a ~A class (or class name) with NAME." class)))
    `(progn
       (defvar ,map
         (make-hash-table :test 'equal)
         ,map-docstring)
       (defun ,find (name &key (if-does-not-exist :error))
         ,find-docstring
         (or (gethash name ,map)
             (inexistent-entry name if-does-not-exist)))
       (defun (setf ,find) (class name &key (if-exists :warn))
         ,find-setf-docstring
         (when-let (old (,find name :if-does-not-exist nil))
           (when (not (replace-entry-p old class if-exists))
             (return-from ,find class)))
         (setf (gethash name ,map) class))
       ',class)))
