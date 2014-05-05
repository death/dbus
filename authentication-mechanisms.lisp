;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Authentication mechanisms

(define-name-class-mapping
  :class authentication-mechanism
  :map *authentication-mechanism-classes*
  :find find-authentication-mechanism-class)

(defclass standard-authentication-mechanism (authentication-mechanism)
  ((name :initarg :name :reader authentication-mechanism-name)
   (textual :initarg :textual :reader authentication-mechanism-textual-p))
  (:default-initargs :textual nil)
  (:documentation "Represents a standard authentication mechanism."))

(defclass generic-authentication-mechanism (standard-authentication-mechanism)
  ()
  (:documentation "Represents an authentication mechanism that is not
supported by the D-BUS system."))

(defmethod feed-authentication-mechanism ((mechanism generic-authentication-mechanism) challenge)
  (declare (ignore challenge))
  (values :error))

(defun parse-authentication-response (line &key as-string)
  "Parse authentication response line and return two values:

  :REJECTED

    Current authentication exchanged failed; the second value is a
    list of authentication mechanisms.

  :OK

    Client has been authenticated; the second value is the server's
    UUID.

  :DATA

    Data are available; the second value is either an octet vector or
    a string, depending on the value of AS-STRING.

  :ERROR

    Bad command or arguments; the second value is NIL.

  :UNEXPECTED

    Unexpected command; the second value is the response line."
  (cond ((starts-with-subseq "REJECTED " line)
         (values :rejected (split-sequence #\Space line :start 9)))
        ((starts-with-subseq "OK " line)
         (values :ok (subseq line 3)))
        ((starts-with-subseq "DATA " line)
         (let ((data (decode-hex-string line :start 5)))
           (values :data (if as-string (octets-to-string data :encoding :utf-8) data))))
        ((starts-with-subseq "ERROR " line)
         (values :error nil))
        (t (values :unexpected line))))

(defun format-authentication-command (command &rest arguments)
  "Format and return authentication command line.  Command is one
of :AUTH, :CANCEL, :BEGIN, :DATA, or :ERROR, and takes arguments in
accordance with the D-BUS specification."
  (ecase command
    (:auth
     (destructuring-bind (&optional mechanism initial-response) arguments
       (format nil "AUTH ~@[~A~]~@[ ~A~]" mechanism initial-response)))
    (:cancel "CANCEL ")
    (:begin "BEGIN ")
    (:data
     (destructuring-bind (data) arguments
       (format nil "DATA ~A" (encode-hex-string data))))
    (:error
     (destructuring-bind (&optional explanation) arguments
       (format nil "ERROR ~@[~A~]" explanation)))))

(defun receive-authentication-response (connection &key as-string expect)
  "Receive authentication response line from the server.  If EXPECT is
NIL, just return the response command and argument.  Otherwise,
compare its value to the response command.  If they are the same, just
return the argument; otherwise, signal an authentication error."
  (multiple-value-bind (command argument)
      (parse-authentication-response (receive-line connection)
                                     :as-string as-string)
    (cond ((null expect) (values command argument))
          ((eq command expect) argument)
          (t (error 'authentication-error :command command :argument argument)))))

(defun send-authentication-command (connection command &rest arguments)
  "Send an authentication command to the server."
  (send-line (apply #'format-authentication-command command arguments)
             connection))
