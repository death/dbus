;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Utilities

(define-condition dbus-error (error)
  ()
  (:documentation "The supertype errors related to the DBUS system."))

(defun make-octet-vector (size &rest array-options)
  "Return a fresh vector whose element type is (unsigned-byte 8)."
  (apply #'make-array size :element-type '(unsigned-byte 8) array-options))

(define-condition inexistent-entry (dbus-error)
  ((designator :initarg :designator :reader inexistent-entry-designator))
  (:report (lambda (condition stream)
             (format stream "An inexistent entry was sought using ~S as designator."
                     (inexistent-entry-designator condition)))))

(defun inexistent-entry (designator if-does-not-exist)
  "Called when an inexistent entry was sought using DESIGNATOR, and
acts according to the value of IF-DOES-NOT-EXIST:

  :ERROR - signal an INEXISTENT-ENTRY error with a USE-VALUE restart.

  NIL - return NIL."
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

(define-condition entry-replacement-attempt (dbus-error)
  ((old :initarg :old :reader entry-replacement-attempt-old)
   (new :initarg :new :reader entry-replacement-attempt-new))
  (:report (lambda (condition stream)
             (format stream "Attempted to replace ~S by ~S."
                     (entry-replacement-attempt-old condition)
                     (entry-replacement-attempt-new condition)))))

(defun replace-entry-p (old new if-exists)
  "Return true if the new entry should replace the old one.

IF-EXISTS determines how to find out:

  :ERROR - signal an ENTRY-ALREADY-EXISTS error with a CONTINUE
           restart to replace the entry, and an ABORT restart to not
           replace it.

  :WARN - replace the entry after signaling a warning.

  :DONT-REPLACE - don't replace entry.

  :REPLACE - replace entry."
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

(defun call-with-if-failed-handler (if-failed function)
  "Call FUNCTION in a context according to IF-FAILED:

  :ERROR - signal an error on failure.

  NIL - return NIL on failure."
  (ecase if-failed
    (:error (funcall function))
    ((nil) (ignore-errors (funcall function)))))

(defmacro with-if-failed-handler (if-failed-form &body forms)
  "Sugar for CALL-WITH-IF-FAILED-HANDLER."
  `(call-with-if-failed-handler ,if-failed-form (lambda () ,@forms)))


;;;; Protocol classes and generic functions

(defclass server-address ()
  ()
  (:documentation "Represents a DBUS server address, consisting of a
transport name and zero or more properties."))

(defclass connection ()
  ()
  (:documentation "Represents a DBUS connection to a server."))

(defclass authentication-mechanism ()
  ()
  (:documentation "Represents a way to authenticate a client with a
server."))

(defgeneric server-address-transport-name (server-address)
  (:documentation "Return the transport name for the server
address."))

(defgeneric server-address-property (name server-address &key if-does-not-exist)
  (:documentation "Return the value of the server address's property
with the supplied name."))

(defgeneric open-connection (server-address &key if-failed)
  (:documentation "Open a connection to the server designated by the
server address and return a connection object.  The default value for
IF-FAILED is :ERROR."))

(defgeneric connection-server-address (connection)
  (:documentation "Return the address of the server associated with
the connection."))

(defgeneric connection-server-uuid (connection)
  (:documentation "Return the unique ID of the server associated with
the connection."))

(defgeneric (setf connection-server-uuid) (uuid connection)
  (:documentation "Set the unique ID of the server associated with the
connection.  If an ID is already set and is not EQUAL to the new ID,
signal a continuable error."))

(defgeneric close-connection (connection)
  (:documentation "Close an open connection."))

(defgeneric receive-line (connection)
  (:documentation "Read a line of text from the server and return it as
a string.  The operation blocks until a whole line can be read.  The
string will not contain newline characters."))

(defgeneric send-line (line connection)
  (:documentation "Send a line of text, represented by a string, to
the server.  The operation will force (but not finish) output before
returning.  The string should not contain any newline characters."))

(defgeneric send-nul-byte (connection)
  (:documentation "Send a \"nul byte\" (i.e. an octet whose value is
0) to the server.  The operation will force (but not finish) output
before returning."))

(defgeneric authentication-mechanism-name (authentication-mechanism)
  (:documentation "Return the name for the authentication
mechanism."))

(defgeneric supported-authentication-mechanisms (connection)
  (:documentation "Return a list of authentication mechanisms
supported by the server."))

(defgeneric authenticate (authentication-mechanism connection &key if-failed)
  (:documentation "Attempt to authenticate with the server.  Return
true if successful.  The default value for IF-FAILED is :ERROR."))


;;;; Mapping of names (strings) to classes (or class names)

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

(define-name-class-mapping
  :class server-address
  :map *server-address-classes*
  :find find-server-address-class)

(define-name-class-mapping
  :class authentication-mechanism
  :map *authentication-mechanism-classes*
  :find find-authentication-mechanism-class)


;;;; Convenience methods

(defmethod open-connection ((addresses list) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (or (some (lambda (address)
                (open-connection address :if-failed nil))
              addresses)
        (error "No server addresses left to try to open."))))

(defmethod authenticate ((mechanisms list) connection &key (if-failed :error))
  (with-if-failed-handler if-failed
    (or (some (lambda (mechanism)
                (authenticate mechanism connection :if-failed nil))
              mechanisms)
        (error "No authentication mechanisms left to try."))))


;;;; Server addresses

(defclass standard-server-address (server-address)
  ((transport-name :initarg :transport-name :reader server-address-transport-name)
   (properties :initarg :properties :reader server-address-properties))
  (:documentation "Represents a standard server address with a
transport name and a table of properties."))

(defmethod open-connection :around ((server-address standard-server-address)
                                    &key (if-failed :error))
  (with-if-failed-handler if-failed
    (call-next-method)))

(defmethod server-address-property (name (server-address standard-server-address)
                                    &key (if-does-not-exist :error))
  (or (gethash name (server-address-properties server-address))
      (inexistent-entry name if-does-not-exist)))

(defclass generic-server-address (standard-server-address)
  ()
  (:documentation "Represents a server address whose transport is not
supported by the DBUS system."))

(defmethod open-connection ((address generic-server-address) &key (if-failed :error))
  (declare (ignore if-failed))
  (error "Unsupported transport mechanism for ~S." address))

(defun parse-server-addresses-from-stream (in)
  "Parse unescaped server addresses text from a character stream and
return a list of server addresses."
  (let ((server-addresses '())
        (token (make-string-output-stream))
        (current-server-address '())
        (char nil))
    (labels ((consume ()
               (or (setf char (read-char in nil nil))
                   (finish)))
             (finish ()
               (finish-token)
               (finish-server-address)
               (return-from parse-server-addresses-from-stream
                 (nreverse server-addresses)))
             (finish-token (&optional ignore-empty)
               (let ((string (get-output-stream-string token)))
                 (when (or (plusp (length string))
                           (not ignore-empty))
                   (push string current-server-address))))
             (finish-server-address (&optional ignore-empty)
               (finish-token ignore-empty)
               (when current-server-address
                 (destructuring-bind (type &rest plist)
                     (nreverse current-server-address)
                   (push (make-instance
                          (or (find-server-address-class type :if-does-not-exist nil)
                              'generic-server-address)
                          :transport-name type
                          :properties (plist-hash-table plist :test 'equal))
                         server-addresses))
                 (setf current-server-address '())))
             (add-to-token ()
               (write-char char token)))
      (tagbody
       transport
         (case (consume)
           (#\: (finish-token) (go key))
           (t (add-to-token) (go transport)))
       key
         (case (consume)
           (#\; (finish-server-address t) (go transport))
           (#\= (finish-token) (go value))
           (t (add-to-token) (go key)))
       value
         (case (consume)
           (#\, (finish-token) (go key))
           (#\; (finish-server-address) (go transport))
           (t (add-to-token) (go value)))))))

(defun unescape-server-addresses-string (string)
  "Unescape a server addresses string per the DBUS specification's
escaping rules and return the unescaped string.  The string returned
may be the same as the string supplied if no unescaping is needed."
  (let ((escapes (count #\% string)))
    (if (zerop escapes)
        string
        (let ((octets (make-octet-vector (- (length string) (* 2 escapes))
                                         :fill-pointer 0)))
          (with-input-from-string (in string)
            (loop for char = (read-char in nil nil)
                  while char do
                  (vector-push
                   (if (char= #\% char)
                       (logior (ash (digit-char-p (read-char in) 16) 4)
                               (digit-char-p (read-char in) 16))
                       (char-code char))
                   octets)))
          (babel:octets-to-string octets :encoding :utf-8)))))

(defun parse-server-addresses-string (string)
  "Parse a (possibly escaped) server addresses string into a list of
server addresses."
  (with-input-from-string (in (unescape-server-addresses-string string))
    (parse-server-addresses-from-stream in)))

(defun session-server-addresses ()
  "Return a list of server addresses for the current session."
  (when-let (string (iolib.syscalls:getenv "DBUS_SESSION_BUS_ADDRESS"))
    (parse-server-addresses-string string)))


;;;; Connections

(defclass standard-connection (connection)
  ((server-address :initarg :server-address :reader connection-server-address)
   (uuid :initarg :uuid :accessor connection-server-uuid))
  (:default-initargs :uuid nil)
  (:documentation "Represents a standard DBUS connection."))

(defmethod (setf connection-server-uuid) :before (new-uuid (connection standard-connection))
  (let ((old-uuid (connection-server-uuid connection)))
    (when (and old-uuid (not (equal old-uuid new-uuid)))
      (cerror "Set new ID and continue."
              "A server ID is already assigned to this connection."))))
