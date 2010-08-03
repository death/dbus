;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Utilities

(define-condition dbus-error (error)
  ()
  (:documentation "The supertype for errors related to the DBUS
system."))

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

(define-condition entry-replacement-attempt (dbus-error)
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

    Signal an ENTRY-ALREADY-EXISTS error with a CONTINUE restart to
    replace the entry, and an ABORT restart to not replace it.

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
      (babel:string-to-octets data :encoding :utf-8 :start start :end end)))
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
  (nth-value 0 (iolib.syscalls:getpwuid (iolib.syscalls:getuid))))

(defun random-challenge-string (&optional (num-octets 16))
  "Return a string containing a hex-encoded representation of a number
of random octet values."
  (with-output-to-string (out)
    (loop repeat (* 2 num-octets) do
          (write-char (char-downcase (digit-char (random 16) 16)) out))))

(defmacro with-binary-writers ((stream endianness align u8 u16 u32 u64) &body forms)
  "Evaluate forms with functions to write binary data to the stream in
a given endianness.

  STREAM

    A form evaluating to a binary output stream with a file position.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

  ALIGN

    A name to be bound to a function that takes an integer and ensures
    the stream's file position is aligned to it.  It does so by
    writing the appropriate number of 0 octets.

  U8, U16, U32, U64

    Names to be bound to functions that take 8-, 16-, 32-, and 64-bit
    unsigned byte values, respectively, and write these values to the
    stream, in the appropriate endianness.  The values are always
    naturally aligned before written."
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
              (,body-function-name (u 8) (u 16) (u 32) (u 64)))))))))

(defvar *stream-read-positions*
  (make-hash-table :weakness :key)
  "A mapping from a stream (weakly referenced) to a read position.")

(defun stream-read-position (stream)
  "Return the stream's read position (zero by default)."
  (gethash stream *stream-read-positions* 0))

(defun (setf stream-read-position) (new-read-position stream)
  "Set the stream's read position to a new value."
  (setf (gethash stream *stream-read-positions*) new-read-position))

(defmacro with-binary-readers ((stream endianness align u8 u16 u32 u64) &body forms)
  "Evaluate forms with functions to read binary data from the stream
in a given endianness.

  STREAM

    A form evaluating to a binary input stream.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

  ALIGN

    A name to be bound to a function that takes an integer and ensures
    the stream's read position is aligned to it.  It does so by
    reading and ignoring the appropriate number of octets.

  U8, U16, U32, U64

    Names to be bound to functions that read 8-, 16-, 32-, and 64-bit
    unsigned byte values, respectively, from the stream, in the
    appropriate endianness.  The read position is ensured to be
    naturally aligned before reading the value."
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
              (,body-function-name (u 8) (u 16) (u 32) (u 64)))))))))
  
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

(defun double-to-unsigned (value)
  "Return an unsigned 64-bit byte representing the double-float value
passed."
  (logior (ash (signed-to-unsigned (sb-kernel:double-float-high-bits value) 32) 32)
          (sb-kernel:double-float-low-bits value)))

(defun unsigned-to-double (value)
  "Return the double-float value represented by the unsigned 64-bit
byte supplied."
  (sb-kernel:make-double-float
   (unsigned-to-signed (ldb (byte 32 32) value) 32)
   (ldb (byte 32 0) value)))


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

(defgeneric open-connection (event-base server-address &key if-failed)
  (:documentation "Open a connection to the server designated by the
server address and return a connection object.  The default value for
IF-FAILED is :ERROR.  An IOLIB event base object must be passed.
Should also send the initial \"nul byte\"."))

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

(defgeneric connection-fd (connection)
  (:documentation "Return the file descriptor associated with
the (open) connection."))

(defgeneric connection-pending-messages (connection)
  (:documentation "Return a list of the currently pending messages
associated with the connection, from newest to oldest."))

(defgeneric (setf connection-pending-messages) (new-list connection)
  (:documentation "Set the list of currently pending messages
associated with the connection."))

(defgeneric connection-next-serial (connection)
  (:documentation "Return a 32-bit integer for associating request
messages and their replies."))

(defgeneric drain-pending-messages (connection)
  (:documentation "Return a list of the currently pending messages
associated with the connection, from oldest to newest, and consider
these messages no longer pending."))

(defgeneric close-connection (connection)
  (:documentation "Close an open connection."))

(defgeneric wait-for-reply (serial connection)
  (:documentation "Wait for a reply message with the supplied serial
to be received via connection."))

(defgeneric receive-message (connection)
  (:documentation "Read a D-BUS message from the server."))

(defgeneric receive-line (connection)
  (:documentation "Read a line of text from the server and return it as
a string.  The operation blocks until a whole line can be read.  The
string will not contain newline characters."))

(defgeneric send-message (encoded-message connection)
  (:documentation "Send an encoded message to the server.  The
operation will force (but not finish) output before returning."))

(defgeneric send-line (line connection)
  (:documentation "Send a line of text, represented by a string, to
the server.  The operation will force (but not finish) output before
returning.  The string should not contain any newline characters."))

(defgeneric authentication-mechanism-name (authentication-mechanism)
  (:documentation "Return the name for the authentication
mechanism."))

(defgeneric authentication-mechanism-textual-p (authentication-mechanism)
  (:documentation "Return true if data from server should be converted
to a string, and false if it should remain an octet vector."))

(defgeneric feed-authentication-mechanism (authentication-mechanism challenge)
  (:documentation "Feed authentication mechanism with a challenge,
which is either a string or an octet vector in accordance with the
mechanism's textuality, or :INITIAL-RESPONSE.  The method should
return one of the following:

  :CONTINUE <response>

    Continue with the authentication conversation and send <response>
    to the server.

  :OK <response>

    After sending <response> to the server the client is finished and
    expecting an :OK response.

  :ERROR

    The challenge was invalid."))

(defgeneric supported-authentication-mechanisms (connection)
  (:documentation "Return a list of authentication mechanisms
supported by the server."))

(defgeneric authenticate (authentication-mechanisms connection &key if-failed)
  (:documentation "Attempt to authenticate with the server associated
with the connection, and return true if successful.  The default value
for IF-FAILED is :ERROR."))


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


;;;; Convenience

(defmethod open-connection (event-base (addresses list) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (or (some (lambda (address)
                (open-connection event-base address :if-failed nil))
              addresses)
        (error "No server addresses left to try to open."))))


(defmacro with-open-connection ((connection event-base server-addresses &key (if-failed :error)) &body forms)
  `(let ((,connection (open-connection ,event-base ,server-addresses :if-failed ,if-failed)))
     (unwind-protect
          (progn ,@forms)
       (when ,connection
         (close-connection ,connection)))))


;;;; Server addresses

(defclass standard-server-address (server-address)
  ((transport-name :initarg :transport-name :reader server-address-transport-name)
   (properties :initarg :properties :reader server-address-properties))
  (:documentation "Represents a standard server address with a
transport name and a table of properties."))

(defmethod open-connection :around (event-base
                                    (server-address standard-server-address)
                                    &key (if-failed :error))
  (declare (ignore event-base))
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

(defmethod open-connection (event-base (address generic-server-address) &key (if-failed :error))
  (declare (ignore event-base if-failed))
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

(defun system-server-addresses ()
  "Return a list of server addresses for the current system."
  (parse-server-addresses-string
   (or (iolib.syscalls:getenv "DBUS_SYSTEM_BUS_ADDRESS")
       "unix:path=/var/run/dbus/system_bus_socket")))    


;;;; Connections

(defclass standard-connection (connection)
  ((server-address :initarg :server-address :reader connection-server-address)
   (uuid :initarg :uuid :accessor connection-server-uuid)
   (pending-messages :initform '() :accessor connection-pending-messages)
   (event-base :initarg :event-base :reader connection-event-base)
   (serial :initform 1))
  (:default-initargs :uuid nil)
  (:documentation "Represents a standard DBUS connection."))

(defmethod (setf connection-server-uuid) :before (new-uuid (connection standard-connection))
  (let ((old-uuid (connection-server-uuid connection)))
    (when (and old-uuid (not (equal old-uuid new-uuid)))
      (cerror "Set new ID and continue."
              "A server ID is already assigned to this connection."))))

(defmethod connection-next-serial ((connection standard-connection))
  (with-slots (serial) connection
    (prog1 serial
      (setf serial
            (let ((x (logand (1+ serial) #xFFFFFFFF)))
              (if (zerop x) 1 x))))))

(defmethod drain-pending-messages ((connection standard-connection))
  (prog1 (nreverse (connection-pending-messages connection))
    (setf (connection-pending-messages connection) '())))

(defmethod wait-for-reply (serial (connection standard-connection))
  (let ((reply nil))
    (flet ((reply-p (message)
             (when (and (typep message '(or error-message method-return-message))
                        (= serial (message-reply-serial message)))
               (setf reply message))))
      (with-accessors ((pending-messages connection-pending-messages)) connection
        (setf pending-messages (delete-if (lambda (message) (reply-p message)) pending-messages))
        (unless reply
          (loop
           (iolib:event-dispatch (connection-event-base connection) :one-shot t)
           (when (reply-p (first pending-messages))
             (pop pending-messages)
             (return))))))
    (values (message-body reply) reply)))

(defun activate-io-handlers (connection)
  (iolib:set-io-handler
   (connection-event-base connection)
   (connection-fd connection)
   :read
   (lambda (fd event error)
     (declare (ignore fd event))
     (if error
         (error "Connection I/O error: ~S." error)
         (push (receive-message connection) (connection-pending-messages connection))))))

(defmethod authenticate :around (mechanisms (connection standard-connection) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (when (call-next-method)
      (activate-io-handlers connection)
      t)))


;;;; Socket-based connection mixin

(defclass socket-connection-mixin ()
  ((socket :initarg :socket :reader connection-socket)))

(defun open-socket-connection (address-family socket-address)
  (let ((socket (iolib:make-socket :address-family address-family
                                   :external-format '(:utf-8 :eol-style :crlf))))
    (unwind-protect
         (progn
           (iolib:connect socket socket-address)
           (write-byte 0 socket)
           (force-output socket)
           (prog1 socket
             (setf socket nil)))
      (when socket
        (close socket)))))

(defmethod connection-fd ((connection socket-connection-mixin))
  (iolib:fd-of (connection-socket connection)))

(defmethod close-connection ((connection socket-connection-mixin))
  (close (connection-socket connection)))

(defmethod receive-message ((connection socket-connection-mixin))
  (decode-message (connection-socket connection)))

(defmethod receive-line ((connection socket-connection-mixin))
  (read-line (connection-socket connection)))

(defmethod send-line (line (connection socket-connection-mixin))
  (write-line line (connection-socket connection))
  (force-output (connection-socket connection)))

(defmethod send-message (encoded-message (connection socket-connection-mixin))
  (write-sequence encoded-message (connection-socket connection))
  (force-output (connection-socket connection)))
   

;;;; Authentication mechanisms

(defclass standard-authentication-mechanism (authentication-mechanism)
  ((name :initarg :name :reader authentication-mechanism-name)
   (textual :initarg :textual :reader authentication-mechanism-textual-p))
  (:default-initargs :textual nil)
  (:documentation "Represents a standard authentication mechanism."))

(defclass generic-authentication-mechanism (standard-authentication-mechanism)
  ()
  (:documentation "Represents an authentication mechanism that is not
supported by the DBUS system."))

(defmethod feed-authentication-mechanism ((mechanism generic-authentication-mechanism) challenge)
  (declare (ignore challenge))
  (values :error))

(define-condition authentication-error (dbus-error)
  ((command :initarg :command :reader authentication-error-command)
   (argument :initarg :argument :reader authentication-error-argument))
  (:report (lambda (condition stream)
             (format stream "Authentication error, command ~S with argument ~S."
                     (authentication-error-command condition)
                     (authentication-error-argument condition)))))

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
           (values :data (if as-string (babel:octets-to-string data :encoding :utf-8) data))))
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

(defmethod supported-authentication-mechanisms ((connection standard-connection))
  (send-authentication-command connection :auth)
  (mapcar (lambda (name)
            (make-instance
             (or (find-authentication-mechanism-class name :if-does-not-exist nil)
                 'generic-authentication-mechanism)
             :name name))
          (receive-authentication-response connection :expect :rejected)))

(defmethod authenticate (mechanisms (connection standard-connection) &key (if-failed :error))
  (declare (ignore if-failed))
  (setf mechanisms (ensure-list mechanisms))
  (let (op arg mechanism)
    (flet ((send (command &rest args)
             (apply #'send-authentication-command connection command args))
           (receive ()
             (receive-authentication-response connection :as-string (authentication-mechanism-textual-p mechanism))))
      (tagbody
       initial
         (if (null mechanisms)
             (error "No more mechanisms to try.")
             (setf mechanism (pop mechanisms)))
         (multiple-value-setq (op arg) (feed-authentication-mechanism mechanism :initial-response))
         (when (eq op :error)
           (go initial))
         (send :auth (authentication-mechanism-name mechanism) arg)
         (ecase op
           (:ok (go waiting-for-ok))
           (:continue (go waiting-for-data)))
       waiting-for-data
         (multiple-value-setq (op arg) (receive))
         (case op
           (:data
            (multiple-value-setq (op arg) (feed-authentication-mechanism mechanism arg))
            (ecase op
              (:continue (send :data arg) (go waiting-for-data))
              (:ok (send :data arg) (go waiting-for-ok))
              (:error (if arg (send :error arg) (send :error)) (go waiting-for-data))))
           (:rejected (go initial))
           (:error (send :cancel) (go waiting-for-reject))
           (:ok (send :begin) (go authenticated))
           (t (send :error) (go waiting-for-data)))
       waiting-for-ok
         (multiple-value-setq (op arg) (receive))
         (case op
           (:ok (send :begin) (go authenticated))
           (:reject (go initial))
           ((:data :error) (send :cancel) (go waiting-for-reject))
           (t (send :error) (go waiting-for-ok)))
       waiting-for-reject
         (multiple-value-setq (op arg) (receive))
         (case op
           (:reject (go initial))
           (t (error 'authentication-error :command op :argument arg)))
       authenticated
         (setf (connection-server-uuid connection) arg))))
  t)


;;;; Unix Domain Sockets

(defclass unix-server-address (standard-server-address)
  ((socket-address :reader server-address-socket-address))
  (:documentation "Represents a DBUS server address with Unix Domain
Sockets for transport."))

(setf (find-server-address-class "unix") 'unix-server-address)

(defmethod shared-initialize :after ((address unix-server-address) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (let ((abstract (server-address-property "abstract" address :if-does-not-exist nil))
        (path (server-address-property "path" address :if-does-not-exist nil)))
    (with-slots (socket-address) address
      (setf socket-address
            (iolib:ensure-address (or abstract path)
                                  :family :local
                                  :abstract (if abstract t nil))))))

(defclass unix-connection (socket-connection-mixin standard-connection)
  ()
  (:documentation "Represents a connection to a DBUS server over Unix
Domain Sockets."))

(defmethod open-connection (event-base (address unix-server-address) &key (if-failed :error))
  (declare (ignore if-failed))
  (make-instance 'unix-connection
                 :socket (open-socket-connection :local (server-address-socket-address address))
                 :server-address address
                 :uuid (server-address-property "guid" address :if-does-not-exist nil)
                 :event-base event-base))


;;;; DBUS Cookie SHA1 authentication mechanism

(defclass dbus-cookie-sha1-authentication-mechanism (standard-authentication-mechanism)
  ()
  (:default-initargs :textual t)
  (:documentation "Authenticate using a local cookie and SHA1."))

(setf (find-authentication-mechanism-class "DBUS_COOKIE_SHA1")
      'dbus-cookie-sha1-authentication-mechanism)

(defvar *keyrings-directory*
  (merge-pathnames (make-pathname :directory '(:relative ".dbus-keyrings"))
                   (user-homedir-pathname))
  "The directory holding context files containing cookies.")

(defun find-cookie (context-name cookie-id &key (if-does-not-exist :error))
  "Find the cookie corresponding to COOKIE-ID in the appropriate
context file."
  (with-open-file (in (make-pathname :name context-name
                                     :defaults *keyrings-directory*)
                      :direction :input)
    (loop for line = (read-line in nil nil)
          while line do
          (destructuring-bind (id ctime cookie)
              (split-sequence #\Space line)
            (declare (ignore ctime))
            (when (equal id cookie-id)
              (return-from find-cookie cookie)))))
  (inexistent-entry (list context-name cookie-id) if-does-not-exist))

(defmethod feed-authentication-mechanism ((mechanism dbus-cookie-sha1-authentication-mechanism) challenge)
  (if (eq challenge :initial-response)
      (values :continue (encode-hex-string (current-username)))
      (destructuring-bind (context-name cookie-id challenge-string)
          (split-sequence #\Space challenge)
        (let* ((my-challenge-string (random-challenge-string))
               (cookie (find-cookie context-name cookie-id))
               (message (format nil "~A:~A:~A" challenge-string my-challenge-string cookie))
               (digest (ironclad:digest-sequence :sha1 (babel:string-to-octets message :encoding :utf-8))))
          (values :ok (format nil "~A ~A" my-challenge-string (encode-hex-string digest)))))))


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


;;;; Packing and unpacking

(defun pack (stream endianness sigexp &rest values)
  "Pack values according to the signature expression and endianness
into stream."
  (with-binary-writers (stream endianness align u8 u16 u32 u64)
    (labels ((str (value)
               (let ((octets (babel:string-to-octets value :encoding :utf-8)))
                 (u32 (length octets))
                 (map nil #'u8 octets)
                 (u8 0)))
             (sig (value)
               (let ((octets (babel:string-to-octets value :encoding :utf-8)))
                 (u8 (length octets))
                 (map nil #'u8 octets)
                 (u8 0)))
             (arr (element-type value)
               (align 4)
               (let ((length-position (file-position stream)))
                 (u32 0)
                 (typecase element-type
                   ((or (member :int64 :uint64 :double)
                        (cons (member :struct :dict-entry)))
                    (align 8)))
                 (let ((start-position (file-position stream)))
                   (pack-seq (circular-list element-type) value)
                   (let ((end-position (file-position stream)))
                     (file-position stream length-position)
                     (u32 (- end-position start-position))
                     (file-position stream end-position)))))
             (struct (field-types value)
               (align 8)
               (map nil (lambda (type element)
                          (pack-1 type element))
                    field-types value))
             (var (type value)
               (pack-1 :signature type)
               (pack-1 (first type) value))
             (pack-1 (type value)
               (etypecase type
                 ((eql :byte) (u8 value))
                 ((eql :boolean) (u32 (if value 1 0)))
                 ((eql :int16) (u16 (signed-to-unsigned value 16)))
                 ((eql :uint16) (u16 value))
                 ((eql :int32) (u32 (signed-to-unsigned value 32)))
                 ((eql :uint32) (u32 value))
                 ((eql :int64) (u64 (signed-to-unsigned value 64)))
                 ((eql :uint64) (u64 value))
                 ((eql :double) (u64 (double-to-unsigned (float value 0.0d0))))
                 ((member :string :object-path) (str value))
                 ((eql :signature) (sig (signature value)))
                 ((cons (eql :array)) (arr (second type) value))
                 ((cons (member :struct :dict-entry)) (struct (rest type) value))
                 ((eql :variant) (var (sigexp (first value)) (second value)))))
             (pack-seq (types values)
               (map nil #'pack-1 types values)))
      (pack-seq (sigexp sigexp) values))))

(defun unpack (stream endianness sigexp)
  "Unpack values from stream according to endianness and the signature
expression and return them as a list."
  (with-binary-readers (stream endianness align u8 u16 u32 u64)
    (labels ((str (length)
               (prog1
                   (babel:octets-to-string
                    (map-into (make-octet-vector length) #'u8)
                    :encoding :utf-8)
                 (u8)))
             (arr (element-type length)
               (loop with start = (stream-read-position stream)
                     with end = (+ start length)
                     until (= end (stream-read-position stream))
                     collect (unpack-1 element-type)))
             (struct (field-types)
               (align 8)
               (unpack-seq field-types))
             (unpack-1 (type)
               (etypecase type
                 ((eql :byte) (u8))
                 ((eql :boolean) (if (zerop (u32)) nil t))
                 ((eql :int16) (unsigned-to-signed (u16) 16))
                 ((eql :uint16) (u16))
                 ((eql :int32) (unsigned-to-signed (u32) 32))
                 ((eql :uint32) (u32))
                 ((eql :int64) (unsigned-to-signed (u64) 64))
                 ((eql :uint64) (u64))
                 ((eql :double) (unsigned-to-double (u64)))
                 ((member :string :object-path) (str (u32)))
                 ((eql :signature) (str (u8)))
                 ((cons (eql :array)) (arr (second type) (u32)))
                 ((cons (member :struct :dict-entry)) (struct (rest type)))
                 ((eql :variant) (unpack-1 (first (sigexp (str (u8))))))))
             (unpack-seq (types)
               (map 'list #'unpack-1 types)))
      (unpack-seq (sigexp sigexp)))))


;;;; Encoding and decoding messages

(defun encode-message (endianness type flags major-protocol-version
                       serial path interface member error-name reply-serial
                       destination sender signature body)
  "Encode a DBUS message and return it as an octet vector."
  (flexi-streams:with-output-to-sequence (out)
    (pack out endianness "yyyyuua(yv)"
          (ecase endianness
            (:little-endian (char-code #\l))
            (:big-endian (char-code #\B)))
          (ecase type
            (:method-call 1)
            (:method-return 2)
            (:error 3)
            (:signal 4))
          flags
          major-protocol-version
          0
          serial
          (loop for code from 1
                for value in (list path interface member error-name
                                   reply-serial destination sender signature)
                for type across "osssussg"
                when value collect (list code (list (string type) value))))
    (with-binary-writers (out endianness align u8 u16 u32 u64)
      (align 8)
      (let ((body-start (file-position out)))
        (apply #'pack out endianness (or signature "") body)
        (let ((body-end (file-position out)))
          (file-position out 4)
          (u32 (- body-end body-start))
          (file-position out body-end))))))

(defclass message ()
  ())

(defclass standard-message (message)
  ((endianness :initarg :endianness :reader message-endianness)
   (flags :initarg :flags :reader message-flags)
   (major-protocol-version :initarg :major-protocol-version :reader message-major-protocol-version)
   (body-length :initarg :body-length :reader message-body-length)
   (serial :initarg :serial :reader message-serial)
   (destination :initarg :destination :reader message-destination)
   (sender :initarg :sender :reader message-sender)
   (signature :initarg :signature :reader message-signature)
   (body :initarg :body :reader message-body)))

(defclass method-call-message (standard-message)
  ((path :initarg :path :reader message-path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass signal-message (standard-message)
  ((path :initarg :path :reader message-path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass method-return-message (standard-message)
  ((reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defclass error-message (standard-message)
  ((error-name :initarg :error-name :reader message-error-name)
   (reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defconstant message-no-reply-expected 1)
(defconstant message-no-auto-start 2)

(defun decode-message (stream)
  "Decode a DBUS message from the stream into a MESSAGE object."
  (let ((endianness (ecase (code-char (read-byte stream))
                      (#\l :little-endian)
                      (#\B :big-endian))))
    (setf (stream-read-position stream) 1)
    (destructuring-bind (type-code flags major-protocol-version
                                   body-length serial fields)
        (unpack stream endianness "yyyuua(yv)")
      (with-binary-readers (stream endianness align u8 u16 u32 u64)
        (align 8)
        (let (body path interface member error-name
                   reply-serial destination sender signature)
          (loop for (field-code field-value) in fields
                do (case field-code
                     (1 (setf path field-value))
                     (2 (setf interface field-value))
                     (3 (setf member field-value))
                     (4 (setf error-name field-value))
                     (5 (setf reply-serial field-value))
                     (6 (setf destination field-value))
                     (7 (setf sender field-value))
                     (8 (setf signature field-value))
                     (t (warn "Unknown field code ~D; ignoring field." field-code))))
          (setf body (unpack stream endianness signature))
          (macrolet ((make-message (class-name &rest additional-initargs)
                       `(make-instance ,class-name
                                       :endianness endianness
                                       :flags flags
                                       :major-protocol-version major-protocol-version
                                       :body-length body-length
                                       :serial serial
                                       :destination destination
                                       :sender sender
                                       :signature signature
                                       :body body
                                       ,@additional-initargs)))
            (case type-code
              (1 (make-message 'method-call-message :path path :interface interface :member member))
              (2 (make-message 'method-return-message :reply-serial reply-serial))
              (3 (make-message 'error-message :error-name error-name :reply-serial reply-serial))
              (4 (make-message 'signal-message :path path :interface interface :member member))
              (t (warn "Unknown message type code ~D; ignoring message." type-code)))))))))


;;;; Low-level way to invoke D-BUS methods

(defun invoke-method (connection member &key path signature arguments interface destination
                      no-reply no-auto-start asynchronous (endianness :little-endian))
  (let ((serial (connection-next-serial connection)))
    (send-message
     (encode-message endianness :method-call
                     (logior (if no-reply message-no-reply-expected 0)
                             (if no-auto-start message-no-auto-start 0))
                     1 serial path interface member nil nil
                     destination nil signature arguments)
     connection)
    (if (or no-reply asynchronous)
        serial
        (wait-for-reply serial connection))))

(defun test-invoke-method ()
  (iolib:with-event-base (event-base)
    (with-open-connection (connection event-base (session-server-addresses))
      (authenticate (supported-authentication-mechanisms connection) connection)
      (invoke-method connection "Hello"
                     :path "/org/freedesktop/DBus"
                     :interface "org.freedesktop.DBus"
                     :destination "org.freedesktop.DBus")
      (invoke-method connection "Notify"
                     :path "/org/freedesktop/Notifications"
                     :interface "org.freedesktop.Notifications"
                     :destination "org.freedesktop.Notifications"
                     :signature "susssasa{sv}i"
                     :arguments (list "Test" 0 "" "Test" "This is a test; I repeat, this is a test." '() '() -1)))))
