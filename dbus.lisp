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


;;;; Convenience methods

(defmethod open-connection ((addresses list) &key (if-failed :error))
  (with-if-failed-handler if-failed
    (or (some (lambda (address)
                (open-connection address :if-failed nil))
              addresses)
        (error "No server addresses left to try to open."))))


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
  (with-if-failed-handler if-failed
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
           (if arg
               (send :auth (authentication-mechanism-name mechanism) arg)
               (send :auth (authentication-mechanism-name mechanism)))
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
    t))


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

(defclass unix-connection (standard-connection)
  ((socket :initarg :socket :reader connection-socket))
  (:documentation "Represents a connection to a DBUS server over Unix
Domain Sockets."))

(defmethod open-connection ((address unix-server-address) &key (if-failed :error))
  (declare (ignore if-failed))
  (let ((socket (iolib:make-socket :address-family :local
                                   :external-format '(:utf-8 :eol-style :crlf))))
    (unwind-protect
         (prog1
             (make-instance 'unix-connection
                            :socket socket
                            :server-address address
                            :uuid (server-address-property "guid" address :if-does-not-exist nil))
           (iolib:connect socket (server-address-socket-address address))
           (setf socket nil))
      (when socket
        (close socket)))))

(defmethod close-connection ((connection unix-connection))
  (close (connection-socket connection)))

(defmethod send-nul-byte ((connection unix-connection))
  (write-byte 0 (connection-socket connection))
  (force-output (connection-socket connection)))

(defmethod send-line (line (connection unix-connection))
  (write-line line (connection-socket connection))
  (force-output (connection-socket connection)))

(defmethod receive-line ((connection unix-connection))
  (read-line (connection-socket connection)))


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
