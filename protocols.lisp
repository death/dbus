;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Server address protocol

(defclass server-address ()
  ()
  (:documentation "Represents a DBUS server address, consisting of a
transport name and zero or more properties."))

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


;;;; Connection protocol

(defclass connection ()
  ()
  (:documentation "Represents a DBUS connection to a server."))

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

(defgeneric supported-authentication-mechanisms (connection)
  (:documentation "Return a list of authentication mechanisms
supported by the server."))

(defgeneric authenticate (authentication-mechanisms connection &key if-failed)
  (:documentation "Attempt to authenticate with the server associated
with the connection, and return true if successful.  The default value
for IF-FAILED is :ERROR."))

(defgeneric supports-unix-fd-passing-p (connection)
  (:documentation "Return true if Unix file descriptors can be passed
over the connection, and false otherwise."))


;;;; Authentication mechanism protocol

(defclass authentication-mechanism ()
  ()
  (:documentation "Represents a way to authenticate a client with a
server."))

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
