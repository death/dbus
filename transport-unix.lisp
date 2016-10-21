;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus/transport-unix
  (:use #:cl #:dbus/server-addresses #:dbus/protocols #:dbus/connections)
  (:import-from #:iolib #:ensure-address)
  (:export
   #:unix-server-address
   #:unix-connection))

(in-package #:dbus/transport-unix)


;;;; Unix Domain Sockets transport

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
            (ensure-address (or abstract path)
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
