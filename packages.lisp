;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus
  (:use #:cl #:alexandria)
  (:export
   #:dbus-error
   #:inexistent-entry
   #:inexistent-entry-designator
   #:entry-replacement-attempt
   #:entry-replacement-attempt-old
   #:entry-replacement-attempt-new
   #:server-address
   #:connection
   #:authentication-mechanism
   #:server-address-transport-name
   #:server-address-property
   #:open-connection
   #:connection-server-address
   #:connection-server-uuid
   #:close-connection
   #:receive-line
   #:send-line
   #:send-nul-byte
   #:authentication-mechanism-name
   #:supported-authentication-mechanisms
   #:authenticate
   #:find-server-address-class
   #:find-authentication-mechanism-class
   #:standard-server-address
   #:generic-server-address
   #:parse-server-addresses-string
   #:session-server-addresses
   #:standard-connection))
