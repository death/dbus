;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus
  (:use #:cl #:alexandria #:split-sequence)
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
   #:connection-next-serial
   #:close-connection
   #:receive-line
   #:send-line
   #:authentication-mechanism-name
   #:authentication-mechanism-textual-p
   #:feed-authentication-mechanism
   #:supported-authentication-mechanisms
   #:authenticate
   #:find-server-address-class
   #:find-authentication-mechanism-class
   #:standard-server-address
   #:generic-server-address
   #:parse-server-addresses-string
   #:session-server-addresses
   #:system-server-addresses
   #:standard-connection
   #:standard-authentication-mechanism
   #:generic-authentication-mechanism
   #:authentication-error
   #:authentication-error-command
   #:authentication-error-argument
   #:unix-server-address
   #:unix-connection
   #:dbus-cookie-sha1-authentication-mechanism))
