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
   #:server-address-transport-name
   #:server-address-property
   #:standard-server-address
   #:generic-server-address
   #:find-server-address-class
   #:parse-server-addresses-string
   #:session-server-addresses))
