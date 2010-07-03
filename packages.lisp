;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus
  (:use #:cl #:alexandria)
  (:export
   #:dbus-error
   #:inexistent-entry
   #:inexistent-entry-designator
   #:server-address
   #:server-address-transport-name
   #:server-address-property
   #:parse-server-addresses-string
   #:session-server-addresses))
