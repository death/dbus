;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:dbus
  :description "A D-BUS client library for Common Lisp"
  :author "death <github.com/death>"
  :license "BSD"
  :depends-on (#:alexandria #:babel #:cl-xmlspam #:flexi-streams
               #:iolib #:ironclad #:split-sequence #:trivial-garbage
               #:ieee-floats #:xml-emitter #:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "protocols")
   (:file "conditions")
   (:file "types")
   (:file "type-definitions")
   (:file "messages")
   (:file "server-addresses")
   (:file "authentication-mechanisms")
   (:file "connections")
   (:file "introspect")
   (:file "convenience")
   (:file "standard-interfaces")
   (:file "transport-unix")
   (:file "server")
   (:file "auth-dbus-cookie-sha1")
   (:file "auth-dbus-external")))
