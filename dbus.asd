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
               #:iolib #:ironclad #:split-sequence)
  :serial t
  :components
  ((:file "packages")
   #+sbcl (:file "dbus-sbcl")
   #+ecl  (:file "dbus-ecl")
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
   (:file "transport-unix")
   (:file "auth-dbus-cookie-sha1")))
