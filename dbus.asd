;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:dbus
  :depends-on (#:alexandria #:babel #:cl-xmlspam #:flexi-streams
               #:iolib #:ironclad #:split-sequence)
  :serial t
  :components
  ((:file "packages")
   #+sbcl (:file "dbus-sbcl")
   (:file "utils")
   (:file "protocols")
   (:file "conditions")
   (:file "type-signatures")
   (:file "messages")
   (:file "server-addresses")
   (:file "authentication-mechanisms")
   (:file "connections")
   (:file "introspect")
   (:file "convenience")
   (:file "transport-unix")
   (:file "auth-dbus-cookie-sha1")))
