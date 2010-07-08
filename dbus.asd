;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:dbus
  :depends-on (#:alexandria #:babel #:iolib #:split-sequence)
  :serial t
  :components
  ((:file "packages")
   (:file "dbus")))
