;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages :iolib '(:iolib.syscalls))
(asdf:register-system-packages :cl-xmlspam '(:xspam))

(asdf:defsystem #:dbus
  :description "A D-BUS client library for Common Lisp"
  :author "death <github.com/death>"
  :license "BSD"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("dbus/all"))
