;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

;;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:register-system-packages :iolib '(:iolib.syscalls))

(asdf:defsystem #:dbus
  :description "A D-BUS client library for Common Lisp"
  :author "death <github.com/death>"
  :license "BSD"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  ;; cxml and cl-ppcre are only required by dbus/xspam.
  :depends-on ("cxml" "cl-ppcre" "dbus/all"))
