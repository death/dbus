;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:publish-introspection-example
  (:use #:cl #:dbus)
  (:export #:publish-introspection-example))

(in-package #:publish-introspection-example)

(defclass dbus-introspection-object
    (introspection-mixin child-object-mixin dbus-object) ())

(define-dbus-object root
  (:path "/")
  (:class dbus-introspection-object))

(define-dbus-object my-service
  (:path "/org/adeht/MyService")
  (:class dbus-introspection-object)
  (:parent root))

(define-dbus-method (my-service my-method) ((s1 :string) (s2 :string)) (:string)
  (:interface "org.adeht.MyService")
  (concatenate 'string s1 s2))

(define-dbus-signal-handler (my-service on-signal) ((s :string))
  (:interface "org.adeht.MyService")
  (format t "Got signal with arg ~S~%" s))

(defun publish-example ()
  (handler-case
      (with-open-bus (bus (session-server-addresses))
        (format t "Bus connection name: ~A~%" (bus-name bus))
        (publish-objects bus))
    (end-of-file ()
      :disconnected-by-bus)))
