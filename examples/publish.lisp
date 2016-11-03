;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:publish-example
  (:use #:cl #:dbus)
  (:export #:publish-example))

(in-package #:publish-example)

(define-dbus-object my-service
  (:path "/org/adeht/MyService"))

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
