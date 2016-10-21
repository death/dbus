;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus/auth-dbus-external
  (:use #:cl #:dbus/utils #:dbus/protocols #:dbus/authentication-mechanisms)
  (:export
   #:dbus-external-authentication-mechanism))

(in-package #:dbus/auth-dbus-external)


;;;; DBUS external authentication mechanism

(defclass dbus-external-authentication-mechanism (standard-authentication-mechanism)
  ()
  (:default-initargs :textual t)
  (:documentation "Authenticate using external SASL method."))

(setf (find-authentication-mechanism-class "EXTERNAL")
      'dbus-external-authentication-mechanism)

(defmethod feed-authentication-mechanism ((mechanism dbus-external-authentication-mechanism) challenge)
  (if (eq challenge :initial-response)
      (values :continue (encode-hex-string (current-username)))
      (error "More than one response requested for EXTERNAL authentication.")))
