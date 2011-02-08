;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Higher-level interface

(defun hello (connection)
  (invoke-method connection "Hello"
                 :path "/org/freedesktop/DBus"
                 :interface "org.freedesktop.DBus"
                 :destination "org.freedesktop.DBus"))

(defclass bus ()
  ((connection :initarg :connection :reader bus-connection)
   (name :initarg :name :reader bus-name)))

(defun call-with-open-bus (function event-base server-addresses)
  (with-open-connection (connection event-base server-addresses)
    (authenticate (supported-authentication-mechanisms connection) connection)
    (funcall function (make-instance 'bus :name (hello connection) :connection connection))))

(defmacro with-open-bus ((bus-var server-addresses &key event-base) &body forms)
  (if (null event-base)
      (with-gensyms (event-base)
        `(with-event-base (,event-base)
           (with-open-bus (,bus-var ,server-addresses :event-base ,event-base)
             ,@forms)))
      (once-only (server-addresses event-base)
        `(call-with-open-bus (lambda (,bus-var) ,@forms) ,event-base ,server-addresses))))

(defmacro with-introspected-object ((name bus path destination) &body forms)
  (with-gensyms (object)
    `(let ((,object (make-object-from-introspection (bus-connection ,bus) ,path ,destination)))
       (flet ((,name (interface-name method-name &rest args)
                (apply #'object-invoke ,object interface-name method-name args)))
         ,@forms))))
