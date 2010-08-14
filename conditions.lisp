;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Condition types

(define-condition dbus-error (error)
  ()
  (:documentation "The supertype for errors related to the DBUS
system."))

(define-condition authentication-error (dbus-error)
  ((command :initarg :command :reader authentication-error-command)
   (argument :initarg :argument :reader authentication-error-argument))
  (:report (lambda (condition stream)
             (format stream "Authentication error, command ~S with argument ~S."
                     (authentication-error-command condition)
                     (authentication-error-argument condition)))))

(define-condition method-error (dbus-error)
  ((arguments :initarg :arguments :reader method-error-arguments))
  (:report (lambda (condition stream)
             (format stream "Method error: ~S."
                     (let ((all-args (method-error-arguments condition))
                           (first-arg (first (method-error-arguments condition))))
                       (if (stringp first-arg)
                           first-arg
                           all-args))))))
