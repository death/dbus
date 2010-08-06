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
