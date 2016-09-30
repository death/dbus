(ql:quickload :dbus)

(defpackage :dbus-server-example
  (:use #:cl #:dbus)
  (:export #:function-1))

(in-package :dbus-server-example)

(defmethod function-1 ((arg1 string) (arg2 integer))
  (values (1+ arg2)
          (concatenate 'string
                       "--" arg1 "--")))

(define-dbus-method function-2 (arg1 arg2)
                    (:in "si"
                     :out "is"
                     :dbus-name function3)
                    (values (1+ arg2)
                            (concatenate 'string
                                         "--" arg1 "--")))

(defun server-example ()
  (with-open-bus (bus (session-server-addresses))
    (dbus-serve bus "my.test.here" "org.my.ma" nil)))


#+nil
(defparameter *server-thread* (sb-thread:make-thread
                          (lambda () 
                            (server-example))
                          :name "dbus-server"))

#+nil
(dbus::build-introspection-of-store dbus::*default-dbus-store* "/")

#+nil
(sb-thread:terminate-thread *server-thread*)

#+nil
(with-open-bus (bus (session-server-addresses))
  (with-introspected-object (my bus "org.my.ma" "my.test.here")
    (my "int" "meth")))

