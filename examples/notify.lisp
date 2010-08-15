(defpackage #:notify-example
  (:use #:cl #:dbus)
  (:export #:notify-example))

(in-package #:notify-example)

(defun notify-example ()
  (with-open-bus (bus (session-server-addresses))
    (with-introspected-object (notifications bus "/org/freedesktop/Notifications" "org.freedesktop.Notifications")
      (notifications "org.freedesktop.Notifications" "Notify"
                     "Test" 0 "" "Test" "This is a test; I repeat, this is a test." '() '() -1))))
