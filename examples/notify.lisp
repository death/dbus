(defpackage #:notify-example
  (:use #:cl #:dbus)
  (:export #:notify-example))

(in-package #:notify-example)

(defun notify-example ()
  (iolib:with-event-base (event-base)
    (with-open-connection (connection event-base (session-server-addresses))
      (authenticate (supported-authentication-mechanisms connection) connection)
      (invoke-method connection "Hello"
                     :path "/org/freedesktop/DBus"
                     :interface "org.freedesktop.DBus"
                     :destination "org.freedesktop.DBus")
      (invoke-method connection "Notify"
                     :path "/org/freedesktop/Notifications"
                     :interface "org.freedesktop.Notifications"
                     :destination "org.freedesktop.Notifications"
                     :signature "susssasa{sv}i"
                     :arguments (list "Test" 0 "" "Test" "This is a test; I repeat, this is a test." '() '() -1)))))
