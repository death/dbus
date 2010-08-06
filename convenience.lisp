;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Low-level way to invoke D-BUS methods

(defun invoke-method (connection member &key path signature arguments interface destination
                      no-reply no-auto-start asynchronous (endianness :little-endian))
  (let ((serial (connection-next-serial connection)))
    (send-message
     (encode-message endianness :method-call
                     (logior (if no-reply message-no-reply-expected 0)
                             (if no-auto-start message-no-auto-start 0))
                     1 serial path interface member nil nil
                     destination nil signature arguments)
     connection)
    (if (or no-reply asynchronous)
        serial
        (wait-for-reply serial connection))))
