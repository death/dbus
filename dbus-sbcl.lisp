;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; SBCL-specific code

(defun make-weakly-keyed-hash-table ()
  "Return a hash-table with key weakness."
  (make-hash-table :weakness :key))

(defun double-to-unsigned (value)
  "Return an unsigned 64-bit byte representing the double-float value
passed."
  (logior (ash (signed-to-unsigned (sb-kernel:double-float-high-bits value) 32) 32)
          (sb-kernel:double-float-low-bits value)))

(defun unsigned-to-double (value)
  "Return the double-float value represented by the unsigned 64-bit
byte supplied."
  (sb-kernel:make-double-float
   (unsigned-to-signed (ldb (byte 32 32) value) 32)
   (ldb (byte 32 0) value)))
