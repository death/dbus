;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; ECL-specific code

(defun make-weakly-keyed-hash-table ()
  "Return a hash-table with key weakness."
  ;; TODO: weak
  (make-hash-table))

(defun double-to-unsigned (value)
  "Return an unsigned 64-bit byte representing the double-float value
   passed."
  (c-inline (uint64_t) (:double) :uint64_t
            "{
             union {
                 uint64_t u;
                 double d;
             } x;

             x.d = #0;
             @(return 0) = x.u;
             }
             "))

(defun unsigned-to-double (value)
  "Return the double-float value represented by the unsigned 64-bit
byte supplied."
  (c-inline (uint64_t) (:uint64_t) :double
                "{
                 union {
                     uint64_t u;
                     double d;
                 } x;

                 x.u = #0;
                 @(return 0) = x.d;
                 }
               "))
