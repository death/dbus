;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; DBUS type definitions

(define-dbus-type :byte
  :signature #\y
  :alignment 1
  :pack (u8 value)
  :unpack (u8))

(define-dbus-type :boolean
  :signature #\b
  :alignment 4
  :pack (u32 (if value 1 0))
  :unpack (if (zerop (u32)) nil t))

(define-dbus-type :int16
  :signature #\n
  :alignment 2
  :pack (u16 (signed-to-unsigned value 16))
  :unpack (unsigned-to-signed (u16) 16))

(define-dbus-type :uint16
  :signature #\q
  :alignment 2
  :pack (u16 value)
  :unpack (u16))

(define-dbus-type :int32
  :signature #\i
  :alignment 4
  :pack (u32 (signed-to-unsigned value 32))
  :unpack (unsigned-to-signed (u32) 32))

(define-dbus-type :uint32
  :signature #\u
  :alignment 4
  :pack (u32 value)
  :unpack (u32))

(define-dbus-type :int64
  :signature #\x
  :alignment 8
  :pack (u64 (signed-to-unsigned value 64))
  :unpack (unsigned-to-signed (u64) 64))

(define-dbus-type :uint64
  :signature #\t
  :alignment 8
  :pack (u64 value)
  :unpack (u64))

(define-dbus-type :double
  :signature #\d
  :alignment 8
  :pack (u64 (double-to-unsigned (float value 0.0d0)))
  :unpack (unsigned-to-double (u64)))

(define-dbus-type :string
  :signature #\s
  :alignment 4
  :pack (pack-string stream endianness value 32)
  :unpack (unpack-string stream endianness (u32)))

(define-dbus-type :object-path
  :signature #\o
  :alignment 4
  :pack (pack-string stream endianness value 32)
  :unpack (unpack-string stream endianness (u32)))

(define-dbus-type :signature
  :signature #\g
  :alignment 1
  :pack (pack-string stream endianness (signature value) 8)
  :unpack (unpack-string stream endianness (u8)))

(define-dbus-type :array
  :signature #\a
  :composite t
  :alignment 4
  :pack (pack-array stream endianness (first element-types) value)
  :unpack (unpack-array stream endianness (first element-types) (u32)))

(define-dbus-type :struct
  :signature #\(
  :composite #\)
  :alignment 8
  :pack (pack-seq stream endianness element-types value)
  :unpack (unpack-seq stream endianness element-types))

(define-dbus-type :variant
  :signature #\v
  :alignment 1
  :pack (pack-variant stream endianness (sigexp (first value)) (second value))
  :unpack (unpack-variant stream endianness))

(define-dbus-type :dict-entry
  :signature #\{
  :composite #\}
  :alignment 8
  :pack (pack-seq stream endianness element-types value)
  :unpack (unpack-seq stream endianness element-types))
