;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Encoding and decoding messages

(defun encode-message (endianness type flags major-protocol-version
                       serial path interface member error-name reply-serial
                       destination sender signature body)
  "Encode a DBUS message and return it as an octet vector."
  (with-output-to-sequence (out)
    (pack out endianness "yyyyuua(yv)"
          (ecase endianness
            (:little-endian (char-code #\l))
            (:big-endian (char-code #\B)))
          (ecase type
            (:method-call 1)
            (:method-return 2)
            (:error 3)
            (:signal 4))
          flags
          major-protocol-version
          0
          serial
          (loop for code from 1
                for value in (list path interface member error-name
                                   reply-serial destination sender signature)
                for type across "osssussg"
                when value collect (list code (list (string type) value))))
    (with-binary-writers (out endianness)
      (align 8)
      (let ((body-start (file-position out)))
        (apply #'pack out endianness (or signature "") body)
        (let ((body-end (file-position out)))
          (file-position out 4)
          (u32 (- body-end body-start))
          (file-position out body-end))))))

(defclass message ()
  ())

(defclass standard-message (message)
  ((endianness :initarg :endianness :reader message-endianness)
   (flags :initarg :flags :reader message-flags)
   (major-protocol-version :initarg :major-protocol-version :reader message-major-protocol-version)
   (body-length :initarg :body-length :reader message-body-length)
   (serial :initarg :serial :reader message-serial)
   (destination :initarg :destination :reader message-destination)
   (sender :initarg :sender :reader message-sender)
   (signature :initarg :signature :reader message-signature)
   (body :initarg :body :reader message-body)))

(defclass method-call-message (standard-message)
  ((path :initarg :path :reader message-path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass signal-message (standard-message)
  ((path :initarg :path :reader message-path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass method-return-message (standard-message)
  ((reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defclass error-message (standard-message)
  ((error-name :initarg :error-name :reader message-error-name)
   (reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defconstant message-no-reply-expected 1)
(defconstant message-no-auto-start 2)

(defun decode-message (stream)
  "Decode a DBUS message from the stream into a MESSAGE object."
  (let ((endianness (ecase (code-char (read-byte stream))
                      (#\l :little-endian)
                      (#\B :big-endian))))
    (setf (stream-read-position stream) 1)
    (destructuring-bind (type-code flags major-protocol-version
                                   body-length serial fields)
        (unpack stream endianness "yyyuua(yv)")
      (with-binary-readers (stream endianness)
        (align 8)
        (let (body path interface member error-name
                   reply-serial destination sender signature)
          (loop for (field-code field-value) in fields
                do (case field-code
                     (1 (setf path field-value))
                     (2 (setf interface field-value))
                     (3 (setf member field-value))
                     (4 (setf error-name field-value))
                     (5 (setf reply-serial field-value))
                     (6 (setf destination field-value))
                     (7 (setf sender field-value))
                     (8 (setf signature field-value))
                     (t (warn "Unknown field code ~D; ignoring field." field-code))))
          (setf body (unpack stream endianness signature))
          (macrolet ((make-message (class-name &rest additional-initargs)
                       `(make-instance ,class-name
                                       :endianness endianness
                                       :flags flags
                                       :major-protocol-version major-protocol-version
                                       :body-length body-length
                                       :serial serial
                                       :destination destination
                                       :sender sender
                                       :signature signature
                                       :body body
                                       ,@additional-initargs)))
            (case type-code
              (1 (make-message 'method-call-message :path path :interface interface :member member))
              (2 (make-message 'method-return-message :reply-serial reply-serial))
              (3 (make-message 'error-message :error-name error-name :reply-serial reply-serial))
              (4 (make-message 'signal-message :path path :interface interface :member member))
              (t (warn "Unknown message type code ~D; ignoring message." type-code)))))))))


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
        (multiple-value-bind (body message)
            (wait-for-reply serial connection)
          (etypecase message
            (method-return-message (values-list body))
            (error-message (error 'method-error :arguments body)))))))
