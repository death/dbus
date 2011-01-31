;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


;;;; Packing and unpacking

(defun alignment (type)
  "Return the number of octets to which elements of the supplied type
should be aligned."
  (etypecase type
    ((member :byte :signature :variant) 1)
    ((member :int16 :uint16) 2)
    ((or (member :boolean :int32 :uint32 :string :object-path)
         (cons (member :array))) 4)
    ((or (member :int64 :uint64 :double)
         (cons (member :struct :dict-entry))) 8)))

(defun pack (stream endianness sigexp &rest values)
  "Pack values according to the signature expression and endianness
into stream."
  (with-binary-writers (stream endianness)
    (labels ((str (value)
               (let ((octets (babel:string-to-octets value :encoding :utf-8)))
                 (u32 (length octets))
                 (map nil #'u8 octets)
                 (u8 0)))
             (sig (value)
               (let ((octets (babel:string-to-octets value :encoding :utf-8)))
                 (u8 (length octets))
                 (map nil #'u8 octets)
                 (u8 0)))
             (arr (element-type value)
               (let ((length-position (file-position stream)))
                 (u32 0)
                 (align (alignment element-type))
                 (let ((start-position (file-position stream)))
                   (pack-seq (circular-list element-type) value)
                   (let ((end-position (file-position stream)))
                     (file-position stream length-position)
                     (u32 (- end-position start-position))
                     (file-position stream end-position)))))
             (struct (field-types value)
               (map nil (lambda (type element)
                          (pack-1 type element))
                    field-types value))
             (var (type value)
               (pack-1 :signature type)
               (pack-1 (first type) value))
             (pack-1 (type value)
               (align (alignment type))
               (etypecase type
                 ((eql :byte) (u8 value))
                 ((eql :boolean) (u32 (if value 1 0)))
                 ((eql :int16) (u16 (signed-to-unsigned value 16)))
                 ((eql :uint16) (u16 value))
                 ((eql :int32) (u32 (signed-to-unsigned value 32)))
                 ((eql :uint32) (u32 value))
                 ((eql :int64) (u64 (signed-to-unsigned value 64)))
                 ((eql :uint64) (u64 value))
                 ((eql :double) (u64 (double-to-unsigned (float value 0.0d0))))
                 ((member :string :object-path) (str value))
                 ((eql :signature) (sig (signature value)))
                 ((cons (eql :array)) (arr (second type) value))
                 ((cons (member :struct :dict-entry)) (struct (rest type) value))
                 ((eql :variant) (var (sigexp (first value)) (second value)))))
             (pack-seq (types values)
               (map nil #'pack-1 types values)))
      (pack-seq (sigexp sigexp) values))))

(defun unpack (stream endianness sigexp)
  "Unpack values from stream according to endianness and the signature
expression and return them as a list."
  (with-binary-readers (stream endianness)
    (labels ((str (length)
               (prog1
                   (babel:octets-to-string
                    (map-into (make-octet-vector length) #'u8)
                    :encoding :utf-8)
                 (u8)))
             (arr (element-type length)
               (align (alignment element-type))
               (loop with start = (stream-read-position stream)
                     with end = (+ start length)
                     until (= end (stream-read-position stream))
                     collect (unpack-1 element-type)))
             (struct (field-types)
               (unpack-seq field-types))
             (unpack-1 (type)
               (align (alignment type))
               (etypecase type
                 ((eql :byte) (u8))
                 ((eql :boolean) (if (zerop (u32)) nil t))
                 ((eql :int16) (unsigned-to-signed (u16) 16))
                 ((eql :uint16) (u16))
                 ((eql :int32) (unsigned-to-signed (u32) 32))
                 ((eql :uint32) (u32))
                 ((eql :int64) (unsigned-to-signed (u64) 64))
                 ((eql :uint64) (u64))
                 ((eql :double) (unsigned-to-double (u64)))
                 ((member :string :object-path) (str (u32)))
                 ((eql :signature) (str (u8)))
                 ((cons (eql :array)) (arr (second type) (u32)))
                 ((cons (member :struct :dict-entry)) (struct (rest type)))
                 ((eql :variant) (unpack-1 (first (sigexp (str (u8))))))))
             (unpack-seq (types)
               (map 'list #'unpack-1 types)))
      (unpack-seq (sigexp sigexp)))))


;;;; Encoding and decoding messages

(defun encode-message (endianness type flags major-protocol-version
                       serial path interface member error-name reply-serial
                       destination sender signature body)
  "Encode a DBUS message and return it as an octet vector."
  (flexi-streams:with-output-to-sequence (out)
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
