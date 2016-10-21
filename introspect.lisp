;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus/introspect
  (:use #:cl #:dbus/messages)
  (:import-from #:alexandria #:hash-table-values)
  (:import-from #:xspam
                #:with-xspam-source #:make-xspam-source #:element
                #:one-or-more #:attribute #:_ #:zero-or-more
                #:optional-attribute #:one-of)
  (:import-from #:flexi-streams #:make-in-memory-input-stream)
  (:shadow #:method #:make-method #:signal)
  (:export
   #:object-connection
   #:object-path
   #:object-destination
   #:object-interface
   #:list-object-interfaces
   #:interface-name
   #:interface-method
   #:interface-property
   #:interface-signal
   #:list-interface-methods
   #:list-interface-properties
   #:list-interface-signals
   #:method-name
   #:method-signature
   #:method-argument-names
   #:method-argument-types
   #:method-result-types
   #:property-name
   #:property-type
   #:property-access
   #:signal-name
   #:signal-argument-names
   #:signal-argument-types
   #:parse-introspection-document
   #:make-object-from-introspection
   #:object-invoke))

(in-package #:dbus/introspect)


;;;; Support for introspection of D-BUS objects

(defclass object ()
  ((connection :initarg :connection :reader object-connection)
   (path :initarg :path :reader object-path)
   (destination :initarg :destination :reader object-destination)
   (interfaces :initform (make-hash-table :test 'equal) :reader object-interfaces)))

(defmethod print-object ((object object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (object-path object)))
  object)

(defun object-interface (name object)
  (gethash name (object-interfaces object)))

(defun (setf object-interface) (interface name object)
  (setf (gethash name (object-interfaces object)) interface))

(defun list-object-interfaces (object)
  (hash-table-values (object-interfaces object)))

(defun make-object (connection path destination interfaces)
  (let ((object (make-instance 'object :connection connection :path path :destination destination)))
    (dolist (interface interfaces)
      (setf (object-interface (interface-name interface) object) interface))
    object))

(defclass interface ()
  ((name :initarg :name :reader interface-name)
   (methods :initform (make-hash-table :test 'equal) :reader interface-methods)
   (properties :initform (make-hash-table :test 'equal) :reader interface-properties)
   (signals :initform (make-hash-table :test 'equal) :reader interface-signals)))

(defmethod print-object ((interface interface) stream)
  (print-unreadable-object (interface stream :type t)
    (format stream "~S" (interface-name interface)))
  interface)

(defun interface-method (name interface)
  (gethash name (interface-methods interface)))

(defun interface-property (name interface)
  (gethash name (interface-properties interface)))

(defun interface-signal (name interface)
  (gethash name (interface-signals interface)))

(defun (setf interface-method) (method name interface)
  (setf (gethash name (interface-methods interface)) method))

(defun (setf interface-property) (property name interface)
  (setf (gethash name (interface-properties interface)) property))

(defun (setf interface-signal) (signal name interface)
  (setf (gethash name (interface-signals interface)) signal))

(defun list-interface-methods (interface)
  (hash-table-values (interface-methods interface)))

(defun list-interface-properties (interface)
  (hash-table-values (interface-properties interface)))

(defun list-interface-signals (interface)
  (hash-table-values (interface-signals interface)))

(defun make-interface (name methods properties signals)
  (let ((interface (make-instance 'interface :name name)))
    (dolist (method methods)
      (setf (interface-method (method-name method) interface) method))
    (dolist (property properties)
      (setf (interface-property (property-name property) interface) property))
    (dolist (signal signals)
      (setf (interface-signal (signal-name signal) interface) signal))
    interface))

(defclass method ()
  ((name        :initarg :name      :reader method-name)
   (signature   :initarg :signature :reader method-signature)
   (arg-names   :initarg :args      :reader method-argument-names)
   (arg-types   :initarg :arg-types :reader method-argument-types)
   (results     :initarg :res       :reader method-result-types)))

(defmethod print-object ((method method) stream)
  (print-unreadable-object (method stream :type t)
    (format stream "~S ~A" (method-name method) (method-signature method)))
  method)

(defun make-method (name signature parm-names parm-types results)
  (make-instance 'method
                 :name name
                 :signature signature
                 :args      parm-names
                 :arg-types parm-types
                 :res results))

(defclass property ()
  ((name        :initarg :name   :reader property-name)
   (type        :initarg :type   :reader property-type)
   (access      :initarg :access :reader property-access)))

(defmethod print-object ((property property) stream)
  (print-unreadable-object (property stream :type t)
    (format stream "~S" (property-name property)))
  property)

(defun make-property (name type access)
  (make-instance 'property
                 :name name
                 :type type
                 :access access))

(defclass signal ()
  ((name        :initarg :name      :reader signal-name)
   (arg-names   :initarg :args      :reader signal-argument-names)
   (arg-types   :initarg :arg-types :reader signal-argument-types)))

(defmethod print-object ((signal signal) stream)
  (print-unreadable-object (signal stream :type t)
    (format stream "~S" (signal-name signal)))
  signal)

(defun make-signal (name parm-names parm-types)
  (make-instance 'signal
                 :name      name
                 :args      parm-names
                 :arg-types parm-types))

(defun dont-resolve-entities (a b)
  (declare (ignore a b))
  (make-in-memory-input-stream nil))

(defmacro defaulted-attribute (name default-value &body forms)
  `(let ((_ (or (optional-attribute ,name _) ,default-value)))
     ,@forms))

(defun parse-introspection-document (input)
  (with-xspam-source (make-xspam-source input :entity-resolver #'dont-resolve-entities)
    (element :node
      (let (interfaces)
        (one-or-more
         (element :interface
           (let (interface-name)
             (attribute :name (setf interface-name _))
             (let (methods properties signals)
               (zero-or-more
                (one-of
                 (element :method
                   (let (method-name
                         (signature (make-string-output-stream))
                         (parm-names ())
                         (parm-types ())
                         (result-types ()))
                     (attribute :name (setf method-name _))
                     (zero-or-more
                      ;; TODO: annotation
                      (element :arg
                        (defaulted-attribute :direction "in"
                          (when (equal _ "out")
                            (attribute :type
                              (push _ result-types)))
                          (when (equal _ "in")
                            (defaulted-attribute :name nil
                              (push _ parm-names))
                            (attribute :type
                              (push _ parm-types)
                              (write-string _ signature))))))
                     (push (make-method method-name
                                        (get-output-stream-string signature)
                                        (reverse parm-names)
                                        (reverse parm-types)
                                        (reverse result-types))
                           methods)))
                 (element :property
                   (let (property-name property-type property-access)
                     (attribute :name (setf property-name _))
                     (attribute :type (setf property-type _))
                     (attribute :access (setf property-access _))
                     (push (make-property property-name property-type property-access) properties)))
                 (element :signal
                   (let ((signal-name)
                         (parm-names ())
                         (parm-types ()))
                     (attribute :name (setf signal-name _))
                     (zero-or-more
                      (element :arg
                        (defaulted-attribute :name nil
                          (push _ parm-names))
                        (attribute :type (push _ parm-types))))
                     (push (make-signal signal-name
                                        (nreverse parm-names)
                                        (nreverse parm-types))
                           signals)))))
               (push (make-interface interface-name (nreverse methods) (nreverse properties) (nreverse signals)) interfaces)))))
        (nreverse interfaces)))))

(defun make-object-from-introspection (connection path destination)
  (make-object connection path destination
               (parse-introspection-document
                (fetch-introspection-document connection path destination))))

(defun fetch-introspection-document (connection path destination)
  (invoke-method connection "Introspect"
                 :path path
                 :destination destination
                 :interface "org.freedesktop.DBus.Introspectable"))

(defun object-invoke (object interface-name method-name &rest args)
  (invoke-method (object-connection object)
                 method-name
                 :path (object-path object)
                 :interface interface-name
                 :destination (object-destination object)
                 :signature (signature-for-method method-name interface-name object)
                 :arguments args))

(defun signature-for-method (method-name interface-name object)
  (method-signature (interface-method method-name (object-interface interface-name object))))
