;;;; +----------------------------------------------------------------+
;;;; | DBUS                                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)


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

(defclass interface ()
  ((name :initarg :name :reader interface-name)
   (methods :initform (make-hash-table :test 'equal) :reader interface-methods)))

(defmethod print-object ((interface interface) stream)
  (print-unreadable-object (interface stream :type t)
    (format stream "~S" (interface-name interface)))
  interface)

(defclass method ()
  ((name :initarg :name :reader method-name)
   (signature :initarg :signature :reader method-signature)))

(defmethod print-object ((method method) stream)
  (print-unreadable-object (method stream :type t)
    (format stream "~S" (method-name method)))
  method)

(defun interface-method (name interface)
  (gethash name (interface-methods interface)))

(defun (setf interface-method) (method name interface)
  (setf (gethash name (interface-methods interface)) method))

(defun object-interface (name object)
  (gethash name (object-interfaces object)))

(defun (setf object-interface) (interface name object)
  (setf (gethash name (object-interfaces object)) interface))

(defun list-object-interfaces (object)
  (hash-table-values (object-interfaces object)))

(defun list-interface-methods (interface)
  (hash-table-values (interface-methods interface)))

(defun make-object (connection path destination interfaces)
  (let ((object (make-instance 'object :connection connection :path path :destination destination)))
    (dolist (interface interfaces)
      (setf (object-interface (interface-name interface) object) interface))
    object))

(defun make-interface (name methods)
  (let ((interface (make-instance 'interface :name name)))
    (dolist (method methods)
      (setf (interface-method (method-name method) interface) method))
    interface))

(defun make-method (name signature)
  (make-instance 'method :name name :signature signature))

(defun dont-resolve-entities (a b)
  (declare (ignore a b))
  (flexi-streams:make-in-memory-input-stream nil))

(defun parse-introspection-document (input)
  (with-xspam-source (make-xspam-source input :entity-resolver #'dont-resolve-entities)
    (element :node
      (let (interfaces)
        (one-or-more
         (element :interface
           (let (interface-name)
             (attribute :name (setf interface-name _))
             (let (methods)
               (zero-or-more
                (element :method
                  (let (method-name)
                    (attribute :name (setf method-name _))
                    (let ((signature (make-string-output-stream)))
                      (zero-or-more
                       (element :arg
                         (attribute :direction
                           (when (equal _ "in")
                             (attribute :type
                               (write-string _ signature))))))
                      (push (make-method method-name (get-output-stream-string signature)) methods)))))
               (push (make-interface interface-name (nreverse methods)) interfaces)))))
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
