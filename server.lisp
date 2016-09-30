(in-package #:dbus)



;;;; Method stores
;;;
;;; A method store is a accumulation of methods that can
;;; be called via DBus.

(defclass dbus-export-table ()
  ;; for now just a list.
  ;; TODO: hash (interface path) => list of methods?
  ((methods :initform ()
            :accessor dbus-export-table-methods)))


(defvar *default-dbus-store* 
  (make-instance 'dbus-export-table)
  "The default DBus method store.")


(defun replace-method-in-store (store new)
  (flet
    ((is-same-method? (m)
       (or
         (eq (method-lisp-name m)
             (method-lisp-name new))
         (and (eq (method-name m)
                  (method-name new))
              (equal (method-signature m)
                     (method-signature new))))))
    (setf (dbus-export-table-methods store)
          (cons new
            (remove-if #'is-same-method? 
                       (dbus-export-table-methods store))))))


(defmacro define-dbus-method (lisp-name args 
                              (&key in out 
                                    (dbus-name lisp-name)
                                    (store '*default-dbus-store*))
                              &body body)
  "Registers a function/method with the lisp symbol LISP-NAME and ARGS;
   IN and OUT describe the arguments and return values;
   a different DBUS-NAME can be set; or a different STORE than
   *DEFAULT-DBUS-STORE* can be used.

   While DBUS-NAME might be used multiple times(?), LISP-NAME
   must be unique."
  (unless in
    (error "need an IN signature"))
  (unless out
    (error "need an OUT signature"))
  (alexandria:with-gensyms (method)
    `(progn
       ;; TODO: (declaim (ftype (function ...) ,lisp-name)) ??
       (defmethod ,lisp-name ,args
         ,@body)
       (let* ((,method (make-method ,(string-downcase (string dbus-name))
                                    ,in
                                    ',(alexandria:parse-ordinary-lambda-list args)
                                    nil ;; TODO
                                    ,out
                                    ',lisp-name)))
         (replace-method-in-store ,store ,method))
       ',lisp-name)))


(defmethod convert-to-store ((package package))
  (loop with store = (make-instance 'dbus-export-table)
        for sym being the external-symbols of package
        for gf = (ignore-errors
                   (symbol-function sym)) 
        if (and gf 
                (eq (symbol-package sym)
                    package))
        ;; TODO: better case-reverse? depends on readtable of the package.
        do (loop with name = (string-downcase (symbol-name sym))
                 with arg-names = (closer-mop:generic-function-lambda-list gf)
                 for cl-method in (closer-mop:generic-function-methods 
                                    (symbol-function
                                      sym))
                 for arg-sig = (map 'string
                                    #'lisp-type-to-dbus
                                    (closer-mop:method-specializers cl-method))
                 for method = (make-method name
                                           arg-sig
                                           arg-names ; (alexandria:parse-ordinary-lambda-list arg-names)
                                           nil ;; TODO
                                           "v" ; TODO
                                           sym)
                 do (replace-method-in-store store method)
                 finally (return store))))




;;;; Basic server operation


(defclass dbus-server-data ()
  ((table :initarg :table :reader dbus-server-table)
   (name :initarg :name :reader dbus-server-name)
   (connection :initarg :conn :reader dbus-server-conn)
   (interface :initarg :interface :reader dbus-server-interface)
   (introspect-xml :initarg :introspect-xml :accessor dbus-server-i-s-xml)))
  

(defun return-introspection (name arg-names arg-sig return-sig)
  (flet 
    ((lc-name (name)
       ;; TODO: better case-reverse? depends on readtable of the package...
       (string-downcase (string name))))
    (with-tag ("method" `(("name" ,(lc-name name))))
      (loop for a-name in arg-names
        ;; make arg-types optional
        for a-type in (sigexp arg-sig)
        do (empty-tag "arg" 
                      `(("direction" "in")
                        ("type" ,(signature (list
                                              (find-dbus-type a-type))))
                        ("name" ,(lc-name a-name)))))
      ;; TODO - multiple values
      (loop for a-type in (sigexp return-sig)
        do (empty-tag "arg" 
                      `(("direction" "out")
                        ("type" ,(or (signature (list
                                                  (find-dbus-type a-type)))
                                     "v"))))))))


;; <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
;;    "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
;;  <node name="/interface">
;;    <interface name="...">
;;      <method name="...">
;;        <arg direction="in"  type="as" name="..." />
;;        <arg direction="out" type="a(sa{ss})" />
;;      </method>
(defun build-introspection-of-store (store interface)
  "Returns the XML describing methods in STORE."
  ;;(with-xml-output (stream)) is incompatible...
  (with-output-to-string (xml-emitter::*xml-output-stream*)
    (princ "<!DOCTYPE node PUBLIC 
            \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" 
            \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">"
           xml-emitter::*xml-output-stream*)
    (with-tag ("node")
      (with-tag ("interface" `(("name" ,interface)))
        (loop for method in (dbus-export-table-methods store)
              do (return-introspection (method-name method)
                                       (method-argument-names method) 
                                       (method-signature method) 
                                       (method-result-types method)))))))

#+nil
(build-introspection-of-store *default-dbus-store* "interface")
#+nil
(sigexp "si")


(defvar *message* nil
  "The current dbus message")

(defvar *server-data* nil
  "The current dbus server data")

(defvar *reply-signature* nil
  "The signature of the reply, may be taken from the called methods
   OUT signature.")



(defun reply-to-message (message connection reply &key 
                                 signature 
                                 (flags message-no-reply-expected))
  (if (and reply
           (zerop (logand (message-flags message)
                          message-no-reply-expected)))
    (send-message
      (encode-message (message-endianness message)
                      :method-return
                      flags
                      1 
                      (message-serial message)
                      (message-path message)
                      (message-interface message)
                      (message-member message)
                      nil ; error-name
                      (message-serial message) ; reply-serial
                      (message-sender message) ; destination nil
                      nil
                      (or signature
                          (make-string (length reply)
                                       :initial-element #\v)
                          #+nil ; TODO?
                          (map 'string
                               #'lisp-type-to-dbus
                               (mapcar #'type-of 
                                       reply)))
                      reply)
      connection)))


(defgeneric server-handle-message (connection message server))


(defun handle-one-message (message server)
  (let ((connection (dbus-server-conn server))
        (*message* message)
        (*reply-signature* nil))
    (handler-case   
      (server-handle-message
                           connection
                           *message*
                           server)
      (:no-error (&rest reply)
       (reply-to-message *message*
                         connection
                         reply
                         :signature *reply-signature*))
      (T (condition)
       (reply-to-message *message*
                         connection
                         (list (format nil "ERROR: ~a" condition))
                         :flags message-error-reply
                         :signature "s")))))


(defmethod server-loop ((connection standard-connection) (server dbus-server-data))
  (let ((connection (dbus-server-conn server)))
    (flet
      ()
      ;; Can't replace, 
      ;;   "FD 14 is already monitored for event READ"
      #+nil
      (set-io-handler
        (connection-event-base connection)
        (connection-fd connection)
        :read
        (lambda (fd event error)
          (declare (ignore fd event))
          (if error
            (error "Connection I/O error: ~S." error)
            (handle-one-message (receive-message connection)
                                server))))
      (loop
        (event-dispatch 
          (connection-event-base connection)
          :one-shot t)
        (loop for msg in (reverse 
                           (shiftf 
                             (connection-pending-messages connection)
                             ()))
              ;; until EOF
              while msg
              do (handle-one-message msg server))))))


(defmethod dbus-serve ((bus bus) server-name interface store 
                                 &key (request-flags '(:do-not-queue :replace-existing :allow-replacement)))
  "Registers SERVER-NAME on BUS, using FLAGS (see REQUEST-NAME for details), 
   and then serves STORE (defaults to *DEFAULT-DBUS-STORE*);
   but also see MAKE-STORE-FROM-PACKAGE."
  (let* ((defaulted-store (or store
                              *default-dbus-store*))
         (connection (bus-connection bus))
         (*server-data* 
           (make-instance 'dbus-server-data
                          :conn connection
                          :table defaulted-store
                          :name server-name
                          :interface interface
                          :introspect-xml (build-introspection-of-store 
                                            defaulted-store
                                            interface))))
    (apply #'request-name bus server-name request-flags)
    (server-loop connection *server-data*)))



;;;; Message handling


;; #<DBUS:SIGNAL-MESSAGE {1008C6BB23}>
;;   [standard-object]
;; 
;; Slots with :INSTANCE allocation:
;;   ENDIANNESS              = :LITTLE-ENDIAN
;;   FLAGS                   = 1
;;   MAJOR-PROTOCOL-VERSION  = 1
;;   BODY-LENGTH             = 17
;;   SERIAL                  = 3
;;   DESTINATION             = ":1.341"
;;   SENDER                  = "org.freedesktop.DBus"
;;   SIGNATURE               = "s"
;;   BODY                    = ("my.test.here")
;;   PATH                    = "/org/freedesktop/DBus"
;;   INTERFACE               = "org.freedesktop.DBus"
;;   MEMBER                  = "NameAcquired"
(defmethod server-handle-message ((connection standard-connection)
                                  (message signal-message)
                                  (server dbus-server-data))
  (declare (ignore connection server))
  (describe message)
  (values))


;; #<DBUS:METHOD-CALL-MESSAGE {1008E01B93}>
;;   [standard-object]
;; 
;; Slots with :INSTANCE allocation:
;;   ENDIANNESS              = :LITTLE-ENDIAN
;;   FLAGS                   = 0
;;   MAJOR-PROTOCOL-VERSION  = 1
;;   BODY-LENGTH             = 0
;;   SERIAL                  = 2
;;   DESTINATION             = "my.test.here"
;;   SENDER                  = ":1.342"
;;   SIGNATURE               = NIL
;;   BODY                    = NIL
;;   PATH                    = "/"
;;   INTERFACE               = "org.freedesktop.DBus.Introspectable"
;;   MEMBER                  = "Introspect"
(defmethod server-handle-message ((connection standard-connection)
                                  (msg method-call-message)
                                  (server dbus-server-data))
  (declare (ignore connection))
  (cond 
    ((and (equal (message-interface msg)
                 "org.freedesktop.DBus.Introspectable")
          (equal (message-member msg)
                 "Introspect"))
     (setf *reply-signature* "s")
     (dbus-server-i-s-xml server))
    ((and (equal (message-interface msg)
                 "org.freedesktop.DBus.Properties")
          (equal (message-body)
                 (list "org.freedesktop.DBus.Introspectable"))
          (equal (message-member msg)
                 "GetAll"))
     (error "No such interface 'org.freedesktop.DBus.Properties' at object path '/'"))
    (T
     (describe msg)
     (let ((method (find (message-member msg)
                         (dbus-export-table-methods
                           (dbus-server-table server))
                         :test #'equal
                         :key #'method-name)))
       (cond
         (method
           (setf *reply-signature*
                 (method-result-types method))
           (apply (method-lisp-name method)
                  (message-body msg)))
         (T
          (error "404 not found")))))))
