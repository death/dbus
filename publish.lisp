;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus/publish
  (:use #:cl
        #:dbus/utils #:dbus/protocols #:dbus/messages #:dbus/connections
        #:dbus/types)
  (:import-from #:iolib #:event-dispatch)
  (:export
   #:define-dbus-object
   #:define-dbus-method
   #:define-dbus-signal-handler
   #:publish-objects))

(in-package #:dbus/publish)


;;;; Publish objects to DBUS

;;; Define objects

(defparameter *all-dbus-objects* '())

(defclass dbus-object ()
  ((name :initarg :name :reader dbus-object-name)
   (path :initarg :path :accessor dbus-object-path)
   (method-handlers :initform (make-hash-table :test 'equal) :reader dbus-object-method-handlers)
   (signal-handlers :initform (make-hash-table :test 'equal) :reader dbus-object-signal-handlers)))

(defgeneric dbus-object-handler-lookup-table (message object))

(defmethod dbus-object-handler-lookup-table ((message signal-message) (object dbus-object))
  (dbus-object-signal-handlers object))

(defmethod dbus-object-handler-lookup-table ((message method-call-message) (object dbus-object))
  (dbus-object-method-handlers object))

(defun find-dbus-object (name)
  (check-type name symbol)
  (get name 'dbus-object))

(defun (setf find-dbus-object) (new-value name)
  (check-type new-value (or null dbus-object))
  (cond ((null new-value)
         (setf *all-dbus-objects* (remove name *all-dbus-objects*))
         (remprop name 'dbus-object)
         nil)
        (t
         (pushnew name *all-dbus-objects*)
         (setf (get name 'dbus-object) new-value))))

(defun register-dbus-object (name path)
  (check-type name symbol)
  (check-type path string)
  (if (find-dbus-object name)
      ;; If we already have an object with that name, just update its
      ;; path.
      (setf (dbus-object-path (find-dbus-object name)) path)
      (setf (find-dbus-object name)
            (make-instance 'dbus-object
                           :name name
                           :path path)))
  name)

(defun require-dbus-object (name)
  (loop with object = (find-dbus-object name)
        while (not (typep object 'dbus-object))
        do (setf object (inexistent-entry name :error))
           ;; We can also accept a new object name.
           (when (symbolp object)
             (setf object (find-dbus-object object)))
        finally (return (values object (dbus-object-name object)))))

(defmacro define-dbus-object (name &body options)
  (let ((path nil))
    (dolist (option options)
      (when (and (consp option) (eq (car option) :path))
        (setf path (cadr option))))
    `(register-dbus-object ',name ,path)))

;;; Define handlers

(defclass handler ()
  ((object-name :initarg :object-name :reader handler-object-name)
   (lisp-name :initarg :lisp-name :reader handler-lisp-name)
   (name :initarg :name :reader handler-name)
   (interface :initarg :interface :reader handler-interface)
   (input-signature :initarg :input-signature :reader handler-input-signature)
   (function :initarg :function :reader handler-function)))

(defgeneric handler-full-lisp-name (handler))

(defmethod handler-full-lisp-name ((handler handler))
  (list (handler-object-name handler) (handler-lisp-name handler)))

(defun full-member-name (interface member)
  (concatenate 'string interface "." member))

(defun stringify-lisp-name (lisp-name)
  "Return a string that is the capitalized symbol name of LISP-NAME,
sans dashes."
  (remove #\- (string-capitalize lisp-name)))

(defclass method-handler (handler)
  ((output-signature :initarg :output-signature :reader handler-output-signature)))

(defun register-dbus-method (object-name method-name name-string interface parameter-types return-types handler)
  (check-type method-name symbol)
  (check-type name-string string)
  (check-type interface string)
  (multiple-value-bind (object object-name)
      (require-dbus-object object-name)
    (setf (gethash (full-member-name interface name-string)
                   (dbus-object-method-handlers object))
          (make-instance 'method-handler
                         :object-name object-name
                         :lisp-name method-name
                         :name name-string
                         :interface interface
                         :input-signature parameter-types
                         :output-signature return-types
                         :function handler))
    (list object-name method-name)))

(defmacro define-dbus-method ((object-name method-name) (&rest parameters) (&rest return-types) &body body)
  (let ((name-string (stringify-lisp-name method-name))
        (interface nil)
        (parameter-names (mapcar #'first parameters))
        (parameter-types (mapcar #'second parameters)))
    ;; Set options.
    (loop while (and (consp (car body)) (keywordp (caar body)))
          do (let ((option (pop body)))
               (ecase (car option)
                 (:name
                  (setf name-string (cadr option)))
                 (:interface
                  (setf interface (cadr option))))))
    ;; Register the method with the object.
    `(register-dbus-method ',object-name
                           ',method-name
                           ,name-string
                           ,interface
                           ',parameter-types
                           ',return-types
                           (lambda (,@parameter-names)
                             ,@body))))

(defclass signal-handler (handler)
  ())

(defun register-dbus-signal-handler (object-name handler-name name-string interface parameter-types handler)
  (check-type handler-name symbol)
  (check-type name-string string)
  (check-type interface string)
  (multiple-value-bind (object object-name)
      (require-dbus-object object-name)
    (setf (gethash (full-member-name interface name-string)
                   (dbus-object-signal-handlers object))
          (make-instance 'signal-handler
                         :object-name object-name
                         :lisp-name handler-name
                         :name name-string
                         :interface interface
                         :input-signature parameter-types
                         :function handler))
    (list object-name handler-name)))

(defmacro define-dbus-signal-handler ((object-name handler-name) (&rest parameters) &body body)
  (let ((name-string (stringify-lisp-name handler-name))
        (interface nil)
        (parameter-names (mapcar #'first parameters))
        (parameter-types (mapcar #'second parameters)))
    ;; Set options.
    (loop while (and (consp (car body)) (keywordp (caar body)))
          do (let ((option (pop body)))
               (ecase (car option)
                 (:name
                  (setf name-string (cadr option)))
                 (:interface
                  (setf interface (cadr option))))))
    ;; Register the signal handler with the object.
    `(register-dbus-signal-handler ',object-name
                                   ',handler-name
                                   ,name-string
                                   ,interface
                                   ',parameter-types
                                   (lambda (,@parameter-names)
                                     ,@body))))

;;; Publishing objects

(defgeneric publish-objects (connection &optional object-names))
(defgeneric dispatch-message (message object connection))
(defgeneric lookup-handler (message object))
(defgeneric apply-handler (handler message connection))
(defgeneric missing-handler (message connection))
(defgeneric signature-mismatch (expected-signature message connection))
(defgeneric handler-error (condition handler message connection))
(defgeneric method-handler-bad-results (results method message connection))
(defgeneric method-return-reply (results method message connection))
(defgeneric method-error-reply (error-name error-description message connection))

(defmethod publish-objects ((connection standard-connection) &optional (object-names *all-dbus-objects*))
  (let ((objects-by-path (make-object-index object-names)))
    ;; At this point we have an index by object path.  Note that if we
    ;; redefine an object with a new path later on, the index will be
    ;; stale.  Avoid doing that :)
    (loop
     (dolist (message (drain-pending-messages connection))
       (let ((object (gethash (message-path message) objects-by-path)))
         (if (null object)
             (missing-handler message connection)
             (dispatch-message message object connection))))
     (event-dispatch (connection-event-base connection) :one-shot t))))

(defun make-object-index (object-names)
  (let ((index (make-hash-table :test 'equal)))
    (dolist (object-name object-names)
      (with-simple-restart (skip "Skip publishing object")
        (multiple-value-bind (object object-name)
            (require-dbus-object object-name)
          (symbol-macrolet ((index-entry (gethash (dbus-object-path object) index)))
            (when (or (null index-entry)
                      (replace-entry-p index-entry object :error))
              (setf index-entry object))))))
    index))

(defun matching-signatures-p (signature1 signature2)
  (equal (signature signature1) (signature signature2)))

(defmethod dispatch-message (message (object dbus-object) (connection connection))
  (let ((handler (lookup-handler message object)))
    (cond ((null handler)
           (missing-handler message connection))
          ((not (matching-signatures-p (message-signature message)
                                       (handler-input-signature handler)))
           (signature-mismatch (handler-input-signature handler) message connection))
          (t
           (apply-handler handler message connection)))))

(defmethod lookup-handler ((message message) (object dbus-object))
  (gethash (full-member-name (message-interface message) (message-member message))
           (dbus-object-handler-lookup-table message object)))

(defmethod apply-handler ((handler signal-handler) (message signal-message) (connection connection))
  (handler-case
      (apply (handler-function handler) (message-body message))
    (error (condition)
      (handler-error condition handler message connection))))

(defmethod apply-handler ((handler method-handler) (message method-call-message) (connection connection))
  (let ((results (handler-case
                     (multiple-value-list
                      (apply (handler-function handler) (message-body message)))
                   (error (condition)
                     (handler-error condition handler message connection)
                     (return-from apply-handler)))))
    (if (valid-body-p results (handler-output-signature handler))
        (method-return-reply results handler message connection)
        (method-handler-bad-results results handler message connection))))

(defmethod missing-handler (message connection)
  (method-error-reply "MissingHandler"
                      (format nil "Missing ~A handler at path ~A interface ~A name ~A"
                              (if (typep message 'signal-message) "signal" "method-call")
                              (message-path message)
                              (message-interface message)
                              (message-member message))
                      message connection))

(defmethod signature-mismatch (expected-signature message connection)
  (method-error-reply "SignatureMismatch"
                      (format nil "Mismatching signature; expected=~S, actual=~S."
                              (signature expected-signature)
                              (signature (message-signature message)))
                      message connection))

(defmethod handler-error (condition (handler signal-handler) (message message) (connection connection))
  (warn "Signal handler signaled an error: ~A." condition))

(defmethod handler-error (condition (handler method-handler) (message message) (connection connection))
  (warn "Method handler ~S signaled an error: ~A."
        (handler-full-lisp-name handler) condition)
  (method-error-reply "MethodError"
                      (format nil "Method ~A signaled an error: ~A"
                              (handler-name handler) condition)
                      message connection))

(defmethod method-handler-bad-results (results handler message connection)
  (cerror "Continue, sending an error to the bus"
          "Method handler ~S returned bad results; expected-signature=~S, results=~S."
          (handler-full-lisp-name handler)
          (handler-output-signature handler)
          results)
  (method-error-reply "InternalMethodError"
                      (format nil "Method ~A is buggy." (handler-name handler))
                      message connection))

(defmethod method-return-reply (results handler message connection)
  (unless (logtest message-no-reply-expected (message-flags message))
    (send-message
     (encode-message (message-endianness message) :method-return 0 1
                     (connection-next-serial connection) nil nil nil nil
                     (message-serial message) (message-sender message)
                     nil (handler-output-signature handler) results)
     connection)))

;; Do nothing; signals don't reply.
(defmethod method-error-reply (error-name error-description (message signal-message) connection)
  (declare (ignore error-name error-description connection)))

(defmethod method-error-reply (error-name error-description (message method-call-message) connection)
  (unless (logtest message-no-reply-expected (message-flags message))
    (send-message
     (encode-message (message-endianness message) :error 0 1
                     (connection-next-serial connection) nil nil nil
                     ;; TODO: Not invent error names like that.
                     (concatenate 'string (message-interface message) ".Error." error-name)
                     (message-serial message) (message-sender message) nil
                     "s" (list error-description))
     connection)))
