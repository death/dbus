;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(in-package #:dbus)

(defun get-property (bus service object interface property)
  "Invokes the Get method to retrieve an object property."
  (invoke-method (bus-connection bus)
                 "Get"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.Properties"
                 :signature "ss"
                 :arguments (list interface property)))

(defun get-all-properties (bus service object interface)
  "Invokes the GetAll method to retrieve all the properties of an object."
  (invoke-method (bus-connection bus)
                 "GetAll"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.Properties"
                 :signature "s"
                 :arguments (list interface)))

(defun get-managed-objects (bus service object)
  (invoke-method (bus-connection bus)
                 "GetManagedObjects"
                 :destination service
                 :path object
                 :interface "org.freedesktop.DBus.ObjectManager"
                 :signature ""))

(defun add-match (bus &rest parameters)
  "Invokes AddMatch bus method.  Valid parameters are:

  :type           (:signal, :method-call, :method-return, :error)
  :sender         bus-name
  :interface      interface-name
  :member         (method-name, symbol-name)
  :path           object-path
  :path-namespace object-path
  :destination    unique-name
  :argN [N=0~63]  string"
  (when (oddp (length parameters))
    (error "Even number of parameters needed.~%"))
  (flet ((unlispify-symbols (list)
	   (loop for item in list
                 collecting (if (symbolp item)
                                (substitute #\_ #\- (format nil "~(~A~)" item))
                                (format nil "~A" item)))))
    (invoke-method
     (bus-connection bus)
     "AddMatch"
     :destination "org.freedesktop.DBus"
     :path "/org/freedesktop/DBus"
     :interface "org.freedesktop.DBus"
     :signature "s"
     :arguments
     (list (format nil "~{~(~A~)=~A~^,~}" (unlispify-symbols parameters))))))

(defun request-name (bus name &rest flags)
  "Asks DBus to assign a name to the bus.  Valid flags
are :allow-replacement, :replace-existing, and :do-not-queue."
  (let ((flags-value
	 (reduce #'logior
		 (mapcar (lambda (flag)
			   (case flag
			     (:allow-replacement 1)
			     (:replace-existing  2)
			     (:do-not-queue      4)
			     (t (error "Invalid flag ~A.~%" flag))))
			 flags))))
    (case (invoke-method (bus-connection bus)
                         "RequestName"
                         :destination "org.freedesktop.DBus"
                         :path "/org/freedesktop/DBus"
                         :interface "org.freedesktop.DBus"
                         :signature "su"
                         :arguments (list name flags-value))
      (1 :primary-owner)
      (2 :in-queue)
      (3 :exists)
      (4 :already-owner)
      (t (error "Unknown response received.~%")))))

(defun list-names (bus)
  "Returns a list of all currently-owned names on the bus via
ListNames method invocation."
  (invoke-method (bus-connection bus)
                 "ListNames"
                 :destination "org.freedesktop.DBus"
                 :path "/"
                 :interface "org.freedesktop.DBus"
                 :signature ""))

(defun get-machine-id (bus)
  "Gets the Machine UUID of the machine hosting the object."
  (invoke-method (bus-connection bus)
                 "GetMachineId"
                 :interface "org.freedesktop.DBus.Peer"
                 :path "/"))
