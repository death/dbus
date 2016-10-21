;;;; +----------------------------------------------------------------+
;;;; | DBUS                                                           |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus/auth-dbus-cookie-sha1
  (:use #:cl #:dbus/utils #:dbus/protocols #:dbus/authentication-mechanisms)
  (:import-from #:split-sequence #:split-sequence)
  (:import-from #:babel #:string-to-octets)
  (:import-from #:ironclad #:digest-sequence)
  (:export
   #:dbus-cookie-sha1-authentication-mechanism))

(in-package #:dbus/auth-dbus-cookie-sha1)


;;;; DBUS Cookie SHA1 authentication mechanism

(defclass dbus-cookie-sha1-authentication-mechanism (standard-authentication-mechanism)
  ()
  (:default-initargs :textual t)
  (:documentation "Authenticate using a local cookie and SHA1."))

(setf (find-authentication-mechanism-class "DBUS_COOKIE_SHA1")
      'dbus-cookie-sha1-authentication-mechanism)

(defvar *keyrings-directory*
  (merge-pathnames (make-pathname :directory '(:relative ".dbus-keyrings"))
                   (user-homedir-pathname))
  "The directory holding context files containing cookies.")

(defun find-cookie (context-name cookie-id &key (if-does-not-exist :error))
  "Find the cookie corresponding to COOKIE-ID in the appropriate
context file."
  (with-open-file (in (make-pathname :name context-name
                                     :defaults *keyrings-directory*)
                      :direction :input)
    (loop for line = (read-line in nil nil)
          while line do
          (destructuring-bind (id ctime cookie)
              (split-sequence #\Space line)
            (declare (ignore ctime))
            (when (equal id cookie-id)
              (return-from find-cookie cookie)))))
  (inexistent-entry (list context-name cookie-id) if-does-not-exist))

(defun random-challenge-string (&optional (num-octets 16))
  "Return a string containing a hex-encoded representation of a number
of random octet values."
  (with-output-to-string (out)
    (loop repeat (* 2 num-octets) do
          (write-char (char-downcase (digit-char (random 16) 16)) out))))

(defmethod feed-authentication-mechanism ((mechanism dbus-cookie-sha1-authentication-mechanism) challenge)
  (if (eq challenge :initial-response)
      (values :continue (encode-hex-string (current-username)))
      (destructuring-bind (context-name cookie-id challenge-string)
          (split-sequence #\Space challenge)
        (let* ((my-challenge-string (random-challenge-string))
               (cookie (find-cookie context-name cookie-id))
               (message (format nil "~A:~A:~A" challenge-string my-challenge-string cookie))
               (digest (digest-sequence :sha1 (string-to-octets message :encoding :utf-8))))
          (values :ok (format nil "~A ~A" my-challenge-string (encode-hex-string digest)))))))
