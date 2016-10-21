;;;; +----------------------------------------------------------------+
;;;; | DBUS                                          DEATH, 2010-2011 |
;;;; +----------------------------------------------------------------+

(defpackage #:dbus
  (:use #:cl)
  (:import-from #:split-sequence #:split-sequence)
  (:import-from #:alexandria
                #:circular-list #:ensure-list #:plist-hash-table
                #:starts-with-subseq #:when-let #:with-gensyms
                #:once-only #:hash-table-values)
  (:import-from #:xspam
                #:with-xspam-source #:make-xspam-source #:element
                #:one-or-more #:attribute #:_ #:zero-or-more
                #:optional-attribute #:one-of)
  (:import-from #:babel #:string-to-octets #:octets-to-string)
  (:import-from #:ironclad #:digest-sequence)
  (:import-from #:iolib
                #:event-dispatch #:set-io-handler #:make-socket
                #:connect #:fd-of #:with-event-base
                #:ensure-address)
  (:import-from #:iolib.syscalls
                #:getenv #:getpwuid #:getuid)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream
                #:with-output-to-sequence)
  (:import-from #:trivial-garbage #:make-weak-hash-table)
  (:import-from #:ieee-floats #:encode-float64 #:decode-float64)
  (:shadow #:method #:make-method #:signal)
  (:export
   ;; Utilities
   #:inexistent-entry
   #:inexistent-entry-designator
   #:entry-replacement-attempt
   #:entry-replacement-attempt-old
   #:entry-replacement-attempt-new
   ;; Server address protocol
   #:server-address
   #:server-address-transport-name
   #:server-address-property
   #:open-connection
   ;; Connection protocol
   #:connection
   #:connection-server-address
   #:connection-server-uuid
   #:connection-fd
   #:connection-pending-messages
   #:connection-next-serial
   #:drain-pending-messages
   #:close-connection
   #:wait-for-reply
   #:receive-message
   #:receive-line
   #:send-message
   #:send-line
   #:supported-authentication-mechanisms
   #:authenticate
   #:supports-unix-fd-passing-p
   ;; Authentication mechanism protocol
   #:authentication-mechanism
   #:authentication-mechanism-name
   #:authentication-mechanism-textual-p
   #:feed-authentication-mechanism
   ;; Conditions
   #:dbus-error
   #:authentication-error
   #:authentication-error-command
   #:authentication-error-argument
   #:method-error
   #:method-error-arguments
   ;; Type-related operators
   #:sigexp
   #:signature
   #:pack
   #:unpack
   #:valid-body-p
   ;; Messages
   #:encode-message
   #:message
   #:standard-message
   #:message-endianness
   #:message-flags
   #:message-major-protocol-version
   #:message-body-length
   #:message-serial
   #:message-destination
   #:message-sender
   #:message-signature
   #:message-body
   #:method-call-message
   #:message-path
   #:message-interface
   #:message-member
   #:signal-message
   #:method-return-message
   #:message-reply-serial
   #:error-message
   #:message-error-name
   #:message-no-reply-expected
   #:message-no-auto-start
   #:decode-message
   #:invoke-method
   ;; Server addresses
   #:find-server-address-class
   #:standard-server-address
   #:generic-server-address
   #:parse-server-addresses-string
   #:session-server-addresses
   #:system-server-addresses
   ;; Authentication mechanisms
   #:find-authentication-mechanism-class
   #:standard-authentication-mechanism
   #:generic-authentication-mechanism
   #:dbus-cookie-sha1-authentication-mechanism
   #:dbus-external-authentication-mechanism
   ;; Connections
   #:with-open-connection
   #:standard-connection
   #:socket-connection-mixin
   #:open-socket-connection
   ;; Introspection
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
   #:object-invoke
   ;; Convenience
   #:hello
   #:bus
   #:bus-connection
   #:bus-name
   #:with-open-bus
   #:with-introspected-object
   ;; Standard interfaces
   #:get-machine-id
   #:get-property
   #:get-all-properties
   #:get-managed-objects
   #:add-match
   #:request-name
   #:list-names
   ;; Unix Domain Sockets transport
   #:unix-server-address
   #:unix-connection))
