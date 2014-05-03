(in-package :hunchensocket)

(define-constant +websocket-magic-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(define-constant +continuation-frame+    #x0)
(define-constant +text-frame+            #x1)
(define-constant +binary-frame+          #x2)
(define-constant +connection-close+      #x8)
(define-constant +ping+                  #x9)
(define-constant +pong+                  #xA)

(defvar *websocket-socket* nil
  "The currently active WebSocket socket")


;;; API
(defvar *websocket-dispatch-table* nil
  "List of handler closures that will be queried for new connections")

(defclass websocket-acceptor (acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 60
                      :documentation "Custom WebSocket timeout override."))
  (:default-initargs :request-class 'websocket-request
                     :reply-class 'websocket-reply))

(defclass websocket-ssl-acceptor (websocket-acceptor ssl-acceptor) ()
  (:documentation "Special WebSocket SSL acceptor"))

(defclass websocket-client ()
  ((stream     :initarg stream
               :initform (error "Must make clients with streams"))
   (request    :initarg request
               :reader client-request
               :initform (error "Must make clients with requests"))
   (write-lock :initform (make-lock))))

(defmethod initialize-instance :after ((client websocket-client)
                                       &key &allow-other-keys)
  "Allows CLIENT to be passed more keywords on MAKE-INSTANCE.")

(defclass websocket-resource ()
  ((clients :initform nil :reader clients)
   (client-class :initarg :client-class :initform 'websocket-client)
   (lock :initform (make-lock))))

(defgeneric text-message-received (resource client message))

(defgeneric binary-message-received  (resource client binary))

(defgeneric client-connected (resource client)
  (:method (resource client)
    (declare (ignore resource client))))

(defgeneric client-disconnected (resource client)
  (:method (resource client)
    (declare (ignore resource client))))

(defgeneric check-message (resource client opcode length)
  (:method (resource client (opcode (eql +text-frame+)) length)
    (declare (ignore resource client))
    (when (> length #xffff)
      (websocket-error 1009 "Message too big")))
  (:method (resource client (opcode (eql +text-frame+)) length)
    (websocket-error 1003 "Binaries not accepted")))

(defun send-text-message (client message)
  "MESSAGE is a string"
  (with-slots (write-lock stream) client
    (with-lock-held (write-lock)
      (write-frame stream +text-frame+
                   (flexi-streams:string-to-octets message
                                                   :external-format :utf-8)))))


;;; Request/reply Hunchentoot overrides
;;;
(defclass websocket-request (request)
  ((handler :accessor websocket-resource
            :initform nil
            :documentation "Message handler of the current request")))

(defclass websocket-reply (reply) ())

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs)
  "Set the reply's external format to Unix EOL / UTF-8 explicitly."
  (declare (ignore initargs))
  (setf (reply-external-format reply)
        (make-external-format :utf8 :eol-style :lf)))


;;; Conditions

(define-condition websocket-error (simple-error)
  ((error-status :initarg :status))
  (:documentation "Superclass for all errors related to Websocket."))

(defun websocket-error (status format-control &rest format-arguments)
  "Signals an error of type HUNCHENTOOT-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'websocket-error
         :status status
         :format-control format-control
         :format-arguments format-arguments))


;;; Client and resource machinery
;;;
(defmethod initialize-instance :after ((resource websocket-resource)
                                       &key client-class)
  (assert (subtypep client-class 'websocket-client)))

(defun call-with-new-client-for-resource (client resource fn)
  (with-slots (clients lock) resource
    (unwind-protect
         (progn
           (bt:with-lock-held (lock)
             (push client clients))
           (client-connected resource client)
           (funcall fn))
      (client-disconnected resource client)
      (bt:with-lock-held (lock)
        (setq clients (remove client clients))))))

(defmacro with-new-client-for-resource ((client-sym) (resource stream request)
                                        &body body)
  (alexandria:once-only (resource request)
    `(let ((,client-sym (apply #'make-instance
                               (slot-value ,resource 'client-class)
                               'stream ,stream
                               'request ,request
                               :request ,request
                               (loop for (header . value)
                                       in (headers-in ,request)
                                     collect header collect value))))
       (call-with-new-client-for-resource ,client-sym
                                          ,resource
                                          #'(lambda () ,@body)))))

(defun websocket-uri (request host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))


;;; Binary reading/writing machinery
;;;
(defun read-unsigned-big-endian (stream n)
  "Read N bytes from stream and return the big-endian number"
  (loop repeat n for i from 0
        sum (* (read-byte stream) (expt 256 i))))

(defun read-n-bytes-into-sequence (stream n)
  "Return an array of N bytes read from stream"
  (let* ((array (make-array n :element-type '(unsigned-byte 8)))
         (read (read-sequence array stream)))
    (assert (= read n) nil
            "Expected to read ~a bytes, but read ~a" n read)
    array))

(defclass frame ()
  ((opcode          :initarg :opcode :accessor frame-opcode)
   (data                             :accessor frame-data)
   (finp            :initarg :finp)
   (payload-length  :initarg :payload-length :accessor frame-payload-length)
   (masking-key     :initarg :masking-key)))

(defun read-frame (stream)
  "Read a text or binary message from STREAM."
  (let* ((first-byte       (read-byte stream))
         (fin              (ldb (byte 1 7) first-byte))
         (extensions       (ldb (byte 3 4) first-byte))
         (opcode           (ldb (byte 4 0) first-byte))
         (second-byte      (read-byte stream))
         (mask-p           (plusp (ldb(byte 1 7) second-byte)))
         (payload-length   (ldb (byte 7 0) second-byte))
         (payload-length   (cond ((<= 0 payload-length 125)
                                  payload-length)
                                 (t
                                  (read-unsigned-big-endian
                                   stream (case payload-length
                                            (126 2)
                                            (127 8))))))
         (masking-key      (if mask-p (read-n-bytes-into-sequence stream 4)))
         (extension-data   nil))
    (declare (ignore extension-data))
    (when (plusp extensions)
      (websocket-error 1002
                       "No extensions negotiated, but client sends ~a!"  extensions))
    (make-instance 'frame :opcode opcode
                          :finp (plusp fin) 
                          :masking-key masking-key
                          :payload-length payload-length)))

(defun read-application-data (stream len masking-key)
  (let ((application-data (read-n-bytes-into-sequence stream len)))
  (when masking-key
      ;; RFC6455 Masking
      ;;
      ;; Octet i of the transformed data
      ;; ("transformed-octet-i") is the XOR of octet i
      ;; of the original data ("original-octet-i")
      ;; with octet at index i modulo 4 of the masking
      ;; key ("masking-key-octet-j"):
      (loop for i from 0 below len
            do (setf (aref application-data i)
                     (logxor (aref application-data i)
                             (aref masking-key
                                   (mod i 4))))))
    application-data))

(defun write-frame (stream opcode &optional data)
  (let* ((first-byte     #x00)
         (second-byte    #x00)
         (len            (if data (length data) 0))
         (payload-length (cond ((< len 125)         len)
                               ((< len (expt 2 16)) 126)
                               (t                   127)))
         (mask-p         nil))
    (setf (ldb (byte 1 7) first-byte)  1
          (ldb (byte 3 4) first-byte)  0
          (ldb (byte 4 0) first-byte)  opcode
          (ldb (byte 1 7) second-byte) (if mask-p 1 0)
          (ldb (byte 7 0) second-byte) payload-length)
    (write-byte first-byte stream)
    (write-byte second-byte stream)
    (loop repeat (cond ((= payload-length 126) 2)
                       ((= payload-length 127) 8)
                       (t                      0))
          for out = len then (ash out -8)
          do (write-byte (logand out #xff) stream))
    ;; (if mask-p
    ;;     (error "sending masked messages not implemented yet"))
    (if data (write-sequence data stream))
    (force-output stream)))

(defun send-control (client opcode data)
  (with-slots (write-lock stream) client
    (with-lock-held (write-lock)
      (write-frame stream opcode data))))

(defun close-connection (client &key data status reason)
  (send-control client
                +connection-close+
                (or data
                    (concatenate 'vector
                                 (coerce (list (logand (ash status -8) #xff)
                                               (logand status #xff))
                                         'vector)
                                 (flexi-streams:string-to-octets
                                  reason
                                  :external-format :utf-8)))))


;;; State machine and main websocket loop
;;;
(defun handle-frame (resource client state pending-fragments frame)
  (with-slots (opcode finp payload-length masking-key data) frame
    (labels ((control-frame-p ()
               (plusp (logand #x8 opcode)))
             (maybe-accept-frame ()
               (unless (control-frame-p)
                 (check-message resource client opcode
                                (+ payload-length
                                   (reduce #'+ (mapcar
                                                #'frame-payload-length
                                                pending-fragments)))))
               (setq data
                     (read-application-data (slot-value client 'stream)
                                            payload-length masking-key))))
      (cond
        ((eq :awaiting-close state)
         (unless (eq opcode +connection-close+)
           (websocket-error
            1002 "Expected connection close from client, got 0x~x" opcode))
         (setq state :closed))
        ((not finp)
         ;; this is a non-FIN fragment and there are pending fragments. Check
         ;; opcode, append to client's fragments.
         (unless (or (not pending-fragments)
                     (= opcode +continuation-frame+))
           (websocket-error
            1002 "Fragments fragments must have opcode 0x~x"
            +continuation-frame+))
         (maybe-accept-frame)
         (push frame pending-fragments))
        ((and pending-fragments
              (not (or (control-frame-p)
                       (= opcode +continuation-frame+))))
         ;; this is a FIN fragment and (1) there are pending fragments and (2)
         ;; this isn't a control or continuation frame. Error out.
         (websocket-error
          1002 "Only control frames can interleave fragment sequences."))
        (t
         ;; This is a final, FIN fragment. If there are pending fragments and
         ;; this is a continuation frame, join the fragments and keep on
         ;; processing. Join any outstanding fragments and process the message.
         (maybe-accept-frame)
         (when (and pending-fragments
                    (eq opcode +continuation-frame+))
           (let ((in-order (reverse
                            (cons frame
                                  pending-fragments))))
             (setq data (apply #'concatenate 'array
                               data
                               (mapcar #'frame-data
                                       (cdr in-order))))
             (setf pending-fragments nil)
             (setf opcode (frame-opcode (first in-order)))))
         (cond ((eq +text-frame+ opcode)
                (text-message-received resource
                                       client
                                       (flexi-streams:octets-to-string
                                        data :external-format :utf-8)))
               ((eq +binary-frame+ opcode)
                (binary-message-received resource client data))
               ((eq +ping+ opcode)
                (send-control client +pong+ data))
               ((eq +connection-close+ opcode)
                (close-connection client :data data)
                (setq state :closed))
               ((eq +pong+ opcode)
                ;; probably just a heartbeat, don't do anything.
                )
               (t
                (websocket-error
                 1002 "Client sent unknown opcode ~a" opcode)))))))
  (list state pending-fragments))


(defun read-handle-loop (resource stream client
                         &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, CLIENT handles the actual
payloads."
  (ecase version
    (:rfc-6455
     (handler-bind ((websocket-error
                      #'(lambda (e)
                          (with-slots (status format-control format-arguments) e
                            (close-connection
                             client
                             :status status
                             :reason (princ-to-string e)))))
                    (flexi-streams:external-format-error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1007
                                            :reason "Bad UTF-8")))
                    (error
                      #'(lambda (e)
                          (declare (ignore e))
                          (close-connection client :status 1011
                                            :reason "Unexpected condition"))))
       (loop for (state pending-fragments)
               = (handle-frame
                  resource
                  client
                  state
                  pending-fragments
                  (read-frame stream))
             while (not (eq :closed state)))))))


;;; Hook onto normal Hunchentoot processing
;;;
;;; The `:after' specilization of `process-request' will happen after
;;; the main Hunchentoot one. It is hunchentoot which eventually calls
;;; our specialization of `acceptor-dispatch-request', who will, in
;;; turn, try to figure out if the client is requesting
;;; websockets. Hunchentoot's `process-request' will also eventually
;;; reply to the client. In the `:after' specialization we might enter
;;; into `read-handle-loop' and thus keep the socket alive. That happens
;;; if:
;;;
;;; 1. There are suitable "Connection" and "Upgrade" headers and
;;;    `websocket-resource' object is found for request.
;;;
;;; 2. The websocket handshake completes sucessfully, whereby the
;;;    callees of `acceptor-dispatch-request' will have set
;;;    `+http-switching-protocols+' accordingly.
;;;
;;; If any of these steps fail, errors might be signalled, but normal
;;; hunchentoot processing of the HTTP request still happens.

(defmethod process-connection :around ((*acceptor* websocket-acceptor)
                                       (socket t))
  "Sprinkle the current connection with dynamic bindings."
  (let ((*websocket-socket* socket))
    (call-next-method)))

(defmethod process-request :after ((request websocket-request))
  "After HTTP processing REQUEST, maybe hijack into WebSocket loop."
  (let ((stream (content-stream request)))
    (when (= +http-switching-protocols+ (return-code*))
      (force-output stream)
      (let* ((timeout (websocket-timeout (request-acceptor request)))
             (resource (websocket-resource request)))
        (with-new-client-for-resource (client) (resource stream request)
          (set-timeouts *websocket-socket* timeout timeout)
          (catch 'websocket-done
            (handler-bind ((error #'(lambda (e)
                                      (maybe-invoke-debugger e)
                                      (log-message* :error "Error: ~a" e)
                                      (throw 'websocket-done nil))))
              (read-handle-loop resource stream client))))))))

(defmethod handle-handshake ((acceptor websocket-acceptor) request reply)
  "Analyse REQUEST for WebSocket handshake.

Destructively modify REPLY accordingly in case of success, exit
non-locally with an error instead."
  ;; Implements 4.2.2.  Sending the Server's Opening Handshake
  (let ((requested-version (header-in* :sec-websocket-version request)))
    (cond ((not (equal "13" requested-version))
           (websocket-error 1002
            "Unsupported websocket version ~a" requested-version))
          ((header-in :sec-websocket-draft request)
           (websocket-error 1002
            "Websocket draft is unsupported"))
          ((header-in :sec-websocket-key request)
           (let ((sec-websocket-key+magic
                   (concatenate 'string (header-in :sec-websocket-key request)
                                "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
             (setf (header-out :sec-websocket-accept reply)
                   (base64:usb8-array-to-base64-string
                    (ironclad:digest-sequence
                     'ironclad:sha1
                     (ironclad:ascii-string-to-byte-array
                      sec-websocket-key+magic))))
             (setf (header-out :sec-websocket-origin reply)
                   (header-in :origin request))
             (setf (header-out :sec-websocket-location reply)
                   (or (websocket-uri request (header-in :host request)
                                      (ssl-p (request-acceptor request)))))
             (setf (header-out :sec-websocket-protocol reply)
                   (first (split "\\s*,\\s*" (header-in :sec-websocket-protocol
                                                        request))))
             ;; A (possibly empty) list representing the
             ;; protocol-level extensions the server is ready to use.
             ;;
             (setf (header-out :sec-websocket-extensions reply) nil)
             (setf (return-code* reply) +http-switching-protocols+
                   (header-out :upgrade reply) "WebSocket"
                   (header-out :connection reply) "Upgrade"
                   (content-type* reply) "application/octet-stream")
             ;; HACK! but a decent one I think. Notice that we set both
             ;; in and out "Connection" headers to "Upgrade". The out
             ;; header is per RFC, but the in-header is for a different
             ;; reason. If the client additionally send "Keep-Alive" in
             ;; that header, hunchentoot will eventually take it as a
             ;; reason to clobber our out-header value of "Upgrade" with
             ;; "Keep-Alive <timeout>".
             (setf (cdr (find :connection (headers-in request) :key #'car))
                   "Upgrade")))
          (t (websocket-error 1002 "Unsupported unknown websocket version")))))

(defun find-websocket-resource (request)
  "Find the resource for REQUEST by looking up *WEBSOCKET-DISPATCH-TABLE*."
  (some #'(lambda (dispatcher)
            (funcall dispatcher request))
        *websocket-dispatch-table*))

(defmethod acceptor-dispatch-request ((acceptor websocket-acceptor)
                                      (request websocket-request))
  "Attempt WebSocket connection, else fall back to HTTP"
  (cond ((and (member "upgrade" (split "\\s*,\\s*" (header-in* :connection))
                      :test #'string-equal)
              (string= "websocket" (string-downcase (header-in* :upgrade))))
         (cond ((setf (websocket-resource *request*)
                      (find-websocket-resource *request*))
                ;; Found the websocket resource
                (handle-handshake acceptor *request* *reply*)
                ;; HACK! the empty string is also important because if there's
                ;; no content Hunchentoot will declare the connection closed and
                ;; set "Connection: Closed". But there can't be any actual
                ;; content since otherwise it will piggyback onto the first
                ;; websocket frame, which is interpreted as invalid by the
                ;; client. It's also forbidden by the HTTP RFC2616:
                ;;
                ;;    [...] All 1xx (informational), 204 (no content), and 304
                ;;    (not modified) responses MUST NOT include a
                ;;    message-body. [...]
                ;;
                ;; There is a slight non-conformance here: this trick makes
                ;; Hunchentoot send "Content-length: 0". Most browsers don't
                ;; seem to care, but RFC2616 kind of implies that is forbidden,
                ;; since it says the
                ;;
                ;;    [...] the presence of a message-body is signaled by the
                ;;    inclusion of a Content-Length or Transfer-Encoding header
                ;;    field in the requests's message-headers [...]
                ;;
                ;; Note however, we're sending a response, not a request.
                ;;
                (values "" nil nil))
               (t
                ;; Didn't find the websocket-specific resource, return 404.
                (setf (return-code *reply*) +http-not-found+)
                (values nil nil nil))))
        (t
         ;; Client is not requesting websockets, let Hunchentoot do its HTTP
         ;; thing undisturbed.
         (call-next-method))))


;; Local Variables:
;; coding: utf-8-unix
;; End:
