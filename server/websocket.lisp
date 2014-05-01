(in-package :hunchensocket)

(define-constant +websocket-terminator+
    '(#x00 #xff)
  :test #'equal
  :documentation "Fixed WebSocket terminator value")
(define-constant +websocket-magic-key+
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  :test #'string=
  :documentation "Fixed magic WebSocket UUIDv4 key use in handshakes")

(defvar *websocket-stream* nil
  "The currently active WebSocket stream")
(defvar *websocket-stream-mutex* nil
  "Mutex lock for the currently active WebSocket stream")
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
  ((stream     :initarg :stream
               :initform (error "Must make clients with streas"))
   (request    :initarg :request
               :reader client-request
               :initform (error "Must make clients with requests"))
   (write-lock :initform (make-lock))))

(defclass websocket-resource ()
  ((clients :initform nil :reader clients)
   (client-class :initarg :client-class :initform 'websocket-client)
   (lock :initform (make-lock))))

(defgeneric message-received (resource message client))
(defgeneric binary-received (resource binary client))


;;; Requests, replies and websocket-specific conditions
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

(define-condition websocket-illegal-frame-type (condition)
  ((type :initarg :type :reader websocket-illegal-frame-type-of
         :initform (required-argument :type)
         :documentation "Spurious frame type received"))
  (:documentation "Signal if client sends spurious frame type"))


;;; client and resource machinery
;;;
(defmethod initialize-instance :after ((resource websocket-resource) &key client-class)
  (assert (subtypep client-class 'websocket-client)))

(defun call-with-new-client-for-resource (client resource fn)
  (with-slots (clients lock) resource
    (unwind-protect
           (progn
             (bt:with-lock-held (lock)
               (push client clients))
             (funcall fn))
        (bt:with-lock-held (lock)
          (setq clients (remove client clients))))))

(defmacro with-new-client-for-resource ((client-sym) (resource stream request)
                                        &body body)
  (alexandria:once-only (resource)
    `(let ((,client-sym (make-instance (slot-value ,resource 'client-class)
                                       :stream ,stream
                                       :request ,request)))
       (call-with-new-client-for-resource ,client-sym
                                          ,resource
                                          #'(lambda () ,@body)))))

(defun websocket-uri (request host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))


;;; Binary reading/writing machinery
;;;
(defconstant +continuation-frame+    #x0)
(defconstant +text-frame+            #x1)
(defconstant +binary-frame+          #x2)
;; (defconstant +non-control-frames+   '(#x3 . #x7))
(defconstant +connection-close+      #x8)
(defconstant +ping+                  #x9)
(defconstant +pong+                  #xA)
;; (defconstant +control-frames+       '(#xB . #xF))

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

(defun read-frame (stream)
  "Read a text or binary message from STREAM.

Will exit non-locally by throwing `websocket-done' if client wants to
disconnect."
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
         (extension-data   nil)
         (application-data (read-n-bytes-into-sequence stream payload-length)))
    (declare (ignore extension-data fin))
    (when (plusp extensions) (error "No extensions negotiated, but
                                  client sends ~a!"  extensions))
    (when masking-key
      ;; RFC6455 Masking
      ;;
      ;; Octet i of the transformed data
      ;; ("transformed-octet-i") is the XOR of octet i
      ;; of the original data ("original-octet-i")
      ;; with octet at index i modulo 4 of the masking
      ;; key ("masking-key-octet-j"):
      (loop for i from 0 below payload-length
            do (setf (aref application-data i)
                     (logxor (aref application-data i)
                             (aref masking-key
                                   (mod i 4))))))
    (list opcode application-data)))

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


;;; State machine and main websocket loop
;;; 
(defun handle-frame (opcode data stream resource client)
  (declare (ignore resource client))
  (cond ((= opcode +text-frame+)
         (write-frame stream opcode data))))

(defun websocket-loop (resource stream client
                       &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, CLIENT handles the actual
payloads."
  (ecase version
    (:rfc-6455
     (handler-bind ((error #'(lambda (e)
                               (declare (ignore e))
                               (write-frame stream +connection-close+))))
       (loop for state = :open then (handle-frame opcode data stream resource client)
             while (not (eq :closed state))
             for (opcode data) = (read-frame stream))))))


;;; Hook onto normal hunchentoot processing
;;;
;;; The `:after' specilization of `process-request' will happen after
;;; the main Hunchentoot one. It is hunchentoot which eventually calls
;;; our specialization of `acceptor-dispatch-request', who will, in
;;; turn, try to figure out if the client is requesting
;;; websockets. Hunchentoot's `process-request' will also eventually
;;; reply to the client. In the `:after' specialization we might enter
;;; into `websocket-loop' and thus keep the socket alive. That happens
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
                                      (log-message* :error "Websocket error: ~a" e)
                                      (throw 'websocket-done nil))))
              (websocket-loop resource stream client))))))))

(defun websocket-handle-handshake (request reply)
  "Analyse REQUEST for WebSocket handshake.

Destructively modify REPLY accordingly in case of success, exit
non-locally with an error instead. "
  (let ((requested-version (header-in* :sec-websocket-version request)))
    (cond ((not (equal "13" requested-version))
           (hunchentoot-error
            "Unsupported websocket version ~a" requested-version))
          ((header-in :sec-websocket-draft request)
           (hunchentoot-error
            "Websocket draft is unsupported"))
          ((header-in :sec-websocket-key request)
           (let ((sec-websocket-key+magic
                   (concatenate 'string (header-in :sec-websocket-key request)
                                "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
             (setf (header-out :sec-websocket-accept reply)
                   (base64:usb8-array-to-base64-string
                    (ironclad:digest-sequence
                     'ironclad:sha1
                     (ironclad:ascii-string-to-byte-array sec-websocket-key+magic))))
             (setf (header-out :sec-websocket-origin reply)
                   (header-in :origin request))
             (setf (header-out :sec-websocket-location reply)
                   (or (websocket-uri request (header-in :host request)
                                      (ssl-p (request-acceptor request)))))
             (setf (header-out :sec-websocket-protocol reply)
                   (header-in :sec-websocket-protocol request))
             (setf (return-code* reply) +http-switching-protocols+
                   (header-out :upgrade reply) "WebSocket"
                   (header-out :connection reply) "Upgrade"
                   (content-type* reply) "application/octet-stream")))
          (t (hunchentoot-error "Unsupported unknown websocket version")))))

(defun find-websocket-resource (request)
  "Find the resource for REQUEST by looking up *WEBSOCKET-DISPATCH-TABLE*."
  (some #'(lambda (dispatcher)
            (funcall dispatcher request))
        *websocket-dispatch-table*))

(defmethod acceptor-dispatch-request ((acceptor websocket-acceptor)
                                      (request websocket-request))
  "Attempt WebSocket connection, else fall back to HTTP"
  (cond ((and (search "upgrade" (string-downcase (header-in* :connection)))
              (string= "websocket" (string-downcase (header-in* :upgrade)))
              (setf (websocket-resource *request*)
                    (find-websocket-resource *request*)))
         (websocket-handle-handshake *request* *reply*)
         (values nil nil nil) ;; as per `handle-request''s contract.
         )
        (t
         (call-next-method))))


;;; Chat example
;;;
(defclass websocket-chat-resource (websocket-resource)
  ())

(defmethod message-received ((resource websocket-chat-resource) message client)
  (loop for client in (remove client (clients resource))
        do (send-message client (format nil "~a, meow" message))))



;; Local Variables:
;; coding: utf-8-unix
;; End:
