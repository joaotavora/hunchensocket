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



;;; Binary reading machinery
;;;

(defun read-bytes-array (stream number)
  "Read NUMBER bytes from Chunga STREAM into array and return it."
  (let ((result (make-array number :element-type '(unsigned-byte 8))))
    (dotimes (index number result)
      (setf (aref result index)
            (char-int (read-char* stream t))))))

(defun read-key3 (request)
  "Read eight bytes from REQUEST's content stream and return them as a byte
array."
  (read-bytes-array (content-stream request) 8))

(defun digest-key (key)
  "Evaluates to MD5 digest of KEY sequence."
  (digest-sequence :md5 key))

(defun websocket-uri (request host &optional ssl)
  "Form WebSocket URL (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))

(defun skip-bytes (stream number)
  "Read and discard NUMBER bytes from STREAM."
  (dotimes (num number)
    (read-byte stream)))

(defun send-message (message client)
  "Encode MESSAGE as UTF-8 bytes and send it across STREAM in a proper frame."
  (with-slots (stream lock) client
    (when (> (length message) 0) ; empty message would send terminator
      (log-message* :debug "Going to send websocket message ~a" message)
      (with-lock-held (lock)
        (write-byte #x00 stream)
        (write-utf-8-bytes message stream)
        (write-byte #xff stream)
        (force-output stream)))))


;;; Main websocket loop
;;; 
(defun websocket-loop (stream client
                       &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, CLIENT handles the actual
payloads."
  (declare (ignore stream client))
  (ecase version
    (:rfc-6455
     (hunchentoot-error "Not implemented yet!"))))


;;; Hook onto normal hunchentoot processing
;;;
;;; The `:around' specilization of `process-request' will first call
;;; the main hunchentoot one, which eventually call our specialization
;;; of `acceptor-dispatch-request'. That will in turn try to figure
;;; out if the client is requesting websockets, handshake and set
;;; `+http-switching-protocols+'. Hunchentoot's `process-request' will
;;; also eventually reply to the client but in the stream is kept
;;; alive. That happens if:
;;;
;;; 1. There are suitable "Connection" and "Upgrade" headers and
;;;    `websocket-resource' object is found for request.
;;; 2. The websocket handshake completes sucessfully
;;; 
;;; If 1 fails, normal hunchentoot processing is resumed. If 2 or 3
;;; fail, an error is signalled.

(defmethod process-connection :around ((*acceptor* websocket-acceptor)
                                       (socket t))
  "Sprinkle the current connection with dynamic bindings."
  (let ((*websocket-socket* socket))
    (call-next-method)))

(defmethod process-request :around ((request websocket-request))
  "First process REQUEST as HTTP, maybe hijack into WebSocket loop."
  (call-next-method)
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
              (websocket-loop stream client))))))))

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
