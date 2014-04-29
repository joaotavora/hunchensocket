;;;; Hunchensocket - websocket.lisp
;;;;
;;;; Hunchentoot-based WebSocket (draft) implementation Copyright (C)
;;;; 2011 Alexander Kahl <e-user@fsfe.org>
;;;; 2014 Joao Tavora
;;;;
;;;; This file is part of Hunchensocket.  Redistribution and use in
;;;; source and binary forms, with or without modification, are
;;;; permitted provided that the following conditions are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

(defparameter *websocket-dispatchers* nil
  "List of handler closures that will be queried for new connections")

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

(defclass websocket-acceptor (acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 60
                      :documentation "Custom WebSocket timeout override."))
  (:default-initargs :request-class 'websocket-request
                     :reply-class 'websocket-reply))

(defclass websocket-ssl-acceptor (websocket-acceptor ssl-acceptor) ()
  (:documentation "Special WebSocket SSL acceptor"))

(define-condition websocket-illegal-frame-type (condition)
  ((type :initarg :type :reader websocket-illegal-frame-type-of
         :initform (required-argument :type)
         :documentation "Spurious frame type received"))
  (:documentation "Signal if client sends spurious frame type"))

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

(defun websocket-send-term (&optional
                              (stream *websocket-stream*)
                              (mutex *websocket-stream-mutex*))
  "Send the magic WebSocket termination sequence across STREAM."
  (with-lock-held (mutex)
    (write-sequence +websocket-terminator+ stream)
    (force-output stream)))

(defun websocket-send-message (message &optional
                                         (stream *websocket-stream*)
                                         (mutex *websocket-stream-mutex*))
  "Encode MESSAGE as UTF-8 bytes and send it across STREAM in a proper frame."
  (when (> (length message) 0) ; empty message would send terminator
    (log-message* :debug "Going to send websocket message ~a" message)
    (with-lock-held (mutex)
      (write-byte #x00 stream)
      (write-utf-8-bytes message stream)
      (write-byte #xff stream)
      (force-output stream))))

(defun skip-bytes (stream number)
  "Read and discard NUMBER bytes from STREAM."
  (dotimes (num number)
    (read-byte stream)))

(defun websocket-loop (stream message-handler
                       &optional (version :rfc-6455))
  "Implements the main WebSocket loop for supported protocol
versions. Framing is handled automatically, MESSAGE-HANDLER ought to
handle the actual payloads.

*Not really a REPL because it doesn't print implicitly but it does
what you'd expect from the name."
  (ecase version
    ((:draft-hixie-76 :draft-hybi-00)
     (loop for type = (read-byte stream)
           do (cond
                ((= #x00 type)
                 (loop with reader = (make-in-memory-output-stream)
                       for byte = (read-byte stream)
                       until (= byte #xff)
                       do (write-byte byte reader)
                       finally (return
                                 (funcall message-handler
                                          (utf-8-bytes-to-string
                                           (get-output-stream-sequence
                                            reader))))))
                ((= #xff type)
                 (let ((data (read-byte stream)))
                   (if (= #x00 data)
                       (return) ; regular termination
                       (do* ((data data (read-byte stream))
                             (length (logand #x7f data)
                                     (+ (* 128 length) (logand #x7f data))))
                            ((= #x80 (logand #x80 data))
                             (skip-bytes stream length))))))
                (t (error 'websocket-illegal-frame-type ; irregular termination
                          :type type)))))
    (:rfc-6455
     (hunchentoot-error "Not implemented yet!"))))

(defmethod process-connection :around ((*acceptor* websocket-acceptor)
                                       (socket t))
  "Continue the process with *WEBSOCKET-SOCKET* bound to the original
TCP socket and *HUNCHENTOOT-VERSION* enhanced by the Hunchensocket
version."
  (let ((*websocket-socket* socket))
    (call-next-method)))

(defun find-websocket-resource (request)
  "Compute a suitable handler for REQUEST.

A handler is a function of two arguments STREAM and MUTEX. STREAM can
be read and written to and MUTEX shouldn't be there."
  (some #'(lambda (handler)
            (funcall handler request))
        *websocket-dispatchers*))


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

(defmethod process-request :around ((request websocket-request))
  "Process REQUEST as HTTP, then hijack connection if WebSocket."
  (let ((stream (call-next-method)))
    (prog1 stream
      (when (= +http-switching-protocols+ (return-code*))
        (force-output stream)
        (let ((timeout (websocket-timeout (request-acceptor request)))
              (*websocket-stream* stream)
              (*websocket-stream-mutex* (make-lock)))
          (set-timeouts *websocket-socket* timeout timeout)
          (catch 'websocket-disconnect
            (handler-bind ((error #'(lambda (e)
                                      (maybe-invoke-debugger e)
                                      (throw 'websocket-disconnect nil))))
              (websocket-loop stream (websocket-resource request)))))))))

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

(defmethod acceptor-dispatch-request ((acceptor websocket-acceptor)
                                      (request websocket-request))
  (cond ((and (search "upgrade" (string-downcase (header-in* :connection)))
              (string= "websocket" (string-downcase (header-in* :upgrade)))
              (setf (websocket-resource *request*)
                    (find-websocket-resource *request*)))
         (websocket-handle-handshake *request* *reply*)
         (values nil nil nil) ;; as per `handle-request''s contract.
         )
        (t
         (call-next-method))))

;; Local Variables:
;; coding: utf-8-unix
;; End:
