;;;; Hunchensocket - websocket.lisp Hunchentoot-based WebSocket (draft) implementation
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Hunchensocket.
;;;; Hunchensocket is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; Hunchensocket is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :hunchensocket)

(define-constant +websocket-terminator+ '(#x00 #xff) :test #'equal
                 :documentation "Fixed WebSocket terminator value")
(define-constant +websocket-magic-key+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" :test #'string=
                 :documentation "Fixed magic WebSocket UUIDv4 key used for handshakes (see draft-ietf-hybi-thewebsocketprotocol-06 or later")

(defvar *websocket-stream* nil
  "The currently active WebSocket stream")
(defvar *websocket-stream-mutex* nil
  "Mutex lock for the currently active WebSocket stream")
(defvar *websocket-socket* nil
  "The currently active WebSocket socket")

(defparameter *websocket-handlers* nil
  "List of handler closures that will be queried for new connections")

(defclass websocket-request (request)
  ((handler :accessor websocket-request-handler
            :initform nil
            :documentation "Message handler of the current request"))
  (:documentation "Subclass of the regular Hunchentoot request"))

(defclass websocket-reply (reply) ()
  (:documentation "Subclass of the regular Hunchentoot reply (used only for dispatching)"))

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs &key &allow-other-keys)
  "initialize-instance :after websocket-reply &rest initargs &key &allow-other-keys => side effect

Set the reply's external format to Unix EOL / UTF-8 explicitly."
  (declare (ignore initargs))
  (setf (reply-external-format reply) (make-external-format :utf8 :eol-style :lf)))

(defclass websocket-acceptor (acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 60
                      :documentation "Custom WebSocket timeout overriding the default usocket one"))
  (:default-initargs :request-class 'websocket-request :reply-class 'websocket-reply)
  (:documentation "Special WebSocket acceptor to be used in place of the regular Hunchentoot one"))

(defclass websocket-ssl-acceptor (websocket-acceptor ssl-acceptor) ()
  (:documentation "Special WebSocket SSL acceptor"))

(define-condition websocket-unsupported-version (condition)
  ((version :initarg :version :reader websocket-unsupported-version-of
            :initform (required-argument :version)
            :documentation "Original version argument that is unsupported"))
  (:documentation "Signal for unsupported / unrecognized WebSocket versions"))

(define-condition websocket-illegal-key (condition)
  ((key :initarg :key :reader websocket-illegal-key-of
        :initform (required-argument :key)
        :documentation "Original, spurious key argument"))
  (:documentation "Signal in case the client has sent an erroneous handshake key"))

(define-condition websocket-illegal-frame-type (condition)
  ((type :initarg :type :reader websocket-illegal-frame-type-of
         :initform (required-argument :type)
         :documentation "Spurious frame type received"))
  (:documentation "Signal in case the client has sent a spurious frame type"))

(defun integer-octets-32be (number)
  "integer-octets-32be number => unsigned byte array

Transform NUMBER into 32bit big endian octets. Required for the (luckily
obsoleted) old \"hixie\" and early \"hybi\" WebSocket drafts."
  (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (index 4 result)
      (let ((position #+little-endian (- 24 (* 8 index)) #-little-endian (* 8 index)))
        (setf (aref result index)
              (ldb (byte 8 position) number))))))

(let ((digit-scanner (create-scanner "[^\\d]"))
      (space-scanner (create-scanner "[^ ]")))
  (defun websocket-keyhash (key)
    "websocket-keyhash key => unsigned byte array

Process a (legacy) WebSocket key and evaluate to a byte sequence that becomes
part of the challenge. An error will be signalled in case the key is spurious."
    (let ((number (parse-integer (regex-replace-all digit-scanner key "")
                                 :junk-allowed nil))
          (spaces (length (regex-replace-all space-scanner key ""))))
      (if (or (zerop spaces)
              (not (zerop (mod number spaces))))
          (error 'socket.io-websocket-illegal-key :initarg key)
          (integer-octets-32be (/ number spaces)))))) ; crack-smoking mac pussies at google want big endian

(defun read-bytes-array (stream number)
  "read-bytes-array stream number => unsigned byte array

Read NUMBER bytes from Chunga STREAM into array and return it."
  (let ((result (make-array number :element-type '(unsigned-byte 8))))
    (dotimes (index number result)
      (setf (aref result index)
            (char-int (read-char* stream t))))))

(defun read-key3 (request)
  "read-key3 request => unsigned byte array

Read eight bytes from REQUEST's content stream and return them as a byte
array."
  (read-bytes-array (content-stream request) 8))

(defun digest-key (key)
  "digest-key key => unsigned byte array

Evaluates to MD5 digest of KEY sequence."
  (digest-sequence :md5 key))

(defun websocket-uri (request host &optional ssl)
  "websocket-uri request host &optional ssl => URI string

Evaluate arguments to corresponding WebSocket (ws:// or wss://) URL."
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))

(defun handle-handshake-legacy (request reply)
  "handle-handshake-legacy request reply => challenge

Legacy (< draft-hybi-02) method of performing a WebSocket handshake. Still in
use for FF4 (if activated) and many versions of Chromium. Also sets the
Sec-WebSocket-Origin, Sec-WebSocket-Location and Sec-WebSocket-Protocol response
headers, if successful. The challenge is sent over the raw TCP socket."
  (prog1
      (let* ((stream (make-in-memory-output-stream)))
        (mapc #'(lambda (key)
                  (write-sequence key stream))
              (list (websocket-keyhash (header-in :sec-websocket-key1 request))
                    (websocket-keyhash (header-in :sec-websocket-key2 request))
                    (read-key3 request)))
        (digest-key (get-output-stream-sequence stream)))
    (setf (header-out :sec-websocket-origin reply) (header-in :origin request)
          (header-out :sec-websocket-location reply) (or (websocket-uri request (header-in :host request)
                                                                        (ssl-p (request-acceptor request))))
          (header-out :sec-websocket-protocol reply) (header-in :sec-websocket-protocol request))))

(defun websocket-handle-handshake (request reply)
  "websocket-handle-handshake request reply => handshake

Try to determine the WebSocket handshake version by checking REQUEST headers and
handle accordingly, if possible."
  (handler-case
      (prog1
          (cond ((header-in :sec-websocket-version request) ; >= draft-hybi-04 FIXME
                 (error 'websocket-unsupported-version (header-in :sec-websocket-version request)))
                ((header-in :sec-websocket-draft request) ; (and (>= draft-hybi-02) (<= draft-hybi-03)) FIXME
                 (error 'websocket-unsupported-version (header-in :sec-websocket-draft request)))
                ((and (header-in :sec-websocket-key1 request) ; < draft-hybi-02
                      (header-in :sec-websocket-key2 request))
                 (handle-handshake-legacy request reply))
                (t (error 'websocket-unsupported-version :unknown)))
        (setf (return-code* reply) +http-switching-protocols+
              (header-out :upgrade reply) "WebSocket"
              (header-out :connection reply) "Upgrade"
              (content-type* reply) "application/octet-stream"))
    (websocket-illegal-key (condition)
      (hunchentoot-error "Illegal key ~a encountered" (websocket-illegal-key-of condition)))
    (websocket-unsupported-version ()
      (hunchentoot-error "WebSocket handshake failed because of unsupported protocol version"))))

(defun websocket-send-term (&optional (stream *websocket-stream*) (mutex *websocket-stream-mutex*))
  "websocket-send-term &optional stream mutex => side effect

Send the magic WebSocket termination sequence across STREAM."
  (with-lock-held (mutex)
    (write-sequence +websocket-terminator+ stream)
    (force-output stream)))

(defun websocket-send-message (message &optional (stream *websocket-stream*) (mutex *websocket-stream-mutex*))
  "websocket-send-message &optional stream mutex => side effect

Encode MESSAGE as UTF-8 bytes and send it across STREAM in a proper frame."
  (when (> (length message) 0) ; empty message would send terminator
    (log-message :debug "Going to send websocket message ~a" message)
    (with-lock-held (mutex)
      (write-byte #x00 stream)
      (write-utf-8-bytes message stream)
      (write-byte #xff stream)
      (force-output stream))))

(defun skip-bytes (stream number)
  "skip-bytes stream number => side effect

Read and discard NUMBER bytes from STREAM."
  (dotimes (num number)
    (read-byte stream)))

(defun websocket-process-connection (stream message-handler &optional (version :draft-hixie-76))
  "websocket-process-connection stream message-handler &optional version => REPL*

Implements the main WebSocket loop for supported protocol versions. Framing is
handled automatically, MESSAGE-HANDLER ought to handle the actual payloads.

*Not really a REPL because it doesn't print implicitly but it does what you'd
expect from the name."
  (ecase version
    ((:draft-hixie-76 :draft-hybi-00)
     (loop for type = (read-byte stream) do 
          (cond ((= #x00 type)
                 (do ((reader (make-in-memory-output-stream))
                      (data (read-byte stream) (read-byte stream)))
                     ((= #xff data)
                      (funcall message-handler (utf-8-bytes-to-string (get-output-stream-sequence reader))))
                   (write-byte data reader)))
                ((= #xff type)
                 (let ((data (read-byte stream)))
                   (if (= #x00 data)
                       (return) ; regular termination
                       (do* ((data data (read-byte stream))
                             (length (logand #x7f data) (+ (* 128 length) (logand #x7f data))))
                            ((= #x80 (logand #x80 data))
                             (skip-bytes stream length))))))
                (t (error 'websocket-illegal-frame-type ; irregular termination
                          :type type))))))) 

(defmethod process-connection :around ((*acceptor* websocket-acceptor) (socket t))
  "process-connection :around websocket-acceptor t => context

Continue the process with *WEBSOCKET-SOCKET* bound to the original TCP socket
and *HUNCHENTOOT-VERSION* enhanced by the Hunchensocket version."
  (let ((*websocket-socket* socket)
        (hunchentoot-asd:*hunchentoot-version* (format nil "~a Hunchensocket 0" hunchentoot-asd:*hunchentoot-version*)))
    (call-next-method)))

(defmethod process-connection ((*acceptor* websocket-acceptor) (socket t))
  "process-connection websocket-acceptor t => context

Enclose the connection processing with a general error handler w/logging."
  (handler-case
      (call-next-method)
    (error (condition)
      (log-message :error "WebSocket connection terminated unexpectedly: ~a" condition)
      (log-message :debug "~@[~%~a~]"
                   (print-backtrace condition :output nil)))))

(defmethod process-request :around ((request websocket-request))
  "process-request :around websocket-request => context / side effect / stream

First, continue the process with HTTP 101 added to Hunchentoot's list of
*APPROVED-RETURN-CODES*. If that status code got issued in the response after
regular processing, hijack the connection and continue to process it as a
WebSocket one.

PS: I *do* know what I'm doing, Mister!"
  (let ((*approved-return-codes* (cons +http-switching-protocols+
                                       *approved-return-codes*)))
    (let ((stream (call-next-method)))
      (prog1 stream
        (when (= +http-switching-protocols+ (return-code*))
          (force-output stream)
          (let ((timeout (websocket-timeout (request-acceptor request))))
            (set-timeouts *websocket-socket* timeout timeout))
          (handler-case
              (let ((*websocket-stream* stream)
                    (*websocket-stream-mutex* (make-lock)))
                (websocket-process-connection stream
                                              (funcall (websocket-request-handler request) stream *websocket-stream-mutex*))) ; custom handshake
            (end-of-file ()
              (log-message :debug "WebSocket connection terminated"))
            (websocket-illegal-frame-type (condition)
              (log-message :error "WebSocket illegal frame type 0x~x encountered, terminating"
                           (websocket-illegal-frame-type-of condition)))))))))

(defun dispatch-websocket-handlers (request)
  "dispatch-websocket-handlers request => handler

Try to dispatch REQUEST against the available *WEBSOCKET-HANDLERS*, evaluating
to the first match which should be a `λ stream mutex -> λ message' handshake /
message handler closure (please consult the README)."
  (some #'(lambda (handler)
            (funcall handler request))
        *websocket-handlers*))

(defmethod handle-request ((*acceptor* websocket-acceptor) (*request* websocket-request))
  "handle-request websocket-acceptor websocket-request => context

WebSocket junction. Try to continue as a WebSocket connection if the Upgrade and
WebSocket request headers were issued and a suitable WebSocket handler could be
found."
  (if (and (string= "upgrade" (string-downcase (header-in* :connection)))
           (string= "websocket" (string-downcase (header-in* :upgrade))))
      (if-let ((handler (dispatch-websocket-handlers *request*)))
        (prog1 (websocket-handle-handshake *request* *reply*)
          (setf (websocket-request-handler *request*) handler))
        (hunchentoot-error "WebSocket request rejected (no suitable handler found)"))
      (call-next-method)))
