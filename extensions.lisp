;;;; Compression and decompression routines to support the permessage-deflate
;;;; extension to the WebSockets protocol (RFC 7692).
;;;; Isaac Stead, October 2020

(in-package #:hunchensocket)

(defmethod initialize-instance :after ((acceptor websocket-acceptor) &key)
  "Validate the :extension-data argument"
  (let ((extension-data (websocket-acceptor-extensions acceptor)))
    (when extension-data
      (handler-case 
          (mapcar (lambda (sublist)
                    (assert (and (eq :extension (first sublist))
                                 (eq :headers (third sublist))
                                 (eq :parameters (fifth sublist))))
                    (check-type (fourth sublist) list)
                    (check-type (sixth sublist) list))
                  extension-data)
        (error (condition)
          (error "The extension-data argument list ~a is malformed.~&Error: ~s"
                 extension-data condition))))))

(defmethod format-extension-headers ((acceptor websocket-acceptor))
  (when-let (extension-data (websocket-acceptor-extensions acceptor))
    (str:join "; " (loop for sublist in extension-data
                         append (fourth sublist)))))

(defmethod initialize-extensions ((acceptor websocket-acceptor))
  "Initialize extensions specified for this acceptor so they are ready
to be passed to a client instance."
  (let ((extension-data (websocket-acceptor-extensions acceptor)))
    (mapcar (lambda (sublist)
              (format t "~a~&" sublist)
              (apply #'make-instance (second sublist) (sixth sublist)))
            extension-data)))

(defclass websocket-extension ()
  ()
  (:documentation "Base class for extensions to the websocket protocol.
Instances of this class will be created on HUNCHENSOCKET:CLIENT
objects and take care of extension-specific processing on incoming and
outgoing frames and/or messages."))

(defgeneric process-send-message (extension message)
  (:documentation "Process an outgoing message"))

(defgeneric process-receive-message (extension message)
  (:documentation "Process an incoming message"))

(defgeneric process-send-frames (extension frames)
  (:documentation "Process outgoing frames"))

(defgeneric process-receive-frames (extension frames)
  (:documentation "Process incoming frames"))

(defclass permessage-deflate (websocket-extension)
  ((inflate-stream :initform (make-instance 'persistent-z-stream :operation :inflate)
                   :reader pmd-inflate-stream)
   (deflate-stream :initform (make-instance 'persistent-z-stream :operation :deflate)
     :reader pmd-deflate-stream)))

(defun array-slice (vector start end)
  "Slice an array by indices"
  (let ((end (cond ((eq end :end)
                    (length vector))
                   ((minusp end)
                    (+ (length vector) end))
                   (:else end))))
    (make-array (- end start)
                :element-type (array-element-type vector)
                :displaced-to vector
                :displaced-index-offset start)))

(defmethod process-receive-frames ((ext permessage-deflate) frames)
  "Decompress received frames, if the frame header bit is set"
  (with-accessors ((opcode frame-opcode)
                   (ext-data frame-extension-data)) (car frames)
    (let* ((msg-bytes (apply #'concatenate '(vector (unsigned-byte 8))
                             (mapcar #'frame-data frames)))
           (maybe-processed (if (= ext-data #x04)
                                (with-input-from-sequence
                                    (in (concatenate '(vector (unsigned-byte 8))
                                                     msg-bytes #(#x00 #x00 #xff #xff)))
                                  (with-output-to-sequence (out)
                                    (stream-operation (pmd-inflate-stream ext) in out)))
                                msg-bytes)))
      (cond ((= opcode +text-frame+)
             (utf-8-bytes-to-string maybe-processed))
            ((= opcode +binary-frame+)
             maybe-processed)
            (:else
             (websocket-error 1002 "Client sent unknown opcode ~a" opcode))))))

(defmethod process-send-message ((ext permessage-deflate) message-bytes)
  "Compress an outgoing message, following the procedure in RFC7692"
  (array-slice (with-input-from-sequence (in message-bytes)
                 (with-output-to-sequence (out)
                   (stream-operation (pmd-deflate-stream ext)
                                     in out :flush-type zlib-ffi::+z-sync-flush+)))
               0 -4))
