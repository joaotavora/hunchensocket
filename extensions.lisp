;;;; Compression and decompression routines to support the permessage-deflate
;;;; extension to the WebSockets protocol (RFC 7692).
;;;; Isaac Stead, October 2020

(in-package #:hunchensocket)

(defclass websocket-extension ()
  ((headers :initarg :headers :reader extension-headers
            :documentation "List of necessary headers for the
            initial handshake needed by the extension")
   (frame-header-bits :initarg :frame-header-bits :reader extension-frame-data
                      :documentation "Return the frame header bits
                      necessary for the protocol extension."))
  (:documentation "Base class for extensions to the websocket protocol."))

(defgeneric process-send-message (extension message)
  (:documentation "Process an outgoing message"))

(defgeneric process-receive-message (extension message)
  (:documentation "Process an incoming message"))

(defun format-headers (headers)
  "Concatenate extension header strings in the correct format"
  (str:join "; " headers))

(defclass permessage-deflate (websocket-extension)
  ()
  (:default-initargs :headers '("permessage-deflate")
                     :frame-header-bits #x04))

(defmethod process-send-message ((ext permessage-deflate) message-bytes)
  (declare (ignore ext))
  (compress-bytes message-bytes))

(defmethod process-receive-message ((ext permessage-deflate) message-bytes)
  (declare (ignore ext))
  (decompress-bytes message-bytes))

(defun compress-bytes (bytes)
  ;; Normally, Z_SYNC_FLUSH would be used to append an empty deflate
  ;; block. But according to the spec (7.2.3.4), we can use deflate
  ;; blocks with the BFINAL bit set to 1, and append 0x00 to allow
  ;; decompression by the same method as with Z_SYNC_FLUSH output.
  ;; I'm using this method because I couldn't find a way of getting
  ;; the Z_SYNC_FLUSH behaviour with any of the CL deflate libraries.
  (concatenate '(vector (unsigned-byte 8))
               (salza2:compress-data bytes 'salza2:deflate-compressor)
               #(#x00)))

(defun decompress-bytes (bytes)
  ;; If the BFINAL bit in the deflate header is set to 0 the decompression
  ;; will fail, so set it if necessary
  (setf (ldb (byte 1 0)
             (aref bytes 0))
        #x01)
  ;; Now do the actual decompression
  (chipz:decompress nil *dstate*
                    ;; The decompression method specified in RFC7692
                    (concatenate '(vector (unsigned-byte 8))
                                 bytes #(#x00 #x00 #xff #xff))))

;; (defun send-message (resource client message &key (type :text))
;;   "TODO documentation"
;;   (let* ((extension     (resource-extension resource))
;;          (message-bytes (ecase type
;;                           (:text   (flexi-streams:string-to-octets message :external-format :utf-8))
;;                           (:binary message)))
;;          (frame-type    (ecase type
;;                           (:text +text-frame+)
;;                           (:binary +binary-frame+)))
;;          (maybe-processed-message (if extension
;;                                       (process-send-message extension message-bytes)
;;                                       message)))
;;     (with-slots (write-lock output-stream) client
;;       (with-lock-held (write-lock)
;;         (write-frame output-stream
;;                      frame-type
;;                      maybe-processed-message
;;                      (if extension
;;                          (extension-frame-data extension)
;;                          #x00))))))

(defparameter *z-deflate-stream* (deoxybyte-gzip::z-stream-open :deflate :suppress-header t :gzip-format nil))
(defparameter *z-inflate-stream* (deoxybyte-gzip::z-stream-open :inflate :suppress-header t :gzip-format nil))

(defun persistent-z-stream-operation (zs operation source dest input-fn output-fn buffer-size)
  "Like deoxybyte:z-stream-operation, but called on an external
z-stream so the compression / decompression algorithm state can be
maintained between messages. Uses Z_SYNC_FLUSH rather than Z_FINISH as in the original function."
  (let ((op-fn (ecase operation
                 (:inflate #'zlib-ffi:%inflate)
                 (:deflate #'zlib-ffi:%deflate)))
        (in-buffer  (zlib-ffi::make-shareable-byte-vector buffer-size))
        (out-buffer (zlib-ffi::make-shareable-byte-vector buffer-size)))
    (unwind-protect
         (cffi:with-foreign-slots ((zlib-ffi::avail-in zlib-ffi::next-in
                                    zlib-ffi::avail-out zlib-ffi::next-out 
                                    zlib-ffi::total-in zlib-ffi::total-out)
                                   zs zlib-ffi::z-stream)
           (cffi:with-pointer-to-vector-data (in in-buffer)
             (cffi:with-pointer-to-vector-data (out out-buffer)
               (flet ((read-from-zs ()
                        (when (zerop zlib-ffi::avail-in)
                          (let ((num-read (funcall input-fn in-buffer
                                                   source buffer-size)))
                            (declare (type deoxybyte-gzip::vector-index num-read))
                            (when (plusp num-read)
                              (setf zlib-ffi::next-in in
                                    zlib-ffi::avail-in num-read))))
                        zlib-ffi::avail-in)
                      (write-from-zs ()
                        (let ((fullp (zerop zlib-ffi::avail-out))
                              (output-bytes (- buffer-size zlib-ffi::avail-out)))
                          (unless (zerop output-bytes)
                            (funcall output-fn out-buffer dest output-bytes)
                            (setf zlib-ffi::next-out out
                                  zlib-ffi::avail-out buffer-size))
                          fullp))) ; Was the output buffer full on writing?
                 (setf zlib-ffi::next-out out
                       zlib-ffi::avail-out buffer-size)
                 (loop
                    for num-read = (read-from-zs)
                    while (plusp num-read)
                    do (let ((flush (if (= buffer-size num-read)
                                        zlib-ffi:+z-no-flush+
                                        zlib-ffi::+z-sync-flush+)))
                         (loop
                            with out-full = t
                            while out-full
                            do (let ((x (funcall op-fn zs flush)))
                                 (when (= zlib-ffi:+z-stream-error+ x)
                                   (deoxybyte-gzip::z-error x))
                                 (setf out-full (write-from-zs)))))
                    finally (progn
                              (return (values zlib-ffi::total-in zlib-ffi::total-out)))))))))))
