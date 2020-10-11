(in-package #:compression)

(defclass persistent-z-stream ()
  ((operation       :initarg :operation
                    :initform (error "Must specify one of (:inflate :deflate)")
                    :reader persistent-op)
   (compression     :initarg :compression
                    :initform zlib-ffi::+z-default-compression+)
   (suppress-header :initarg :suppress-header
                    :initform t)
   (gzip-format     :initarg :gzip-format
                    :initform nil)
   (window-bits     :initarg :window-bits
                    :initform 15)
   (mem-level       :initarg :mem-level
                    :initform 8)
   (strategy        :initarg strategy
                    :initform :default-strategy)
   (z-stream  :initform nil
              :accessor persistent-stream))
  (:documentation "Wrapper class for ZLIB-FFI:Z-STREAM. 
Accessing the Zlib z-stream pointer this way allows the foreign memory
to be automatically freed when objects of this class are garbage
collected. It also means we don't need to specify the operation we
want to do (:inflate or :deflate), as this is a property of the
stream."))

(defmethod initialize-instance :after ((object persistent-z-stream) &key)
  "Set up the foreign pointer to the zlib stream and take care of
freeing the foreign memory when the object is garbage-collected."
  (let ((op (persistent-op object)) ; This is needed in the FINALIZE lambda so we don't want to close
                                    ; over the object
        (zs (with-slots (operation compression suppress-header gzip-format
                         window-bits mem-level strategy) object
              (z-stream-open operation :compression compression
                             :suppress-header suppress-header
                             :gzip-format gzip-format :window-bits window-bits
                             :mem-level mem-level :strategy strategy))))
    (setf (persistent-stream object) zs)
    ;; Function to call when the object is garbage collected to
    ;; release the foreign memory
    (finalize object (lambda ()
                       (handler-case
                             (z-stream-close zs op)
                         (zlib-error (condition)
                           ;; According to the Zlib manual, if a Z_DATA_ERROR is
                           ;; returned when closing a stream, the memory is still
                           ;; freed, so we can ignore this error.
                           (if (= +z-data-error+ (errno-of condition))
                               (values nil condition)
                               condition)))))))

(defgeneric stream-operation (stream-object source dest &key))

(defmethod stream-operation ((object persistent-z-stream) source dest
                             &key (input-fn #'fill-from-stream) (output-fn #'empty-to-stream)
                                  (buffer-size +default-zlib-buffer-size+) (flush-type +z-sync-flush+))
  "Reimplementation of DEOXYBYTE-GZIP:Z-STREAM-OPERATION.

Additionally allows specification of zlib flush type, which is needed for
WebSocket compression extensions."
  (let ((zs    (persistent-stream object))
        (op-fn (ecase (persistent-op object)
                 (:inflate #'%inflate)
                 (:deflate #'%deflate)))
        (in-buffer  (make-shareable-byte-vector buffer-size))
        (out-buffer (make-shareable-byte-vector buffer-size)))
    (with-foreign-slots ((avail-in next-in
                          avail-out next-out 
                          total-in total-out)
                         zs z-stream)
           (with-pointer-to-vector-data (in in-buffer)
             (with-pointer-to-vector-data (out out-buffer)
               (flet ((read-from-zs ()
                        (when (zerop avail-in)
                          (let ((num-read (funcall input-fn in-buffer
                                                   source buffer-size)))
                            (declare (type vector-index num-read))
                            (when (plusp num-read)
                              (setf next-in in
                                    avail-in num-read))))
                        avail-in)
                      (write-from-zs ()
                        (let ((fullp (zerop avail-out))
                              (output-bytes (- buffer-size avail-out)))
                          (unless (zerop output-bytes)
                            (funcall output-fn out-buffer dest output-bytes)
                            (setf next-out out
                                  avail-out buffer-size))
                          fullp))) ; Was the output buffer full on writing?
                 (setf next-out out
                       avail-out buffer-size)
                 (loop
                    for num-read = (read-from-zs)
                    while (plusp num-read)
                    do (let ((flush (if (= buffer-size num-read)
                                        +z-no-flush+
                                        flush-type)))
                         (loop
                            with out-full = t
                            while out-full
                            do (let ((x (funcall op-fn zs flush)))
                                 (when (= +z-stream-error+ x)
                                   (z-error x))
                                 (setf out-full (write-from-zs)))))
                    finally (progn
                              (return (values total-in total-out))))))))))
