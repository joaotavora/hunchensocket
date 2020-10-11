(cl:defpackage :hunchensocket
  (:use :cl :alexandria :hunchentoot :cl-ppcre :alexandria
   :flexi-streams :trivial-utf-8 :bordeaux-threads)
  (:import-from :ironclad :digest-sequence)
  (:import-from :chunga :read-char*)
  (:import-from :trivial-backtrace :print-backtrace)
  (:import-from :hunchentoot :log-message*)
  (:import-from #:compression
                #:persistent-z-stream
                #:stream-operation)
  (:export
   ;; acceptor classes
   #:websocket-acceptor
   #:websocket-ssl-acceptor

   ;; dispatch table
   #:*websocket-dispatch-table*

   ;; resource and client classes
   #:websocket-resource
   #:websocket-client
   #:client-request
   #:close-connection

   ;; message validation
   #:check-message

   ;; resource accessors
   #:clients
   
   ;; status updates
   ;;
   #:client-connected
   #:client-disconnected
   
   ;; receiving and sending messages
   ;; 
   #:binary-message-received
   #:text-message-received
   #:send-text-message
   #:send-binary-message))

(cl:defpackage #:compression
  (:use #:common-lisp)
  (:import-from #:zlib-ffi
                #:%inflate
                #:%deflate
                #:avail-in
                #:avail-out
                #:next-in
                #:next-out
                #:total-in
                #:total-out
                #:z-stream
                #:make-shareable-byte-vector
                #:+z-no-flush+
                #:+z-finish+
                #:+z-sync-flush+
                #:+z-stream-error+
                #:+z-data-error+)
  (:import-from #:deoxybyte-gzip
                #:z-stream-open
                #:z-stream-close
                #:fill-from-stream
                #:empty-to-stream
                #:+default-zlib-buffer-size+
                #:vector-index
                #:z-error
                #:zlib-error
                #:errno-of
                #:fill-from-stream
                #:empty-to-stream)
  (:import-from #:trivial-garbage
                #:finalize)
  (:import-from #:cffi
                #:with-foreign-slots
                #:with-pointer-to-vector-data)
  (:import-from #:flexi-streams
                #:with-input-from-sequence
                #:with-output-to-sequence
                #:string-to-octets)
  (:export #:persistent-z-stream
           #:stream-operation))
