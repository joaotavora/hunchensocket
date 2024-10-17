(cl:defpackage :hunchensocket
  (:use :cl :alexandria :hunchentoot :cl-ppcre :alexandria
   :flexi-streams :trivial-utf-8 :bordeaux-threads)
  (:import-from :chunga :read-char*)
  (:import-from :trivial-backtrace :print-backtrace)
  (:import-from :hunchentoot :log-message*)
  (:export
   ;; acceptor classes
   #:websocket-acceptor
   #:websocket-ssl-acceptor
   #:*websocket-maximum-fragment-size*
   #:*websocket-maximum-message-size*

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
   #:listening-p
   
   ;; receiving and sending messages
   ;;
   #:binary-message-received
   #:text-message-received
   #:send-ping
   #:send-text-message
   #:send-binary-message))
