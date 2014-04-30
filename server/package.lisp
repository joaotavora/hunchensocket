; FIXME upstream
(in-package :hunchentoot)
(export (list 'content-stream 'set-timeouts 'acceptor-listen-socket))

(in-package :hunchensocket-system)

(defpackage :hunchensocket
  (:use :cl :alexandria :hunchentoot :cl-ppcre :alexandria
   :flexi-streams :trivial-utf-8 :bordeaux-threads)
  (:import-from :ironclad :digest-sequence)
  (:import-from :chunga :read-char*)
  (:import-from :trivial-backtrace :print-backtrace)
  (:import-from :hunchentoot :log-message*)
  (:export
   #:websocket-acceptor
   #:websocket-ssl-acceptor
   #:websocket-handle-handshake
   #:websocket-send-message
   #:websocket-send-term
   #:default-websocket-handler
   #:*websocket-stream*
   #:*websocket-stream-mutex*
   #:*websocket-dispatch-table*
   #:websocket-echo-resource
   #:websocket-resource
   #:websocket-client
   #:message-received
   #:binary-received
   #:client-request))




