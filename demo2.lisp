;; A chat server in 30 lines
;; -------------------------

;; First define classes for rooms and users. Make these subclasses of
;; `websocket-resource` and `websocket-client`.

(defvar *package-list* '(:hunchensocket :hunchentoot :cl-who :parenscript))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload *package-list*))

(defpackage :my-chat #.(append '(:use :cl) *package-list*)
(in-package :my-chat)

(setq *attribute-quote-char* #\")
  
(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

;; Define a list of rooms. Notice that
;; `hunchensocket:*websocket-dispatch-table*` works just like
;; `hunchentoot:*dispatch-table*`, but for websocket specific resources.

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)

;; OK, now a helper function and the dynamics of a chat room.

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))  

;; define an easy handler

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name) (format nil "<h1>Hey~@[ ~A~]!</h1>" name))

;; Finally, start the server. `hunchensocket:websocket-acceptor` works
;; just like `hunchentoot:acceptor`, and you can probably also use
;; `hunchensocket:websocket-ssl-acceptor`.
;; also `hunchensocket:websocket-easy-acceptor`.
;; probably also `hunchensocket:websocket-ssl-easy-acceptor`.

(defvar *server* (make-instance 'hunchensocket:websocket-easy-acceptor :port 12345))

(unless (hunchensocket:listening-p *server*)           ; should be
                                                       ; hunchentoot:listening-p
                                                       ; if it existed
  (hunchentoot:start *server*))

;; Now open two browser windows on http://www.websocket.org/echo.html,
;; enter `ws://localhost:12345/bongo` as the host and play around chatting with
;; yourself.

;; ;; Examples

;; The ps macro takes Parenscript code in the form of s-expressions (Parenscript code and Common Lisp code share the same representation), translates as much as it can into constant strings at macro-expansion time, and expands into a form that will evaluate to a string containing JavaScript code.

(hunchentoot:define-easy-handler (example1 :uri "/example1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "Hello World"))
                "Hello World")))))

;; One way to include Parenscript code in web pages is to inline it in HTML script tags:

(hunchentoot:define-easy-handler (example2 :uri "/example2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps
                      (defun greeting-callback ()
                        (alert "Hello World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))

;; Another way to integrate Parenscript into a web application is to serve the generated JavaScript as a separate HTTP resource. Requests to this resource can then be cached by the browser:

(hunchentoot:define-easy-handler (example3 :uri "/example3.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    #| start javascript |#
    (defun find-location (path)
      (if path
	  (concatenate 'string "ws" (location.origin.substr 4) path)
	  (concatenate 'string "ws" (location.href.substr 4))))
    (defun make-websocket (url)
      (-web-socket url)
      (alert "Hello World"))
    (defun greeting-callback ()
      (alert "Hello World"))
    (defun greeting-callback2 ()
      (alert "Hello World2"))
    (defun greeting-callback3 ()
      (alert "Hello World3"))
    #| end javascript |#))
