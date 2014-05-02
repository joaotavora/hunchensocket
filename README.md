Hunchensocket - WebSockets for Hunchentoot
==========================================

Hunchensocket is a Common Lisp implementation of [WebSocket]s realized
as an extension to [Edi Weitz'] [edi] excellent [Hunchentoot] web
server.

Note also that this is a not-yet-sanctioned fork of the original
library by Alexander Kahl. See the COPYING for license details file.

Installation
------------

Hunchensocket is supported by ASDF and is not (yet?) supported by
[Quicklisp][Quicklisp]. Do this for now:

```
$ cd ~/quicklisp/local-projects/
$ git clone https://github.com/capitaomorte/hunchensocket.git
```

then `(ql:quickload :hunchensocket)` in your REPL.

Support
-------

Hunchensocket implements an mostly compliant [RFC6455][RFC6455]
server.

Usage
-----

Hunchensocket is meant to be used as a Hunchentoot extension. To
establish WebSockets, follow the regular Hunchentoot
[documentation](http://www.weitz.de/hunchentoot/#start) but exchange
the acceptor classes by Hunchensocket versions.

Then, a websocket-specific API is available to process messages,
inspired both by [clws][clws]'s and [Hunchentoot's][Hunchentoot].

Here's a simple chat server:

```lisp
(defpackage :my-chat (:use :cl))
(in-package :my-chat)

(defvar *chat-rooms* nil)

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name)))

(defmethod initialize-instance :after ((room chat-room) &key)
  (push room *chat-rooms*))

(defun find-chat-room (request)
  ;; returning NIL will make Hunchentoot reply with a 404
  (let ((name (hunchentoot:script-name request)))
    (or (find name *chat-rooms* :test #'string= :key #'name)
        (and (< (length *chat-rooms*) 5)
             (make-instance 'chat-room :name name)))))

(pushnew 'find-chat-room hunchensocket:*websocket-dispatch-table*)

(defmethod hunchensocket:message-received ((resource chat-room) message client)
  (loop for peer in (hunchensocket:clients resource)
        do (hunchensocket:send-message peer (format nil
                                                    "~a says ~a"
                                                    peer message))))

(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))

(hunchentoot:start *server*)
```

[WebSocket]: http://en.wikipedia.org/wiki/WebSocket  
[edi]: http://weitz.de/
[RFC6455]: https://tools.ietf.org/html/rfc6455
[clws]: https://github.com/3b/clws
[Hunchentoot]: http://weitz.de/hunchentoot/
[Quicklisp]: http://www.quicklisp.org/  
