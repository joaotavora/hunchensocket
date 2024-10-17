![build and test](https://github.com/joaotavora/hunchensocket/actions/workflows/build-and-test.yml/badge.svg)

Hunchensocket - WebSockets for Hunchentoot
==========================================

Hunchensocket is a Common Lisp implementation of [WebSocket]s realized
as an extension to [Edi Weitz'] [edi] excellent [Hunchentoot] web
server. Hunchensocket implements a compliant [RFC6455][RFC6455] server. 

Note that Alexander Kahl, the original author, has desactivated his 
[old version][kahl] that only supports the drafts of the protocol.

Installation
------------

Hunchensocket is in [Quicklisp][Quicklisp], so if you have that
setup just do `(ql:quickload :hunchensocket)`.

Quicklisp is also good to use the trunk alongside with other 
dependencies, perhaps to test a new feature or a bugfix:

```
$ cd ~/Source/Lisp/
$ git clone https://github.com/joaotavora/hunchensocket.git
```

```lisp
(push "~/Source/Lisp" ql:*local-project-directories*)
(ql:quickload :hunchensocket) ;; use local hunchensocket and pull
                              ;; dependencies from quicklisp
```

A chat server in 30 lines
-------------------------

First define classes for rooms and users. Make these subclasses of
`websocket-resource` and `websocket-client`.

```lisp
(defpackage :my-chat (:use :cl))
(in-package :my-chat)

(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))
```

Define a list of rooms. Notice that
`hunchensocket:*websocket-dispatch-table*` works just like
`hunchentoot:*dispatch-table*`, but for websocket specific resources.

```lisp
(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)
```

OK, now a helper function and the dynamics of a chat room.

```lisp
(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))  
```

Finally, start the server. `hunchensocket:websocket-acceptor` works
just like `hunchentoot:acceptor`, and you can probably also use
`hunchensocket:websocket-ssl-acceptor`.


```lisp
(defvar *server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))
(hunchentoot:start *server*)
```

Now open two browser windows on https://www.piesocket.com/websocket-tester
enter `ws://localhost:12345/bongo` as the host and play around chatting with
yourself.

License
-------

See [COPYING][copying] for license details.

Design
------

Main sources of inspiration:

* Original implementation by Alexander Kahl, which cleverly hijacks
  the Hunchentoot connection after the HTTP response and keeps the
  connection alive, just like in a Head request.
* [clws][clws]'s API because it explicitly defines websocket "resources"
* [Hunchentoot's][Hunchentoot]'s API because it uses CLOS

Contributing
------------

* Propose patches using GitHub's pull request feature, as usual.  You
  may ping the maintainers if you don't get some kind of feedback
  after a week or so.

* When composing a PR, it's OK to include nonessential
  cleanup/housekeeping changes like fixing the odd bug, typo, or
  indentation mishap.  But please segregate these changes into their
  own separate commit.  You may freely `git rebase --interactice` and
  `git push -f` to rewrite your PR if needed.

* Each commit's message should strictly follow the GNU ChangeLog
  format described [here][gnu-changelog].  It's what the Emacs project
  uses.  There's a short subject line in the imperative form, followed
  optionally by longer explanatory text, all formatted to less than 70
  columns.  A bit like a nice text email from the twentieth century.
  In the end there are entries for each file and
  function/macro/top-level entity changed within it, along with very
  brief descriptions of the change.  You can use `C-x 4 a` in Emacs to
  help you generate these, if you want.

  Here are some examples:

```
Closes #12: Avoid bordeaux-threads bug when nullifying locks

Elias Mårtenson noticed that its default implementation, the
BT:WITH-LOCK-HELD macro evaluates its argument twice and thus
nullifying that slot while holding it is not only unnecessary, but
problematic.

See https://github.com/joaotavora/hunchensocket/pull/12.

This fix is conceptually cleaner than the one proposed there.

* hunchensocket.lisp (call-with-new-client-for-resource): Don't
  nullify client's WRITE-LOCK while locking it.
```

```
Fix many bugs and add automatic tests for robustness.

* hunchensocket-tests.lisp: New file.

* hunchensocket.asd (:hunchensocket-tests): New system.
(:hunchensocket): Depends on cl-fad.

* hunchensocket.lisp (control-frame-p): New function.
(websocket-client): Separate input and output stream slots. State
goes in client.
(check-message): Receive fragment and total length.
(send-text-message): Use SEND-FRAME.
(close-connection, send-frame): New functions.
(websocket-error): Add error status reader.
(with-new-client-for-resource): Adapt to separate stream slots.
(read-unsigned-big-endian): Fix big bug. Was reading little-endian!
(read-frame): Optionally read payload. Error out when control
frame is too large.
(read-frame-from-client): New function.
(mask-unmask): New function.
(read-application-data): New function.
(handle-frame): Rework completely. Bigger but slightly easier to
read.
(read-handle-loop): Simplify.
(process-request): Use new WITH-NEW-CLIENT-FOR-RESOURCE.
```


[WebSocket]: http://en.wikipedia.org/wiki/WebSocket  
[edi]: http://weitz.de/
[kahl]: https://github.com/e-user/hunchensocket
[RFC6455]: https://tools.ietf.org/html/rfc6455
[clws]: https://github.com/3b/clws
[copying]: https://github.com/joaotavora/hunchensocket/blob/master/COPYING
[Hunchentoot]: http://weitz.de/hunchentoot/
[Quicklisp]: http://www.quicklisp.org/  
[gnu-changelog]: https://www.gnu.org/prep/standards/html_node/Style-of-Change-Logs.html
