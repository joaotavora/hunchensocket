;;;; Hunchensocket - package.lisp
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Hunchensocket.
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;; 
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  (:export :websocket-acceptor
           :websocket-ssl-acceptor
           :websocket-handle-handshake
           :websocket-send-message
           :websocket-send-term
           :default-websocket-handler
           :*websocket-handlers*
           :*websocket-stream*
           :*websocket-stream-mutex*))
