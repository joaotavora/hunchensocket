;;;; Hunchensocket - package.lisp
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Hunchensocket.
;;;; Hunchensocket is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; Hunchensocket is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
