(in-package :cl-user)

(defpackage :hunchensocket-system-test
  (:use :cl :asdf))

(in-package :hunchensocket-system-test)

(defsystem :hunchensocket-test
  :description "WebSockets for Hunchentoot - Tests"
  :author "Alexander Kahl <e-user@fsfe.org>"
  :license "MIT"
  :depends-on (:hunchensocket :hu.dwim.stefil)
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "websocket")))))
