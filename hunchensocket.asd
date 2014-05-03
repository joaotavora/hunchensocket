(asdf:defsystem :hunchensocket
  :description "WebSockets for Hunchentoot"
  :license "MIT"
  :depends-on (:hunchentoot
               :alexandria
               :ironclad
               :flexi-streams
               :chunga
               :trivial-utf-8
               :trivial-backtrace
               :bordeaux-threads
               :cl-fad)
  :serial t
  :components
  ((:file "package")
   (:file "hunchensocket")))

(asdf:defsystem :hunchensocket-tests
  :description "Tests for Hunchensoket"
  :depends-on (:stefil
               :hunchensocket)
  :serial t
  :components
  ((:file "hunchensocket-tests")))
