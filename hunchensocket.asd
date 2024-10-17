(asdf:defsystem :hunchensocket
  :description "WebSockets for Hunchentoot"
  :author "capitaomorte <https://github.com/capitaomorte>"
  :license "MIT"
  :version #.(with-open-file (f "VERSION") (string (read f)))
  :depends-on (:hunchentoot
               :alexandria
               :cl-base64
               :sha1
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

