(asdf:defsystem :hunchensocket-tests
  :description "Tests for Hunchensocket"
  :version #.(with-open-file (f "VERSION") (string (read f)))
  :depends-on (:fiasco
               :hunchensocket)
  :serial t
  :components
  ((:file "hunchensocket-tests")))

