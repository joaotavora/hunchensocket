;; -*- Lisp -*-

(ql:quickload :hunchensocket-tests)
(handler-case
    (unless (fiasco:run-package-tests :package :hunchensocket-tests)
      (error "test(s) failed"))
  (error (e)
    (format t ";; ~A~%" e)
    (sb-ext:exit :code 1)))
(sb-ext:exit :code 0)
