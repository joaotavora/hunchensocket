;;;; Hunchensocket - websocket.lisp Hunchensocket tests
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

(in-package :hunchensocket)

(use-package (list :hu.dwim.stefil :flexi-streams))

(defsuite all)
(in-suite all)
(defsuite hunchensocket)
(in-suite hunchensocket)

(defvar *input-fixture*)
(defixture input-fixture
  (let ((*input-fixture* (make-in-memory-input-stream (string-to-octets "foobarbazbop"))))
    (-body-)))

(deftest test-integer-octets-32be ()
  (mapc #'(lambda (number octets)
            (is (equalp octets (integer-octets-32be number))))
        (list 123456789 987654321 13579)
        (list #(7 91 205 21) #(58 222 104 177) #(0 0 53 11))))

(deftest read-bytes-array ()
  (with-fixture input-fixture
    (is (string= "foobarba" (octets-to-string (read-bytes-array *input-fixture* 8))))))

; examples from hybi-00
(deftest test-websocket-keyhash ()
  (flet ((formatting-keyhash (key)
           (mapcar #'(lambda (number)
                       (format nil "0x~2,'0x" number))
                   (coerce (websocket-keyhash key) 'list))))
   (is (equal (list "0x36" "0x09" "0x65" "0x65")
              (formatting-keyhash "3e6b263  4 17 80")))
   (is (equal (list "0x0A" "0xB9" "0x67" "0x33")
              (formatting-keyhash "17  9 G`ZD9   2 2b 7X 3 /r90")))))
