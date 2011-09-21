;;;; Hunchensocket - websocket.lisp Hunchensocket tests
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
