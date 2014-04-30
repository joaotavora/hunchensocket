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
