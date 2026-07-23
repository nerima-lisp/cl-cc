;;;; packages/vm/tests/json-tests.lisp — FR-684 JSON reader/writer tests

(in-package :cl-cc/test)



(it-sequential "json-parse-object-array-null"
  (let ((json:*json-null* :null))
    (let ((obj (json:parse "{\"a\":1,\"b\":[true,false,null,\"x\"]}")))
      (expect (= 1 (gethash "a" obj)) :to-be-truthy)
      (expect (coerce (gethash "b" obj) 'list) :to-equal '(t nil :null "x")))))

(it-sequential "json-stringify-roundtrip"
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "name" table) "cl-cc"
          (gethash "ok" table) t
          (gethash "items" table) #(1 2 3))
    (let ((parsed (json:parse (json:stringify table))))
      (expect (gethash "name" parsed) :to-equal "cl-cc")
      (expect (gethash "ok" parsed) :to-be-truthy)
      (expect (coerce (gethash "items" parsed) 'list) :to-equal '(1 2 3)))))

(it-sequential "json-streaming-parse-and-stringify"
  (with-input-from-string (in " [1, 2, 3] ")
    (expect (coerce (json:parse-stream in) 'list) :to-equal '(1 2 3)))
  (let ((out (make-string-output-stream)))
    (json:stringify-stream '("a" "b") out)
    (expect (get-output-stream-string out) :to-equal "[\"a\",\"b\"]")))
