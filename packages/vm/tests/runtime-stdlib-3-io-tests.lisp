;;;; runtime-stdlib-3-io-tests.lisp — FR-959/965/1051/1088/1100/1042

(in-package :cl-cc/test)



(it-sequential "stdlib3-external-format-utf8-roundtrip-and-bom"
  (let* ((text "héλ")
         (bytes (cl-cc/vm:vm-encode-string text :utf-8))
         (bom-bytes (concatenate '(vector (unsigned-byte 8)) #(239 187 191) bytes)))
    (expect (typep bytes '(vector (unsigned-byte 8))) :to-be-truthy)
    (expect (cl-cc/vm:vm-decode-bytes bytes :utf-8) :to-equal text)
    (expect (cl-cc/vm:vm-decode-bytes bom-bytes :utf-8) :to-equal text)))

(it-sequential "stdlib3-external-format-ascii-error-modes"
  (expect (equalp #(65 63) (cl-cc/vm:vm-encode-string "Aé" :ascii :error-mode :replace)) :to-be-truthy)
  (expect (cl-cc/vm:vm-decode-bytes #(65 255) :ascii :error-mode :ignore) :to-equal "A"))

(it-sequential "stdlib3-stream-external-format-side-table"
  (let ((stream (make-string-output-stream)))
    (expect (cl-cc/vm:vm-stream-external-format stream) :to-equal :utf-8)
    (expect (cl-cc/vm:vm-set-stream-external-format stream :utf-16) :to-equal :utf-16le)
    (expect (cl-cc/vm:vm-stream-external-format stream) :to-equal :utf-16le)))

(it-sequential "stdlib3-format-p-and-w-directives"
  (expect (cl-cc/vm::%vm-format-native "~D file~:P" '(1)) :to-equal "1 file")
  (expect (cl-cc/vm::%vm-format-native "~D file~:P" '(2)) :to-equal "2 files")
  (expect (cl-cc/vm::%vm-format-native "~D stor~:@P" '(1)) :to-equal "1 story")
  (expect (cl-cc/vm::%vm-format-native "~D stor~:@P" '(2)) :to-equal "2 stories")
  (expect (cl-cc/vm::%vm-format-native "~W" '((:a 1))) :to-equal "(:A 1)"))

(it-sequential "stdlib3-print-control-variables"
  (let ((cl-cc/vm:*print-case* :downcase)
        (cl-cc/vm:*print-base* 16)
        (cl-cc/vm:*print-radix* t)
        (cl-cc/vm:*print-array* nil))
    (expect (cl-cc/vm::vm-write-object-to-string 'foo :escape t) :to-equal "foo")
    (expect (string-downcase (cl-cc/vm::vm-write-object-to-string 16 :escape t)) :to-equal "#x10")
    (expect (search "#<" (cl-cc/vm::vm-write-object-to-string #(1 2) :escape t)) :to-be-truthy)))

(it-sequential "stdlib3-ryu-float-to-string"
  (expect (cl-cc/vm:vm-float-to-string 1.5d0) :to-equal "1.5")
  (expect (search "e" (cl-cc/vm:vm-float-to-string 1000.0d0 :mode :exponential)) :to-be-truthy)
  (expect (cl-cc/vm:vm-float-to-string sb-ext:double-float-positive-infinity) :to-equal "+inf.0")
  (expect (cl-cc/vm:vm-float-to-string (sb-kernel:make-double-float #x7FF80000 0)) :to-equal "+nan.0"))

(it-sequential "stdlib3-terminal-ansi-and-size"
  (let ((out (make-string-output-stream)))
    (cl-cc/vm:vm-ansi-color out :red)
    (cl-cc/vm:vm-ansi-reset out)
    (expect (get-output-stream-string out) :to-equal (format nil "~C[31m~C[0m" #\Esc #\Esc)))
  (multiple-value-bind (cols rows) (cl-cc/vm:vm-terminal-size)
    (expect (plusp cols) :to-be-truthy)
    (expect (plusp rows) :to-be-truthy)))

(it-sequential "stdlib3-fasl-roundtrip"
  (let ((stream (make-string-output-stream))
        (object '(:answer 42 :items (a b c))))
    (cl-cc/vm:vm-write-to-fasl object stream)
    (expect (cl-cc/vm:vm-read-from-fasl
                   (make-string-input-stream (get-output-stream-string stream))) :to-equal object)))
