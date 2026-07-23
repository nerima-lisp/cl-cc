(in-package :cl-cc/test)


(it-sequential "runtime-ubsan-heap-ref-rejects-non-integer-index"
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* t))
      (signals error (cl-cc/runtime:rt-heap-ref heap :bad-index)))))

(it-sequential "runtime-ubsan-heap-set-rejects-negative-index"
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* t))
      (signals error (cl-cc/runtime:rt-heap-set heap -1 42)))))

(it-sequential "runtime-ubsan-heap-access-works-when-disabled"
  (let ((heap (cl-cc/runtime:make-rt-heap)))
    (let ((cl-cc/runtime:*rt-ubsan-enabled* nil))
      (cl-cc/runtime:rt-heap-set heap 0 123)
      (expect (= 123 (cl-cc/runtime:rt-heap-ref heap 0)) :to-be-truthy))))
