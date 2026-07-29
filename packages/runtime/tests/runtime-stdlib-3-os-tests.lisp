;;;; runtime-stdlib-3-os-tests.lisp — Runtime OS/thread/signal FR tests

(in-package :cl-cc/test)

;; rt-shell forks; forking while parallel test workers hold heap/malloc
;; locks deadlocks the child before exec on macOS. Run it serially.

(it-sequential "rt-process-management-shell-output"
  (expect (cl-cc/runtime:rt-shell "printf hello") :to-equal "hello"))


(it-sequential "rt-stackmap-compression-roundtrip"
  (let* ((slots '((8 . :object) (24 . :fixnum) (32 . :object)))
         (compressed (cl-cc/runtime:rt-compress-stackmap-slots slots)))
    (expect (cl-cc/runtime:rt-decompress-stackmap-slots compressed) :to-equal slots)
    (expect (search ".gc_map" (cl-cc/runtime:rt-gc-map-section-documentation)) :to-be-truthy)
    (cl-cc/runtime:rt-emit-gc-safepoint :kind :test :frame-id :rt-test-frame :live-slots slots)
    (expect (gethash :rt-test-frame cl-cc/runtime::*rt-gc-stackmap-table*) :to-be-truthy)))
