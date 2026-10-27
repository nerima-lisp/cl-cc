;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package :cl-cc/test)



;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

