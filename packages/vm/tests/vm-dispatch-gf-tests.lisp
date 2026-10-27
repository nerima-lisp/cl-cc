;;;; tests/unit/vm/vm-dispatch-gf-tests.lisp
;;;; Unit tests for EQL specializer helpers in src/vm/vm-dispatch-gf.lisp.
;;;;
;;;; Covers: %eql-specializer-p, %eql-specializer-matches-p,
;;;;   %vm-extract-eql-specializer-keys.
;;;;
;;;; vm-classify-arg and vm-generic-function-p are already covered in
;;;; vm-dispatch-tests.lisp.

(in-package :cl-cc/test)

;;; ─── %eql-specializer-p ──────────────────────────────────────────────────

;;; ─── %eql-specializer-matches-p ──────────────────────────────────────────

;;; ─── %vm-extract-eql-specializer-keys ───────────────────────────────────

;;; ─── *method-combination-operators* data table ───────────────────────────

;;; ─── %resolve-combination-operator ──────────────────────────────────────

;;; ─── %vm-dispatch-key-collect (extracted combination generator) ──────────

