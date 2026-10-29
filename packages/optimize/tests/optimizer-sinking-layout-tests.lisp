;;;; tests/unit/optimize/optimizer-sinking-layout-tests.lisp
;;;; Unit tests for hot-cold layout, SLP vectorization, function outlining,
;;;; if-conversion, and code-sinking optimization passes.
;;;;
;;;; Covers: opt-pass-hot-cold-layout, opt-pass-slp-vectorize,
;;;;   opt-pass-function-outlining, opt-pass-if-conversion, opt-pass-code-sinking.

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::defsuite cl-cc/test::cl-cc-optimizer-sinking-layout-tests-suite
    :parent cl-cc/test::cl-cc-integration-suite)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-optimizer-sinking-layout-tests-suite))

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun %sinking-layout-test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm:vm-label)
                      (equal (cl-cc/vm:vm-name inst) label-name)))
               instructions))

(defun %sinking-layout-test-jump-zero-position (instructions target-label)
  "Return the position of a vm-jump-zero targeting TARGET-LABEL, or NIL."
  (position-if (lambda (inst)
                  (and (typep inst 'cl-cc/vm:vm-jump-zero)
                       (equal (cl-cc/vm:vm-label-name inst) target-label)))
                instructions))

(defun %make-slp-array-map (&key (op :add))
  "Build a four-lane straight-line scalar array map for SLP tests."
  (append
   (loop for i below 4
         collect (make-vm-const :dst (intern (format nil "I~D" i) :keyword) :value i))
   (loop for i below 4
         for idx = (intern (format nil "I~D" i) :keyword)
         for lhs = (intern (format nil "A~D" i) :keyword)
         for rhs = (intern (format nil "B~D" i) :keyword)
         for val = (intern (format nil "V~D" i) :keyword)
         append (list (cl-cc/vm:make-vm-aref :dst lhs :array-reg :array-a :index-reg idx)
                      (cl-cc/vm:make-vm-aref :dst rhs :array-reg :array-b :index-reg idx)
                      (ecase op
                        (:add (make-vm-add :dst val :lhs lhs :rhs rhs))
                        (:logxor (cl-cc/vm:make-vm-logxor :dst val :lhs lhs :rhs rhs)))
                      (cl-cc/vm:make-vm-aset :array-reg :array-c :index-reg idx :val-reg val)))))

;;; ─── opt-pass-function-outlining ──────────────────────────────────────────

;;; ─── opt-pass-slp-vectorize ───────────────────────────────────────────────

;;; ─── opt-pass-if-conversion ───────────────────────────────────────────────

;;; ─── opt-pass-hot-cold-layout ─────────────────────────────────────────────

;;; ─── opt-pass-code-sinking ────────────────────────────────────────────────

