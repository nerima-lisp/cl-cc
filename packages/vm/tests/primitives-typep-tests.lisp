;;;; tests/unit/vm/primitives-typep-tests.lisp — VM vm-typep Tests
;;;
;;; Tests for vm-typep, vm-typep-check, and *vm-compound-type-handlers*.
;;; Depends on %run-unary-inst-with defined in primitives-tests.lisp.

(in-package :cl-cc/test)

(defun %run-vm-typep (value type-name)
  "Run vm-typep against VALUE and TYPE-NAME, returning the destination register."
  (%run-unary-inst-with
   (lambda (src)
     (cl-cc:make-vm-typep :dst 0 :src src :type-name type-name))
   value))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-typep General Predicate (primitive and compound)
;;; ═══════════════════════════════════════════════════════════════════════════

(it-sequential "prim-typep-structural-refinement-object"
  (let ((refined-int (cl-cc/type:make-type-refinement :base type-int :predicate #'plusp)))
    (expect (= 1 (%run-vm-typep 42 refined-int)) :to-be-truthy)
    (expect (= 0 (%run-vm-typep -1 refined-int)) :to-be-truthy)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-typep-check and *vm-primitive-type-predicates* table
;;; ═══════════════════════════════════════════════════════════════════════════

