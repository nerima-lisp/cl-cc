;;;; tests/unit/bytecode/encode-ops-objects-tests.lisp
;;;; Coverage for src/bytecode/encode-ops-objects.lisp:
;;;;   closure/upvalue ops, object/slot ops, collections,
;;;;   type-check predicates, multiple-values, exception ops.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defmacro assert-opcode= (expected-op word)
  `(expect (= ,expected-op (ldb (byte 8 24) ,word)) :to-be-truthy))

(defmacro assert-dst= (expected word)
  `(expect (= ,expected (ldb (byte 8 16) ,word)) :to-be-truthy))

(defmacro assert-src1= (expected word)
  `(expect (= ,expected (ldb (byte 8  8) ,word)) :to-be-truthy))

(defmacro assert-src2= (expected word)
  `(expect (= ,expected (ldb (byte 8  0) ,word)) :to-be-truthy))

;;; ─── Closure & Upvalue Encoders ──────────────────────────────────────────

(it-sequential "closure-upvalue-encoders make-closure"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-make-closure  1 2 3) cl-cc/bytecode:+op-make-closure+)
    (assert-opcode= expected-op word)))

(it-sequential "closure-upvalue-encoders get-upvalue"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-get-upvalue   3 4) cl-cc/bytecode:+op-get-upvalue+)
    (assert-opcode= expected-op word)))

(it-sequential "closure-upvalue-encoders set-upvalue"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-set-upvalue   4 5) cl-cc/bytecode:+op-set-upvalue+)
    (assert-opcode= expected-op word)))

(it-sequential "closure-upvalue-encoders close-upvalue"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-close-upvalue 6) cl-cc/bytecode:+op-close-upvalue+)
    (assert-opcode= expected-op word)))

(it-sequential "encode-make-closure-field-layout"
  (let ((w (cl-cc/bytecode:encode-make-closure 1 2 3)))
    (assert-dst=  1 w)
    (assert-src1= 2 w)
    (assert-src2= 3 w)))

(it-sequential "encode-get-upvalue-field-layout"
  (let ((w (cl-cc/bytecode:encode-get-upvalue 5 7)))
    (assert-dst=  5 w)
    (assert-src1= 7 w)))

;;; ─── Object/Slot Encoders ────────────────────────────────────────────────

(it-sequential "object-slot-encoders-opcodes get-slot"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-get-slot      1 2 3) cl-cc/bytecode:+op-get-slot+)
    (assert-opcode= expected-op word)))

(it-sequential "object-slot-encoders-opcodes set-slot"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-set-slot      2 3 4) cl-cc/bytecode:+op-set-slot+)
    (assert-opcode= expected-op word)))

(it-sequential "object-slot-encoders-opcodes get-global"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-get-global    5 6) cl-cc/bytecode:+op-get-global+)
    (assert-opcode= expected-op word)))

(it-sequential "object-slot-encoders-opcodes set-global"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-set-global    6 7) cl-cc/bytecode:+op-set-global+)
    (assert-opcode= expected-op word)))

(it-sequential "object-slot-encoders-opcodes make-instance"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-make-instance 1 2 3) cl-cc/bytecode:+op-make-instance+)
    (assert-opcode= expected-op word)))

;;; ─── Collection Encoders ─────────────────────────────────────────────────

(it-sequential "collection-encoders-opcodes cons"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-cons        1 2 3) cl-cc/bytecode:+op-cons+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes car"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-car         1 2) cl-cc/bytecode:+op-car+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes cdr"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-cdr         1 2) cl-cc/bytecode:+op-cdr+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes make-vector"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-make-vector 1 2 3) cl-cc/bytecode:+op-make-vector+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes vector-ref"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-vector-ref  1 2 3) cl-cc/bytecode:+op-vector-ref+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes vector-set"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-vector-set  1 2 3) cl-cc/bytecode:+op-vector-set+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes make-hash"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-make-hash   1 4) cl-cc/bytecode:+op-make-hash+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes hash-ref"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-hash-ref    1 2 3) cl-cc/bytecode:+op-hash-ref+)
    (assert-opcode= expected-op word)))

(it-sequential "collection-encoders-opcodes hash-set"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-hash-set    1 2 3) cl-cc/bytecode:+op-hash-set+)
    (assert-opcode= expected-op word)))

(it-sequential "encode-cons-field-layout"
  (let ((w (cl-cc/bytecode:encode-cons 0 1 2)))
    (assert-dst=  0 w)
    (assert-src1= 1 w)
    (assert-src2= 2 w)))

;;; ─── Type Check Predicates ───────────────────────────────────────────────

(it-sequential "type-predicate-encoders-opcodes type-check"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-type-check 1 2 3) cl-cc/bytecode:+op-type-check+)
    (assert-opcode= expected-op word)))

(it-sequential "type-predicate-encoders-opcodes fixnump"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-fixnump    1 2) cl-cc/bytecode:+op-fixnump+)
    (assert-opcode= expected-op word)))

(it-sequential "type-predicate-encoders-opcodes consp"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-consp      1 2) cl-cc/bytecode:+op-consp+)
    (assert-opcode= expected-op word)))

(it-sequential "type-predicate-encoders-opcodes symbolp"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-symbolp    1 2) cl-cc/bytecode:+op-symbolp+)
    (assert-opcode= expected-op word)))

(it-sequential "type-predicate-encoders-opcodes functionp"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-functionp  1 2) cl-cc/bytecode:+op-functionp+)
    (assert-opcode= expected-op word)))

(it-sequential "type-predicate-encoders-opcodes stringp"
  (destructuring-bind (word expected-op) (list (cl-cc/bytecode:encode-stringp    1 2) cl-cc/bytecode:+op-stringp+)
    (assert-opcode= expected-op word)))

;;; ─── Multiple Values ─────────────────────────────────────────────────────

(it-sequential "encode-values-places-nvals-in-dst"
  (let ((w (cl-cc/bytecode:encode-values 3)))
    (assert-opcode= cl-cc/bytecode:+op-values+ w)
    (assert-dst= 3 w)))

(it-sequential "encode-recv-values-places-nvals-in-dst"
  (let ((w (cl-cc/bytecode:encode-recv-values 2)))
    (assert-opcode= cl-cc/bytecode:+op-recv-values+ w)
    (assert-dst= 2 w)))

;;; ─── Exception Handling ──────────────────────────────────────────────────

(it-sequential "encode-push-handler-opcode"
  (let ((w (cl-cc/bytecode:encode-push-handler 10 0)))
    (assert-opcode= cl-cc/bytecode:+op-push-handler+ w)))

(it-sequential "encode-pop-handler-is-zero-operands"
  (let ((w (cl-cc/bytecode:encode-pop-handler)))
    (assert-opcode= cl-cc/bytecode:+op-pop-handler+ w)
    (expect (= 0 (ldb (byte 24 0) w)) :to-be-truthy)))

(it-sequential "encode-signal-opcode"
  (let ((w (cl-cc/bytecode:encode-signal 5)))
    (assert-opcode= cl-cc/bytecode:+op-signal+ w)
    (assert-dst= 5 w)))

(it-sequential "encode-push-unwind-opcode"
  (let ((w (cl-cc/bytecode:encode-push-unwind 100)))
    (assert-opcode= cl-cc/bytecode:+op-push-unwind+ w)))
