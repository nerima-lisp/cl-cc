;;;; tests/unit/emit/target-tests.lisp — Target Descriptor Tests
;;;;
;;;; Tests for target-desc struct, target registry, and utility functions.

(in-package :cl-cc/test)




;;; ─── Target Descriptor Struct ───────────────────────────────────────────────

(it-sequential "target-x86-64-basics"
  (let ((t1 cl-cc/target:*x86-64-target*))
    (expect (cl-cc/target:target-name t1) :to-be :x86-64)
    (expect (cl-cc/target:target-word-size t1) :to-equal 8)
    (expect (cl-cc/target:target-endianness t1) :to-be :little)
    (expect (cl-cc/target:target-gpr-count t1) :to-equal 16)))

(it-sequential "target-aarch64-basics"
  (let ((t1 cl-cc/target:*aarch64-target*))
    (expect (cl-cc/target:target-name t1) :to-be :aarch64)
    (expect (cl-cc/target:target-gpr-count t1) :to-equal 31)
    (expect (cl-cc/target:target-ret-reg t1) :to-be :x0)))

(it-sequential "target-riscv64-basics"
  (let ((t1 cl-cc/target:*riscv64-target*))
    (expect (cl-cc/target:target-name t1) :to-be :riscv64)
    (expect (cl-cc/target:target-gpr-count t1) :to-equal 32)
    (expect (cl-cc/target:target-ret-reg t1) :to-be :a0)))

(it-sequential "target-wasm32-stack-machine"
  (let ((t1 cl-cc/target:*wasm32-target*))
    (expect (cl-cc/target:target-name t1) :to-be :wasm32)
    (expect (cl-cc/target:target-word-size t1) :to-equal 4)
    (expect (cl-cc/target:target-gpr-count t1) :to-equal 0)
    (expect (cl-cc/target:target-arg-regs t1) :to-be-null)
    (expect (cl-cc/target:target-ret-reg t1) :to-be-null)))

;;; ─── Target Registry ────────────────────────────────────────────────────────

(it-sequential "target-registry-find"
  (let ((x86 (cl-cc/target:find-target :x86-64)))
    (expect (cl-cc/target:target-desc-p x86) :to-be-truthy)
    (expect (cl-cc/target:target-name x86) :to-be :x86-64))
  (expect (cl-cc/target:target-desc-p (cl-cc/target:find-target :aarch64)) :to-be-truthy)
  (expect (cl-cc/target:target-desc-p (cl-cc/target:find-target :riscv64)) :to-be-truthy)
  (expect (cl-cc/target:target-desc-p (cl-cc/target:find-target :wasm32)) :to-be-truthy)
  (expect (cl-cc/target:find-target :pdp-11) :to-be-null))

;;; ─── Target Utility Functions ───────────────────────────────────────────────

(it-sequential "target-64-bit-p-classification x86-64"
  (destructuring-bind (target verify) (list cl-cc/target:*x86-64-target* (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification aarch64"
  (destructuring-bind (target verify) (list cl-cc/target:*aarch64-target* (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification riscv64"
  (destructuring-bind (target verify) (list cl-cc/target:*riscv64-target* (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification wasm32"
  (destructuring-bind (target verify) (list cl-cc/target:*wasm32-target* (lambda (target)
             (assert-false (cl-cc/target:target-64-bit-p target))))
    (funcall verify target)))

(it-sequential "target-has-feature-p-cases x86-sysv"
  (destructuring-bind (target feature verify) (list cl-cc/target:*x86-64-target* :sysv-abi (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases arm-aapcs"
  (destructuring-bind (target feature verify) (list cl-cc/target:*aarch64-target* :aapcs64 (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases wasm-structured"
  (destructuring-bind (target feature verify) (list cl-cc/target:*wasm32-target* :structured-control-flow (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases x86-no-wasm-feat"
  (destructuring-bind (target feature verify) (list cl-cc/target:*x86-64-target* :structured-control-flow (lambda (target feature)
             (assert-false (cl-cc/target:target-has-feature-p target feature))))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases wasm-no-sysv"
  (destructuring-bind (target feature verify) (list cl-cc/target:*wasm32-target* :sysv-abi (lambda (target feature)
             (assert-false (cl-cc/target:target-has-feature-p target feature))))
    (funcall verify target feature)))

(it-sequential "target-allocatable-regs-behavior"
  (let ((alloc (cl-cc/target:target-allocatable-regs cl-cc/target:*x86-64-target*)))
    (expect (member :rsp alloc) :to-be-falsy)
    (expect (member :r11 alloc) :to-be-falsy)
    (expect (member :rax alloc) :to-be-truthy))
  (expect (cl-cc/target:target-allocatable-regs cl-cc/target:*wasm32-target*) :to-be-null))

(it-sequential "target-caller-saved-subset-of-allocatable"
  (let ((caller (cl-cc/target:target-caller-saved cl-cc/target:*x86-64-target*))
        (alloc  (cl-cc/target:target-allocatable-regs cl-cc/target:*x86-64-target*)))
    (expect (every (lambda (r) (member r alloc)) caller) :to-be-truthy)))

(it-sequential "target-reg-index-lookup rax"
  (destructuring-bind (expected reg) (list 0 :rax)
    (expect (cl-cc/target:target-reg-index cl-cc/target:*x86-64-target* reg) :to-equal expected)))

(it-sequential "target-reg-index-lookup rdi"
  (destructuring-bind (expected reg) (list 7 :rdi)
    (expect (cl-cc/target:target-reg-index cl-cc/target:*x86-64-target* reg) :to-equal expected)))

(it-sequential "target-reg-index-lookup unknown"
  (destructuring-bind (expected reg) (list nil :r99)
    (expect (cl-cc/target:target-reg-index cl-cc/target:*x86-64-target* reg) :to-equal expected)))

(it-sequential "target-op-legal-default"
  (expect (cl-cc/target:target-op-legal-p cl-cc/target:*x86-64-target* :add) :to-be-truthy))
