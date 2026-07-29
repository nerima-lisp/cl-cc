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
             (expect (cl-cc/target:target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification aarch64"
  (destructuring-bind (target verify) (list cl-cc/target:*aarch64-target* (lambda (target)
             (expect (cl-cc/target:target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification riscv64"
  (destructuring-bind (target verify) (list cl-cc/target:*riscv64-target* (lambda (target)
             (expect (cl-cc/target:target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-p-classification wasm32"
  (destructuring-bind (target verify) (list cl-cc/target:*wasm32-target* (lambda (target)
             (expect (cl-cc/target:target-64-bit-p target) :to-be-falsy)))
    (funcall verify target)))

(it-sequential "target-has-feature-p-cases x86-sysv"
  (destructuring-bind (target feature verify) (list cl-cc/target:*x86-64-target* :sysv-abi (lambda (target feature)
             (expect (cl-cc/target:target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases arm-aapcs"
  (destructuring-bind (target feature verify) (list cl-cc/target:*aarch64-target* :aapcs64 (lambda (target feature)
             (expect (cl-cc/target:target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases wasm-structured"
  (destructuring-bind (target feature verify) (list cl-cc/target:*wasm32-target* :structured-control-flow (lambda (target feature)
             (expect (cl-cc/target:target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases x86-no-wasm-feat"
  (destructuring-bind (target feature verify) (list cl-cc/target:*x86-64-target* :structured-control-flow (lambda (target feature)
             (expect (cl-cc/target:target-has-feature-p target feature) :to-be-falsy)))
    (funcall verify target feature)))

(it-sequential "target-has-feature-p-cases wasm-no-sysv"
  (destructuring-bind (target feature verify) (list cl-cc/target:*wasm32-target* :sysv-abi (lambda (target feature)
             (expect (cl-cc/target:target-has-feature-p target feature) :to-be-falsy)))
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

(it-sequential "target-op-legal-p-honors-explicit-illegal-entry"
  (let ((tgt (cl-cc/target:make-target-desc :name :legal-test)))
    (setf (gethash :fancy-op (cl-cc/target:target-legal-ops tgt)) nil)
    (expect (cl-cc/target:target-op-legal-p tgt :fancy-op) :to-be-falsy)
    ;; ops with no entry remain legal by default
    (expect (cl-cc/target:target-op-legal-p tgt :add) :to-be-truthy)))

(it-sequential "target-op-expand-invokes-registered-legalizer"
  (let ((tgt (cl-cc/target:make-target-desc :name :expand-test)))
    (setf (gethash :wide-mul (cl-cc/target:target-legal-ops tgt))
          (lambda (op dst srcs) (list (list :expanded op dst srcs))))
    (expect (cl-cc/target:target-op-expand tgt :wide-mul :d '(:a :b)) :to-equal '((:expanded :wide-mul :d (:a :b))))
    ;; ops legal by default (T entry) have no expansion function → NIL
    (expect (cl-cc/target:target-op-expand tgt :add :d '(:a :b)) :to-be-null)))
