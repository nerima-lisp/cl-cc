;;;; tests/unit/emit/x86-64-regs-tests.lisp
;;;; Coverage for src/emit/x86-64-regs.lisp:
;;;;   x86-64-red-zone-spill-p, vm-reg-to-x86, vm-reg-to-xmm,
;;;;   x86-64-compute-float-vregs, vm-const-to-integer,
;;;;   x86-64-double-float-bits, *vm-reg-map*, *phys-reg-to-x86-code*.

(in-package :cl-cc/test)



;;; ─── x86-64-red-zone-spill-p ─────────────────────────────────────────────

(it-sequential "x86-64-regs-red-zone-spill-leaf-within-limit-returns-true"
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p t 1) :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p t 16) :to-be-truthy))

(it-sequential "x86-64-regs-red-zone-spill-non-leaf-always-false"
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p nil 1) :to-be-falsy)
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p nil 16) :to-be-falsy))

(it-sequential "x86-64-regs-red-zone-spill-leaf-exceeds-limit-returns-false"
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p t 17) :to-be-falsy)
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p t 100) :to-be-falsy))

(it-sequential "x86-64-regs-red-zone-spill-zero-count-returns-false"
  (expect (cl-cc/codegen::x86-64-red-zone-spill-p t 0) :to-be-falsy))

(it-sequential "x86-64-stack-frame-packing-mixed-width-locals"
  (multiple-value-bind (layout frame-size)
      (cl-cc/codegen::x86-64-pack-stack-frame-locals
       '((flag 1 1) (wide 8 8) (word 4 4))
       :stack-alignment 8)
    (expect layout :to-equal '((wide . -8) (word . -12) (flag . -13)))
    (expect frame-size :to-equal 16)))

(it-sequential "x86-64-stack-frame-packing-accepts-plist-locals"
  (multiple-value-bind (layout frame-size)
      (cl-cc/codegen::x86-64-pack-stack-frame-locals
       '((:name :byte :size 1 :align 1)
         (:name :double :size 8 :align 8))
       :stack-alignment 16)
    (expect layout :to-equal '((:double . -8) (:byte . -9)))
    (expect frame-size :to-equal 16)))

;;; ─── vm-reg-to-x86 (no regalloc) ─────────────────────────────────────────

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r0"
  (destructuring-bind (vreg expected-code) (list :R0 0)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r1"
  (destructuring-bind (vreg expected-code) (list :R1 1)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r2"
  (destructuring-bind (vreg expected-code) (list :R2 2)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r3"
  (destructuring-bind (vreg expected-code) (list :R3 3)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r4"
  (destructuring-bind (vreg expected-code) (list :R4 6)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r5"
  (destructuring-bind (vreg expected-code) (list :R5 7)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r6"
  (destructuring-bind (vreg expected-code) (list :R6 8)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-naive-map r7"
  (destructuring-bind (vreg expected-code) (list :R7 9)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-x86 vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-x86-out-of-range-errors"
  (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-reg-to-x86-code* nil))
    (signals error (cl-cc/codegen::vm-reg-to-x86 :R99))))

;;; ─── vm-reg-to-xmm (no regalloc) ────────────────────────────────────────

(it-sequential "x86-64-regs-vm-reg-to-xmm-naive-map xmm0"
  (destructuring-bind (vreg expected-code) (list :R0 0)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-xmm vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-xmm-naive-map xmm1"
  (destructuring-bind (vreg expected-code) (list :R1 1)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-xmm vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-xmm-naive-map xmm2"
  (destructuring-bind (vreg expected-code) (list :R2 2)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-xmm vreg)) :to-be-truthy))))

(it-sequential "x86-64-regs-vm-reg-to-xmm-naive-map xmm7"
  (destructuring-bind (vreg expected-code) (list :R7 7)
    (let ((cl-cc/codegen::*current-regalloc* nil)
        (cl-cc/codegen::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (expect (= expected-code (cl-cc/codegen::vm-reg-to-xmm vreg)) :to-be-truthy))))

;;; ─── vm-const-to-integer ─────────────────────────────────────────────────

(it-sequential "x86-64-regs-vm-const-to-integer-cases nil"
  (destructuring-bind (val expected) (list nil 0)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

(it-sequential "x86-64-regs-vm-const-to-integer-cases t"
  (destructuring-bind (val expected) (list t 1)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

(it-sequential "x86-64-regs-vm-const-to-integer-cases zero"
  (destructuring-bind (val expected) (list 0 0)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

(it-sequential "x86-64-regs-vm-const-to-integer-cases pos"
  (destructuring-bind (val expected) (list 42 42)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

(it-sequential "x86-64-regs-vm-const-to-integer-cases neg"
  (destructuring-bind (val expected) (list -7 -7)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

(it-sequential "x86-64-regs-vm-const-to-integer-cases other"
  (destructuring-bind (val expected) (list 3.14 0)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer val)) :to-be-truthy)))

;;; ─── x86-64-double-float-bits ────────────────────────────────────────────

(it-sequential "x86-64-regs-double-float-bits-valid-64-bit-range"
  (let ((bits (cl-cc/codegen::x86-64-double-float-bits 1.0)))
    (expect (integerp bits) :to-be-truthy)
    (expect (>= bits 0) :to-be-truthy)
    (expect (< bits (expt 2 64)) :to-be-truthy)))

(it-sequential "x86-64-regs-double-float-bits-ieee754-values"
  (expect (= 0 (cl-cc/codegen::x86-64-double-float-bits 0.0)) :to-be-truthy)
  (expect (= #x3FF0000000000000 (cl-cc/codegen::x86-64-double-float-bits 1.0)) :to-be-truthy))

;;; ─── x86-64-compute-float-vregs ──────────────────────────────────────────

(it-sequential "x86-64-regs-compute-float-vregs-empty-returns-empty-table"
  (let ((result (cl-cc/codegen::x86-64-compute-float-vregs nil)))
    (expect (hash-table-p result) :to-be-truthy)
    (expect (= 0 (hash-table-count result)) :to-be-truthy)))

(it-sequential "x86-64-regs-compute-float-vregs-float-const-marks-register"
  (let* ((insts (list (cl-cc:make-vm-const :dst :R0 :value 3.14)))
         (result (cl-cc/codegen::x86-64-compute-float-vregs insts)))
    (expect (gethash :R0 result) :to-be-truthy)))

(it-sequential "x86-64-regs-compute-float-vregs-int-const-does-not-mark"
  (let* ((insts (list (cl-cc:make-vm-const :dst :R0 :value 42)))
         (result (cl-cc/codegen::x86-64-compute-float-vregs insts)))
    (expect (gethash :R0 result) :to-be-falsy)))

(it-sequential "x86-64-regs-compute-float-vregs-propagates-via-move"
  (let* ((insts (list (cl-cc:make-vm-const :dst :R0 :value 1.0)
                      (cl-cc:make-vm-move :dst :R1 :src :R0)))
         (result (cl-cc/codegen::x86-64-compute-float-vregs insts)))
    (expect (gethash :R0 result) :to-be-truthy)
    (expect (gethash :R1 result) :to-be-truthy)))

;;; ─── *vm-reg-map* and *phys-reg-to-x86-code* data checks ─────────────────

(it-sequential "x86-64-regs-vm-reg-map-covers-eight-vregs"
  (expect (= 8 (length cl-cc/codegen::*vm-reg-map*)) :to-be-truthy))

(it-sequential "x86-64-regs-phys-reg-to-x86-code-covers-fifteen-registers"
  (expect (= 15 (length cl-cc/codegen::*phys-reg-to-x86-code*)) :to-be-truthy))
