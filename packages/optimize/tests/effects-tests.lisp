;;;; tests/unit/optimize/effects-tests.lisp — Effect-Kind System Tests
;;;
;;; Tests for vm-inst-effect-kind, opt-inst-pure-p, opt-inst-dce-eligible-p,
;;; and opt-inst-cse-eligible-p (Phase 0 of optimizer modernization).

(in-package :cl-cc/test)

;;; ─── vm-inst-effect-kind ─────────────────────────────────────────────────

(it-sequential "effect-kind-pure vm-const"
  (destructuring-bind (inst) (list (make-vm-const :dst :r0 :value 42))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-pure vm-move"
  (destructuring-bind (inst) (list (make-vm-move  :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-pure vm-add"
  (destructuring-bind (inst) (list (make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-pure vm-sub"
  (destructuring-bind (inst) (list (make-vm-sub   :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-pure vm-mul"
  (destructuring-bind (inst) (list (make-vm-mul   :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-control vm-ret"
  (destructuring-bind (inst) (list (make-vm-ret  :reg :r0))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :control)))

(it-sequential "effect-kind-control vm-jump"
  (destructuring-bind (inst) (list (make-vm-jump :label "L0"))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :control)))

(it-sequential "effect-kind-call-unknown"
  (expect (cl-cc/optimize:vm-inst-effect-kind
                        (make-vm-call :dst :r0 :func :r1 :args nil)) :to-be :unknown))

(it-sequential "opt-infer-transitive-function-purity-acyclic"
  (let* ((pure-leaf-closure (make-vm-closure :dst :r10 :label "pure-leaf" :params '(:r0)))
         (pure-leaf-label   (make-vm-label :name "pure-leaf"))
         (pure-leaf-body    (make-vm-add :dst :r1 :lhs :r0 :rhs :r0))
         (pure-leaf-ret     (make-vm-ret :reg :r1))
         (pure-wrapper-closure (make-vm-closure :dst :r11 :label "pure-wrapper" :params '(:r2)))
         (pure-wrapper-label   (make-vm-label :name "pure-wrapper"))
         (pure-wrapper-ref     (make-vm-func-ref :dst :r12 :label "pure-leaf"))
         (pure-wrapper-call    (make-vm-call :dst :r13 :func :r12 :args '(:r2)))
         (pure-wrapper-ret     (make-vm-ret :reg :r13))
         (impure-leaf-closure  (make-vm-closure :dst :r20 :label "impure-leaf" :params '(:r3)))
         (impure-leaf-label    (make-vm-label :name "impure-leaf"))
         (impure-leaf-write    (make-vm-set-global :src :r3 :name 'x))
         (impure-leaf-ret      (make-vm-ret :reg :r3))
         (impure-wrapper-closure (make-vm-closure :dst :r21 :label "impure-wrapper" :params '(:r4)))
         (impure-wrapper-label   (make-vm-label :name "impure-wrapper"))
         (impure-wrapper-ref     (make-vm-func-ref :dst :r22 :label "impure-leaf"))
         (impure-wrapper-call    (make-vm-call :dst :r23 :func :r22 :args '(:r4)))
         (impure-wrapper-ret     (make-vm-ret :reg :r23))
         (insts (list pure-leaf-closure pure-leaf-label pure-leaf-body pure-leaf-ret
                      pure-wrapper-closure pure-wrapper-label pure-wrapper-ref pure-wrapper-call pure-wrapper-ret
                      impure-leaf-closure impure-leaf-label impure-leaf-write impure-leaf-ret
                      impure-wrapper-closure impure-wrapper-label impure-wrapper-ref impure-wrapper-call impure-wrapper-ret))
         (pure (cl-cc/optimize::opt-infer-transitive-function-purity insts)))
    (expect (gethash "pure-leaf" pure) :to-be-truthy)
    (expect (gethash "pure-wrapper" pure) :to-be-truthy)
    (expect (gethash "impure-leaf" pure) :to-be-falsy)
    (expect (gethash "impure-wrapper" pure) :to-be-falsy)))

(it-sequential "opt-infer-transitive-function-purity-recursive-conservative"
  (let* ((loop-closure (make-vm-closure :dst :r30 :label "loop" :params '(:r0)))
         (loop-label   (make-vm-label :name "loop"))
         (self-ref     (make-vm-func-ref :dst :r31 :label "loop"))
         (self-call    (make-vm-call :dst :r32 :func :r31 :args '(:r0)))
         (loop-ret     (make-vm-ret :reg :r32))
         (pure (cl-cc/optimize::opt-infer-transitive-function-purity
                (list loop-closure loop-label self-ref self-call loop-ret))))
    (expect (gethash "loop" pure) :to-be-falsy)))

(it-sequential "pure-function-memo-table-behavior pure-label"
  (destructuring-bind (label-name register-p expected-value) (list "pure-f" t 3)
    (let ((memo (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pure (make-hash-table :test #'equal)))
    (when register-p (setf (gethash label-name pure) t))
    (cl-cc/optimize::opt-pure-function-memo-put memo pure label-name '(1 2) 3)
    (multiple-value-bind (value found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pure label-name '(1 2))
      (if register-p
          (progn (expect found-p :to-be-truthy)
                 (expect value :to-equal expected-value))
          (progn (expect found-p :to-be-falsy)
                 (expect value :to-be-falsy)))))))

(it-sequential "pure-function-memo-table-behavior impure-label"
  (destructuring-bind (label-name register-p expected-value) (list "impure-f" nil nil)
    (let ((memo (cl-cc/optimize::opt-make-pure-function-memo-table))
        (pure (make-hash-table :test #'equal)))
    (when register-p (setf (gethash label-name pure) t))
    (cl-cc/optimize::opt-pure-function-memo-put memo pure label-name '(1 2) 3)
    (multiple-value-bind (value found-p)
        (cl-cc/optimize::opt-pure-function-memo-get memo pure label-name '(1 2))
      (if register-p
          (progn (expect found-p :to-be-truthy)
                 (expect value :to-equal expected-value))
          (progn (expect found-p :to-be-falsy)
                 (expect value :to-be-falsy)))))))

;;; ─── opt-inst-pure-p ─────────────────────────────────────────────────────

(it-sequential "opt-pure-arithmetic add"
  (destructuring-bind (inst) (list (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-arithmetic sub"
  (destructuring-bind (inst) (list (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-arithmetic mul"
  (destructuring-bind (inst) (list (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-arithmetic neg"
  (destructuring-bind (inst) (list (make-vm-neg :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-arithmetic inc"
  (destructuring-bind (inst) (list (make-vm-inc :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-arithmetic dec"
  (destructuring-bind (inst) (list (make-vm-dec :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-comparison lt"
  (destructuring-bind (inst) (list (make-vm-lt :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-comparison gt"
  (destructuring-bind (inst) (list (make-vm-gt :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-comparison le"
  (destructuring-bind (inst) (list (make-vm-le :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-comparison ge"
  (destructuring-bind (inst) (list (make-vm-ge :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-type-predicates null-p"
  (destructuring-bind (inst) (list (make-vm-null-p   :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-type-predicates cons-p"
  (destructuring-bind (inst) (list (make-vm-cons-p   :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-pure-type-predicates number-p"
  (destructuring-bind (inst) (list (make-vm-number-p :dst :r0 :src :r1))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-truthy)))

(it-sequential "opt-not-pure-io print"
  (destructuring-bind (inst) (list (make-vm-print :reg :r0))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-falsy)))

(it-sequential "opt-not-pure-io call"
  (destructuring-bind (inst) (list (make-vm-call  :dst :r0 :func :r1 :args nil))
    (expect (cl-cc/optimize:opt-inst-pure-p inst) :to-be-falsy)))

;;; ─── opt-inst-dce-eligible-p ─────────────────────────────────────────────

(it-sequential "dce-eligible-simple pure-add"
  (destructuring-bind (inst) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-truthy)))

(it-sequential "dce-eligible-simple alloc-cons"
  (destructuring-bind (inst) (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-truthy)))

(it-sequential "dce-not-eligible-simple io-print"
  (destructuring-bind (inst) (list (make-vm-print      :reg :r0))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-falsy)))

(it-sequential "dce-not-eligible-simple set-global"
  (destructuring-bind (inst) (list (make-vm-set-global :src :r0 :name 'x))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-falsy)))

;;; ─── opt-inst-cse-eligible-p ─────────────────────────────────────────────

(it-sequential "cse-eligibility"
  (expect (cl-cc/optimize:opt-inst-cse-eligible-p
                  (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2)) :to-be-truthy)
  (expect (cl-cc/optimize:opt-inst-cse-eligible-p
                  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)) :to-be-falsy))

;;; ─── Effect Kind: IO / Alloc / Read-Only / Write-Global ─────────────────

(it-sequential "effect-kind-io vm-print"
  (destructuring-bind (inst) (list (make-vm-print      :reg :r0))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :io)))

(it-sequential "effect-kind-io vm-format"
  (destructuring-bind (inst) (list (make-vm-format-inst :dst :r0 :fmt "" :arg-regs nil))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :io)))

(it-sequential "effect-kind-alloc vm-cons"
  (destructuring-bind (inst) (list (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :alloc)))

(it-sequential "effect-kind-alloc vm-make-string"
  (destructuring-bind (inst) (list (make-vm-make-string  :dst :r0 :src :r1 :char nil))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :alloc)))

(it-sequential "effect-kind-read-only vm-car"
  (destructuring-bind (inst) (list (make-vm-car        :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :read-only)))

(it-sequential "effect-kind-read-only vm-cdr"
  (destructuring-bind (inst) (list (make-vm-cdr        :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :read-only)))

(it-sequential "effect-kind-read-only vm-get-global"
  (destructuring-bind (inst) (list (make-vm-get-global  :dst :r0 :name 'x))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :read-only)))

(it-sequential "effect-kind-write-global vm-set-global"
  (destructuring-bind (inst) (list (make-vm-set-global :src :r0 :name 'x))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :write-global)))

(it-sequential "effect-kind-write-global vm-rplaca"
  (destructuring-bind (inst) (list (make-vm-rplaca     :cons :r0 :val :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :write-global)))

(it-sequential "effect-kind-write-global vm-rplacd"
  (destructuring-bind (inst) (list (make-vm-rplacd     :cons :r0 :val :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :write-global)))

(it-sequential "effect-kind-bitwise-pure vm-not"
  (destructuring-bind (inst) (list (make-vm-not    :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-bitwise-pure vm-lognot"
  (destructuring-bind (inst) (list (make-vm-lognot :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

(it-sequential "effect-kind-bitwise-pure vm-bswap"
  (destructuring-bind (inst) (list (make-vm-bswap  :dst :r0 :src :r1))
    (expect (cl-cc/optimize:vm-inst-effect-kind inst) :to-be :pure)))

;;; ─── CSE / DCE Properties of New Kinds ──────────────────────────────────

(it-sequential "dce-eligible-kinds pure-add"
  (destructuring-bind (inst) (list (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-truthy)))

(it-sequential "dce-eligible-kinds alloc-cons"
  (destructuring-bind (inst) (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-truthy)))

(it-sequential "dce-not-eligible-kinds io-print"
  (destructuring-bind (inst) (list (make-vm-print      :reg :r0))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-falsy)))

(it-sequential "dce-not-eligible-kinds write-set-global"
  (destructuring-bind (inst) (list (make-vm-set-global :src :r0 :name 'x))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-falsy)))

(it-sequential "dce-not-eligible-kinds read-get-global"
  (destructuring-bind (inst) (list (make-vm-get-global :dst :r0 :name 'x))
    (expect (cl-cc/optimize:opt-inst-dce-eligible-p inst) :to-be-falsy)))

(it-sequential "cse-not-eligible-non-pure alloc-cons"
  (destructuring-bind (inst) (list (make-vm-cons       :dst :r0 :car-src :r1 :cdr-src :r2))
    (expect (cl-cc/optimize:opt-inst-cse-eligible-p inst) :to-be-falsy)))

(it-sequential "cse-not-eligible-non-pure io-print"
  (destructuring-bind (inst) (list (make-vm-print       :reg :r0))
    (expect (cl-cc/optimize:opt-inst-cse-eligible-p inst) :to-be-falsy)))

(it-sequential "cse-not-eligible-non-pure read-get-global"
  (destructuring-bind (inst) (list (make-vm-get-global  :dst :r0 :name 'x))
    (expect (cl-cc/optimize:opt-inst-cse-eligible-p inst) :to-be-falsy)))

;;; ─── DCE Extended Coverage ───────────────────────────────────────────────

(it-sequential "dce-add-elimination"
  (let* ((dead-insts (list (make-vm-const :dst :r0 :value 1)
                           (make-vm-const :dst :r1 :value 2)
                           (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1) ; dead: r2 unused
                           (make-vm-ret   :reg :r0)))
         (used-insts (list (make-vm-const :dst :r0 :value 1)
                           (make-vm-const :dst :r1 :value 2)
                           (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                           (make-vm-ret   :reg :r2)))) ; r2 is used here
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-add))
                        (cl-cc/optimize::opt-pass-dce dead-insts)) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-add))
                        (cl-cc/optimize::opt-pass-dce used-insts)) :to-be-truthy)))

;;; ─── effect-row->effect-kind bridge ─────────────────────────────────────

(defun make-effect-row (&rest effect-names)
  "Helper: build a type-effect-row with the given effect name symbols."
  (cl-cc/type:make-type-effect-row
   :effects (mapcar (lambda (n) (cl-cc/type:make-type-effect-op :name n :args nil)) effect-names)
   :row-var nil))

(it-sequential "effect-row->kind pure"
  (destructuring-bind (row expected) (list (make-effect-row) :pure)
    (expect (cl-cc:effect-row->effect-kind row) :to-be expected)))

(it-sequential "effect-row->kind io"
  (destructuring-bind (row expected) (list (make-effect-row 'io) :io)
    (expect (cl-cc:effect-row->effect-kind row) :to-be expected)))

(it-sequential "effect-row->kind state"
  (destructuring-bind (row expected) (list (make-effect-row 'state) :write-global)
    (expect (cl-cc:effect-row->effect-kind row) :to-be expected)))

(it-sequential "effect-row->kind error"
  (destructuring-bind (row expected) (list (make-effect-row 'error) :control)
    (expect (cl-cc:effect-row->effect-kind row) :to-be expected)))

(it-sequential "effect-row->kind unknown-tag"
  (destructuring-bind (row expected) (list (make-effect-row 'network) :unknown)
    (expect (cl-cc:effect-row->effect-kind row) :to-be expected)))

(it-sequential "effect-kind-vm-label-is-control"
  (expect (cl-cc/optimize:vm-inst-effect-kind (make-vm-label :name "L0")) :to-be :control))
