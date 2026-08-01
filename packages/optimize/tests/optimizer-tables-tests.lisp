;;;; tests/unit/optimize/optimizer-tables-tests.lisp
;;;; Coverage tests for src/optimize/optimizer-tables.lisp
;;;;
;;;; Covers the data tables:
;;;;   *opt-binary-fold-table*, *opt-binary-cmp-fold-table*,
;;;;   *opt-unary-fold-table*, *opt-type-pred-fold-table*,
;;;;   *opt-binary-zero-guard-types*, *opt-binary-no-fold-types*,
;;;;   *opt-commutative-inst-types*, *opt-binary-lhs-rhs-types*,
;;;;   *opt-unary-src-types*, *opt-read-regs-table*
;;;; and the dispatchers:
;;;;   opt-inst-dst, opt-inst-read-regs.
;;;;
;;;; Note: opt-falsep, opt-register-keyword-p, opt-binary-lhs-rhs-p,
;;;; opt-unary-src-p, opt-foldable-unary-arith-p, opt-foldable-type-pred-p
;;;; are already covered in optimizer-tests.lisp.

(in-package :cl-cc/test)

;;; ─── *opt-binary-fold-table* ─────────────────────────────────────────────

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-add"
  (destructuring-bind (type-sym) (list 'vm-add)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-integer-add"
  (destructuring-bind (type-sym) (list 'vm-integer-add)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-float-add"
  (destructuring-bind (type-sym) (list 'vm-float-add)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-sub"
  (destructuring-bind (type-sym) (list 'vm-sub)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-mul"
  (destructuring-bind (type-sym) (list 'vm-mul)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-mod"
  (destructuring-bind (type-sym) (list 'vm-mod)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-min"
  (destructuring-bind (type-sym) (list 'vm-min)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-max"
  (destructuring-bind (type-sym) (list 'vm-max)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-logand"
  (destructuring-bind (type-sym) (list 'vm-logand)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-logior"
  (destructuring-bind (type-sym) (list 'vm-logior)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-logxor"
  (destructuring-bind (type-sym) (list 'vm-logxor)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-ash"
  (destructuring-bind (type-sym) (list 'vm-ash)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-div"
  (destructuring-bind (type-sym) (list 'vm-div)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-cl-div"
  (destructuring-bind (type-sym) (list 'cl-cc/vm::vm-cl-div)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-gcd"
  (destructuring-bind (type-sym) (list 'vm-gcd)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-arithmetic-ops-registered vm-lcm"
  (destructuring-bind (type-sym) (list 'vm-lcm)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)))

(it-sequential "binary-fold-table-function-evaluation-cases add"
  (destructuring-bind (type-sym a b expected) (list 'vm-add 3 4 7)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (expect (= expected (funcall fn a b)) :to-be-truthy))))

(it-sequential "binary-fold-table-function-evaluation-cases sub"
  (destructuring-bind (type-sym a b expected) (list 'vm-sub 9 4 5)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (expect (= expected (funcall fn a b)) :to-be-truthy))))

(it-sequential "binary-fold-table-function-evaluation-cases mul"
  (destructuring-bind (type-sym a b expected) (list 'vm-mul 6 7 42)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (expect (= expected (funcall fn a b)) :to-be-truthy))))

(it-sequential "binary-fold-table-function-evaluation-cases div-floor"
  (destructuring-bind (type-sym a b expected) (list 'vm-div 10 3 3)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (expect (= expected (funcall fn a b)) :to-be-truthy))))

(it-sequential "binary-fold-table-function-evaluation-cases div-rational"
  (destructuring-bind (type-sym a b expected) (list 'cl-cc/vm::vm-cl-div 3 4 3/4)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (expect (= expected (funcall fn a b)) :to-be-truthy))))

;;; ─── *opt-binary-cmp-fold-table* ─────────────────────────────────────────

(it-sequential "binary-cmp-fold-table-ops-registered vm-lt"
  (destructuring-bind (type-sym) (list 'vm-lt)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy)))

(it-sequential "binary-cmp-fold-table-ops-registered vm-gt"
  (destructuring-bind (type-sym) (list 'vm-gt)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy)))

(it-sequential "binary-cmp-fold-table-ops-registered vm-le"
  (destructuring-bind (type-sym) (list 'vm-le)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy)))

(it-sequential "binary-cmp-fold-table-ops-registered vm-ge"
  (destructuring-bind (type-sym) (list 'vm-ge)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy)))

(it-sequential "binary-cmp-fold-table-ops-registered vm-num-eq"
  (destructuring-bind (type-sym) (list 'vm-num-eq)
    (expect (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy)))

(it-sequential "binary-cmp-fold-table-lt-function-evaluates-correctly"
  (let ((fn (gethash 'vm-lt cl-cc/optimize::*opt-binary-cmp-fold-table*)))
    (expect (funcall fn 3 5) :to-be-truthy)
    (expect (funcall fn 5 3) :to-be-falsy)))

;;; ─── *opt-unary-fold-table* ──────────────────────────────────────────────

(it-sequential "unary-fold-table-ops-registered vm-neg"
  (destructuring-bind (type-sym) (list 'vm-neg)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-abs"
  (destructuring-bind (type-sym) (list 'vm-abs)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-inc"
  (destructuring-bind (type-sym) (list 'vm-inc)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-dec"
  (destructuring-bind (type-sym) (list 'vm-dec)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-lognot"
  (destructuring-bind (type-sym) (list 'vm-lognot)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-rational"
  (destructuring-bind (type-sym) (list 'vm-rational)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-rationalize"
  (destructuring-bind (type-sym) (list 'vm-rationalize)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-numerator"
  (destructuring-bind (type-sym) (list 'vm-numerator)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-denominator"
  (destructuring-bind (type-sym) (list 'vm-denominator)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-car"
  (destructuring-bind (type-sym) (list 'vm-car)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-cdr"
  (destructuring-bind (type-sym) (list 'vm-cdr)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-ops-registered vm-not"
  (destructuring-bind (type-sym) (list 'vm-not)
    (expect (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)))

(it-sequential "unary-fold-table-function-evaluation-cases neg"
  (destructuring-bind (type-sym input expected) (list 'vm-neg 7 -7)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases abs"
  (destructuring-bind (type-sym input expected) (list 'vm-abs -5 5)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases rational"
  (destructuring-bind (type-sym input expected) (list 'vm-rational 0.5 1/2)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases numerator"
  (destructuring-bind (type-sym input expected) (list 'vm-numerator 9/12 3)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases denominator"
  (destructuring-bind (type-sym input expected) (list 'vm-denominator 9/12 4)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases car"
  (destructuring-bind (type-sym input expected) (list 'vm-car '(7 . 8) 7)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-function-evaluation-cases cdr"
  (destructuring-bind (type-sym input expected) (list 'vm-cdr '(7 . 8) 8)
    (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (= expected (funcall fn input)) :to-be-truthy))))

(it-sequential "unary-fold-table-not-nil-returns-t"
  (let ((fn (gethash 'vm-not cl-cc/optimize::*opt-unary-fold-table*)))
    (expect (funcall fn nil) :to-be-truthy)
    (expect (funcall fn t) :to-be-null)))

;;; ─── *opt-type-pred-fold-table* ──────────────────────────────────────────

(it-sequential "type-pred-fold-table-ops-registered vm-null-p"
  (destructuring-bind (type-sym) (list 'vm-null-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-ops-registered vm-cons-p"
  (destructuring-bind (type-sym) (list 'vm-cons-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-ops-registered vm-symbol-p"
  (destructuring-bind (type-sym) (list 'vm-symbol-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-ops-registered vm-number-p"
  (destructuring-bind (type-sym) (list 'vm-number-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-ops-registered vm-integer-p"
  (destructuring-bind (type-sym) (list 'vm-integer-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-ops-registered vm-function-p"
  (destructuring-bind (type-sym) (list 'vm-function-p)
    (expect (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy)))

(it-sequential "type-pred-fold-table-null-p-evaluates-correctly"
  (let ((fn (gethash 'vm-null-p cl-cc/optimize::*opt-type-pred-fold-table*)))
    (expect (funcall fn nil) :to-be-truthy)
    (expect (funcall fn 42) :to-be-falsy)))

(it-sequential "type-pred-fold-table-function-p-always-false-for-constants"
  (let ((fn (gethash 'vm-function-p cl-cc/optimize::*opt-type-pred-fold-table*)))
    (expect (funcall fn 42) :to-be-null)
    (expect (funcall fn nil) :to-be-null)))

;;; ─── Table coverage and derived type lists ─────────────────────────────────

(it-sequential "binary-fold-tables-register-arithmetic-and-comparison-ops"
  (expect (gethash 'vm-add cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-mul cl-cc/optimize::*opt-binary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-lt cl-cc/optimize::*opt-binary-cmp-fold-table*) :to-be-truthy))

(it-sequential "unary-fold-tables-register-fold-and-predicate-ops"
  (expect (gethash 'vm-neg cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-car cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-cdr cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-abs cl-cc/optimize::*opt-unary-fold-table*) :to-be-truthy)
  (expect (gethash 'vm-null-p cl-cc/optimize::*opt-type-pred-fold-table*) :to-be-truthy))

(it-sequential "binary-zero-guard-types-contains-division-ops"
  (expect (member 'vm-div cl-cc/optimize::*opt-binary-zero-guard-types*) :to-be-truthy)
  (expect (member 'vm-mod cl-cc/optimize::*opt-binary-zero-guard-types*) :to-be-truthy)
  (expect (member 'vm-rem cl-cc/optimize::*opt-binary-zero-guard-types*) :to-be-truthy))

(it-sequential "binary-no-fold-types-contains-floor-ceiling"
  (expect (member 'vm-floor-inst   cl-cc/optimize::*opt-binary-no-fold-types*) :to-be-truthy)
  (expect (member 'vm-ceiling-inst cl-cc/optimize::*opt-binary-no-fold-types*) :to-be-truthy)
  (expect (member 'vm-truncate     cl-cc/optimize::*opt-binary-no-fold-types*) :to-be-truthy)
  (expect (member 'vm-round-inst   cl-cc/optimize::*opt-binary-no-fold-types*) :to-be-truthy))

(it-sequential "commutative-inst-types-contains-add-and-mul"
  (expect (member 'vm-add    cl-cc/optimize::*opt-commutative-inst-types*) :to-be-truthy)
  (expect (member 'vm-mul    cl-cc/optimize::*opt-commutative-inst-types*) :to-be-truthy)
  (expect (member 'vm-logand cl-cc/optimize::*opt-commutative-inst-types*) :to-be-truthy)
  (expect (member 'vm-num-eq cl-cc/optimize::*opt-commutative-inst-types*) :to-be-truthy))

(it-sequential "commutative-inst-types-excludes-sub-and-div"
  (expect (member 'vm-sub cl-cc/optimize::*opt-commutative-inst-types*) :to-be-falsy))

;;; ─── opt-inst-dst ─────────────────────────────────────────────────────────

(it-sequential "opt-inst-dst-returns-dst-register"
  (expect (cl-cc/optimize:opt-inst-dst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)) :to-be :r3))

(it-sequential "opt-inst-dst-nil-for-no-dst-instruction"
  (expect (cl-cc/optimize:opt-inst-dst (make-vm-jump :label "end")) :to-be-null))

;;; ─── opt-inst-read-regs ───────────────────────────────────────────────────

(it-sequential "opt-inst-read-regs-single-src-cases vm-const"
  (destructuring-bind (inst expected) (list (make-vm-const :dst :r0 :value 42) nil)
    (expect (cl-cc/optimize:opt-inst-read-regs inst) :to-equal expected)))

(it-sequential "opt-inst-read-regs-single-src-cases vm-move"
  (destructuring-bind (inst expected) (list (make-vm-move  :dst :r1 :src :r0) '(:r0))
    (expect (cl-cc/optimize:opt-inst-read-regs inst) :to-equal expected)))

(it-sequential "opt-inst-read-regs-single-src-cases vm-neg"
  (destructuring-bind (inst expected) (list (make-vm-neg   :dst :r1 :src :r0) '(:r0))
    (expect (cl-cc/optimize:opt-inst-read-regs inst) :to-equal expected)))

(it-sequential "opt-inst-read-regs-single-src-cases vm-car"
  (destructuring-bind (inst expected) (list (make-vm-car   :dst :r1 :src :r0) '(:r0))
    (expect (cl-cc/optimize:opt-inst-read-regs inst) :to-equal expected)))

(it-sequential "opt-inst-read-regs-single-src-cases vm-cdr"
  (destructuring-bind (inst expected) (list (make-vm-cdr   :dst :r1 :src :r0) '(:r0))
    (expect (cl-cc/optimize:opt-inst-read-regs inst) :to-equal expected)))

(it-sequential "opt-inst-read-regs-lhs-rhs-cases vm-add"
  (destructuring-bind (inst) (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (member :r0 regs) :to-be-truthy)
    (expect (member :r1 regs) :to-be-truthy))))

(it-sequential "opt-inst-read-regs-lhs-rhs-cases vm-lt"
  (destructuring-bind (inst) (list (make-vm-lt  :dst :r2 :lhs :r0 :rhs :r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (member :r0 regs) :to-be-truthy)
    (expect (member :r1 regs) :to-be-truthy))))

;;; ─── %opt-branch-target-labels ───────────────────────────────────────────

(it-sequential "branch-target-labels-cases jump-only"
  (destructuring-bind (insts expected-labels) (list (list (make-vm-jump      :label "a")) '("a"))
    (let ((targets (cl-cc/optimize::%opt-branch-target-labels insts)))
    (expect (= (length expected-labels) (hash-table-count targets)) :to-be-truthy)
    (dolist (lbl expected-labels)
      (expect (gethash lbl targets) :to-be-truthy)))))

(it-sequential "branch-target-labels-cases jump-zero-only"
  (destructuring-bind (insts expected-labels) (list (list (make-vm-jump-zero :reg :r0 :label "b")) '("b"))
    (let ((targets (cl-cc/optimize::%opt-branch-target-labels insts)))
    (expect (= (length expected-labels) (hash-table-count targets)) :to-be-truthy)
    (dolist (lbl expected-labels)
      (expect (gethash lbl targets) :to-be-truthy)))))

(it-sequential "branch-target-labels-cases both-kinds"
  (destructuring-bind (insts expected-labels) (list (list (make-vm-jump      :label "a")
                                   (make-vm-jump-zero :reg :r0 :label "b")) '("a" "b"))
    (let ((targets (cl-cc/optimize::%opt-branch-target-labels insts)))
    (expect (= (length expected-labels) (hash-table-count targets)) :to-be-truthy)
    (dolist (lbl expected-labels)
      (expect (gethash lbl targets) :to-be-truthy)))))

(it-sequential "branch-target-labels-cases no-jumps"
  (destructuring-bind (insts expected-labels) (list (list (make-vm-const :dst :r0 :value 1)
                                   (make-vm-ret   :reg :r0)) '())
    (let ((targets (cl-cc/optimize::%opt-branch-target-labels insts)))
    (expect (= (length expected-labels) (hash-table-count targets)) :to-be-truthy)
    (dolist (lbl expected-labels)
      (expect (gethash lbl targets) :to-be-truthy)))))

;;; ─── %fold-vm-jump-zero ──────────────────────────────────────────────────

(it-sequential "fold-vm-jump-zero-known-false-becomes-unconditional-jump"
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (setf (gethash :r0 env) nil) ; nil is the canonical false value
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (expect (= 1 (length emitted)) :to-be-truthy)
    (expect (typep (first emitted) 'cl-cc/vm::vm-jump) :to-be-truthy)
    (expect (cl-cc/vm::vm-label-name (first emitted)) :to-equal "target")))

(it-sequential "fold-vm-jump-zero-known-true-eliminates-branch"
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (setf (gethash :r0 env) 1) ; truthy value
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (expect (= 0 (length emitted)) :to-be-truthy)))

(it-sequential "fold-vm-jump-zero-unknown-condition-emits-unchanged"
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (expect (= 1 (length emitted)) :to-be-truthy)
    (expect (typep (first emitted) 'cl-cc/vm::vm-jump-zero) :to-be-truthy)))

;;; ─── %opt-known-constant-p ───────────────────────────────────────────────

(it-sequential "opt-known-constant-p-cases integer"
  (destructuring-bind (val expected) (list 42 t)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

(it-sequential "opt-known-constant-p-cases zero"
  (destructuring-bind (val expected) (list 0 t)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

(it-sequential "opt-known-constant-p-cases float"
  (destructuring-bind (val expected) (list 1.5 t)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

(it-sequential "opt-known-constant-p-cases unknown"
  (destructuring-bind (val expected) (list :unknown nil)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

(it-sequential "opt-known-constant-p-cases symbol"
  (destructuring-bind (val expected) (list :r0 nil)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

(it-sequential "opt-known-constant-p-cases nil"
  (destructuring-bind (val expected) (list nil nil)
    (if expected
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-known-constant-p val) :to-be-falsy))))

;;; ─── %opt-apply-algebraic-action ─────────────────────────────────────────

(it-sequential "opt-apply-algebraic-action-move-lhs"
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action :move-lhs :r2 :r0 :r1)))
    (expect (typep result 'cl-cc/vm::vm-move) :to-be-truthy)
    (expect (vm-dst result) :to-be :r2)
    (expect (vm-src result) :to-be :r0)))

(it-sequential "opt-apply-algebraic-action-move-rhs"
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action :move-rhs :r2 :r0 :r1)))
    (expect (typep result 'cl-cc/vm::vm-move) :to-be-truthy)
    (expect (vm-dst result) :to-be :r2)
    (expect (vm-src result) :to-be :r1)))

(it-sequential "opt-apply-algebraic-action-const"
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action '(:const 0) :r2 :r0 :r1)))
    (expect (typep result 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (vm-dst result) :to-be :r2)
    (expect (= 0 (vm-value result)) :to-be-truthy)))

(it-sequential "opt-apply-algebraic-action-neg-lhs"
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action '(:neg :lhs) :r2 :r0 :r1)))
    (expect (typep result 'cl-cc/vm::vm-neg) :to-be-truthy)
    (expect (vm-dst result) :to-be :r2)
    (expect (vm-src result) :to-be :r0)))

(it-sequential "opt-apply-algebraic-action-unknown-returns-nil"
  (expect (cl-cc/optimize::%opt-apply-algebraic-action :unknown-action :r2 :r0 :r1) :to-be-null))
