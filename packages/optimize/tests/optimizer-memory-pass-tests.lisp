;;;; tests/unit/optimize/optimizer-memory-pass-tests.lisp
;;;; Unit tests for optimizer-memory-alias.lisp — alias queries, store/load passes
;;;;
;;;; Covers: opt-must-alias-p, opt-may-alias-p, opt-slot-alias-key,
;;;;   opt-rewrite-inst-regs, opt-pass-dead-store-elim,
;;;;   opt-pass-store-to-load-forward, opt-pass-cons-slot-forward,
;;;;   mem-pass-state helpers, *opt-heap-root-kind-table* integrity,
;;;;   *opt-interval-binop-table*, %mps-pending-uses-reg-p,
;;;;   %mps-flush-if-src-overwritten, %mps-flush-dependent-on-reg.

(in-package :cl-cc/test)

;;; ─── opt-must-alias-p / opt-may-alias-p ─────────────────────────────────

(defun %make-alias-table (&rest entries)
  "Build a :eq hash-table from alternating key/value ENTRIES."
  (let ((ht (make-hash-table :test #'eq)))
    (loop for (k v) on entries by #'cddr do (setf (gethash k ht) v))
    ht))

(defmacro assert-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

(it-sequential "must-alias-cases same-root"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r1 '(:r0 :root0 :r1 :root0) t)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-falsy)))))

(it-sequential "must-alias-cases diff-roots"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r1 '(:r0 :root0 :r1 :root1) nil)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-falsy)))))

(it-sequential "must-alias-cases unknown-root"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r1 '(:r0 :root0) nil)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-must-alias-p reg-a reg-b alias) :to-be-falsy)))))

(it-sequential "may-alias-cases same-root"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r1 '(:r0 :root0 :r1 :root0) t)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-falsy)))))

(it-sequential "may-alias-cases unknown-register"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r99 '(:r0 :root0) t)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-falsy)))))

(it-sequential "may-alias-cases diff-known-roots"
  (destructuring-bind (reg-a reg-b entries expected) (list :r0 :r1 '(:r0 :root0 :r1 :root1) nil)
    (let ((alias (apply #'%make-alias-table entries)))
    (assert-boolean-case expected
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-truthy)
        (expect (cl-cc/optimize:opt-may-alias-p reg-a reg-b alias) :to-be-falsy)))))

;;; ─── opt-slot-alias-key / opt-rewrite-inst-regs ────────────────────────

(it-sequential "slot-alias-key-uses-canonical-root-when-known"
  (let ((alias (make-hash-table :test #'eq)))
    (setf (gethash :r0 alias) :root0)
    (expect (cl-cc/optimize::opt-slot-alias-key :r0 'x alias) :to-equal '(:slot :root0 x))))

(it-sequential "slot-alias-key-falls-back-to-register-when-unknown"
  (let ((alias (make-hash-table :test #'eq)))
    (expect (cl-cc/optimize::opt-slot-alias-key :r5 'y alias) :to-equal '(:slot :r5 y))))

(it-sequential "rewrite-inst-regs-substitutes-source-registers"
  (let* ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r5)
    (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs (make-vm-add :dst :r2 :lhs :r0 :rhs :r1) copies)))
      (expect (cl-cc/vm::vm-dst inst) :to-be :r2)
      (expect (cl-cc/vm::vm-lhs inst) :to-be :r5))))

(it-sequential "rewrite-inst-regs-substitutes-ssa-keyword-registers"
  (let ((copies (make-hash-table :test #'eq)))
    (setf (gethash :i copies) :i.1
          (gethash :one copies) :one.0)
    (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs
                 (make-vm-add :dst :i.3 :lhs :i :rhs :one)
                 copies)))
      (expect (cl-cc/vm::vm-dst inst) :to-be :i.3)
      (expect (cl-cc/vm::vm-lhs inst) :to-be :i.1)
      (expect (cl-cc/vm::vm-rhs inst) :to-be :one.0))))

(it-sequential "rewrite-inst-regs-never-rewrites-dst"
  (let* ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r99)
    (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs (make-vm-add :dst :r0 :lhs :r1 :rhs :r2) copies)))
      (expect (cl-cc/vm::vm-dst inst) :to-be :r0))))

(it-sequential "rewrite-inst-regs-no-op-on-empty-copy-map"
  (let ((inst (cl-cc/optimize::opt-rewrite-inst-regs
               (make-vm-move :dst :r0 :src :r1)
               (make-hash-table :test #'eq))))
    (expect (cl-cc/vm::vm-src inst) :to-be :r1)))

;;; ─── opt-pass-dead-store-elim ────────────────────────────────────────────

(defun %count-insts-of-type (result type-pred)
  "Count instructions in RESULT matching TYPE-PRED."
  (count-if type-pred result))

(it-sequential "dead-store-elim-overwrite-without-read-removes-earlier-store"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'g)
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'g)
                        (make-vm-ret :reg :r1)))))
    (expect (= 1 (%count-insts-of-type result #'vm-set-global-p)) :to-be-truthy)))

(it-sequential "dead-store-elim-read-between-stores-preserves-both"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'g)
                        (make-vm-get-global :dst :r1 :name 'g)
                        (make-vm-ret :reg :r1)))))
    (expect (= 1 (%count-insts-of-type result #'vm-set-global-p)) :to-be-truthy)
    (expect (= 1 (%count-insts-of-type result #'vm-get-global-p)) :to-be-truthy)))

(it-sequential "dead-store-elim-store-reaching-ret-is-preserved"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :r0 :value 42)
                        (make-vm-set-global :src :r0 :name 'result)
                        (make-vm-ret :reg :r0)))))
    (expect (= 1 (%count-insts-of-type result #'vm-set-global-p)) :to-be-truthy)))

;;; ─── opt-pass-store-to-load-forward ──────────────────────────────────────

(it-sequential "store-to-load-forward-prior-store-replaces-get-global"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 10)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-get-global :dst :r1 :name 'x)
                        (make-vm-ret :reg :r1)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move))       result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result) :to-be-falsy)))

(it-sequential "store-to-load-forward-no-prior-store-preserves-get-global"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-get-global :dst :r0 :name 'unknown)
                        (make-vm-ret :reg :r0)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result) :to-be-truthy)))

(it-sequential "store-to-load-forward-prior-slot-write-replaces-slot-read"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'slot :value-reg :value)
                        (cl-cc:make-vm-slot-read  :dst :out :obj-reg :obj :slot-name 'slot)
                        (make-vm-ret :reg :out)))))
    (expect (some (lambda (i)
              (and (typep i 'cl-cc/vm::vm-move)
                   (eq :out (cl-cc/vm::vm-dst i))
                   (eq :value (cl-cc/vm::vm-src i))))
           result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-slot-read)) result) :to-be-falsy)))

(it-sequential "store-to-load-forward-cross-block-dominating-store-replaces-get-global"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 10)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r7 :value 1)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r8 :value 2)
                        (make-vm-label :name "join")
                        (make-vm-get-global :dst :r1 :name 'x)
                        (make-vm-ret :reg :r1)))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r1 (cl-cc/vm::vm-dst i))
                  (eq :r0 (cl-cc/vm::vm-src i))))
           result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result) :to-be-falsy)))

(it-sequential "store-to-load-forward-join-disagree-preserves-get-global"
  (let* ((load (make-vm-get-global :dst :r2 :name 'x))
         (result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'x)
                        (make-vm-label :name "join")
                        load
                        (make-vm-ret :reg :r2)))))
    (expect (member load result :test #'eq) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r2 (cl-cc/vm::vm-dst i))))
            result) :to-be-falsy)))

(it-sequential "store-to-load-forward-cross-block-dominating-slot-write-replaces-slot-read"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'slot :value-reg :val)
                        (make-vm-jump-zero :reg :r9 :label "else")
                        (make-vm-const :dst :r7 :value 1)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-const :dst :r8 :value 2)
                        (make-vm-label :name "join")
                        (cl-cc:make-vm-slot-read :dst :out :obj-reg :obj :slot-name 'slot)
                        (make-vm-ret :reg :out)))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :out (cl-cc/vm::vm-dst i))
                  (eq :val (cl-cc/vm::vm-src i))))
           result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-slot-read)) result) :to-be-falsy)))

(it-sequential "store-to-load-forward-branch-constant-prunes-infeasible-edge"
  (let* ((result (cl-cc/optimize::opt-pass-store-to-load-forward
                  (list (make-vm-const :dst :r0 :value 1)
                        (make-vm-set-global :src :r0 :name 'x)
                        (make-vm-const :dst :rc :value 0)
                        (make-vm-jump-zero :reg :rc :label "else")
                        (make-vm-const :dst :r1 :value 2)
                        (make-vm-set-global :src :r1 :name 'x)
                        (make-vm-jump :label "join")
                        (make-vm-label :name "else")
                        (make-vm-label :name "join")
                        (make-vm-get-global :dst :r2 :name 'x)
                        (make-vm-ret :reg :r2)))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r2 (cl-cc/vm::vm-dst i))
                  (eq :r0 (cl-cc/vm::vm-src i))))
           result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result) :to-be-falsy)))

;;; ─── opt-pass-cons-slot-forward ──────────────────────────────────────────

(it-sequential "cons-slot-forward-replaces-car-with-original-car-register"
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-car  :dst :out  :src :cell))))
         (move (second result)))
    (expect (typep move 'cl-cc/vm::vm-move) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst move) :to-be :out)
    (expect (cl-cc/vm::vm-src move) :to-be :head)))

(it-sequential "cons-slot-forward-replaces-cdr-through-move-alias"
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-move :dst :alias :src :cell)
                        (make-vm-cdr  :dst :out   :src :alias))))
         (move (third result)))
    (expect (typep move 'cl-cc/vm::vm-move) :to-be-truthy)
    (expect (cl-cc/vm::vm-dst move) :to-be :out)
    (expect (cl-cc/vm::vm-src move) :to-be :tail)))

(it-sequential "cons-slot-forward-source-overwrite-kills-fact"
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons  :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-const :dst :head :value 99)
                        (make-vm-car   :dst :out  :src :cell)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result) :to-be-truthy)
    (expect (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-move)
                               (eq :out (cl-cc/vm::vm-dst i))))
                        result) :to-be-falsy)))

(it-sequential "cons-slot-forward-rplaca-kills-fact"
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons   :dst :cell :car-src :head :cdr-src :tail)
                        (make-vm-rplaca :cons :cell :val :new-head)
                        (make-vm-car    :dst :out :src :cell)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result) :to-be-truthy)))

(it-sequential "cons-slot-forward-cons-overwriting-source-is-conservative"
  (let* ((result (cl-cc/optimize::opt-pass-cons-slot-forward
                  (list (make-vm-cons :dst :head :car-src :head :cdr-src :tail)
                        (make-vm-car  :dst :out  :src :head)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-car)) result) :to-be-truthy)))

;;; ─── mem-pass-state struct helpers ──────────────────────────────────────────

(it-sequential "mps-emit-pushes-instruction-onto-result"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 1)))
    (cl-cc/optimize::%mps-emit state inst)
    (expect (= 1 (length (cl-cc/optimize::mps-result state))) :to-be-truthy)
    (expect (first (cl-cc/optimize::mps-result state)) :to-be inst)))

(it-sequential "mps-remember-store-then-flush-one-emits-stored-instruction"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 7)))
    (cl-cc/optimize::%mps-remember-store state :some-key inst)
    (cl-cc/optimize::%mps-flush-one state :some-key)
    (expect (= 1 (length (cl-cc/optimize::mps-result state))) :to-be-truthy)
    (expect (first (cl-cc/optimize::mps-result state)) :to-be inst)
    (expect (cl-cc/optimize::%mps-pending-store state :some-key) :to-be-null)))

(it-sequential "mps-flush-all-emits-all-pending-stores"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (i1    (make-vm-const :dst :r0 :value 1))
        (i2    (make-vm-const :dst :r1 :value 2)))
    (cl-cc/optimize::%mps-remember-store state :k1 i1)
    (cl-cc/optimize::%mps-remember-store state :k2 i2)
    (cl-cc/optimize::%mps-flush-all state)
    (expect (= 2 (length (cl-cc/optimize::mps-result state))) :to-be-truthy)
    (expect (= 0 (hash-table-count (cl-cc/optimize::mps-pending-by-name state))) :to-be-truthy)))

(it-sequential "mps-drop-pending-removes-key-from-pending"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (inst  (make-vm-const :dst :r0 :value 1)))
    (cl-cc/optimize::%mps-remember-store state :key inst)
    (cl-cc/optimize::%mps-drop-pending state :key)
    (expect (cl-cc/optimize::%mps-pending-store state :key) :to-be-null)
    (expect (cl-cc/optimize::mps-pending-order state) :to-be-null)))

;;; ─── *opt-heap-root-kind-table* data integrity ───────────────────────────

(it-sequential "heap-root-kind-table-integrity-and-lookup"
  (expect (cl-cc/optimize::opt-heap-root-kind (make-vm-const :dst :r0 :value 1)) :to-be-null)
  (expect (= 4 (length cl-cc/optimize::*opt-heap-root-kind-table*)) :to-be-truthy)
  (expect (cdr (assoc 'vm-cons          cl-cc/optimize::*opt-heap-root-kind-table*)) :to-be :cons)
  (expect (cdr (assoc 'vm-make-array    cl-cc/optimize::*opt-heap-root-kind-table*)) :to-be :array)
  (expect (cdr (assoc 'vm-closure       cl-cc/optimize::*opt-heap-root-kind-table*)) :to-be :closure)
  (expect (cdr (assoc 'vm-make-closure  cl-cc/optimize::*opt-heap-root-kind-table*)) :to-be :closure)
  (let ((inst (make-vm-closure :dst :r0 :label "f" :params '(x) :captured nil)))
    (expect (cl-cc/optimize::opt-heap-root-kind inst) :to-be :closure)))

;;; ─── *opt-interval-binop-table* / %opt-update-interval-binop ────────────

(it-sequential "interval-binop-table-coverage"
  (expect (= 6 (length cl-cc/optimize::*opt-interval-binop-table*)) :to-be-truthy)
  (expect (assoc 'vm-add cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy)
  (expect (assoc 'vm-integer-add cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy)
  (expect (assoc 'vm-sub cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy)
  (expect (assoc 'vm-integer-sub cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy)
  (expect (assoc 'vm-mul cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy)
  (expect (assoc 'vm-integer-mul cl-cc/optimize::*opt-interval-binop-table*) :to-be-truthy))

(it-sequential "update-interval-binop-cases add-known"
  (destructuring-bind (fn-sym lhs-val rhs-val expected-lo expected-found) (list 'opt-interval-add 2 3 5 t)
    (let ((intervals (make-hash-table :test #'eq))
        (inst      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (when lhs-val
      (setf (gethash :r0 intervals) (cl-cc/optimize::opt-make-interval lhs-val lhs-val)))
    (when rhs-val
      (setf (gethash :r1 intervals) (cl-cc/optimize::opt-make-interval rhs-val rhs-val)))
    (cl-cc/optimize::%opt-update-interval-binop
     inst intervals (symbol-function fn-sym))
    (assert-boolean-case expected-found
        (expect (= expected-lo (cl-cc/optimize::opt-interval-lo (gethash :r2 intervals))) :to-be-truthy)
        (expect (nth-value 1 (gethash :r2 intervals)) :to-be-falsy)))))

(it-sequential "update-interval-binop-cases sub-known"
  (destructuring-bind (fn-sym lhs-val rhs-val expected-lo expected-found) (list 'opt-interval-sub 5 2 3 t)
    (let ((intervals (make-hash-table :test #'eq))
        (inst      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (when lhs-val
      (setf (gethash :r0 intervals) (cl-cc/optimize::opt-make-interval lhs-val lhs-val)))
    (when rhs-val
      (setf (gethash :r1 intervals) (cl-cc/optimize::opt-make-interval rhs-val rhs-val)))
    (cl-cc/optimize::%opt-update-interval-binop
     inst intervals (symbol-function fn-sym))
    (assert-boolean-case expected-found
        (expect (= expected-lo (cl-cc/optimize::opt-interval-lo (gethash :r2 intervals))) :to-be-truthy)
        (expect (nth-value 1 (gethash :r2 intervals)) :to-be-falsy)))))

(it-sequential "update-interval-binop-cases add-unknown"
  (destructuring-bind (fn-sym lhs-val rhs-val expected-lo expected-found) (list 'opt-interval-add nil nil nil nil)
    (let ((intervals (make-hash-table :test #'eq))
        (inst      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (when lhs-val
      (setf (gethash :r0 intervals) (cl-cc/optimize::opt-make-interval lhs-val lhs-val)))
    (when rhs-val
      (setf (gethash :r1 intervals) (cl-cc/optimize::opt-make-interval rhs-val rhs-val)))
    (cl-cc/optimize::%opt-update-interval-binop
     inst intervals (symbol-function fn-sym))
    (assert-boolean-case expected-found
        (expect (= expected-lo (cl-cc/optimize::opt-interval-lo (gethash :r2 intervals))) :to-be-truthy)
        (expect (nth-value 1 (gethash :r2 intervals)) :to-be-falsy)))))

(it-sequential "optimize-instructions-rewrites-logand-one-eq-zero-to-evenp"
  (let* ((result (cl-cc/optimize:optimize-instructions
                  (list (make-vm-get-global :dst :x :name 'x)
                        (make-vm-const :dst :one :value 1)
                        (make-vm-logand :dst :bit :lhs :x :rhs :one)
                        (make-vm-const :dst :zero :value 0)
                        (make-vm-num-eq :dst :pred :lhs :bit :rhs :zero)
                        (make-vm-ret :reg :pred))
                  :max-iterations 1
                  :pass-pipeline '(:fold :dce)))
         (even-inst (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-evenp)) result)))
    (expect even-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-dst even-inst) :to-be :pred)
    (expect (cl-cc/vm::vm-src even-inst) :to-be :x)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-logand)) result) :to-be-falsy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-num-eq)) result) :to-be-falsy)))

(it-sequential "overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add"
  (let* ((result (cl-cc/optimize:optimize-instructions
                  (list (make-vm-get-global :dst :x :name 'x)
                        (make-vm-get-global :dst :y :name 'y)
                        (make-vm-const :dst :mask :value #xFF)
                        (make-vm-logand :dst :x8 :lhs :x :rhs :mask)
                        (make-vm-logand :dst :y8 :lhs :y :rhs :mask)
                        (cl-cc:make-vm-add-checked :dst :sum :lhs :x8 :rhs :y8)
                        (make-vm-ret :reg :sum))
                  :max-iterations 1
                  :pass-pipeline '(:overflow-check-elim)))
         (unchecked (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-integer-add)) result)))
    (expect unchecked :to-be-truthy)
    (expect (cl-cc/vm::vm-dst unchecked) :to-be :sum)
    (expect (cl-cc/vm::vm-lhs unchecked) :to-be :x8)
    (expect (cl-cc/vm::vm-rhs unchecked) :to-be :y8)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm::vm-add-checked)) result) :to-be-falsy)))

;;; ─── %mps-pending-uses-reg-p ─────────────────────────────────────────────

(it-sequential "mps-pending-uses-reg-p-cases set-global-src-match"
  (destructuring-bind (inst reg expected) (list (make-vm-set-global :src :r0 :name 'g) :r0 t)
    (assert-boolean-case expected
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-truthy)
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-falsy))))

(it-sequential "mps-pending-uses-reg-p-cases set-global-no-match"
  (destructuring-bind (inst reg expected) (list (make-vm-set-global :src :r1 :name 'g) :r0 nil)
    (assert-boolean-case expected
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-truthy)
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-falsy))))

(it-sequential "mps-pending-uses-reg-p-cases slot-write-obj-match"
  (destructuring-bind (inst reg expected) (list (make-vm-slot-write :obj-reg :r0 :slot-name 's :value-reg :r2) :r0 t)
    (assert-boolean-case expected
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-truthy)
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-falsy))))

(it-sequential "mps-pending-uses-reg-p-cases slot-write-value-match"
  (destructuring-bind (inst reg expected) (list (make-vm-slot-write :obj-reg :r1 :slot-name 's :value-reg :r0) :r0 t)
    (assert-boolean-case expected
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-truthy)
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-falsy))))

(it-sequential "mps-pending-uses-reg-p-cases slot-write-no-match"
  (destructuring-bind (inst reg expected) (list (make-vm-slot-write :obj-reg :r1 :slot-name 's :value-reg :r2) :r0 nil)
    (assert-boolean-case expected
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-truthy)
      (expect (cl-cc/optimize::%mps-pending-uses-reg-p inst reg) :to-be-falsy))))

;;; ─── %mps-flush-if-src-overwritten ──────────────────────────────────────────

(it-sequential "mps-flush-if-src-overwritten-matching-reg-triggers-flush"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-if-src-overwritten state :r0)
    (expect (member sg (cl-cc/optimize::mps-result state)) :to-be-truthy)
    (expect (cl-cc/optimize::%mps-pending-store state 'g) :to-be-null)))

(it-sequential "mps-flush-if-src-overwritten-unrelated-reg-leaves-store-pending"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-if-src-overwritten state :r9)
    (expect (cl-cc/optimize::%mps-pending-store state 'g) :to-be-truthy)
    (expect (cl-cc/optimize::mps-result state) :to-be-null)))

;;; ─── %mps-flush-dependent-on-reg ────────────────────────────────────────────

(it-sequential "mps-flush-dependent-on-reg-flushes-matching-stores"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg1   (make-vm-set-global :src :r0 :name 'g1))
        (sg2   (make-vm-set-global :src :r1 :name 'g2)))
    (cl-cc/optimize::%mps-remember-store state 'g1 sg1)
    (cl-cc/optimize::%mps-remember-store state 'g2 sg2)
    (cl-cc/optimize::%mps-flush-dependent-on-reg state :r0)
    (expect (member sg1 (cl-cc/optimize::mps-result state)) :to-be-truthy)
    (expect (cl-cc/optimize::%mps-pending-store state 'g1) :to-be-null)
    (expect (cl-cc/optimize::%mps-pending-store state 'g2) :to-be-truthy)))

(it-sequential "mps-flush-dependent-on-reg-exclude-key-skips-matching-store"
  (let ((state (cl-cc/optimize::make-mem-pass-state))
        (sg    (make-vm-set-global :src :r0 :name 'g)))
    (cl-cc/optimize::%mps-remember-store state 'g sg)
    (cl-cc/optimize::%mps-flush-dependent-on-reg state :r0 :exclude-key 'g)
    (expect (cl-cc/optimize::%mps-pending-store state 'g) :to-be-truthy)
    (expect (cl-cc/optimize::mps-result state) :to-be-null)))

(it-sequential "dead-store-elim-tbaa-disjoint-read-does-not-observe-slot-store"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-const :dst :n :value 4)
                        (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                        (make-vm-make-array :dst :arr :size-reg :n
                                            :initial-element nil :fill-pointer nil
                                            :adjustable nil :element-type nil)
                        (make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r2)
                        (make-vm-slot-read :dst :v :obj-reg :arr :slot-name 'x)
                        (make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r3)
                        (make-vm-ret :reg :v)))))
    (expect (= 1 (%count-insts-of-type result #'vm-slot-write-p)) :to-be-truthy)))

(it-sequential "dead-store-elim-unknown-alias-read-observes-slot-store"
  (let* ((result (cl-cc/optimize::opt-pass-dead-store-elim
                  (list (make-vm-slot-write :obj-reg :a :slot-name 'x :value-reg :r1)
                        (make-vm-slot-read :dst :v :obj-reg :b :slot-name 'x)
                        (make-vm-slot-write :obj-reg :a :slot-name 'x :value-reg :r2)
                        (make-vm-ret :reg :v)))))
    (expect (= 2 (%count-insts-of-type result #'vm-slot-write-p)) :to-be-truthy)))
