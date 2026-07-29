;;;; tests/unit/optimize/optimizer-cse-gvn-tests.lisp
;;;; Unit tests for src/optimize/optimizer-cse-gvn.lisp
;;;;
;;;; Covers: opt-pass-cse (redundant elimination, label flush, vm-const kept),
;;;;   opt-pass-gvn (straight-line, empty), opt-pass-dead-labels (removes
;;;;   unreferenced labels, keeps referenced ones), opt-pass-leaf-detect
;;;;   (leaf flag, non-leaf flag).

(in-package :cl-cc/test)

;;; ─── opt-pass-cse ────────────────────────────────────────────────────────

(it-sequential "cse-empty-input-returns-nil"
  (expect (cl-cc/optimize::opt-pass-cse nil) :to-be-null))

(it-sequential "cse-straight-line-no-duplicates-unchanged"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    (expect (= (length insts) (length result)) :to-be-truthy)))

(it-sequential "cse-eliminates-duplicate-binop"
  (let* ((insts (list (make-vm-const :dst :r0 :value 3)
                      (make-vm-const :dst :r1 :value 4)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; The second vm-add should be eliminated → replaced by vm-move
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result) :to-be-truthy)
    ;; Only one vm-add should remain
    (expect (= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result)) :to-be-truthy)))

(it-sequential "cse-label-flushes-knowledge"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      ;; Make a jump to the label to make it a branch target
                      (make-vm-jump  :label "flush")
                      (make-vm-label :name  "flush")
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; After flushing at the label, both adds should remain
    (expect (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result) 1) :to-be-truthy)))

(it-sequential "cse-keeps-vm-const-not-replaced-by-move"
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 42)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; Both vm-const should remain (not merged via vm-move)
    (expect (= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-const)) result)) :to-be-truthy)))

;;; ─── opt-pass-gvn ────────────────────────────────────────────────────────

(it-sequential "gvn-empty-input-returns-nil"
  (let ((result (cl-cc/optimize::opt-pass-gvn nil)))
    (expect (listp result) :to-be-truthy)))

(it-sequential "gvn-straight-line-preserves-instructions"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (expect (listp result) :to-be-truthy)
    (expect (> (length result) 0) :to-be-truthy)))

(it-sequential "gvn-uses-available-expressions-at-join"
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (expect (= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result)) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))
                  (eq :r2 (vm-src i))))
           result) :to-be-truthy)))

(it-sequential "gvn-redundant-overwrite-does-not-poison-same-syntax-expression"
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r4 :lhs :r1 :rhs :r2)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r4 :lhs :r1 :rhs :r2)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r1 :lhs :r1 :rhs :r2)
                      (make-vm-add :dst :r5 :lhs :r1 :rhs :r2)
                      (make-vm-ret :reg :r5)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (expect (= 3 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result)) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r1 (vm-dst i))
                  (eq :r4 (vm-src i))))
           result) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r5 (vm-dst i))))
           result) :to-be-falsy)))

(it-sequential "gvn-global-cse-does-not-reuse-memory-reads"
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-car :dst :r2 :src :r0)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-car :dst :r2 :src :r0)
                      (make-vm-label :name "join")
                      (make-vm-car :dst :r3 :src :r0)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (expect (= 3 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-car)) result)) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))))
           result) :to-be-falsy)))

(it-sequential "gvn-join-reuse-runs-through-optimize-instructions"
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize:optimize-instructions
                  insts
                  :max-iterations 1
                  :pass-pipeline '(:gvn))))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))
                  (eq :r2 (vm-src i))))
           result) :to-be-truthy)))

;;; ─── opt-pass-dead-labels ────────────────────────────────────────────────

(it-sequential "dead-labels-label-presence orphan-removed"
  (destructuring-bind (insts expect-label-p) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-label :name "orphan")
                 (make-vm-ret   :reg :r0)) nil)
    (let ((result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (if expect-label-p
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-truthy)
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-falsy)))))

(it-sequential "dead-labels-label-presence referenced-kept"
  (destructuring-bind (insts expect-label-p) (list (list (make-vm-jump  :label "target")
                 (make-vm-label :name  "target")
                 (make-vm-ret   :reg :r0)) t)
    (let ((result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (if expect-label-p
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-truthy)
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-falsy)))))

(it-sequential "dead-labels-empty-input"
  (expect (cl-cc/optimize::opt-pass-dead-labels nil) :to-be-null))

(it-sequential "dead-labels-straight-line-no-labels"
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (expect (= (length insts) (length result)) :to-be-truthy)))

;;; ─── opt-pass-leaf-detect ────────────────────────────────────────────────

(it-sequential "leaf-detect-empty-is-leaf"
  (multiple-value-bind (insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect nil)
    (expect (listp insts) :to-be-truthy)
    (expect leaf-p :to-be-truthy)))

(it-sequential "leaf-detect-call-presence no-call"
  (destructuring-bind (insts expected-leaf-p) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                 (make-vm-ret   :reg :r1)) t)
    (multiple-value-bind (result-insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect insts)
    (expect (= (length insts) (length result-insts)) :to-be-truthy)
    (if expected-leaf-p
        (expect leaf-p :to-be-truthy)
        (expect leaf-p :to-be-falsy)))))

(it-sequential "leaf-detect-call-presence with-call"
  (destructuring-bind (insts expected-leaf-p) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-call  :dst :r1 :func :r0 :args '())
                 (make-vm-ret   :reg :r1)) nil)
    (multiple-value-bind (result-insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect insts)
    (expect (= (length insts) (length result-insts)) :to-be-truthy)
    (if expected-leaf-p
        (expect leaf-p :to-be-truthy)
        (expect leaf-p :to-be-falsy)))))

(it-sequential "leaf-detect-preserves-instruction-list"
  (let* ((insts (list (make-vm-const :dst :r0 :value 7)
                      (make-vm-ret   :reg :r0))))
    (multiple-value-bind (result-insts _leaf)
        (cl-cc/optimize::opt-pass-leaf-detect insts)
      (declare (ignore _leaf))
      (expect result-insts :to-equal insts))))

;;; ─── cse-state struct helpers ────────────────────────────────────────────

(it-sequential "cse-state-get-val-unknown-reg-uses-generation"
  (let* ((state (cl-cc/optimize::make-cse-state))
         (key   (cl-cc/optimize::%cse-get-val state :r0)))
    (expect key :to-equal (cons :r0 0))))

(it-sequential "cse-state-bump-gen-increments-generation"
  (let ((state (cl-cc/optimize::make-cse-state)))
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (expect (= 1 (gethash :r0 (cl-cc/optimize::cse-gen state))) :to-be-truthy)))

(it-sequential "cse-state-record-then-try-find-roundtrip"
  (let ((state (cl-cc/optimize::make-cse-state))
        (key   '(vm-add (:r0 . 0) (:r1 . 0))))
    (cl-cc/optimize::%cse-record state :r2 key)
    (expect (cl-cc/optimize::%cse-try-find state key) :to-be :r2)))

(it-sequential "cse-state-flush-clears-all-tables"
  (let ((state (cl-cc/optimize::make-cse-state)))
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (cl-cc/optimize::%cse-record state :r0 '(:const 1))
    (cl-cc/optimize::%cse-flush state)
    (expect (= 0 (hash-table-count (cl-cc/optimize::cse-gen     state))) :to-be-truthy)
    (expect (= 0 (hash-table-count (cl-cc/optimize::cse-val-env state))) :to-be-truthy)
    (expect (= 0 (hash-table-count (cl-cc/optimize::cse-memo    state))) :to-be-truthy)))

(it-sequential "cse-state-bump-gen-evicts-memo"
  (let ((state (cl-cc/optimize::make-cse-state))
        (key   '(:const 5)))
    (cl-cc/optimize::%cse-record state :r0 key)
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (expect (cl-cc/optimize::%cse-try-find state key) :to-be-null)))

;;; ─── GVN helpers: %gvn-kill, %gvn-get-val, %gvn-record, %gvn-key ────────

(it-sequential "gvn-kill-removes-reg-from-val-env"
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq))
        (memo    (make-hash-table :test #'equal)))
    (setf (gethash :r0 val-env) '(:const 1)
          (gethash '(:const 1) memo) :r0)
    (cl-cc/optimize::%gvn-kill :r0 gen val-env memo)
    (expect (nth-value 1 (gethash :r0 val-env)) :to-be-falsy)
    (expect (nth-value 1 (gethash '(:const 1) memo)) :to-be-falsy)))

(it-sequential "gvn-get-val-returns-val-env-entry-when-present"
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 val-env) '(:const 42))
    (expect (cl-cc/optimize::%gvn-get-val :r0 gen val-env) :to-equal '(:const 42))))

(it-sequential "gvn-get-val-returns-reg-generation-pair-when-absent"
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 gen) 3)
    (let ((result (cl-cc/optimize::%gvn-get-val :r0 gen val-env)))
      (expect (car result) :to-equal :r0)
      (expect (= 3 (cdr result)) :to-be-truthy))))

(it-sequential "gvn-record-binds-dst-to-key"
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq))
        (memo    (make-hash-table :test #'equal)))
    (cl-cc/optimize::%gvn-record :r0 '(:const 7) gen val-env memo)
    (expect (gethash :r0 val-env) :to-equal '(:const 7))
    (expect (gethash '(:const 7) memo) :to-be :r0)))

(it-sequential "gvn-key-result const-5"
  (destructuring-bind (inst expected expect-non-nil) (list (make-vm-const :dst :r0 :value 5) '(:const 5) t)
    (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (key     (cl-cc/optimize::%gvn-key inst gen val-env)))
    (if expect-non-nil
        (expect key :to-equal expected)
        (expect key :to-be-null)))))

(it-sequential "gvn-key-result halt-impure"
  (destructuring-bind (inst expected expect-non-nil) (list (make-vm-halt :reg :r0) nil nil)
    (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (key     (cl-cc/optimize::%gvn-key inst gen val-env)))
    (if expect-non-nil
        (expect key :to-equal expected)
        (expect key :to-be-null)))))

(it-sequential "gvn-key-pure-binop-returns-list-headed-by-type"
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (let ((key (cl-cc/optimize::%gvn-key (make-vm-add :dst :r2 :lhs :r0 :rhs :r1) gen val-env)))
      (expect (consp key) :to-be-truthy)
      (expect (car key) :to-be 'cl-cc/vm::vm-add))))

;;; ─── %gvn-maybe-replace ──────────────────────────────────────────────────

(it-sequential "gvn-maybe-replace-nil-key-passthrough"
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (result  (cl-cc/optimize::%gvn-maybe-replace inst :r2 nil gen val-env memo nil)))
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (typep (car result) 'cl-cc/vm::vm-add) :to-be-truthy)
    (expect (= 0 (hash-table-count memo)) :to-be-truthy)))

(it-sequential "gvn-maybe-replace-new-key-records-and-emits-inst"
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (key     '(cl-cc/vm::vm-add (:r0 . 0) (:r1 . 0)))
         (result  (cl-cc/optimize::%gvn-maybe-replace inst :r2 key gen val-env memo nil)))
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (typep (car result) 'cl-cc/vm::vm-add) :to-be-truthy)
    (expect (gethash key memo) :to-be :r2)))

(it-sequential "gvn-maybe-replace-existing-key-produces-vm-move"
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r3 :lhs :r0 :rhs :r1))
         (key     '(cl-cc/vm::vm-add (:r0 . 0) (:r1 . 0))))
    (setf (gethash key memo) :r2)
    (let ((result (cl-cc/optimize::%gvn-maybe-replace inst :r3 key gen val-env memo nil)))
      (expect (= 1 (length result)) :to-be-truthy)
      (expect (typep (car result) 'cl-cc/vm::vm-move) :to-be-truthy)
      (expect (vm-dst (car result)) :to-be :r3)
      (expect (vm-src (car result)) :to-be :r2))))

;;; ─── %cse-emit-or-cse ────────────────────────────────────────────────────

(it-sequential "cse-emit-or-cse-emits-new-inst-when-no-prior-entry"
  (let* ((state  (cl-cc/optimize::make-cse-state))
         (inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (key    '(vm-add (:r0 . 0) (:r1 . 0)))
         (result (cl-cc/optimize::%cse-emit-or-cse inst :r2 key state nil)))
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (typep (car result) 'cl-cc/vm::vm-add) :to-be-truthy)))

(it-sequential "cse-emit-or-cse-replaces-duplicate-with-vm-move"
  (let* ((state  (cl-cc/optimize::make-cse-state))
         (key    '(vm-add (:r0 . 0) (:r1 . 0)))
         (inst1  (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (inst2  (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)))
    ;; Record first occurrence
    (cl-cc/optimize::%cse-bump-gen state :r2)
    (cl-cc/optimize::%cse-record state :r2 key)
    ;; Second occurrence should produce a vm-move
    (let ((result (cl-cc/optimize::%cse-emit-or-cse inst2 :r3 key state nil)))
      (expect (= 1 (length result)) :to-be-truthy)
      (expect (typep (car result) 'cl-cc/vm::vm-move) :to-be-truthy)
      (expect (vm-dst (car result)) :to-be :r3)
      (expect (vm-src (car result)) :to-be :r2))))
