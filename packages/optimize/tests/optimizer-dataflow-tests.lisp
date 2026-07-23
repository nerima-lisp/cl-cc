;;;; tests/unit/optimize/optimizer-dataflow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-dataflow.lisp
;;;;
;;;; Covers: %sccp-env-copy, %sccp-env-equal-p, %sccp-env-merge,
;;;;   %sccp-fold-inst (const, move, jump-zero, binary, unary),
;;;;   %sccp-redirect-successors, %sccp-process-block,
;;;;   opt-pass-sccp (empty, straight-line const fold).

(in-package :cl-cc/test)

;;; ─── %sccp-env-copy ───────────────────────────────────────────────────────

(it-sequential "sccp-env-copy-behavior"
  (let ((empty (make-hash-table :test #'eq)))
    (expect (eq empty (cl-cc/optimize::%sccp-env-copy empty)) :to-be-falsy))
  (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 42
          (gethash :r1 env) 99)
    (let ((copy (cl-cc/optimize::%sccp-env-copy env)))
      (expect (= 42 (gethash :r0 copy)) :to-be-truthy)
      (expect (= 99 (gethash :r1 copy)) :to-be-truthy)
      (setf (gethash :r0 copy) 999)
      (expect (= 42 (gethash :r0 env)) :to-be-truthy))))

;;; ─── %sccp-env-equal-p ───────────────────────────────────────────────────

(it-sequential "sccp-env-equal-p-both-empty-returns-true"
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (expect (cl-cc/optimize::%sccp-env-equal-p a b) :to-be-truthy)))

(it-sequential "sccp-env-equal-p-same-bindings-returns-true"
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 5)
    (expect (cl-cc/optimize::%sccp-env-equal-p a b) :to-be-truthy)))

(it-sequential "sccp-env-equal-p-different-values-returns-false"
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 6)
    (expect (cl-cc/optimize::%sccp-env-equal-p a b) :to-be-falsy)))

(it-sequential "sccp-env-equal-p-different-keys-returns-false"
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 1)
    (setf (gethash :r1 b) 1)
    (expect (cl-cc/optimize::%sccp-env-equal-p a b) :to-be-falsy)))

;;; ─── %sccp-env-merge ─────────────────────────────────────────────────────

(it-sequential "sccp-env-merge-empty-list-returns-empty-table"
  (expect (= 0 (hash-table-count (cl-cc/optimize::%sccp-env-merge nil))) :to-be-truthy))

(it-sequential "sccp-env-merge-singleton-list-returns-distinct-copy"
  (let* ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 7)
    (let ((merged (cl-cc/optimize::%sccp-env-merge (list env))))
      (expect (= 7 (gethash :r0 merged)) :to-be-truthy)
      (expect (eq env merged) :to-be-falsy))))

(it-sequential "sccp-env-merge-intersects-common-keys"
  (let* ((a (make-hash-table :test #'eq))
         (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 5)   ; agree
    (setf (gethash :r1 a) 3  (gethash :r1 b) 9)   ; disagree
    (setf (gethash :r2 a) 1)                        ; only in a
    (let ((merged (cl-cc/optimize::%sccp-env-merge (list a b))))
      (expect (= 5 (gethash :r0 merged)) :to-be-truthy)
      (expect (nth-value 1 (gethash :r1 merged)) :to-be-falsy)
      (expect (nth-value 1 (gethash :r2 merged)) :to-be-falsy))))

;;; ─── %sccp-fold-inst ─────────────────────────────────────────────────────

(it-sequential "sccp-fold-inst-passthrough-cases const"
  (destructuring-bind (inst) (list (make-vm-const :dst :r0 :value 42))
    (expect (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq)) :to-be inst)))

(it-sequential "sccp-fold-inst-passthrough-cases label"
  (destructuring-bind (inst) (list (make-vm-label :name "x"))
    (expect (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq)) :to-be inst)))

(it-sequential "sccp-fold-inst-move-with-known-src-folds-to-const"
  (let* ((inst (make-vm-move :dst :r1 :src :r0))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 77)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (expect (typep result 'cl-cc/vm::vm-const) :to-be-truthy)
      (expect (= 77 (cl-cc/vm::vm-value result)) :to-be-truthy))))

(it-sequential "sccp-fold-inst-unknown-passthrough-cases move"
  (destructuring-bind (inst) (list (make-vm-move :dst :r1 :src :r0))
    (expect (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq)) :to-be inst)))

(it-sequential "sccp-fold-inst-unknown-passthrough-cases binary"
  (destructuring-bind (inst) (list (make-vm-add  :dst :r2 :lhs :r0 :rhs :r1))
    (expect (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq)) :to-be inst)))

(it-sequential "sccp-fold-inst-binary-folds-when-both-known"
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 3
          (gethash :r1 env) 4)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (expect (typep result 'cl-cc/vm::vm-const) :to-be-truthy)
      (expect (= 7 (cl-cc/vm::vm-value result)) :to-be-truthy))))

;;; ─── %sccp-redirect-successors ──────────────────────────────────────────

(it-sequential "sccp-redirect-successors-rewires-edges"
  (let* ((cfg    (cl-cc/optimize:cfg-build
                  (list (make-vm-const :dst :r0 :value 0)
                        (make-vm-jump  :label "lbl")
                        (make-vm-label :name "lbl")
                        (make-vm-ret   :reg :r0))))
         (entry  (cl-cc/optimize:cfg-entry cfg))
         (succ   (first (cl-cc:bb-successors entry)))
         (extra  (cl-cc/optimize::cfg-new-block cfg)))
    (cl-cc/optimize::%sccp-redirect-successors entry (list extra))
    (expect (member extra (cl-cc:bb-successors entry) :test #'eq) :to-be-truthy)
    (expect (member succ  (cl-cc:bb-successors entry) :test #'eq) :to-be-falsy)
    (expect (member entry (cl-cc:bb-predecessors extra) :test #'eq) :to-be-truthy)
    (expect (member entry (cl-cc:bb-predecessors succ)  :test #'eq) :to-be-falsy)))

;;; ─── %sccp-process-block ─────────────────────────────────────────────────

(it-sequential "sccp-process-block-folds-const-in-block"
  (let* ((env   (make-hash-table :test #'eq))
         (cfg   (cl-cc/optimize:cfg-build
                 (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                       (make-vm-ret :reg :r2))))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (setf (gethash :r0 env) 3 (gethash :r1 env) 4)
    (let ((out-env (cl-cc/optimize::%sccp-process-block entry env)))
      (let ((folded (first (cl-cc:bb-instructions entry))))
        (expect (typep folded 'cl-cc/vm::vm-const) :to-be-truthy)
        (expect (= 7 (cl-cc/vm::vm-value folded)) :to-be-truthy))
      (expect (= 7 (gethash :r2 out-env)) :to-be-truthy))))

;;; ─── opt-pass-sccp ───────────────────────────────────────────────────────

(it-sequential "sccp-pass-empty-returns-nil-or-list"
  (let ((result (cl-cc/optimize::opt-pass-sccp nil)))
    (expect (listp result) :to-be-truthy)))

(it-sequential "sccp-pass-fold-and-preserve"
  (let* ((insts (list (make-vm-const :dst :r0 :value 10)
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const)) result)))
      (expect (some (lambda (c) (= 30 (cl-cc/vm::vm-value c))) consts) :to-be-truthy)))
  (let* ((insts (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret  :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) result) :to-be-truthy)))

;;; ─── %sccp-update-env-for-inst ───────────────────────────────────────────

(it-sequential "sccp-update-env-for-inst-cases const-binds"
  (destructuring-bind (inst expected-val) (list (make-vm-const :dst :r0 :value 42) 42)
    (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 99)
    (cl-cc/optimize::%sccp-update-env-for-inst inst env)
    (if expected-val
        (expect (= expected-val (gethash :r0 env)) :to-be-truthy)
        (expect (nth-value 1 (gethash :r0 env)) :to-be-falsy)))))

(it-sequential "sccp-update-env-for-inst-cases non-const-kills"
  (destructuring-bind (inst expected-val) (list (make-vm-add :dst :r0 :lhs :r1 :rhs :r2) nil)
    (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 99)
    (cl-cc/optimize::%sccp-update-env-for-inst inst env)
    (if expected-val
        (expect (= expected-val (gethash :r0 env)) :to-be-truthy)
        (expect (nth-value 1 (gethash :r0 env)) :to-be-falsy)))))

(it-sequential "sccp-update-env-for-inst-no-dst-noop"
  (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 7)
    (cl-cc/optimize::%sccp-update-env-for-inst (make-vm-ret :reg :r0) env)
    (expect (= 7 (gethash :r0 env)) :to-be-truthy)))

;;; ─── %sccp-fold-inst jump-zero cases ─────────────────────────────────────

(it-sequential "sccp-fold-inst-jump-zero-false-becomes-jump"
  (let* ((inst (make-vm-jump-zero :reg :r0 :label "done"))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 0)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (expect (typep result 'cl-cc/vm::vm-jump) :to-be-truthy)
      (expect (cl-cc/vm::vm-label-name result) :to-equal "done"))))

(it-sequential "sccp-fold-inst-jump-zero-true-eliminates-branch"
  (let* ((inst (make-vm-jump-zero :reg :r0 :label "done"))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 1)
    (expect (cl-cc/optimize::%sccp-fold-inst inst env) :to-be-null)))

;;; ─── Generic dataflow worklist ───────────────────────────────────────────

(it-sequential "dataflow-worklist-forward-branch-join-converges"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :r9 :label "else")
                     (make-vm-const :dst :r0 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r1 :value 2)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r0))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-run-dataflow
                  cfg
                  :direction :forward
                  :meet (lambda (states)
                          (remove-duplicates (apply #'append states) :test #'eql))
                  :transfer (lambda (block state)
                              (adjoin (cl-cc:bb-id block) state :test #'eql))
                  :state-equal (lambda (a b)
                                 (and (subsetp a b :test #'eql)
                                      (subsetp b a :test #'eql)))
                  :initial-state nil
                  :boundary-state nil
                  :copy-state #'copy-list))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result))))
    (expect (member 0 join-in :test #'eql) :to-be-truthy)
    (dolist (pred (cl-cc:bb-predecessors join))
      (expect (member (cl-cc:bb-id pred) join-in :test #'eql) :to-be-truthy))))

;;; ─── Available expressions ───────────────────────────────────────────────

(it-sequential "available-expressions-join-intersects-predecessors"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :r9 :label "else")
                     (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                     (make-vm-sub :dst :r3 :lhs :r0 :rhs :r1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-add :dst :r4 :lhs :r0 :rhs :r1)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r0))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-available-expressions cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result)))
         (keys (mapcar #'cl-cc/optimize::%available-expression-entry-key join-in)))
    (expect (member '(cl-cc/vm::vm-add :r0 :r1) keys :test #'equal) :to-be-truthy)
    (expect (member '(cl-cc/vm::vm-sub :r0 :r1) keys :test #'equal) :to-be-falsy)))

;;; ─── Reaching definitions ────────────────────────────────────────────────

(it-sequential "reaching-definitions-join-unions-predecessors"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :r9 :label "else")
                     (make-vm-const :dst :r1 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r1 :value 2)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r1))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-reaching-definitions cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result)))
         (r1-defs (remove :r1 join-in :test-not #'eq :key #'first)))
    (expect (= 2 (length r1-defs)) :to-be-truthy)
    (dolist (pred (cl-cc:bb-predecessors join))
      (expect (some (lambda (definition)
                           (= (cl-cc:bb-id pred) (second definition)))
                          r1-defs) :to-be-truthy))))

;;; ─── Abstract interpretation framework (FR-251 partial) ──────────────────

(it-sequential "abstract-domain-struct-retains-operators"
  (let* ((domain (cl-cc/optimize::make-opt-abstract-domain
                  :name :test
                  :top :top
                  :bottom nil
                  :join (lambda (a b) (append a b))
                  :leq (lambda (a b) (equal a b))
                  :transfer (lambda (_block in) in))))
    (expect (cl-cc/optimize::opt-domain-name domain) :to-be :test)
    (expect (cl-cc/optimize::opt-domain-top domain) :to-be :top)
    (expect (functionp (cl-cc/optimize::opt-domain-join domain)) :to-be-truthy)
    (expect (functionp (cl-cc/optimize::opt-domain-transfer domain)) :to-be-truthy)))

(it-sequential "abstract-interpretation-runs-over-cfg-and-produces-result"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (domain (cl-cc/optimize::make-opt-abstract-domain
                  :name :ids
                  :top nil
                  :bottom nil
                  :join (lambda (a b)
                          (remove-duplicates (append a b) :test #'eql))
                  :leq (lambda (a b)
                         (and (subsetp a b :test #'eql)
                              (subsetp b a :test #'eql)))
                  :transfer (lambda (block in)
                              (adjoin (cl-cc:bb-id block) in :test #'eql))))
         (result (cl-cc/optimize::opt-run-abstract-interpretation cfg domain))
         (out (gethash entry (cl-cc/optimize:opt-dataflow-result-out result))))
    (expect (typep result 'cl-cc/optimize::opt-dataflow-result) :to-be-truthy)
    (expect (member (cl-cc:bb-id entry) out :test #'eql) :to-be-truthy)))

(it-sequential "integer-interval-abstract-interpretation-propagates-const-and-add"
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-const :dst :r0 :value 10)
                     (make-vm-const :dst :r1 :value 20)
                     (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                     (make-vm-ret :reg :r2))))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (result (cl-cc/optimize::opt-run-integer-interval-abstract-interpretation cfg))
         (out (gethash entry (cl-cc/optimize:opt-dataflow-result-out result))))
    (expect (gethash :r0 out) :to-equal '(10 . 10))
    (expect (gethash :r1 out) :to-equal '(20 . 20))
    (expect (gethash :r2 out) :to-equal '(30 . 30))))
