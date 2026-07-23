;;;; tests/unit/optimize/optimizer-copyprop-tests.lisp — Copy Propagation Tests
;;;;
;;;; Tests for src/optimize/optimizer-copyprop.lisp:
;;;;   opt-map-tree, %opt-copy-prop-env-copy/equal-p/canonical/merge,
;;;;   %opt-copy-prop-add/kill, %opt-value<, copyprop-pass-state helpers,
;;;;   opt-pass-copy-prop.

(in-package :cl-cc/test)


;;; ── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-copy-env (&rest pairs)
  "Build a copy-propagation environment hash-table from PAIRS (k v k v ...)."
  (let ((env (make-hash-table :test #'eq)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k env) v))
    env))

(defun %copy-env-get (env key)
  "Return the value for KEY in ENV, or NIL if absent."
  (gethash key env))

;;; ── opt-map-tree ────────────────────────────────────────────────────────────

(it-sequential "copyprop-map-tree-basic atom"
  (destructuring-bind (expected tree fn) (list 2 1 (lambda (x) (if (numberp x) (* x 2) x)))
    (expect (cl-cc/optimize::opt-map-tree fn tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-basic nil-leaf"
  (destructuring-bind (expected tree fn) (list nil nil #'identity)
    (expect (cl-cc/optimize::opt-map-tree fn tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-basic sym-leaf"
  (destructuring-bind (expected tree fn) (list :r0 :r0 #'identity)
    (expect (cl-cc/optimize::opt-map-tree fn tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-basic double-sym"
  (destructuring-bind (expected tree fn) (list :r0 :r0 (lambda (x) (if (eq x :r1) :r0 x)))
    (expect (cl-cc/optimize::opt-map-tree fn tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-structured pair"
  (destructuring-bind (tree expected) (list '(1 . 2) '(2 . 4))
    (expect (cl-cc/optimize::opt-map-tree (lambda (x) (if (numberp x) (* x 2) x)) tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-structured proper"
  (destructuring-bind (tree expected) (list '(1 2 3) '(2 4 6))
    (expect (cl-cc/optimize::opt-map-tree (lambda (x) (if (numberp x) (* x 2) x)) tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-structured improper"
  (destructuring-bind (tree expected) (list '(1 2 . 3) '(2 4 . 6))
    (expect (cl-cc/optimize::opt-map-tree (lambda (x) (if (numberp x) (* x 2) x)) tree) :to-equal expected)))

(it-sequential "copyprop-map-tree-rewrite-register"
  (let* ((sexp   '(:add :r2 :r1 :r1))
         (result (cl-cc/optimize::opt-map-tree (lambda (x) (if (eq x :r1) :r0 x)) sexp)))
    (expect result :to-equal '(:add :r2 :r0 :r0))))

;;; ── %opt-copy-prop-env-copy / %opt-copy-prop-env-equal-p ───────────────────

(it-sequential "copyprop-env-copy-is-independent"
  (let* ((env  (%make-copy-env :r1 :r0 :r2 :r0))
         (copy (cl-cc/optimize::%opt-copy-prop-env-copy env)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p env copy) :to-be-truthy)
    (remhash :r1 copy)
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p env copy) :to-be-falsy)))

(it-sequential "copyprop-env-copy-empty-yields-empty"
  (let* ((env  (%make-copy-env))
         (copy (cl-cc/optimize::%opt-copy-prop-env-copy env)))
    (expect (hash-table-count copy) :to-equal 0)))

(it-sequential "copyprop-env-equal-p both-empty"
  (destructuring-bind (expected pairs-a pairs-b) (list t '() '())
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

(it-sequential "copyprop-env-equal-p one-binding"
  (destructuring-bind (expected pairs-a pairs-b) (list t '(:r1 :r0) '(:r1 :r0))
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

(it-sequential "copyprop-env-equal-p two-bindings"
  (destructuring-bind (expected pairs-a pairs-b) (list t '(:r1 :r0 :r2 :r0) '(:r2 :r0 :r1 :r0))
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

(it-sequential "copyprop-env-equal-p diff-value"
  (destructuring-bind (expected pairs-a pairs-b) (list nil '(:r1 :r0) '(:r1 :r2))
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

(it-sequential "copyprop-env-equal-p size-mismatch"
  (destructuring-bind (expected pairs-a pairs-b) (list nil '(:r1 :r0 :r2 :r0) '(:r1 :r0))
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

(it-sequential "copyprop-env-equal-p extra-key"
  (destructuring-bind (expected pairs-a pairs-b) (list nil '(:r1 :r0) '(:r1 :r0 :r2 :r1))
    (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p a b) :to-equal expected))))

;;; ── %opt-copy-prop-canonical ────────────────────────────────────────────────

(it-sequential "copyprop-canonical absent"
  (destructuring-bind (expected reg pairs) (list :r0 :r0 '())
    (let ((env (apply #'%make-copy-env pairs)))
    (expect (cl-cc/optimize::%opt-copy-prop-canonical reg env) :to-be expected))))

(it-sequential "copyprop-canonical self-absent"
  (destructuring-bind (expected reg pairs) (list :r1 :r1 '())
    (let ((env (apply #'%make-copy-env pairs)))
    (expect (cl-cc/optimize::%opt-copy-prop-canonical reg env) :to-be expected))))

(it-sequential "copyprop-canonical direct"
  (destructuring-bind (expected reg pairs) (list :r0 :r1 '(:r1 :r0))
    (let ((env (apply #'%make-copy-env pairs)))
    (expect (cl-cc/optimize::%opt-copy-prop-canonical reg env) :to-be expected))))

(it-sequential "copyprop-canonical two-hop"
  (destructuring-bind (expected reg pairs) (list :r0 :r2 '(:r2 :r1 :r1 :r0))
    (let ((env (apply #'%make-copy-env pairs)))
    (expect (cl-cc/optimize::%opt-copy-prop-canonical reg env) :to-be expected))))

(it-sequential "copyprop-canonical three-hop"
  (destructuring-bind (expected reg pairs) (list :r0 :r3 '(:r3 :r2 :r2 :r1 :r1 :r0))
    (let ((env (apply #'%make-copy-env pairs)))
    (expect (cl-cc/optimize::%opt-copy-prop-canonical reg env) :to-be expected))))

(it-sequential "copyprop-canonical-termination-cases self-loop"
  (destructuring-bind (pairs pred) (list '(:r0 :r0) (lambda (r) (eq r :r0)))
    (let* ((env    (apply #'%make-copy-env pairs))
         (result (cl-cc/optimize::%opt-copy-prop-canonical :r0 env)))
    (expect (funcall pred result) :to-be-truthy))))

(it-sequential "copyprop-canonical-termination-cases cycle"
  (destructuring-bind (pairs pred) (list '(:r0 :r1 :r1 :r0) (lambda (r) (or (eq r :r0) (eq r :r1))))
    (let* ((env    (apply #'%make-copy-env pairs))
         (result (cl-cc/optimize::%opt-copy-prop-canonical :r0 env)))
    (expect (funcall pred result) :to-be-truthy))))

;;; ── %opt-copy-prop-add / %opt-copy-prop-kill ────────────────────────────────

(it-sequential "copyprop-add-registers-copy"
  (let* ((copies  (make-hash-table :test #'eq))
         (reverse (make-hash-table :test #'eq)))
    (cl-cc/optimize::%opt-copy-prop-add :r1 :r0 copies reverse)
    (expect (gethash :r1 copies) :to-be :r0)
    (expect (member :r1 (gethash :r0 reverse)) :to-be-truthy)))

(it-sequential "copyprop-kill-cases kill-source"
  (destructuring-bind (kill-reg removed-regs surviving-r2-val) (list :r0 '(:r1 :r2) nil)
    (let* ((copies  (%make-copy-env :r1 :r0 :r2 :r0))
         (reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies)))
    (cl-cc/optimize::%opt-copy-prop-kill kill-reg copies reverse)
    (dolist (r removed-regs)
      (expect (gethash r copies) :to-be-falsy))
    (when surviving-r2-val
      (expect (gethash :r2 copies) :to-be surviving-r2-val)))))

(it-sequential "copyprop-kill-cases kill-dst"
  (destructuring-bind (kill-reg removed-regs surviving-r2-val) (list :r1 '(:r1) :r0)
    (let* ((copies  (%make-copy-env :r1 :r0 :r2 :r0))
         (reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies)))
    (cl-cc/optimize::%opt-copy-prop-kill kill-reg copies reverse)
    (dolist (r removed-regs)
      (expect (gethash r copies) :to-be-falsy))
    (when surviving-r2-val
      (expect (gethash :r2 copies) :to-be surviving-r2-val)))))

;;; ── %opt-copy-prop-merge ────────────────────────────────────────────────────

(it-sequential "copyprop-merge-empty-result-cases empty-input"
  (destructuring-bind (envs) (list nil)
    (expect (hash-table-count (cl-cc/optimize::%opt-copy-prop-merge envs)) :to-equal 0)))

(it-sequential "copyprop-merge-empty-result-cases disjoint"
  (destructuring-bind (envs) (list (list (%make-copy-env :r1 :r0) (%make-copy-env :r3 :r2)))
    (expect (hash-table-count (cl-cc/optimize::%opt-copy-prop-merge envs)) :to-equal 0)))

(it-sequential "copyprop-merge-single"
  (let* ((env    (%make-copy-env :r1 :r0 :r2 :r0))
         (result (cl-cc/optimize::%opt-copy-prop-merge (list env))))
    (expect (cl-cc/optimize::%opt-copy-prop-env-equal-p env result) :to-be-truthy)))

(it-sequential "copyprop-merge-disagreement-cases two-way"
  (destructuring-bind (envs) (list (list (%make-copy-env :r1 :r0 :r2 :r0)
                             (%make-copy-env :r1 :r0 :r2 :r3)))
    (let ((result (cl-cc/optimize::%opt-copy-prop-merge envs)))
    (expect (gethash :r1 result) :to-be :r0)
    (expect (gethash :r2 result) :to-be-falsy))))

(it-sequential "copyprop-merge-disagreement-cases three-way"
  (destructuring-bind (envs) (list (list (%make-copy-env :r1 :r0 :r2 :r0)
                             (%make-copy-env :r1 :r0 :r2 :r3)
                             (%make-copy-env :r1 :r0)))
    (let ((result (cl-cc/optimize::%opt-copy-prop-merge envs)))
    (expect (gethash :r1 result) :to-be :r0)
    (expect (gethash :r2 result) :to-be-falsy))))

;;; ── %opt-value-rank / *opt-type-rank-table* ─────────────────────────────────

(it-sequential "copyprop-value-rank-types null"
  (destructuring-bind (expected value) (list 0 nil)
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types number"
  (destructuring-bind (expected value) (list 1 42)
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types character"
  (destructuring-bind (expected value) (list 2 #\a)
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types string"
  (destructuring-bind (expected value) (list 3 "hello")
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types symbol"
  (destructuring-bind (expected value) (list 4 'foo)
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types cons"
  (destructuring-bind (expected value) (list 5 '(a b))
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types vector"
  (destructuring-bind (expected value) (list 6 #(1 2 3))
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

(it-sequential "copyprop-value-rank-types unknown"
  (destructuring-bind (expected value) (list 7 #'identity)
    (expect (= expected (cl-cc/optimize::%opt-value-rank value)) :to-be-truthy)))

;;; ── %opt-value< ─────────────────────────────────────────────────────────────

(it-sequential "copyprop-value<-cross-type nil<num"
  (destructuring-bind (expected a b) (list t nil 1)
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-cross-type num<char"
  (destructuring-bind (expected a b) (list t 1 #\a)
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-cross-type char<str"
  (destructuring-bind (expected a b) (list t #\a "z")
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-cross-type str<sym"
  (destructuring-bind (expected a b) (list t "z" :a)
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-cross-type irrefl-nil"
  (destructuring-bind (expected a b) (list nil nil nil)
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-cross-type irrefl-num"
  (destructuring-bind (expected a b) (list nil 5 5)
    (expect (cl-cc/optimize::%opt-value< a b) :to-equal expected)))

(it-sequential "copyprop-value<-same-type num-less"
  (destructuring-bind (expected a b) (list t 3 5)
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "copyprop-value<-same-type num-greater"
  (destructuring-bind (expected a b) (list nil 5 3)
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "copyprop-value<-same-type str-less"
  (destructuring-bind (expected a b) (list t "ab" "b")
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "copyprop-value<-same-type str-equal"
  (destructuring-bind (expected a b) (list nil "ab" "ab")
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

(it-sequential "copyprop-value<-same-type char-less"
  (destructuring-bind (expected a b) (list t #\a #\z)
    (expect (not (null (cl-cc/optimize::%opt-value< a b))) :to-equal expected)))

;;; ── opt-pass-copy-prop integration ──────────────────────────────────────────

(defun %copyprop-find (instrs type-name)
  "Return the first instruction of TYPE-NAME in INSTRS, or NIL."
  (let ((type (find-symbol (symbol-name type-name) :cl-cc)))
    (find-if (lambda (i) (typep i type)) instrs)))

(it-sequential "copyprop-pass-empty-input"
  (expect (cl-cc/optimize::opt-pass-copy-prop nil) :to-equal nil))

(it-sequential "copyprop-pass-no-moves-unchanged"
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (expect add :to-be-truthy)
    (expect (vm-lhs add) :to-be :r0)
    (expect (vm-rhs add) :to-be :r1)))

(it-sequential "copyprop-pass-basic-rewrite"
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 42)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (expect add :to-be-truthy)
    (expect (vm-lhs add) :to-be :r0)
    (expect (vm-rhs add) :to-be :r0)))

(it-sequential "copyprop-pass-chain-rewrite"
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 7)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-move  :dst :r2 :src :r1)
                       (make-vm-add   :dst :r3 :lhs :r2 :rhs :r0)
                       (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (expect add :to-be-truthy)
    (expect (vm-lhs add) :to-be :r0)))

(it-sequential "copyprop-pass-kill-stops-propagation"
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-const :dst :r1 :value 99)
                       (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (expect add :to-be-truthy)
    (expect (vm-lhs add) :to-be :r1)
    (expect (vm-rhs add) :to-be :r1)))

(it-sequential "copyprop-pass-preserves-labels"
  (let* ((instrs (list (make-vm-label :name "start")
                       (make-vm-const :dst :r0 :value 0)
                       (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (labels (loop for i in result
                       when (typep i 'cl-cc/vm::vm-label)
                       collect (cl-cc:vm-lbl-name i))))
    (expect (member "start" labels :test #'equal) :to-be-truthy)))

;;; ── copyprop-pass-state helpers ─────────────────────────────────────────────

(it-sequential "copyprop-enqueue-is-idempotent"
  (let ((state (cl-cc/optimize::make-copyprop-pass-state))
        (blk   (cl-cc/optimize::cfg-new-block (cl-cc/optimize:make-cfg))))
    (cl-cc/optimize::%copyprop-enqueue blk state)
    (expect (= 1 (length (cl-cc/optimize::cpps-worklist state))) :to-be-truthy)
    (cl-cc/optimize::%copyprop-enqueue blk state)
    (expect (= 1 (length (cl-cc/optimize::cpps-worklist state))) :to-be-truthy)))

(it-sequential "copyprop-enqueue-two-distinct-blocks"
  (let ((state (cl-cc/optimize::make-copyprop-pass-state))
        (b1    (cl-cc/optimize::cfg-new-block (cl-cc/optimize:make-cfg)))
        (b2    (cl-cc/optimize::cfg-new-block (cl-cc/optimize:make-cfg))))
    (cl-cc/optimize::%copyprop-enqueue b1 state)
    (cl-cc/optimize::%copyprop-enqueue b2 state)
    (expect (= 2 (length (cl-cc/optimize::cpps-worklist state))) :to-be-truthy)
    (cl-cc/optimize::%copyprop-enqueue b1 state)
    (expect (= 2 (length (cl-cc/optimize::cpps-worklist state))) :to-be-truthy)))

(it-sequential "copyprop-process-block-propagates-copy-to-successor"
  (let* ((cfg   (cl-cc/optimize:cfg-build
                 (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-jump  :label "end")
                       (make-vm-label :name "end")
                       (make-vm-ret   :reg :r1))))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (state (cl-cc/optimize::make-copyprop-pass-state)))
    (cl-cc/optimize::%copyprop-process-block entry state)
    (let ((out (gethash entry (cl-cc/optimize::cpps-out-envs state))))
      (expect (hash-table-p out) :to-be-truthy))))

;;; ── %opt-copy-prop-build-reverse ─────────────────────────────────────────

(it-sequential "copyprop-build-reverse-builds-reverse-map"
  (let ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r1 copies) :r0
          (gethash :r2 copies) :r0)
    (let ((rev (cl-cc/optimize::%opt-copy-prop-build-reverse copies)))
      (expect (member :r1 (gethash :r0 rev) :test #'eq) :to-be-truthy)
      (expect (member :r2 (gethash :r0 rev) :test #'eq) :to-be-truthy))))

(it-sequential "copyprop-build-reverse-empty-input"
  (let ((rev (cl-cc/optimize::%opt-copy-prop-build-reverse
              (make-hash-table :test #'eq))))
    (expect (= 0 (hash-table-count rev)) :to-be-truthy)))

;;; ── %opt-copy-prop-transfer-block ────────────────────────────────────────

(it-sequential "copyprop-transfer-block-records-move"
  (let* ((blk    (make-instance 'cl-cc/optimize:basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (cl-cc/optimize:bb-instructions blk)
          (list (make-vm-move :dst :r1 :src :r0)))
    (let ((out (cl-cc/optimize::%opt-copy-prop-transfer-block blk in-env)))
      (expect (gethash :r1 out) :to-be :r0))))

(it-sequential "copyprop-transfer-block-kills-overwritten"
  (let* ((blk    (make-instance 'cl-cc/optimize:basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (gethash :r1 in-env) :r0)
    (setf (cl-cc/optimize:bb-instructions blk)
          (list (make-vm-const :dst :r1 :value 99)))
    (let ((out (cl-cc/optimize::%opt-copy-prop-transfer-block blk in-env)))
      (expect (gethash :r1 out) :to-be-null))))

;;; ── %opt-copy-prop-rewrite-inst ─────────────────────────────────────────

(it-sequential "copyprop-rewrite-inst-substitutes-copies"
  (let ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r5)
    (let* ((inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r0))
           (result (cl-cc/optimize::%opt-copy-prop-rewrite-inst inst copies)))
      (expect (cl-cc/vm::vm-lhs result) :to-be :r5)
      (expect (cl-cc/vm::vm-rhs result) :to-be :r5))))

(it-sequential "copyprop-rewrite-inst-identity-when-no-copy"
  (let ((copies (make-hash-table :test #'eq))
        (inst   (make-vm-const :dst :r0 :value 1)))
    (expect (cl-cc/optimize::%opt-copy-prop-rewrite-inst inst copies) :to-be inst)))

;;; ── %opt-copy-prop-rewrite-block ────────────────────────────────────────

(it-sequential "copy-prop-rewrite-block-rewrites-instructions"
  (let* ((blk    (make-instance 'cl-cc/optimize:basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 in-env) :r5)
    (setf (cl-cc/optimize:bb-instructions blk)
          (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r0)))
    (let ((result (cl-cc/optimize::%opt-copy-prop-rewrite-block blk in-env)))
      (expect (= 1 (length result)) :to-be-truthy)
      (expect (cl-cc/vm::vm-lhs (first result)) :to-be :r5))))
