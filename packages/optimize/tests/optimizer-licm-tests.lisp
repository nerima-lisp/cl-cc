;;;; tests/unit/optimize/optimizer-licm-tests.lisp
;;;; Unit tests for src/optimize/optimizer-licm.lisp
;;;;
;;;; Covers: opt-inst-loop-invariant-p, %opt-pre-expression-key,
;;;;   %opt-pre-splice-before-terminator, opt-pass-licm (trivial paths),
;;;;   optimize-with-egraph.

(in-package :cl-cc/test)

(defmacro assert-licm-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── opt-inst-loop-invariant-p ───────────────────────────────────────────

(it-sequential "licm-invariant-p-cases pure-const"
  (destructuring-bind (inst def-reg expected) (list (make-vm-const :dst :r0 :value 42) nil t)
    (let ((loop-def-regs (make-hash-table :test #'eq))
        (loop-members  (make-hash-table :test #'eq))
        (def-sites     (make-hash-table :test #'eq)))
    (when def-reg
      (setf (gethash def-reg loop-def-regs) t))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-truthy)
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-falsy)))))

(it-sequential "licm-invariant-p-cases dst-in-loop"
  (destructuring-bind (inst def-reg expected) (list (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0) :r0 nil)
    (let ((loop-def-regs (make-hash-table :test #'eq))
        (loop-members  (make-hash-table :test #'eq))
        (def-sites     (make-hash-table :test #'eq)))
    (when def-reg
      (setf (gethash def-reg loop-def-regs) t))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-truthy)
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-falsy)))))

(it-sequential "licm-invariant-p-cases impure"
  (destructuring-bind (inst def-reg expected) (list (make-vm-halt  :reg :r0) nil nil)
    (let ((loop-def-regs (make-hash-table :test #'eq))
        (loop-members  (make-hash-table :test #'eq))
        (def-sites     (make-hash-table :test #'eq)))
    (when def-reg
      (setf (gethash def-reg loop-def-regs) t))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-truthy)
      (expect (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites) :to-be-falsy)))))

;;; ─── %opt-pre-expression-key ─────────────────────────────────────────────

(it-sequential "pre-expression-key-cases const"
  (destructuring-bind (inst shape expected) (list (make-vm-const :dst :r0 :value 7) :const '(:const 7))
    (let ((key (cl-cc/optimize::%opt-pre-expression-key inst)))
    (ecase shape
      (:const  (expect key :to-equal expected))
      (:binary (expect (consp key) :to-be-truthy) (expect (car key) :to-be 'cl-cc/vm::vm-add))
      (:null   (expect key :to-be-null))))))

(it-sequential "pre-expression-key-cases binary"
  (destructuring-bind (inst shape expected) (list (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1) :binary nil)
    (let ((key (cl-cc/optimize::%opt-pre-expression-key inst)))
    (ecase shape
      (:const  (expect key :to-equal expected))
      (:binary (expect (consp key) :to-be-truthy) (expect (car key) :to-be 'cl-cc/vm::vm-add))
      (:null   (expect key :to-be-null))))))

(it-sequential "pre-expression-key-cases impure"
  (destructuring-bind (inst shape expected) (list (make-vm-halt  :reg :r0) :null nil)
    (let ((key (cl-cc/optimize::%opt-pre-expression-key inst)))
    (ecase shape
      (:const  (expect key :to-equal expected))
      (:binary (expect (consp key) :to-be-truthy) (expect (car key) :to-be 'cl-cc/vm::vm-add))
      (:null   (expect key :to-be-null))))))

(it-sequential "pre-expression-key-commutative"
  (let ((key-ab (cl-cc/optimize::%opt-pre-expression-key (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
        (key-ba (cl-cc/optimize::%opt-pre-expression-key (make-vm-add :dst :r2 :lhs :r1 :rhs :r0))))
    (expect key-ba :to-equal key-ab)))

;;; ─── %opt-pre-splice-before-terminator ───────────────────────────────────

(it-sequential "pre-splice-inserts-before-terminator before-jump"
  (destructuring-bind (insts term-type) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "end")) 'cl-cc/vm::vm-jump)
    (let* ((extra  (make-vm-const :dst :r9 :value 0))
         (result (cl-cc/optimize::%opt-pre-splice-before-terminator insts (list extra))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (typep (second result) 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (typep (third  result) term-type) :to-be-truthy))))

(it-sequential "pre-splice-inserts-before-terminator before-ret"
  (destructuring-bind (insts term-type) (list (list (make-vm-move  :dst :r0 :src :r1) (make-vm-ret  :reg  :r0)) 'cl-cc/vm::vm-ret)
    (let* ((extra  (make-vm-const :dst :r9 :value 0))
         (result (cl-cc/optimize::%opt-pre-splice-before-terminator insts (list extra))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (typep (second result) 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (typep (third  result) term-type) :to-be-truthy))))

(it-sequential "pre-splice-appends-when-no-terminator"
  (let* ((const (make-vm-const :dst :r0 :value 5))
         (extra (make-vm-const :dst :r1 :value 6))
         (result (cl-cc/optimize::%opt-pre-splice-before-terminator
                  (list const)
                  (list extra))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (typep (second result) 'cl-cc/vm::vm-const) :to-be-truthy)))

;;; ─── %opt-pre-env-evict-dst ──────────────────────────────────────────────

(it-sequential "licm-pre-env-evict-dst-cases evicts-matching"
  (destructuring-bind (dst initial-alist expected-alist) (list :r0 '((:a . :r0) (:b . :r1)) '((:b . :r1)))
    (let ((env (make-hash-table :test #'equal)))
    (dolist (pair initial-alist)
      (setf (gethash (car pair) env) (cdr pair)))
    (cl-cc/optimize::%opt-pre-env-evict-dst env dst)
    (let ((result (loop for k being the hash-keys of env using (hash-value v) collect (cons k v))))
      (expect (= (length expected-alist) (length result)) :to-be-truthy)
      (dolist (pair expected-alist)
        (expect (gethash (car pair) env) :to-equal (cdr pair)))))))

(it-sequential "licm-pre-env-evict-dst-cases noop-no-match"
  (destructuring-bind (dst initial-alist expected-alist) (list :r9 '((:a . :r0) (:b . :r1)) '((:a . :r0) (:b . :r1)))
    (let ((env (make-hash-table :test #'equal)))
    (dolist (pair initial-alist)
      (setf (gethash (car pair) env) (cdr pair)))
    (cl-cc/optimize::%opt-pre-env-evict-dst env dst)
    (let ((result (loop for k being the hash-keys of env using (hash-value v) collect (cons k v))))
      (expect (= (length expected-alist) (length result)) :to-be-truthy)
      (dolist (pair expected-alist)
        (expect (gethash (car pair) env) :to-equal (cdr pair)))))))

(it-sequential "licm-pre-env-evict-dst-cases evicts-all"
  (destructuring-bind (dst initial-alist expected-alist) (list :r0 '((:a . :r0) (:b . :r0)) nil)
    (let ((env (make-hash-table :test #'equal)))
    (dolist (pair initial-alist)
      (setf (gethash (car pair) env) (cdr pair)))
    (cl-cc/optimize::%opt-pre-env-evict-dst env dst)
    (let ((result (loop for k being the hash-keys of env using (hash-value v) collect (cons k v))))
      (expect (= (length expected-alist) (length result)) :to-be-truthy)
      (dolist (pair expected-alist)
        (expect (gethash (car pair) env) :to-equal (cdr pair)))))))

;;; ─── opt-pass-licm (trivial paths) ───────────────────────────────────────

(it-sequential "licm-pass-returns-nil-for-empty-input"
  (expect (cl-cc/optimize::opt-pass-licm nil) :to-be-null))

(it-sequential "licm-pass-returns-straight-line-code-unchanged"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-licm insts)))
    (expect (= (length insts) (length result)) :to-be-truthy)))

;;; ─── %opt-pre-reconstruct-inst ──────────────────────────────────────────

(it-sequential "pre-reconstruct-inst-round-trips-vm-const"
  (let* ((inst   (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::%opt-pre-reconstruct-inst inst)))
    (expect (typep result 'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (cl-cc/vm::vm-value result) :to-equal 42)))

(it-sequential "pre-reconstruct-inst-passthrough-for-unrecognized"
  (let* ((inst   (make-vm-halt :reg :r0))
         (result (cl-cc/optimize::%opt-pre-reconstruct-inst inst)))
    (expect (typep result 'cl-cc/vm::vm-halt) :to-be-truthy)))

;;; ─── %opt-pre-available-in-any-p ────────────────────────────────────────

(defun %pre-envs-with-key (key in-env1-p in-env2-p)
  "Build two predecessor (label . env) pairs; seed KEY in each env according to the boolean flags."
  (let ((e1 (make-hash-table :test #'equal))
        (e2 (make-hash-table :test #'equal)))
    (when in-env1-p (setf (gethash key e1) :r0))
    (when in-env2-p (setf (gethash key e2) :r0))
    (list (cons :p1 e1) (cons :p2 e2))))

(it-sequential "pre-available-in-any-p-cases in-first"
  (destructuring-bind (in1 in2 expected) (list t nil t)
    (let ((preds (%pre-envs-with-key '(:const 1) in1 in2)))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-falsy)))))

(it-sequential "pre-available-in-any-p-cases in-second"
  (destructuring-bind (in1 in2 expected) (list nil t t)
    (let ((preds (%pre-envs-with-key '(:const 1) in1 in2)))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-falsy)))))

(it-sequential "pre-available-in-any-p-cases in-neither"
  (destructuring-bind (in1 in2 expected) (list nil nil nil)
    (let ((preds (%pre-envs-with-key '(:const 1) in1 in2)))
    (assert-licm-boolean-case expected
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-truthy)
      (expect (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds) :to-be-falsy)))))

;;; ─── %licm-collect-def-sites ────────────────────────────────────────────

(it-sequential "licm-collect-def-sites-maps-defined-registers"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc/optimize:cfg-build insts))
         (sites (cl-cc/optimize::%licm-collect-def-sites cfg)))
    (expect (gethash :r0 sites) :to-be-truthy)
    (expect (gethash :r1 sites) :to-be-truthy)
    (expect (gethash :r9 sites) :to-be-null)))

(it-sequential "licm-collect-def-sites-empty-cfg-returns-empty-table"
  (let* ((cfg   (cl-cc/optimize:cfg-build nil))
         (sites (cl-cc/optimize::%licm-collect-def-sites cfg)))
    (expect (= 0 (hash-table-count sites)) :to-be-truthy)))

;;; ─── %licm-loop-def-regs ────────────────────────────────────────────────

(it-sequential "licm-loop-def-regs-collects-from-members"
  (let* ((b (make-instance 'cl-cc/optimize:basic-block))
         (members (make-hash-table :test #'eq)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-const :dst :r5 :value 99)
                (make-vm-add   :dst :r6 :lhs :r5 :rhs :r5)))
    (setf (gethash b members) t)
    (let ((regs (cl-cc/optimize::%licm-loop-def-regs members)))
      (expect (gethash :r5 regs) :to-be-truthy)
      (expect (gethash :r6 regs) :to-be-truthy)
      (expect (gethash :r0 regs) :to-be-null))))

;;; ─── %licm-find-loop-headers (smoke test via opt-pass-licm) ────────────

(it-sequential "licm-find-loop-headers-detects-self-loop"
  (let* ((start (make-vm-label :name "start"))
         (seed  (make-vm-const :dst :r0 :value 0))
         (jmp1  (make-vm-jump  :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (hoist (make-vm-const :dst :r1 :value 1))
         (jmp2  (make-vm-jump-zero :reg :r0 :label "exit"))
         (body  (make-vm-label :name "body"))
         (back  (make-vm-jump  :label "loop"))
         (exit  (make-vm-label :name "exit"))
         (ret   (make-vm-ret   :reg :r1))
         (insts (list start seed jmp1 loop hoist jmp2 body back exit ret))
         (cfg   (cl-cc/optimize:cfg-build insts)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (multiple-value-bind (headers _)
        (cl-cc/optimize::%licm-find-loop-headers cfg)
      (declare (ignore _))
      (expect (consp headers) :to-be-truthy))))

;;; ─── %opt-rewrite-block-terminator (shared helper via licm) ─────────────

(it-sequential "opt-rewrite-block-terminator-cases jump-match"
  (destructuring-bind (term-inst old new expected-type) (list (make-vm-jump      :label "old") "old" "new" 'cl-cc/vm::vm-jump)
    (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) (list term-inst))
    (cl-cc/optimize::%opt-rewrite-block-terminator b old new)
    (let ((result (car (cl-cc/optimize:bb-instructions b))))
      (if expected-type
          (progn
          (expect (typep result expected-type) :to-be-truthy)
          (expect (cl-cc/vm::vm-label-name result) :to-equal new))
          (expect (equal new (cl-cc/vm::vm-label-name result)) :to-be-falsy))))))

(it-sequential "opt-rewrite-block-terminator-cases jump-zero-match"
  (destructuring-bind (term-inst old new expected-type) (list (make-vm-jump-zero :reg :r0 :label "old") "old" "new" 'cl-cc/vm::vm-jump-zero)
    (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) (list term-inst))
    (cl-cc/optimize::%opt-rewrite-block-terminator b old new)
    (let ((result (car (cl-cc/optimize:bb-instructions b))))
      (if expected-type
          (progn
          (expect (typep result expected-type) :to-be-truthy)
          (expect (cl-cc/vm::vm-label-name result) :to-equal new))
          (expect (equal new (cl-cc/vm::vm-label-name result)) :to-be-falsy))))))

(it-sequential "opt-rewrite-block-terminator-cases no-match"
  (destructuring-bind (term-inst old new expected-type) (list (make-vm-jump      :label "other") "old" "new" nil)
    (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) (list term-inst))
    (cl-cc/optimize::%opt-rewrite-block-terminator b old new)
    (let ((result (car (cl-cc/optimize:bb-instructions b))))
      (if expected-type
          (progn
          (expect (typep result expected-type) :to-be-truthy)
          (expect (cl-cc/vm::vm-label-name result) :to-equal new))
          (expect (equal new (cl-cc/vm::vm-label-name result)) :to-be-falsy))))))

;;; ─── %licm-redirect-successor ────────────────────────────────────────────

(it-sequential "licm-redirect-successor-updates-edges"
  (let ((block (make-instance 'cl-cc/optimize:basic-block))
        (old   (make-instance 'cl-cc/optimize:basic-block))
        (new   (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors   block) (list old)
          (cl-cc/optimize:bb-predecessors old)   (list block)
          (cl-cc/optimize:bb-predecessors new)   nil)
    (cl-cc/optimize::%licm-redirect-successor block old new)
    (expect (member new (cl-cc/optimize:bb-successors   block) :test #'eq) :to-be-truthy)
    (expect (member old (cl-cc/optimize:bb-successors   block) :test #'eq) :to-be-falsy)
    (expect (member block (cl-cc/optimize:bb-predecessors old)  :test #'eq) :to-be-falsy)
    (expect (member block (cl-cc/optimize:bb-predecessors new)  :test #'eq) :to-be-truthy)))

;;; ─── %licm-collect-members ──────────────────────────────────────────────

(it-sequential "licm-collect-members-returns-loop-blocks"
  (let* ((header (make-instance 'cl-cc/optimize:basic-block))
         (tail   (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-predecessors header) (list tail)
          (cl-cc/optimize:bb-successors   header) (list tail)
          (cl-cc/optimize:bb-predecessors tail)   (list header)
          (cl-cc/optimize:bb-successors   tail)   (list header))
    (let ((members (cl-cc/optimize::%licm-collect-members header (list tail))))
      (expect (hash-table-p members) :to-be-truthy)
      (expect (gethash header members) :to-be-truthy)
      (expect (gethash tail   members) :to-be-truthy))))

;;; ─── %licm-collect-invariants ────────────────────────────────────────────

(it-sequential "licm-collect-invariants-finds-pure-const"
  (let* ((b        (make-instance 'cl-cc/optimize:basic-block))
         (members  (make-hash-table :test #'eq))
         (def-sites (make-hash-table :test #'eq))
         (c42      (make-vm-const :dst :r0 :value 42)))
    (setf (cl-cc/optimize:bb-instructions b) (list c42))
    (setf (gethash b members) t)
    (setf (gethash :r0 def-sites) (list b))
    (let ((invs (cl-cc/optimize::%licm-collect-invariants members def-sites)))
      (expect (member c42 invs :test #'eq) :to-be-truthy))))

(it-sequential "licm-does-not-hoist-slot-read-across-aliased-slot-write"
  (let* ((start (make-vm-label :name "start"))
         (obj   (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1))
         (jmp   (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r2))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :v))
         (out   (cl-cc/optimize::opt-pass-licm (list start obj jmp loop read write back ret))))
    (expect (member read out :test #'eq) :to-be-truthy)
    (expect (> (position read out :test #'eq)
                    (position loop out :test #'eq)) :to-be-truthy)))

(it-sequential "licm-hoists-slot-read-across-tbaa-disjoint-slot-write"
  (let* ((start (make-vm-label :name "start"))
         (size  (make-vm-const :dst :n :value 4))
         (obj   (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1))
         (arr   (cl-cc:make-vm-make-array :dst :arr :size-reg :n
                                          :initial-element nil :fill-pointer nil
                                          :adjustable nil :element-type nil))
         (jmp   (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :arr :slot-name 'x :value-reg :r2))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :v))
         (out   (cl-cc/optimize::opt-pass-licm (list start size obj arr jmp loop read write back ret))))
    (expect (member read out :test #'eq) :to-be-truthy)
    (expect (< (position read out :test #'eq)
                    (position loop out :test #'eq)) :to-be-truthy)))

(it-sequential "licm-unknown-call-invalidates-slot-read-hoist"
  (let* ((start (make-vm-label :name "start"))
         (obj   (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1))
         (jmp   (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (call  (cl-cc:make-vm-call :dst :ignored :func :fn :args nil))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :v))
         (out   (cl-cc/optimize::opt-pass-licm (list start obj jmp loop read call back ret))))
    (expect (member read out :test #'eq) :to-be-truthy)
    (expect (> (position read out :test #'eq)
                    (position loop out :test #'eq)) :to-be-truthy)))

;;; ─── %opt-pre-block-out-env ──────────────────────────────────────────────

(it-sequential "pre-block-out-env-maps-key-to-defining-register"
  (let* ((b   (make-instance 'cl-cc/optimize:basic-block))
         (c42 (make-vm-const :dst :r0 :value 42)))
    (setf (cl-cc/optimize:bb-instructions b) (list c42))
    (let ((env (cl-cc/optimize::%opt-pre-block-out-env b)))
      (expect (hash-table-p env) :to-be-truthy)
      (expect (gethash '(:const 42) env) :to-equal :r0))))

(it-sequential "pre-block-out-env-removes-stale-entries-on-overwrite"
  (let* ((b  (make-instance 'cl-cc/optimize:basic-block))
         (c1 (make-vm-const :dst :r0 :value 1))
         (c2 (make-vm-const :dst :r0 :value 2)))
    (setf (cl-cc/optimize:bb-instructions b) (list c1 c2))
    (let ((env (cl-cc/optimize::%opt-pre-block-out-env b)))
      (expect (gethash '(:const 1) env) :to-be-null)
      (expect (gethash '(:const 2) env) :to-equal :r0))))

;;; ─── %opt-pre-join-elim ──────────────────────────────────────────────────

(it-sequential "pre-join-elim-no-change-on-straight-line"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (cfg (cl-cc/optimize:cfg-build insts)))
    (expect (cl-cc/optimize::%opt-pre-join-elim cfg) :to-be-falsy)))

;;; ─── %opt-pre-emit-compensating ────────────────────────────────────────

(defun %make-pre-compensate-state (&optional (prior-src nil))
  "Build (pair inst pred-inserts) for %opt-pre-emit-compensating tests.
   When PRIOR-SRC is non-nil, pre-seed key :k with that register in the env."
  (let* ((pred  (make-instance 'cl-cc/optimize:basic-block))
         (env   (make-hash-table :test #'eq))
         (table (make-hash-table :test #'eq))
         (inst  (make-vm-const :dst :r0 :value 7))
         (pair  (cons pred env)))
    (when prior-src (setf (gethash :k env) prior-src))
    (values pair inst table pred)))

(it-sequential "pre-emit-compensating-fresh-key"
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (expect (= 1 (length (gethash pred table))) :to-be-truthy)
    (expect (gethash :k (cdr pair)) :to-be :r0)))

(it-sequential "pre-emit-compensating-different-src"
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state :r1)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (let ((emitted (first (gethash pred table))))
      (expect (cl-cc/vm::vm-move-p emitted) :to-be-truthy)
      (expect (cl-cc/vm::vm-move-dst emitted) :to-be :r0)
      (expect (cl-cc/vm::vm-move-src emitted) :to-be :r1))))

(it-sequential "pre-emit-compensating-same-src-noop"
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state :r0)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (expect (gethash pred table) :to-be-null)))

;;; ─── opt-pass-pre ────────────────────────────────────────────────────────

(it-sequential "pre-pass-returns-instruction-list"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-pre insts)))
    (expect (listp result) :to-be-truthy)
    (expect (> (length result) 0) :to-be-truthy)))

;;; ─── optimize-with-egraph ────────────────────────────────────────────────

(it-sequential "egraph-pass-returns-list-for-empty-input"
  (let ((result (cl-cc/optimize:optimize-with-egraph nil)))
    (expect (listp result) :to-be-truthy)))
