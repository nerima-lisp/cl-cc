;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for optimizer-flow-core.lisp — DCE, jump threading, loop transforms
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-pass-jump, %opt-rewrite-block-terminator, opt-pass-unreachable,
;;;;   loop-rotation, loop-peeling, loop-unrolling, code-sinking,
;;;;   opt-pass-dominated-type-check-elim, opt-passes-preserve-straight-line.

(in-package :cl-cc/test)

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %make-test-basic-block (&key (successors nil) (predecessors nil)
                                     (loop-depth 0) (rpo-index 0)
                                     (instructions nil))
  "Build a minimal basic-block for testing."
  (cl-cc/optimize::make-basic-block
   :successors successors
   :predecessors predecessors
   :loop-depth loop-depth
    :rpo-index rpo-index
    :instructions instructions))

(defun %test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm::vm-label)
                      (equal (cl-cc/vm::vm-name inst) label-name)))
               instructions))

(defun %test-jump-zero-position (instructions target-label)
  "Return the position of a vm-jump-zero targeting TARGET-LABEL, or NIL."
  (position-if (lambda (inst)
                  (and (typep inst 'cl-cc/vm::vm-jump-zero)
                       (equal (cl-cc/vm::vm-label-name inst) target-label)))
                instructions))

(defun %make-slp-array-map (&key (op :add))
  "Build a four-lane straight-line scalar array map for SLP tests."
  (append
   (loop for i below 4
         collect (make-vm-const :dst (intern (format nil "I~D" i) :keyword) :value i))
   (loop for i below 4
         for idx = (intern (format nil "I~D" i) :keyword)
         for lhs = (intern (format nil "A~D" i) :keyword)
         for rhs = (intern (format nil "B~D" i) :keyword)
         for val = (intern (format nil "V~D" i) :keyword)
         append (list (cl-cc/vm::make-vm-aref :dst lhs :array-reg :array-a :index-reg idx)
                      (cl-cc/vm::make-vm-aref :dst rhs :array-reg :array-b :index-reg idx)
                      (ecase op
                        (:add (make-vm-add :dst val :lhs lhs :rhs rhs))
                        (:logxor (cl-cc/vm::make-vm-logxor :dst val :lhs lhs :rhs rhs)))
                      (cl-cc/vm::make-vm-aset :array-reg :array-c :index-reg idx :val-reg val)))))

(defun %make-counted-loop-program (&key limit (include-limit-p t))
  "Build the canonical counted loop shape used by loop-unrolling tests."
  (append
   (list (make-vm-const :dst :i :value 0))
   (when include-limit-p
     (list (make-vm-const :dst :lim :value limit)))
   (list (make-vm-const :dst :one :value 1)
         (make-vm-label :name "Lh")
         (make-vm-lt :dst :c :lhs :i :rhs :lim)
         (make-vm-jump-zero :reg :c :label "Lexit")
         (make-vm-add :dst :sum :lhs :sum :rhs :i)
         (make-vm-add :dst :i :lhs :i :rhs :one)
         (make-vm-jump :label "Lh")
         (make-vm-label :name "Lexit")
         (make-vm-ret :reg :sum))))

(defun %count-vm-jumps-to-label (instructions label-name)
  (count-if (lambda (inst)
              (and (typep inst 'cl-cc/vm::vm-jump)
                   (equal (cl-cc/vm::vm-label-name inst) label-name)))
            instructions))

(defun %count-vm-instructions-of-type (instructions type)
  (count-if (lambda (inst) (typep inst type)) instructions))

(defun %count-index-steps (instructions)
  (count-if (lambda (inst)
              (and (typep inst 'cl-cc/vm::vm-add)
                   (eq (cl-cc/vm::vm-dst inst) :i)))
            instructions))

;;; ─── opt-pass-dce / opt-build-label-index / opt-thread-label ────────────

(it-sequential "dce-eliminates-unread-const"
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 1)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (expect (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-const)
                               (eq (cl-cc/vm::vm-dst i) :r0)))
                        result) :to-be-falsy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r1)))
                       result) :to-be-truthy)))

(it-sequential "dce-keeps-read-const"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r0)))
                       result) :to-be-truthy)
     (expect (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-const)
                               (eq (cl-cc/vm::vm-dst i) :r1)))
                        result) :to-be-truthy)))

(it-sequential "function-outlining-outlines-duplicate-sequences"
  (let* ((seq (list (make-vm-const :dst :r0 :value 1)
                    (make-vm-const :dst :r1 :value 2)
                    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
         (insts (append seq seq (list (make-vm-ret :reg :r2))))
         (out (cl-cc/optimize::opt-pass-function-outlining insts)))
    (expect (= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-call)) out)) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-label)
                  (search cl-cc/optimize::*opt-outlined-label-prefix*
                          (cl-cc/vm::vm-name i))))
           out) :to-be-truthy)))

(it-sequential "function-outlining-leaves-nonduplicate-sequences-unchanged"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-function-outlining insts)))
    (expect (mapcar #'cl-cc/vm::instruction->sexp out) :to-equal (mapcar #'cl-cc/vm::instruction->sexp insts))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-label)
                  (search cl-cc/optimize::*opt-outlined-label-prefix*
                          (cl-cc/vm::vm-name i))))
            out) :to-be-falsy)))

(it-sequential "slp-vectorize-packs-four-adjacent-arithmetic-lanes"
  (let* ((insts (%make-slp-array-map :op :add))
         (out (cl-cc/optimize:opt-pass-slp-vectorize insts))
         (simd (find-if (lambda (inst) (typep inst 'cl-cc/vm:vm-simd-vector-op)) out)))
    (expect simd :to-be-truthy)
    (expect (cl-cc/vm:vm-simd-vector-op-op simd) :to-be :add)
    (expect (cl-cc/vm:vm-simd-vector-op-lhs-array simd) :to-be :array-a)
    (expect (cl-cc/vm:vm-simd-vector-op-rhs-array simd) :to-be :array-b)
    (expect (cl-cc/vm:vm-simd-vector-op-dst-array simd) :to-be :array-c)
    (expect (= 4 (cl-cc/vm:vm-simd-vector-op-lanes simd)) :to-be-truthy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-add)) out) :to-be-falsy)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-aset)) out) :to-be-falsy)))

(it-sequential "slp-vectorize-packs-bitwise-lanes"
  (let* ((insts (%make-slp-array-map :op :logxor))
         (out (cl-cc/optimize:opt-pass-slp-vectorize insts))
         (simd (find-if (lambda (inst) (typep inst 'cl-cc/vm:vm-simd-vector-op)) out)))
    (expect simd :to-be-truthy)
    (expect (cl-cc/vm:vm-simd-vector-op-op simd) :to-be :logxor)
    (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-logxor)) out) :to-be-falsy)))

(it-sequential "slp-vectorize-is-idempotent"
  (let* ((once (cl-cc/optimize:opt-pass-slp-vectorize (%make-slp-array-map :op :add)))
         (twice (cl-cc/optimize:opt-pass-slp-vectorize once)))
    (expect (mapcar #'cl-cc/vm:instruction->sexp twice) :to-equal (mapcar #'cl-cc/vm:instruction->sexp once))))

(it-sequential "dce-eliminates-unread-move"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r5 :src :r0)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result) :to-be-falsy)))

(it-sequential "dce-nil-input-returns-nil"
  (expect (cl-cc/optimize::opt-pass-dce nil) :to-be-null))

(it-sequential "build-label-index-maps-names-to-positions"
  (let* ((lab (make-vm-label :name "loop"))
         (c   (make-vm-const :dst :r0 :value 1)))
    (multiple-value-bind (vec idx)
        (cl-cc/optimize::opt-build-label-index (list c lab))
      (expect (= 2 (length vec)) :to-be-truthy)
      (expect (= 1 (gethash "loop" idx)) :to-be-truthy))))

(it-sequential "build-label-index-empty-input"
  (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index nil)
    (expect (= 0 (length vec)) :to-be-truthy)
    (expect (= 0 (hash-table-count idx)) :to-be-truthy)))

(it-sequential "thread-label-returns-input-unchanged no-chain"
  (destructuring-bind (insts query) (list (list (make-vm-label :name "end") (make-vm-ret :reg :r0)) "end")
    (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index insts)
    (expect (cl-cc/optimize::opt-thread-label query idx vec) :to-equal query))))

(it-sequential "thread-label-returns-input-unchanged unknown"
  (destructuring-bind (insts query) (list nil "nowhere")
    (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index insts)
    (expect (cl-cc/optimize::opt-thread-label query idx vec) :to-equal query))))

;;; ─── opt-pass-jump / %opt-rewrite-block-terminator / opt-pass-unreachable ─

(it-sequential "jump-pass-fallthrough-and-non-fallthrough removes-fallthrough"
  (destructuring-bind (insts expect-jump) (list (list (make-vm-jump  :label "next")
                 (make-vm-label :name  "next")
                 (make-vm-ret   :reg   :r0)) nil)
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
    (if expect-jump
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result) :to-be-truthy)
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result) :to-be-falsy)))))

(it-sequential "jump-pass-fallthrough-and-non-fallthrough keeps-non-fallthrough"
  (destructuring-bind (insts expect-jump) (list (list (make-vm-jump  :label "far")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-label :name "far")
                 (make-vm-ret   :reg :r0)) t)
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
    (if expect-jump
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result) :to-be-truthy)
        (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result) :to-be-falsy)))))

(it-sequential "jump-pass-threads-through-jump-only-block"
  (let* ((insts (list (make-vm-const :dst :r0 :value 0)
                      (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0)))
         (result (cl-cc/optimize::opt-pass-jump insts))
         (jumps  (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))
    (when jumps
      (expect (cl-cc/vm::vm-label-name (first jumps)) :to-equal "end"))))

(it-sequential "opt-rewrite-block-terminator-cases vm-jump-rewrites-label"
  (destructuring-bind (instructions expected-type expected-label expected-reg) (list (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "old")) 'cl-cc/vm::vm-jump "new" nil)
    (let ((b (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) instructions)
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize:bb-instructions b)))))
      (expect (typep term expected-type) :to-be-truthy)
      (expect (cl-cc/vm::vm-label-name term) :to-equal expected-label)
      (when expected-reg
        (expect (cl-cc/vm::vm-reg term) :to-be expected-reg))))))

(it-sequential "opt-rewrite-block-terminator-cases vm-jump-zero-rewrites-label-preserves-reg"
  (destructuring-bind (instructions expected-type expected-label expected-reg) (list (list (make-vm-jump-zero :reg :r0 :label "old")) 'cl-cc/vm::vm-jump-zero "new" :r0)
    (let ((b (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) instructions)
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize:bb-instructions b)))))
      (expect (typep term expected-type) :to-be-truthy)
      (expect (cl-cc/vm::vm-label-name term) :to-equal expected-label)
      (when expected-reg
        (expect (cl-cc/vm::vm-reg term) :to-be expected-reg))))))

(it-sequential "opt-rewrite-block-terminator-cases no-match-unchanged"
  (destructuring-bind (instructions expected-type expected-label expected-reg) (list (list (make-vm-jump :label "other")) 'cl-cc/vm::vm-jump "other" nil)
    (let ((b (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) instructions)
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize:bb-instructions b)))))
      (expect (typep term expected-type) :to-be-truthy)
      (expect (cl-cc/vm::vm-label-name term) :to-equal expected-label)
      (when expected-reg
        (expect (cl-cc/vm::vm-reg term) :to-be expected-reg))))))

(it-sequential "opt-jump-thread-table-covers-both-jump-types"
  (expect (= 2 (length cl-cc/optimize::*opt-jump-thread-table*)) :to-be-truthy)
  (expect (assoc 'vm-jump      cl-cc/optimize::*opt-jump-thread-table*) :to-be-truthy)
  (expect (assoc 'vm-jump-zero cl-cc/optimize::*opt-jump-thread-table*) :to-be-truthy))

(it-sequential "opt-thread-jump-returns-nil-for-fallthrough"
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (expect (cl-cc/optimize::%opt-thread-jump
                    (make-vm-jump :label "next") vec 0 idx) :to-be-null))))

(it-sequential "opt-thread-jump-returns-relabeled-when-threading"
  (let* ((insts (list (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump
                     (make-vm-jump :label "middle") vec 0 idx)))
        (expect result :to-be-truthy)
        (expect (cl-cc/vm::vm-label-name result) :to-equal "end")))))

(it-sequential "opt-thread-jump-zero-always-returns-instruction"
  (let* ((inst  (make-vm-jump-zero :reg :r0 :label "next"))
         (insts (list inst (make-vm-label :name "next") (make-vm-ret :reg :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump-zero inst vec 0 idx)))
        (expect result :to-be-truthy)
        (expect (typep result 'cl-cc/vm::vm-jump-zero) :to-be-truthy)))))

(it-sequential "jump-pass-comparison-constant-propagation-cases fallthrough-gets-1"
  (destructuring-bind (insts expected-value) (list (list (make-vm-const :dst :i :value 1)
                 (make-vm-const :dst :lim :value 3)
                 (make-vm-lt :dst :c :lhs :i :rhs :lim)
                 (make-vm-jump-zero :reg :c :label "false")
                 (make-vm-label :name "true")
                 (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                 (make-vm-ret :reg :c2)
                 (make-vm-label :name "false")
                 (make-vm-ret :reg :c)) 1)
    (let ((out (cl-cc/optimize::opt-pass-jump insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                  (eql (cl-cc/vm::vm-value i) expected-value)))
           out) :to-be-truthy))))

(it-sequential "jump-pass-comparison-constant-propagation-cases taken-gets-0"
  (destructuring-bind (insts expected-value) (list (list (make-vm-const :dst :i :value 5)
                 (make-vm-const :dst :lim :value 3)
                 (make-vm-lt :dst :c :lhs :i :rhs :lim)
                 (make-vm-jump-zero :reg :c :label "false")
                 (make-vm-label :name "true")
                 (make-vm-ret :reg :c)
                 (make-vm-label :name "false")
                 (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                 (make-vm-ret :reg :c2)) 0)
    (let ((out (cl-cc/optimize::opt-pass-jump insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                  (eql (cl-cc/vm::vm-value i) expected-value)))
           out) :to-be-truthy))))

(it-sequential "if-conversion-simple-diamond-emits-vm-select"
  (let* ((insts (list (make-vm-lt :dst :c :lhs :r0 :rhs :r1)
                      (make-vm-jump-zero :reg :c :label "else")
                      (make-vm-move :dst :out :src :then)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-move :dst :out :src :else)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :out)))
         (out (cl-cc/optimize::opt-pass-if-conversion insts))
         (selects (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-select)) out)))
    (expect (= 1 (length selects)) :to-be-truthy)
    (let ((sel (first selects)))
      (expect (cl-cc/vm::vm-dst sel) :to-be :out)
      (expect (cl-cc/vm::vm-select-cond-reg sel) :to-be :c)
      (expect (cl-cc/vm::vm-select-then-reg sel) :to-be :then)
      (expect (cl-cc/vm::vm-select-else-reg sel) :to-be :else))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-lt)) out) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out) :to-be-falsy)))

(it-sequential "if-conversion-skips-externally-referenced-diamond-label"
  (let* ((insts (list (make-vm-jump :label "else")
                      (make-vm-lt :dst :c :lhs :r0 :rhs :r1)
                      (make-vm-jump-zero :reg :c :label "else")
                      (make-vm-move :dst :out :src :then)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-move :dst :out :src :else)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :out)))
         (out (cl-cc/optimize::opt-pass-if-conversion insts)))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-select)) out) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out) :to-be-truthy)))

(it-sequential "jump-pass-kills-comparison-fact-on-source-redefinition"
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "false")
                      (make-vm-label :name "true")
                      (make-vm-ret :reg :c)
                      (make-vm-label :name "false")
                      (make-vm-const :dst :i :value 9)
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-lt)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out) :to-be-truthy)
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out) :to-be-falsy)))

(it-sequential "jump-pass-does-not-propagate-across-loop-back-edge"
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-label :name "header")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "exit")
                      (make-vm-label :name "body")
                      (make-vm-lt :dst :body-c :lhs :i :rhs :lim)
                      (make-vm-jump :label "header")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-lt)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out) :to-be-truthy)))

(it-sequential "jump-pass-combines-chain-threading-with-value-propagation"
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "middle")
                      (make-vm-label :name "true")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)
                      (make-vm-label :name "middle")
                      (make-vm-jump :label "final")
                      (make-vm-label :name "final")
                      (make-vm-ret :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-jump-zero)
                  (equal (cl-cc/vm::vm-label-name i) "final")))
            out) :to-be-truthy)
     (expect (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                   (eql (cl-cc/vm::vm-value i) 1)))
            out) :to-be-truthy)))

;;; ─── opt-pass-hot-cold-layout ─────────────────────────────────────────────

(it-sequential "hot-cold-layout-registers-before-dce"
  (let ((layout-pos (position :hot-cold-layout cl-cc/optimize::*opt-default-convergence-pass-keys*))
        (dce-pos    (position :dce cl-cc/optimize::*opt-default-convergence-pass-keys*)))
    (expect (assoc :hot-cold-layout cl-cc/optimize::*opt-pass-table*) :to-be-truthy)
    (expect layout-pos :to-be-truthy)
    (expect dce-pos :to-be-truthy)
    (expect (< layout-pos dce-pos) :to-be-truthy)))

(it-sequential "hot-cold-layout-keeps-conditional-fallthrough-contiguous"
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-const :dst :r0 :value 42)
                      (make-vm-jump :label "end")
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :r0)
                      (make-vm-label :name "end")
                      (make-vm-ret :reg :r0)))
         (out      (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (jz-pos   (%test-jump-zero-position out "cold"))
         (hot-pos  (%test-label-position out "hot"))
         (cold-pos (%test-label-position out "cold")))
    (expect jz-pos :to-be-truthy)
    (expect hot-pos :to-be-truthy)
    (expect cold-pos :to-be-truthy)
    (expect (= (1+ jz-pos) hot-pos) :to-be-truthy)
    (expect (< hot-pos cold-pos) :to-be-truthy)))

(it-sequential "hot-cold-layout-moves-signal-block-to-tail"
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-const :dst :r0 :value 7)
                      (make-vm-jump :label "end")
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :r0)
                      (make-vm-label :name "end")
                      (make-vm-ret :reg :r0)))
         (out      (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (end-pos  (%test-label-position out "end"))
         (cold-pos (%test-label-position out "cold")))
    (expect end-pos :to-be-truthy)
    (expect cold-pos :to-be-truthy)
    (expect (< end-pos cold-pos) :to-be-truthy)))

(it-sequential "hot-cold-layout-preserves-conditional-jump-target"
  (let* ((insts (list (make-vm-const :dst :cond :value 1)
                      (make-vm-jump-zero :reg :cond :label "cold")
                      (make-vm-label :name "hot")
                      (make-vm-ret :reg :cond)
                      (make-vm-label :name "cold")
                      (make-vm-signal-error :error-reg :cond)))
         (out   (cl-cc/optimize::opt-pass-hot-cold-layout insts))
         (jz    (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) out)))
    (expect jz :to-be-truthy)
    (expect (cl-cc/vm::vm-label-name jz) :to-equal "cold")
    (expect (%test-label-position out "cold") :to-be-truthy)))

(it-sequential "loop-rotation-rotates-simple-while-shape"
  (let* ((insts (list (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts))
         (first-inst (first out))
         (jumps-to-lh (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "Lh")))
                                out))
         (guard-jumps (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump-zero)
                                       (equal (cl-cc/vm::vm-label-name i) "Lexit")))
                                out)))
    (expect (typep first-inst 'cl-cc/vm::vm-jump) :to-be-truthy)
    (expect (= 0 jumps-to-lh) :to-be-truthy)
    (expect (= 1 guard-jumps) :to-be-truthy)))

(it-sequential "loop-transforms-noop-on-nonmatching-shape rotation"
  (destructuring-bind (pass) (list #'cl-cc/optimize::opt-pass-loop-rotation)
    (let* ((insts (list (make-vm-label :name "A")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "B")
                      (make-vm-label :name "B")
                      (make-vm-ret :reg :r0)))
         (out (funcall pass insts)))
    (expect (= (length insts) (length out)) :to-be-truthy))))

(it-sequential "loop-transforms-noop-on-nonmatching-shape peeling"
  (destructuring-bind (pass) (list #'cl-cc/optimize::opt-pass-loop-peel)
    (let* ((insts (list (make-vm-label :name "A")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "B")
                      (make-vm-label :name "B")
                      (make-vm-ret :reg :r0)))
         (out (funcall pass insts)))
    (expect (= (length insts) (length out)) :to-be-truthy))))

(it-sequential "loop-unrolling-fully-unrolls-small-counted-loop"
  (let* ((insts (%make-counted-loop-program :limit 3))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (jump-to-lh (%count-vm-jumps-to-label out "Lh"))
         (lt-count (%count-vm-instructions-of-type out 'cl-cc/vm::vm-lt))
         (step-count (%count-index-steps out)))
    (expect (= 0 jump-to-lh) :to-be-truthy)
    (expect (= 0 lt-count) :to-be-truthy)
    (expect (= 3 step-count) :to-be-truthy)))

(it-sequential "loop-unrolling-non-lt-comparisons le"
  (destructuring-bind (init-inst lim-inst step-inst cmp-inst cmp-type expected-steps) (list (make-vm-const :dst :i :value 0) (make-vm-const :dst :lim :value 2) (make-vm-const :dst :step :value 1) (make-vm-le :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-le 3)
    (let* ((insts (list init-inst lim-inst step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy)
    (expect (= 0 jump-to-lh) :to-be-truthy))))

(it-sequential "loop-unrolling-non-lt-comparisons ge"
  (destructuring-bind (init-inst lim-inst step-inst cmp-inst cmp-type expected-steps) (list (make-vm-const :dst :i :value 3) (make-vm-const :dst :lim :value 1) (make-vm-const :dst :step :value -1) (make-vm-ge :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-ge 3)
    (let* ((insts (list init-inst lim-inst step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy)
    (expect (= 0 jump-to-lh) :to-be-truthy))))

(it-sequential "loop-unrolling-non-lt-comparisons eq"
  (destructuring-bind (init-inst lim-inst step-inst cmp-inst cmp-type expected-steps) (list (make-vm-const :dst :i :value 0) (make-vm-const :dst :lim :value 0) (make-vm-const :dst :step :value 1) (make-vm-eq :dst :c :lhs :i :rhs :lim) 'cl-cc/vm::vm-eq 1)
    (let* ((insts (list init-inst lim-inst step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (expect (= 0 cmp-count) :to-be-truthy)
    (expect (= expected-steps step-count) :to-be-truthy)
    (expect (= 0 jump-to-lh) :to-be-truthy))))

(it-sequential "loop-unrolling-partially-unrolls-when-trip-count-too-large"
  (let* ((insts (%make-counted-loop-program :limit 10))
          (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
          (jump-to-lh (%count-vm-jumps-to-label out "Lh"))
          (lt-count (%count-vm-instructions-of-type out 'cl-cc/vm::vm-lt))
          (step-count (%count-index-steps out)))
    (expect (= 1 jump-to-lh) :to-be-truthy)
    (expect (= 3 lt-count) :to-be-truthy)
    (expect (= 3 step-count) :to-be-truthy)))

(it-sequential "loop-unrolling-partially-unrolls-unknown-trip-with-remainder"
  (let* ((insts (%make-counted-loop-program :include-limit-p nil))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (%count-vm-instructions-of-type out 'cl-cc/vm::vm-lt))
         (jump-to-lh (%count-vm-jumps-to-label out "Lh")))
    (expect (= 3 lt-count) :to-be-truthy)
    (expect (= 1 jump-to-lh) :to-be-truthy)))

(it-sequential "loop-rotation-detects-cfg-natural-loop"
  (let* ((insts (list (make-vm-label :name "Entry")
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts)))
    (expect (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-jump-zero)
                              (equal (cl-cc/vm::vm-label-name x) "Lexit")))
                       out) :to-be-truthy)
    (expect (some (lambda (x)
                          (and (typep x 'cl-cc/vm::vm-jump)
                               (equal (cl-cc/vm::vm-label-name x) "Lh")))
                        out) :to-be-falsy)))



(it-sequential "loop-unrolling-partial-keeps-remainder-loop"
  (let* ((insts (%make-counted-loop-program :limit 10))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (%count-vm-instructions-of-type out 'cl-cc/vm::vm-lt))
         (step-count (%count-index-steps out))
         (jump-to-lh (%count-vm-jumps-to-label out "Lh")))
    (expect (= 3 lt-count) :to-be-truthy)
    (expect (= 3 step-count) :to-be-truthy)
    (expect (= 1 jump-to-lh) :to-be-truthy)))

(it-sequential "cfg-natural-loop-transforms-detected rotation"
  (destructuring-bind (pass insts expected-jumps-to-lh expected-adds) (list #'cl-cc/optimize::opt-pass-loop-rotation (list (make-vm-const :dst :one :value 1)
                 (make-vm-jump :label "Lh")
                 (make-vm-label :name "Lh")
                 (cl-cc:make-vm-integer-p :dst :c :src :i)
                 (make-vm-jump-zero :reg :c :label "Lexit")
                 (make-vm-add :dst :i :lhs :i :rhs :one)
                 (make-vm-jump :label "Lh")
                 (make-vm-label :name "Lexit")
                 (make-vm-ret :reg :i)) 0 1)
    (let* ((out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (expect (= expected-jumps-to-lh jumps-to-lh) :to-be-truthy)
    (expect (= expected-adds add-count) :to-be-truthy))))

(it-sequential "cfg-natural-loop-transforms-detected peeling"
  (destructuring-bind (pass insts expected-jumps-to-lh expected-adds) (list #'cl-cc/optimize::opt-pass-loop-peel (list (make-vm-const :dst :one :value 1)
                 (make-vm-jump :label "Lh")
                 (make-vm-label :name "Lh")
                 (cl-cc:make-vm-integer-p :dst :c :src :i)
                 (make-vm-jump-zero :reg :c :label "Lexit")
                 (make-vm-add :dst :i :lhs :i :rhs :one)
                 (make-vm-jump :label "Lh")
                 (make-vm-label :name "Lexit")
                 (make-vm-ret :reg :i)) 1 2)
    (let* ((out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (expect (= expected-jumps-to-lh jumps-to-lh) :to-be-truthy)
    (expect (= expected-adds add-count) :to-be-truthy))))

(it-sequential "cfg-natural-loop-transforms-detected peeling-plain-while"
  (destructuring-bind (pass insts expected-jumps-to-lh expected-adds) (list #'cl-cc/optimize::opt-pass-loop-peel (list (make-vm-label :name "Lh")
                 (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                 (make-vm-jump-zero :reg :r1 :label "Lexit")
                 (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                 (make-vm-jump :label "Lh")
                 (make-vm-label :name "Lexit")
                 (make-vm-ret :reg :r0)) 1 2)
    (let* ((out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (expect (= expected-jumps-to-lh jumps-to-lh) :to-be-truthy)
    (expect (= expected-adds add-count) :to-be-truthy))))

(it-sequential "code-sinking-moves-const-into-target-block"
  (let* ((insts (list (make-vm-const :dst :r1 :value 42)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Ldead")
                      (make-vm-ret :reg :r0)
                      (make-vm-label :name "Luse")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (r1-const-pos (position-if (lambda (x)
                                      (and (typep x 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst x) :r1)))
                                    out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect r1-const-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> r1-const-pos luse-pos) :to-be-truthy)))

(it-sequential "code-sinking-noop-when-value-is-read-multiple-times"
  (let* ((insts (list (make-vm-const :dst :r1 :value 7)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                      (make-vm-ret :reg :r3)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (r1-const-pos (position-if (lambda (x)
                                      (and (typep x 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst x) :r1)))
                                    out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (expect r1-const-pos :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (< r1-const-pos jump-pos) :to-be-truthy)))

(it-sequential "code-sinking-moves-cons-into-target-block"
  (let* ((insts (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Ldead")
                      (make-vm-ret :reg :r0)
                      (make-vm-label :name "Luse")
                      (make-vm-car :dst :r2 :src :pair)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (pair-cons-pos (position-if (lambda (x)
                                       (and (typep x 'cl-cc/vm::vm-cons)
                                            (eq (cl-cc/vm::vm-dst x) :pair)))
                                     out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect pair-cons-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> pair-cons-pos luse-pos) :to-be-truthy)))

(it-sequential "code-sinking-moves-arithmetic-and-move-into-target-block add"
  (destructuring-bind (insts moved-type) (list (list (make-vm-const :dst :a :value 2)
                         (make-vm-const :dst :b :value 3)
                         (make-vm-add   :dst :v :lhs :a :rhs :b)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v)) 'cl-cc/vm::vm-add)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (moved-pos (position-if (lambda (x)
                                   (and (typep x moved-type)
                                        (eq (cl-cc/optimize::opt-inst-dst x) :v)))
                                 out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect moved-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> moved-pos luse-pos) :to-be-truthy))))

(it-sequential "code-sinking-moves-arithmetic-and-move-into-target-block move"
  (destructuring-bind (insts moved-type) (list (list (make-vm-const :dst :a :value 2)
                         (make-vm-move  :dst :v :src :a)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v)) 'cl-cc/vm::vm-move)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (moved-pos (position-if (lambda (x)
                                   (and (typep x moved-type)
                                        (eq (cl-cc/optimize::opt-inst-dst x) :v)))
                                 out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect moved-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> moved-pos luse-pos) :to-be-truthy))))

(it-sequential "code-sinking-does-not-sink-impure-random"
  (let* ((insts (list (make-vm-const :dst :limit :value 10)
                      (cl-cc/vm::make-vm-random :dst :v :src :limit)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (random-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-random)) out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (expect random-pos :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (< random-pos jump-pos) :to-be-truthy)))

(it-sequential "code-sinking-duplicates-cheap-const-into-conditional-targets"
  (let* ((insts (list (make-vm-const :dst :v :value 1)
                      (make-vm-jump-zero :reg :cond :label "Lzero")
                      (make-vm-label :name "Lnonzero")
                      (make-vm-add :dst :r1 :lhs :v :rhs :x)
                      (make-vm-ret :reg :r1)
                      (make-vm-label :name "Lzero")
                      (make-vm-add :dst :r2 :lhs :v :rhs :y)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (const-count (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-const)
                                       (eq (cl-cc/vm::vm-dst x) :v)))
                                out)))
    (expect (<= 1 const-count 2) :to-be-truthy)))

(it-sequential "code-sinking-noop-for-cons-read-multiple-times"
  (let* ((insts (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-car :dst :r2 :src :pair)
                      (make-vm-cdr :dst :r3 :src :pair)
                      (make-vm-ret :reg :r3)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (pair-cons-pos (position-if (lambda (x)
                                       (and (typep x 'cl-cc/vm::vm-cons)
                                            (eq (cl-cc/vm::vm-dst x) :pair)))
                                     out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (expect pair-cons-pos :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (< pair-cons-pos jump-pos) :to-be-truthy)))

(it-sequential "code-sinking-does-not-sink-slot-read-across-aliased-write"
  (let* ((read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :new))
         (insts (list (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                      read
                      write
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts)))
    (expect (member read out :test #'eq) :to-be-truthy)
    (expect (member write out :test #'eq) :to-be-truthy)
    (expect (< (position read out :test #'eq)
                    (position write out :test #'eq)) :to-be-truthy)))

(it-sequential "code-sinking-sinks-slot-read-across-tbaa-disjoint-write"
  (let* ((read  (cl-cc:make-vm-slot-read :dst :v :obj-reg :obj :slot-name 'x))
         (write (cl-cc:make-vm-slot-write :obj-reg :arr :slot-name 'x :value-reg :new))
         (insts (list (make-vm-const :dst :n :value 4)
                      (make-vm-cons :dst :obj :car-src :r0 :cdr-src :r1)
                      (cl-cc:make-vm-make-array :dst :arr :size-reg :n
                                                :initial-element nil :fill-pointer nil
                                                :adjustable nil :element-type nil)
                      read
                      write
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts)))
    (expect (member read out :test #'eq) :to-be-truthy)
    (expect (member write out :test #'eq) :to-be-truthy)
    (expect (> (position read out :test #'eq)
                    (position write out :test #'eq)) :to-be-truthy)))

(it-sequential "unreachable-removes-dead-code-cases after-ret"
  (destructuring-bind (insts dead-pred) (list (list (make-vm-const :dst :r0 :value 1)
                               (make-vm-ret   :reg :r0)
                               (make-vm-const :dst :r1 :value 2)
                               (make-vm-label :name "ok")
                               (make-vm-ret   :reg :r0)) (lambda (i) (and (typep i 'cl-cc/vm::vm-const) (eq (cl-cc/vm::vm-dst i) :r1))))
    (let ((result (cl-cc/optimize::opt-pass-unreachable insts)))
    (expect (some dead-pred result) :to-be-falsy))))

(it-sequential "unreachable-removes-dead-code-cases after-jump"
  (destructuring-bind (insts dead-pred) (list (list (make-vm-jump  :label "end")
                               (make-vm-const :dst :r0 :value 99)
                               (make-vm-label :name "end")
                               (make-vm-ret   :reg :r0)) (lambda (i) (and (typep i 'cl-cc/vm::vm-const)
                            (eq (cl-cc/vm::vm-dst i) :r0)
                            (= 99 (cl-cc/vm::vm-value i)))))
    (let ((result (cl-cc/optimize::opt-pass-unreachable insts)))
    (expect (some dead-pred result) :to-be-falsy))))

(it-sequential "unreachable-preserves-label-after-ret"
  (let* ((insts (list (make-vm-ret   :reg :r0)
                      (make-vm-label :name "resume")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result) :to-be-truthy)))

(it-sequential "unreachable-straight-line-code-unchanged"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (expect (= (length insts) (length result)) :to-be-truthy)))

(it-sequential "type-check-elim-forget-def-removes-matching-facts"
  (let* ((f1    (list :pred 'p :src :r0 :dst :r1))
         (f2    (list :pred 'q :src :r2 :dst :r3))
         (facts (list f1 f2)))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r0)))
      (expect (member f1 after) :to-be-falsy)
      (expect (member f2 after) :to-be-truthy))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r1)))
      (expect (member f1 after) :to-be-falsy)
      (expect (member f2 after) :to-be-truthy))))
