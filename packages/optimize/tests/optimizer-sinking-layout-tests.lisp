;;;; tests/unit/optimize/optimizer-sinking-layout-tests.lisp
;;;; Unit tests for hot-cold layout, SLP vectorization, function outlining,
;;;; if-conversion, and code-sinking optimization passes.
;;;;
;;;; Covers: opt-pass-hot-cold-layout, opt-pass-slp-vectorize,
;;;;   opt-pass-function-outlining, opt-pass-if-conversion, opt-pass-code-sinking.

(in-package :cl-cc/test)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun %sinking-layout-test-label-position (instructions label-name)
  "Return LABEL-NAME's position in INSTRUCTIONS, or NIL."
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm::vm-label)
                      (equal (cl-cc/vm::vm-name inst) label-name)))
               instructions))

(defun %sinking-layout-test-jump-zero-position (instructions target-label)
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

;;; ─── opt-pass-function-outlining ──────────────────────────────────────────

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

;;; ─── opt-pass-slp-vectorize ───────────────────────────────────────────────

(it-sequential "slp-vectorize-packs-adjacent-lanes add"
  (destructuring-bind (op scalar-type) (list :add 'cl-cc/vm::vm-add)
    (let* ((insts (%make-slp-array-map :op op))
         (out (cl-cc/optimize:opt-pass-slp-vectorize insts))
         (simd (find-if (lambda (inst) (typep inst 'cl-cc/vm:vm-simd-vector-op)) out)))
    (expect simd :to-be-truthy)
    (expect (cl-cc/vm:vm-simd-vector-op-op simd) :to-be op)
    (ecase op
      (:add
       (expect (cl-cc/vm:vm-simd-vector-op-lhs-array simd) :to-be :array-a)
       (expect (cl-cc/vm:vm-simd-vector-op-rhs-array simd) :to-be :array-b)
       (expect (cl-cc/vm:vm-simd-vector-op-dst-array simd) :to-be :array-c)
       (expect (= 4 (cl-cc/vm:vm-simd-vector-op-lanes simd)) :to-be-truthy)
       (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-aset)) out) :to-be-falsy))
      (:logxor t))
    (expect (some (lambda (inst) (typep inst scalar-type)) out) :to-be-falsy))))

(it-sequential "slp-vectorize-packs-adjacent-lanes logxor"
  (destructuring-bind (op scalar-type) (list :logxor 'cl-cc/vm::vm-logxor)
    (let* ((insts (%make-slp-array-map :op op))
         (out (cl-cc/optimize:opt-pass-slp-vectorize insts))
         (simd (find-if (lambda (inst) (typep inst 'cl-cc/vm:vm-simd-vector-op)) out)))
    (expect simd :to-be-truthy)
    (expect (cl-cc/vm:vm-simd-vector-op-op simd) :to-be op)
    (ecase op
      (:add
       (expect (cl-cc/vm:vm-simd-vector-op-lhs-array simd) :to-be :array-a)
       (expect (cl-cc/vm:vm-simd-vector-op-rhs-array simd) :to-be :array-b)
       (expect (cl-cc/vm:vm-simd-vector-op-dst-array simd) :to-be :array-c)
       (expect (= 4 (cl-cc/vm:vm-simd-vector-op-lanes simd)) :to-be-truthy)
       (expect (some (lambda (inst) (typep inst 'cl-cc/vm:vm-aset)) out) :to-be-falsy))
      (:logxor t))
    (expect (some (lambda (inst) (typep inst scalar-type)) out) :to-be-falsy))))

(it-sequential "slp-vectorize-is-idempotent"
  (let* ((once (cl-cc/optimize:opt-pass-slp-vectorize (%make-slp-array-map :op :add)))
         (twice (cl-cc/optimize:opt-pass-slp-vectorize once)))
    (expect (mapcar #'cl-cc/vm:instruction->sexp twice) :to-equal (mapcar #'cl-cc/vm:instruction->sexp once))))

;;; ─── opt-pass-if-conversion ───────────────────────────────────────────────

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
         (jz-pos   (%sinking-layout-test-jump-zero-position out "cold"))
         (hot-pos  (%sinking-layout-test-label-position out "hot"))
         (cold-pos (%sinking-layout-test-label-position out "cold")))
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
         (end-pos  (%sinking-layout-test-label-position out "end"))
         (cold-pos (%sinking-layout-test-label-position out "cold")))
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
    (expect (%sinking-layout-test-label-position out "cold") :to-be-truthy)))

;;; ─── opt-pass-code-sinking ────────────────────────────────────────────────

(it-sequential "code-sinking-moves-unique-use-value-past-label const"
  (destructuring-bind (insts value-type dst-reg) (list (list (make-vm-const :dst :r1 :value 42)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Ldead")
                          (make-vm-ret :reg :r0)
                          (make-vm-label :name "Luse")
                          (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                          (make-vm-ret :reg :r2)) 'cl-cc/vm::vm-const :r1)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (sinkable-pos (position-if (lambda (x)
                                      (and (typep x value-type)
                                           (eq (cl-cc/vm::vm-dst x) dst-reg)))
                                    out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect sinkable-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> sinkable-pos luse-pos) :to-be-truthy))))

(it-sequential "code-sinking-moves-unique-use-value-past-label cons"
  (destructuring-bind (insts value-type dst-reg) (list (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Ldead")
                          (make-vm-ret :reg :r0)
                          (make-vm-label :name "Luse")
                          (make-vm-car :dst :r2 :src :pair)
                          (make-vm-ret :reg :r2)) 'cl-cc/vm::vm-cons :pair)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (sinkable-pos (position-if (lambda (x)
                                      (and (typep x value-type)
                                           (eq (cl-cc/vm::vm-dst x) dst-reg)))
                                    out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (expect sinkable-pos :to-be-truthy)
    (expect luse-pos :to-be-truthy)
    (expect (> sinkable-pos luse-pos) :to-be-truthy))))

(it-sequential "code-sinking-preserves-multi-use-value-before-jump const"
  (destructuring-bind (insts value-type dst-reg) (list (list (make-vm-const :dst :r1 :value 7)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Luse")
                          (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                          (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                          (make-vm-ret :reg :r3)) 'cl-cc/vm::vm-const :r1)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (val-pos (position-if (lambda (x)
                                 (and (typep x value-type)
                                      (eq (cl-cc/vm::vm-dst x) dst-reg)))
                               out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (expect val-pos :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (< val-pos jump-pos) :to-be-truthy))))

(it-sequential "code-sinking-preserves-multi-use-value-before-jump cons"
  (destructuring-bind (insts value-type dst-reg) (list (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                          (make-vm-jump :label "Luse")
                          (make-vm-label :name "Luse")
                          (make-vm-car :dst :r2 :src :pair)
                          (make-vm-cdr :dst :r3 :src :pair)
                          (make-vm-ret :reg :r3)) 'cl-cc/vm::vm-cons :pair)
    (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (val-pos (position-if (lambda (x)
                                 (and (typep x value-type)
                                      (eq (cl-cc/vm::vm-dst x) dst-reg)))
                               out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (expect val-pos :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect (< val-pos jump-pos) :to-be-truthy))))

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
