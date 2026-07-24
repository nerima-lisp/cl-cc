;;;; Unit tests for FR-682 loop peeling.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest fr-682-loop-peel-peels-array-boundary-first-iteration
  "FR-682: a counted loop with an array access on the IV is peeled once before the header."
(let* ((aref (cl-cc:make-vm-aref :dst :elt :array-reg :arr :index-reg :i))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      aref
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :elt)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts))
         (loop-pos (%test-label-position out "loop")))
    (assert-true loop-pos)
    (assert-true (> loop-pos 3))
    (assert-true (typep (nth (- loop-pos 3) out) 'cl-cc/vm::vm-lt))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-aref)
                              (cl-cc/optimize::opt-bounds-check-eliminable-marked-p inst)))
                       out))))

(deftest fr-682-loop-peel-skips-call-bearing-loop
  "FR-682: loops with calls are not peeled because first-iteration semantics may differ."
  (let* ((call (make-vm-call :dst :tmp :func :fn :args (list :i)))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      call
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :tmp)))
         (out (cl-cc/optimize::opt-pass-loop-peel insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))

;;;; ─── FR-514 loop fusion / fission (optimizer-loop-fusion.lisp) ──────────────
;;;;
;;;; Fusion/fission operate on the optimizer's linear "canonical counted loop"
;;;; shape: LABEL / (vm-lt iv limit → cond) / (vm-jump-zero cond → exit) / body… /
;;;; (vm-add iv iv step) / (vm-jump → head) / exit-LABEL. These tests exercise the
;;;; dependence-analysis helpers directly plus the two pass entry points.

;;; ─── memory-dependence arithmetic helpers ───────────────────────────────────

(deftest-each fr-514-gcd-test-safe-cases
  "%loop-fr514-gcd-test-safe-p proves independence when the subscript delta is
   not divisible by gcd(strides); a zero gcd is trivially safe."
  :cases (("coprime-delta"   '(:stride 2 :offset 0) '(:stride 2 :offset 1) t)
          ("divisible-delta" '(:stride 2 :offset 0) '(:stride 2 :offset 2) nil)
          ("zero-strides"    '(:stride 0 :offset 0) '(:stride 0 :offset 5) t))
  (a b expected)
  (let ((result (cl-cc/optimize::%loop-fr514-gcd-test-safe-p a b)))
    (if expected (assert-true result) (assert-false result))))

(deftest-each fr-514-banerjee-safe-cases
  "%loop-fr514-banerjee-safe-p excludes overlap when the offset delta falls
   outside the Banerjee [lo,hi] bound for the trip count."
  :cases (("far-offset"  '(:stride 1 :offset 100) '(:stride 1 :offset 0) 3 t)
          ("same-offset" '(:stride 1 :offset 0)   '(:stride 1 :offset 0) 3 nil))
  (a b trip expected)
  (let ((result (cl-cc/optimize::%loop-fr514-banerjee-safe-p a b trip)))
    (if expected (assert-true result) (assert-false result))))

;;; ─── instruction classification helpers ─────────────────────────────────────

(deftest-each fr-514-memory-inst-p-cases
  "%loop-fr514-memory-inst-p flags array/slot/global accesses only."
  :cases (("aref"  'aref  t)
          ("aset"  'aset  t)
          ("const" 'const nil))
  (kind expected)
  (let ((inst (ecase kind
                (aref  (make-vm-aref :dst :d :array-reg :a :index-reg :i))
                (aset  (make-vm-aset :array-reg :a :index-reg :i :val-reg :v))
                (const (make-vm-const :dst :d :value 1)))))
    (if expected
        (assert-true (cl-cc/optimize::%loop-fr514-memory-inst-p inst))
        (assert-false (cl-cc/optimize::%loop-fr514-memory-inst-p inst)))))

(deftest fr-514-write-inst-p-distinguishes-stores-from-loads
  "%loop-fr514-write-inst-p is true for aset (store), false for aref (load)."
  (assert-true (cl-cc/optimize::%loop-fr514-write-inst-p
                (make-vm-aset :array-reg :a :index-reg :i :val-reg :v)))
  (assert-false (cl-cc/optimize::%loop-fr514-write-inst-p
                 (make-vm-aref :dst :d :array-reg :a :index-reg :i))))

(deftest fr-514-pure-core-p-rejects-control-flow
  "A core of constants is pure; adding a jump makes it impure (unsafe to move)."
  (assert-true (cl-cc/optimize::%loop-fr514-pure-core-p
                (list (make-vm-const :dst :a :value 1)
                      (make-vm-const :dst :b :value 2))))
  (assert-false (cl-cc/optimize::%loop-fr514-pure-core-p
                 (list (make-vm-const :dst :a :value 1)
                       (make-vm-jump :label "somewhere")))))

;;; ─── label + register helpers ───────────────────────────────────────────────

(deftest fr-514-label-name-derives-fresh-keyword
  "%loop-fr514-label-name joins base and suffix into a keyword."
  (assert-eq (intern "HEAD__FISSION_A" :keyword)
             (cl-cc/optimize::%loop-fr514-label-name "HEAD" "FISSION_A")))

(deftest fr-514-find-label-index-locates-label-or-nil
  "%loop-fr514-find-label-index returns the index of a matching label, else NIL."
  (let ((vec (vector (make-vm-const :dst :i :value 0)
                     (make-vm-label :name "target")
                     (make-vm-ret :reg :i))))
    (assert-= 1 (cl-cc/optimize::%loop-fr514-find-label-index vec "target"))
    (assert-null (cl-cc/optimize::%loop-fr514-find-label-index vec "missing"))))

(deftest fr-514-build-envs-tracks-constants-and-defs
  "%loop-fr514-build-envs records integer constants and later non-const defs clear them."
  (let ((insts (list (make-vm-const :dst :a :value 5)
                     (make-vm-const :dst :b :value 7)
                     (make-vm-add :dst :a :lhs :a :rhs :b))))
    (multiple-value-bind (const-env def-env)
        (cl-cc/optimize::%loop-fr514-build-envs insts 3)
      ;; :a was reassigned by a non-const add → dropped from const-env.
      (assert-null (gethash :a const-env))
      (assert-= 7 (gethash :b const-env))
      ;; def-env still records the producing instruction for :a.
      (assert-true (typep (gethash :a def-env) 'cl-cc/vm::vm-add)))))

(deftest fr-514-register-dependencies-detects-cross-core-use
  "%loop-fr514-register-dependencies-p is true when one core reads the other's output."
  (let ((dependent-l (list (make-vm-add :dst :x :lhs :y :rhs :z)))
        (dependent-r (list (make-vm-add :dst :w :lhs :x :rhs :q)))
        (independent-l (list (make-vm-const :dst :x :value 1)))
        (independent-r (list (make-vm-const :dst :y :value 2))))
    (assert-true (cl-cc/optimize::%loop-fr514-register-dependencies-p
                  dependent-l dependent-r))
    (assert-false (cl-cc/optimize::%loop-fr514-register-dependencies-p
                   independent-l independent-r))))

;;; ─── loop fusion pass ───────────────────────────────────────────────────────

(defun %fr-514-counted-loop (head exit iv limit step core)
  "Build a canonical counted loop: label / lt / jz / CORE… / add-step / jump / exit."
  (append (list (make-vm-label :name head)
                (make-vm-lt :dst (intern (format nil "COND-~A" head) :keyword)
                            :lhs iv :rhs limit)
                (make-vm-jump-zero :reg (intern (format nil "COND-~A" head) :keyword)
                                   :label exit))
          core
          (list (make-vm-add :dst iv :lhs iv :rhs step)
                (make-vm-jump :label head)
                (make-vm-label :name exit))))

(deftest fr-514-fusion-merges-adjacent-identical-space-loops
  "opt-pass-loop-fusion fuses two adjacent pure loops sharing init/limit/step."
  (let* ((insts (append
                 (list (make-vm-const :dst :i :value 0)
                       (make-vm-const :dst :j :value 0)
                       (make-vm-const :dst :limit :value 8)
                       (make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headA" "exitA" :i :limit :one
                                       (list (make-vm-const :dst :xa :value 7)))
                 (%fr-514-counted-loop "headB" "exitB" :j :limit :one
                                       (list (make-vm-const :dst :xb :value 9)))
                 (list (make-vm-ret :reg :xa))))
         (out (cl-cc/optimize::opt-pass-loop-fusion insts)))
    ;; Fusion collapses two loop headers into one: fewer instructions, one back-edge.
    (assert-true (< (length out) (length insts)))
    (assert-= 1 (count-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out))
    ;; The surviving exit label is the second loop's exit.
    (assert-true (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (equal (cl-cc/vm::vm-name x) "exitB")))
                       out))))

(deftest fr-514-fusion-skips-loops-with-different-iteration-spaces
  "opt-pass-loop-fusion leaves adjacent loops untouched when limits differ."
  (let* ((insts (append
                 (list (make-vm-const :dst :i :value 0)
                       (make-vm-const :dst :j :value 0)
                       (make-vm-const :dst :limit :value 8)
                       (make-vm-const :dst :limit2 :value 9)
                       (make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headA" "exitA" :i :limit :one
                                       (list (make-vm-const :dst :xa :value 7)))
                 (%fr-514-counted-loop "headB" "exitB" :j :limit2 :one
                                       (list (make-vm-const :dst :xb :value 9)))))
         (out (cl-cc/optimize::opt-pass-loop-fusion insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))

;;; ─── loop fission pass ──────────────────────────────────────────────────────

(deftest fr-514-fission-splits-large-independent-pure-loop
  "opt-pass-loop-fission clones a large independent pure core into two loops."
  (let* ((core (loop for k from 1 to 8
                     collect (make-vm-const
                              :dst (intern (format nil "C~A" k) :keyword)
                              :value k)))
         (insts (append
                 (list (make-vm-const :dst :i :value 0)
                       (make-vm-const :dst :limit :value 8)
                       (make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headF" "exitF" :i :limit :one core)
                 (list (make-vm-ret :reg :c1))))
         (out (cl-cc/optimize::opt-pass-loop-fission insts)))
    ;; Fission produces two suffixed loops (FISSION_A / FISSION_B).
    (assert-true (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (search "FISSION_A" (string (cl-cc/vm::vm-name x)))))
                       out))
    (assert-true (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-label)
                              (search "FISSION_B" (string (cl-cc/vm::vm-name x)))))
                       out))))

(deftest fr-514-fission-leaves-small-loops-unchanged
  "opt-pass-loop-fission does not split a loop whose core is below the size floor."
  (let* ((insts (append
                 (list (make-vm-const :dst :i :value 0)
                       (make-vm-const :dst :limit :value 8)
                       (make-vm-const :dst :one :value 1))
                 (%fr-514-counted-loop "headS" "exitS" :i :limit :one
                                       (list (make-vm-const :dst :x :value 1)))
                 (list (make-vm-ret :reg :x))))
         (out (cl-cc/optimize::opt-pass-loop-fission insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))
