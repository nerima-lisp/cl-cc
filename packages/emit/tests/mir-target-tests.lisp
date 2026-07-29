;;;; tests/unit/emit/mir-target-tests.lisp — MIR printer smoke tests and target-desc tests

(in-package :cl-cc/test)


;;;; ─── printer smoke tests ───────────────────────────────────────────────

(it-sequential "mir-format-value-shows-id-and-name"
  (let* ((fn (mir-make-function :f))
         (v  (mir-new-value fn :name :x))
         (c  (make-mir-const :value 42)))
    (let ((sv (mir-format-value v))
          (sc (mir-format-value c)))
      (expect (search "%0" sv) :to-be-truthy)
      (expect (search "X"  sv) :to-be-truthy)
      (expect (search "42" sc) :to-be-truthy))))

(it-sequential "mir-print-function-shows-name-and-entry"
  (let* ((fn  (mir-make-function :smoke))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn :name :r))
         (c   (make-mir-const :value 7 :type :integer)))
    (mir-emit blk :const :dst dst :srcs (list c))
    (mir-emit blk :ret   :srcs (list dst))
    (let ((out (with-output-to-string (s) (mir-print-function fn s))))
      (expect (search "SMOKE" out) :to-be-truthy)
      (expect (search "ENTRY" out) :to-be-truthy))))

(it-sequential "mir-format-value-phi-operand"
  (let* ((fn  (mir-make-function :f))
         (blk (mir-new-block fn :label :loop))
         (v   (mir-new-value fn :name :x))
         (out (mir-format-value (cons blk v))))
    (expect (search "LOOP" out) :to-be-truthy)
    (expect (search "%0"   out) :to-be-truthy)
    (expect (search "["    out) :to-be-truthy)))

(it-sequential "mir-format-value-unknown-operand-falls-back-to-aesthetic"
  (expect (mir-format-value :foo) :to-equal ":FOO"))

(it-sequential "mir-print-inst-emits-meta-comment"
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn :name :r))
         (inst (mir-emit blk :call :dst dst :meta '(:effect-kind :read-only)))
         (out  (with-output-to-string (s) (mir-print-inst inst s))))
    (expect (search ";" out) :to-be-truthy)
    (expect (search "READ-ONLY" out) :to-be-truthy)))

(it-sequential "mir-print-block-shows-preds-and-phis"
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (blk   (mir-new-block fn :label :merge))
         (phi   (mir-new-value fn :name :p)))
    (mir-add-succ entry blk)
    (mir-emit blk :phi :dst phi)
    (let ((out (with-output-to-string (s) (mir-print-block blk s))))
      (expect (search "MERGE" out) :to-be-truthy)
      (expect (search "preds:" out) :to-be-truthy)
      (expect (search "ENTRY" out) :to-be-truthy)
      (expect (search "PHI" out) :to-be-truthy))))

;;;; ─── target-desc ──────────────────────────────────────────────────────

(it-sequential "target-x86-64-description"
  (expect (target-name *x86-64-target*) :to-be :x86-64)
  (expect (= 8 (target-word-size *x86-64-target*)) :to-be-truthy)
  (expect (target-endianness *x86-64-target*) :to-be :little)
  (expect (= 16 (target-stack-alignment *x86-64-target*)) :to-be-truthy)
  (expect (target-ret-reg *x86-64-target*) :to-be :rax)
  (expect (first (target-arg-regs *x86-64-target*)) :to-be :rdi)
  (expect (= 6 (length (target-arg-regs *x86-64-target*))) :to-be-truthy)
  (expect (member :rbx (target-callee-saved *x86-64-target*)) :to-be-truthy)
  (expect (member :r12 (target-callee-saved *x86-64-target*)) :to-be-truthy))

(it-sequential "target-non-x86-basic aarch64"
  (destructuring-bind (target expected-name expected-word expected-ret expected-args expected-gprs) (list *aarch64-target* :aarch64 8 :x0 8 nil)
    (expect (target-name target) :to-be expected-name) (expect (= expected-word (target-word-size target)) :to-be-truthy) (when expected-ret
    (expect (target-ret-reg target) :to-be expected-ret)) (expect (= expected-args (length (target-arg-regs target))) :to-be-truthy) (when expected-gprs
    (expect (= expected-gprs (target-gpr-count target)) :to-be-truthy))))

(it-sequential "target-non-x86-basic riscv64"
  (destructuring-bind (target expected-name expected-word expected-ret expected-args expected-gprs) (list *riscv64-target* :riscv64 8 :a0 8 32)
    (expect (target-name target) :to-be expected-name) (expect (= expected-word (target-word-size target)) :to-be-truthy) (when expected-ret
    (expect (target-ret-reg target) :to-be expected-ret)) (expect (= expected-args (length (target-arg-regs target))) :to-be-truthy) (when expected-gprs
    (expect (= expected-gprs (target-gpr-count target)) :to-be-truthy))))

(it-sequential "target-non-x86-basic wasm32"
  (destructuring-bind (target expected-name expected-word expected-ret expected-args expected-gprs) (list *wasm32-target* :wasm32 4 nil 0 0)
    (expect (target-name target) :to-be expected-name) (expect (= expected-word (target-word-size target)) :to-be-truthy) (when expected-ret
    (expect (target-ret-reg target) :to-be expected-ret)) (expect (= expected-args (length (target-arg-regs target))) :to-be-truthy) (when expected-gprs
    (expect (= expected-gprs (target-gpr-count target)) :to-be-truthy))))

(it-sequential "target-registry-lookup x86-64"
  (destructuring-bind (name verify) (list :x86-64 (lambda (target)
             (expect target :to-be *x86-64-target*)))
    (funcall verify (find-target name))))

(it-sequential "target-registry-lookup aarch64"
  (destructuring-bind (name verify) (list :aarch64 (lambda (target)
             (expect target :to-be *aarch64-target*)))
    (funcall verify (find-target name))))

(it-sequential "target-registry-lookup riscv64"
  (destructuring-bind (name verify) (list :riscv64 (lambda (target)
             (expect target :to-be *riscv64-target*)))
    (funcall verify (find-target name))))

(it-sequential "target-registry-lookup wasm32"
  (destructuring-bind (name verify) (list :wasm32 (lambda (target)
             (expect target :to-be *wasm32-target*)))
    (funcall verify (find-target name))))

(it-sequential "target-registry-lookup nonexistent"
  (destructuring-bind (name verify) (list :nonexistent (lambda (target)
             (expect target :to-be-null)))
    (funcall verify (find-target name))))

(it-sequential "target-64-bit-predicate x86-64"
  (destructuring-bind (target verify) (list *x86-64-target* (lambda (target)
             (expect (target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-predicate aarch64"
  (destructuring-bind (target verify) (list *aarch64-target* (lambda (target)
             (expect (target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-predicate riscv64"
  (destructuring-bind (target verify) (list *riscv64-target* (lambda (target)
             (expect (target-64-bit-p target) :to-be-truthy)))
    (funcall verify target)))

(it-sequential "target-64-bit-predicate wasm32"
  (destructuring-bind (target verify) (list *wasm32-target* (lambda (target)
             (expect (target-64-bit-p target) :to-be-falsy)))
    (funcall verify target)))

(it-sequential "target-feature-predicate x86-64-fused-cmp"
  (destructuring-bind (target feature verify) (list *x86-64-target* :has-fused-cmp-branch (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-feature-predicate aarch64-tail-call"
  (destructuring-bind (target feature verify) (list *aarch64-target* :has-native-tail-call (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-feature-predicate riscv64-psabi"
  (destructuring-bind (target feature verify) (list *riscv64-target* :riscv-elf-psabi (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-feature-predicate wasm32-wasi"
  (destructuring-bind (target feature verify) (list *wasm32-target* :wasi-0.2 (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-truthy)))
    (funcall verify target feature)))

(it-sequential "target-feature-predicate x86-64-no-wasi"
  (destructuring-bind (target feature verify) (list *x86-64-target* :wasi-0.2 (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-falsy)))
    (funcall verify target feature)))

(it-sequential "target-feature-predicate wasm32-no-sysv"
  (destructuring-bind (target feature verify) (list *wasm32-target* :sysv-abi (lambda (target feature)
             (expect (target-has-feature-p target feature) :to-be-falsy)))
    (funcall verify target feature)))

(it-sequential "target-scratch-regs-excluded-from-allocatable"
  (dolist (target (list *x86-64-target* *aarch64-target* *riscv64-target*))
    (let ((alloc   (target-allocatable-regs target))
          (scratch (target-scratch-regs target)))
      (dolist (sr scratch)
        (expect (member sr alloc) :to-be-falsy)))))

(it-sequential "target-caller-saved-subset-of-allocatable-and-disjoint-from-callee"
  (let* ((alloc  (target-allocatable-regs *x86-64-target*))
         (callee (target-callee-saved *x86-64-target*))
         (caller (target-caller-saved *x86-64-target*)))
    (dolist (r caller)
      (expect (member r alloc) :to-be-truthy))
    (dolist (r caller)
      (expect (member r callee) :to-be-falsy))))

(it-sequential "target-register-and-find-roundtrip"
  (let* ((custom (make-target-desc
                  :name      :test-custom
                  :word-size 8
                  :endianness :little
                  :gpr-count 4
                  :gpr-names #(:r0 :r1 :r2 :r3)
                  :arg-regs  '(:r0 :r1)
                  :ret-reg   :r0
                  :stack-alignment 16))
         (got (progn
                (register-target custom)
                (find-target :test-custom))))
    (expect got :to-be custom)
    (expect (target-name got) :to-be :test-custom)
    (remhash :test-custom *target-registry*)))
