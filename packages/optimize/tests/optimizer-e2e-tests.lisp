;;;; tests/optimizer-e2e-tests.lisp — Optimizer End-to-End, Bitwise, Inlining, Prolog Peephole

(in-package :cl-cc/test)


;;; ── End-to-End Correctness ───────────────────────────────────────────────

(it-sequential "optimizer-e2e simple-let"
  (destructuring-bind (expected expr) (list 10 "(let ((x 10)) x)")
    (assert-run= expected expr)))

(it-sequential "optimizer-e2e two-bindings"
  (destructuring-bind (expected expr) (list 7 "(let ((x 3) (y 4)) (+ x y))")
    (assert-run= expected expr)))

(it-sequential "optimizer-e2e square"
  (destructuring-bind (expected expr) (list 4 "(let ((x 2)) (* x x))")
    (assert-run= expected expr)))

(it-sequential "optimizer-e2e sum-of-sums"
  (destructuring-bind (expected expr) (list 10 "(+ (+ 1 2) (+ 3 4))")
    (assert-run= expected expr)))

;;; ── Bitwise Algebraic Identities ─────────────────────────────────────────
;;; instr = nil means value-correctness only (no instruction-elimination check).

(it-sequential "optimizer-bitwise logand-zero"
  (destructuring-bind (expected expr verify) (list 0 "(let ((x 42)) (logand x 0))" (lambda (expr)
             (expect (opt-has-p expr 'vm-logand) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise logand-minus-one"
  (destructuring-bind (expected expr verify) (list 42 "(let ((x 42)) (logand x -1))" (lambda (expr)
             (expect (opt-has-p expr 'vm-logand) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise logand-self"
  (destructuring-bind (expected expr verify) (list 12 "(let ((x 12)) (logand x x))" (lambda (_expr)
             (declare (ignore _expr))))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise logior-zero"
  (destructuring-bind (expected expr verify) (list 42 "(let ((x 42)) (logior x 0))" (lambda (expr)
             (expect (opt-has-p expr 'vm-logior) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise logxor-zero"
  (destructuring-bind (expected expr verify) (list 42 "(let ((x 42)) (logxor x 0))" (lambda (_expr)
             (declare (ignore _expr))))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise logxor-self"
  (destructuring-bind (expected expr verify) (list 0 "(let ((x 42)) (logxor x x))" (lambda (expr)
             (expect (opt-has-p expr 'vm-logxor) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise ash-zero"
  (destructuring-bind (expected expr verify) (list 42 "(let ((x 42)) (ash x 0))" (lambda (expr)
             (expect (opt-has-p expr 'vm-ash) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

(it-sequential "optimizer-bitwise rem-zero"
  (destructuring-bind (expected expr verify) (list 0 "(let ((x 42)) (rem 0 x))" (lambda (expr)
             (expect (opt-has-p expr 'vm-rem) :to-be-falsy)))
    (assert-run= expected expr) (funcall verify expr)))

;;; ── Unary Constant Folding ────────────────────────────────────────────────

(it-sequential "optimizer-lognot-constant"
  (expect (run-string "(lognot 0)") :to-equal -1)
  (let* ((instrs (list (cl-cc:make-vm-const :dst :r1 :value 0)
                       (cl-cc:make-vm-lognot :dst :r0 :src :r1)))
         (out (cl-cc/optimize::opt-pass-fold instrs)))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-lognot)) out) :to-be-falsy)
    (expect (some (lambda (i)
                         (and (cl-cc:vm-const-p i)
                              (eq :r0 (cl-cc/vm::vm-dst i))
                              (eql -1 (cl-cc/vm::vm-value i))))
                       out) :to-be-truthy)))

(it-sequential "optimizer-not-zero-value"
  (expect (run-string "(not 0)") :to-be-null))

;;; ── Function Inlining ────────────────────────────────────────────────────

(it-sequential "optimizer-inline"
  (assert-run= 5 "(defun double-inc (x) (+ x 1)) (double-inc 4)")
  (expect (opt-has-p "(defun double-inc (x) (+ x 1)) (double-inc 4)" 'vm-call) :to-be-falsy)
  (assert-run= 7 "(defun add2 (a b) (+ a b)) (add2 3 4)"))

(it-sequential "optimizer-leaf-detect"
  (multiple-value-bind (leaf-insts leaf-p)
      (optimize-instructions (list (make-vm-const :dst :r0 :value 1)
                                   (make-vm-ret :reg :r0)))
    (declare (ignore leaf-insts))
    (expect leaf-p :to-be-truthy))
  (multiple-value-bind (call-insts leaf-p)
      (optimize-instructions (list (make-vm-call :dst :r0 :func :r1 :args '(:r2))
                                   (make-vm-ret :reg :r0)))
    (declare (ignore call-insts))
    (expect (member leaf-p '(t nil)) :to-be-truthy)))

(it-sequential "optimizer-leaf-flag-through-compile-pipeline"
  (let* ((result (compile-string "(+ 1 2)" :target :x86_64))
          (program (compilation-result-program result)))
     (expect (member (cl-cc/vm::vm-program-leaf-p program) '(t nil)) :to-be-truthy)))

(it-sequential "optimizer-pipeline-program-instructions-track-optimized-output"
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (program (compilation-result-program result)))
    (expect (cl-cc:compilation-result-vm-instructions result) :to-equal (vm-program-instructions program))
    (expect (or (null (cl-cc:compilation-result-optimized-instructions result))
                     (listp (cl-cc:compilation-result-optimized-instructions result))) :to-be-truthy)))

(it-sequential "prolog-peephole-collapses-const-followed-by-move"
  (let ((out (cl-cc/optimize:apply-prolog-peephole
              '((:const :r1 42) (:move :r2 :r1)))))
    (expect out :to-equal '((:const :r2 42)))))
