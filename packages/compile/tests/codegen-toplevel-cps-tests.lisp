(in-package :cl-cc/test)

(defun %unwrap-captured-cps-entry (captured-expr)
  "Normalize top-level CPS capture shape.
Accept either a raw (lambda (k) ...) form or a singleton list containing it."
  (if (and (consp captured-expr)
           (consp (car captured-expr))
           (eq 'lambda (caar captured-expr)))
      (car captured-expr)
      captured-expr))

;;; ─── %make-compile-opts ──────────────────────────────────────────────────

(it-sequential "codegen-make-compile-opts-defaults"
  (let ((opts (cl-cc/compile::%make-compile-opts)))
    (expect (listp opts) :to-be-truthy)
    (expect (getf opts :pass-pipeline) :to-be-null)
    (expect (getf opts :print-pass-timings) :to-be-null)
    (expect (getf opts :timing-stream) :to-be-null)
    (expect (getf opts :opt-remarks-mode) :to-be :all)
    (expect (getf opts :trace-json-stream) :to-be-null)
    (expect (getf opts :tsan) :to-be-null)))

(it-sequential "codegen-make-compile-opts-explicit-values"
  (let ((opts (cl-cc/compile::%make-compile-opts :pass-pipeline '(:fold :dce)
                                                 :opt-remarks-mode :pass
                                                 :print-pass-stats t
                                                 :tsan t)))
    (expect (getf opts :pass-pipeline) :to-equal '(:fold :dce))
    (expect (getf opts :opt-remarks-mode) :to-be :pass)
    (expect (getf opts :print-pass-stats) :to-be-truthy)
    (expect (getf opts :tsan) :to-be-truthy)))

;;; ─── %result-vm-instructions-without-halt ────────────────────────────────

(it-sequential "codegen-result-vm-instructions-without-halt-strips-terminal-halt-toplevel"
  (let* ((move (cl-cc:make-vm-move :dst :R1 :src :R0))
         (halt (cl-cc:make-vm-halt :reg :R1))
         (result (cl-cc/compile:make-compilation-result
                  :program (cl-cc:make-vm-program :instructions (list move halt) :result-register :R1)
                  :vm-instructions (list move halt))))
    (expect (cl-cc/compile::%result-vm-instructions-without-halt result) :to-equal (list move))))

(it-sequential "codegen-toplevel-block-prefers-cps-primary-path"
  (let* ((source '(block done (return-from done 7) 99))
         (expanded (cl-cc/expand:compiler-macroexpand-all source))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded))
         (captured-expr nil)
         (compile-ast-called nil))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-truthy)
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (expr &rest args)
                               (declare (ignore args))
                               (setf captured-expr expr)
                               (cl-cc/compile:make-compilation-result
                                :program (cl-cc:make-vm-program
                                          :instructions nil
                                          :result-register :R-CPS)
                                :vm-instructions
                                (list (cl-cc:make-vm-halt :reg :R-CPS))
                                :cps '(lambda (k) (funcall k 7)))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (cl-cc/compile:compile-toplevel-forms (list source) :target :vm)))
    (let ((normalized (%unwrap-captured-cps-entry captured-expr)))
      (expect normalized :to-be-truthy)
      (expect (car normalized) :to-be 'lambda))
    (expect compile-ast-called :to-be-falsy)))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defun"
  (destructuring-bind (form) (list '(defun cps-safe-fn (x) x))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defvar"
  (destructuring-bind (form) (list '(defvar *cps-safe-var* 1))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defun-rest"
  (destructuring-bind (form) (list '(defun cps-safe-rest-fn (x &rest rest) x))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defun-optional"
  (destructuring-bind (form) (list '(defun cps-safe-opt-fn (x &optional y) x))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defun-key"
  (destructuring-bind (form) (list '(defun cps-safe-key-fn (&key x) x))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defclass"
  (destructuring-bind (form) (list '(defclass cps-safe-class () ((slot :initarg :slot))))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions defmethod"
  (destructuring-bind (form) (list '(defmethod cps-safe-generic ((x integer)) x))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions set-slot-value"
  (destructuring-bind (form) (list '(setf (slot-value obj 'slot) 1))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions make-instance"
  (destructuring-bind (form) (list '(make-instance 'cps-safe-class))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions make-instance-initargs"
  (destructuring-bind (form) (list '(make-instance 'cps-safe-class :slot 1))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-direct-path-safe-subset-exclusions slot-value"
  (destructuring-bind (form) (list '(slot-value obj 'slot))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy))))

(it-sequential "codegen-toplevel-safe-subset-still-allows-simple-clos-forms defgeneric"
  (destructuring-bind (form) (list '(defgeneric cps-safe-generic (x)))
    (let* ((expanded (cl-cc/expand:compiler-macroexpand-all form))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded)))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-truthy))))

(it-sequential "codegen-toplevel-variadic-lambda-stays-unsafe"
  (let ((lambda-ast (cl-cc/ast:make-ast-lambda
                     :params '(x)
                     :optional-params (list (list 'y nil nil))
                     :body (list (cl-cc:make-ast-var :name 'x)))))
    (expect (cl-cc::%cps-vm-compile-safe-ast-p lambda-ast) :to-be-falsy)))

(it-sequential "codegen-toplevel-unsafe-form-stays-on-direct-path"
  (let ((compile-expression-called nil)
        (compile-ast-called nil))
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (&rest args)
                               (declare (ignore args))
                               (setf compile-expression-called t)
                               (cl-cc/compile:make-compilation-result
                                :program (cl-cc:make-vm-program :instructions nil :result-register :R-CPS)
                                :vm-instructions (list (cl-cc:make-vm-halt :reg :R-CPS)))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (let ((*enable-cps-vm-primary-path* nil))
          (cl-cc/compile:compile-toplevel-forms '((+ 1 2)) :target :vm))))
    (expect compile-expression-called :to-be-truthy)
    (expect compile-ast-called :to-be-falsy)))

(deftest-compile codegen-toplevel-cps-semantic-preservation
  "Multi-form Lisp sources still evaluate to the final value after the top-level CPS routing change."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)"))
  :stdlib nil)

(deftest-compile codegen-toplevel-cps-apply-semantics
  "Top-level CPS routing preserves APPLY behavior."
  :cases (("apply" 6 "(apply (function +) (list 1 2 3))"))
  :stdlib nil)

(it-sequential "codegen-toplevel-multiple-values-avoid-cps-route"
  (dolist (ast (list (cl-cc/ast:make-ast-values :forms nil)
                     (cl-cc/ast:make-ast-values
                      :forms (list (cl-cc/ast:make-ast-int :value 1)))
                     (cl-cc/ast:make-ast-values
                      :forms (list (cl-cc/ast:make-ast-int :value 1)
                                   (cl-cc/ast:make-ast-int :value 2)
                                   (cl-cc/ast:make-ast-int :value 3)))
                     (cl-cc/ast:make-ast-multiple-value-bind
                      :vars (quote (a b))
                      :values-form (cl-cc/ast:make-ast-values
                                    :forms (list (cl-cc/ast:make-ast-int :value 1)
                                                 (cl-cc/ast:make-ast-int :value 2)))
                      :body (list (cl-cc/ast:make-ast-var :name (quote a))))))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-falsy)))

(it-sequential "fr-371-codegen-toplevel-tagbody-prefers-cps-primary-path"
  (let* ((source (quote (tagbody start (go done) done)))
         (expanded (cl-cc/expand:compiler-macroexpand-all source))
         (ast (cl-cc/compile::%lower-toplevel-form-to-ast expanded))
         (captured-expr nil)
         (compile-ast-called nil))
    (expect (cl-cc/compile:%cps-vm-compile-safe-ast-p ast) :to-be-truthy)
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (expr &rest args)
                               (declare (ignore args))
                               (setf captured-expr expr)
                               (cl-cc/compile:make-compilation-result
                                :program (cl-cc:make-vm-program
                                          :instructions nil
                                          :result-register :R-CPS)
                                :vm-instructions
                                (list (cl-cc:make-vm-halt :reg :R-CPS))
                                :cps (quote (lambda (k) (funcall k nil))))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (cl-cc/compile:compile-toplevel-forms (list source) :target :vm)))
    (let ((normalized (%unwrap-captured-cps-entry captured-expr)))
      (expect normalized :to-be-truthy)
      (expect (car normalized) :to-be (quote lambda)))
    (expect compile-ast-called :to-be-falsy)))
(deftest-compile fr-371-codegen-toplevel-tagbody-real-vm-backend
  "CPS local tag continuations compile and execute through the real VM backend."
  :cases (("backward-go-side-effect" 2 "(let ((counter 0)) (tagbody loop (setq counter (+ counter 1)) (if (< counter 2) (go loop))) counter)"))
  :stdlib nil)
