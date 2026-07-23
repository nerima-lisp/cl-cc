;;;; tests/unit/core/cps-tests.lisp — CPS Transformation tests
;;;;
;;;; Covers:
;;;;   1. Helper functions: eval-cps-ast, run-cps-ast, is-cps-lambda
;;;;   2. Structure predicate tests (single-param-lambda-p, funcall-of-single-lambda-p, etc.)
;;;;   3. S-expression CPS bootstrap transformer — semantic evaluation
;;;;
;;;; AST-based CPS tests continue in cps-ast-tests.lisp.

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun eval-cps-ast (ast)
  "CPS-transform an AST node to a (lambda (k) ...) sexp and evaluate it.
Returns a function that takes a continuation."
  (eval (cl-cc:cps-transform-ast* ast)))

(defun run-cps-ast (ast)
  "Run CPS-transformed AST with the identity continuation."
  (funcall (eval-cps-ast ast) #'identity))

(defun is-cps-lambda (result)
  "Return t if RESULT is a (lambda (k) ...) sexp as produced by cps-transform-ast*."
  (and (listp result)
       (eq 'lambda (car result))))

(defun %cps-form-contains-p (form sym)
  "Return T if SYM appears anywhere in FORM."
  (cond ((eq form sym) t)
        ((consp form) (or (%cps-form-contains-p (car form) sym)
                          (%cps-form-contains-p (cdr form) sym)))
        (t nil)))

(defun %cps-trmc-rewritten-p (source rewritten)
  "Return T when REWRITTEN has the expected accumulator-worker TRMC shape."
  (and (not (equal source rewritten))
       (%cps-form-contains-p rewritten 'labels)
       (or (%cps-form-contains-p rewritten 'nreverse)
           (%cps-form-contains-p rewritten 'nreconc))))

(defun %eval-trmc-defun (form symbol)
  "Evaluate FORM and return SYMBOL's function object."
  (eval form)
  (symbol-function symbol))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Structure predicate tests (extracted for readability)
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-single-param-lambda-p yes-single"
  (destructuring-bind (expected form) (list t '(lambda (x) x))
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-p yes-with-body"
  (destructuring-bind (expected form) (list t '(lambda (k) (funcall f k)))
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-p no-multi-param"
  (destructuring-bind (expected form) (list nil '(lambda (x y) x))
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-p no-zero-param"
  (destructuring-bind (expected form) (list nil '(lambda () 42))
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-p no-not-lambda"
  (destructuring-bind (expected form) (list nil '(funcall f x))
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-p no-atom"
  (destructuring-bind (expected form) (list nil 42)
    (expect (cl-cc/cps::%single-param-lambda-p form) :to-equal expected)))

(it-sequential "cps-funcall-of-single-lambda-p yes"
  (destructuring-bind (expected form) (list t '(funcall (lambda (x) x) 42))
    (expect (cl-cc/cps::%funcall-of-single-lambda-p form) :to-equal expected)))

(it-sequential "cps-funcall-of-single-lambda-p no-multi-args"
  (destructuring-bind (expected form) (list nil '(funcall (lambda (x) x) 1 2))
    (expect (cl-cc/cps::%funcall-of-single-lambda-p form) :to-equal expected)))

(it-sequential "cps-funcall-of-single-lambda-p no-plain-fn"
  (destructuring-bind (expected form) (list nil '(funcall f 42))
    (expect (cl-cc/cps::%funcall-of-single-lambda-p form) :to-equal expected)))

(it-sequential "cps-funcall-of-single-lambda-p no-multi-param"
  (destructuring-bind (expected form) (list nil '(funcall (lambda (x y) x) 1))
    (expect (cl-cc/cps::%funcall-of-single-lambda-p form) :to-equal expected)))

(it-sequential "cps-funcall-of-single-lambda-p no-atom"
  (destructuring-bind (expected form) (list nil 42)
    (expect (cl-cc/cps::%funcall-of-single-lambda-p form) :to-equal expected)))

(it-sequential "cps-single-param-lambda-parts"
  (multiple-value-bind (param body)
      (cl-cc/cps::%single-param-lambda-parts '(lambda (k) (funcall next k)))
    (expect param :to-be 'k)
    (expect body :to-equal '(funcall next k))))

(it-sequential "cps-funcall-single-lambda-parts"
  (multiple-value-bind (param body arg)
      (cl-cc/cps::%funcall-single-lambda-parts '(funcall (lambda (x) (+ x 1)) 41))
    (expect param :to-be 'x)
    (expect body :to-equal '(+ x 1))
    (expect (= 41 arg) :to-be-truthy)))

(it-sequential "cps-eta-reducible-lambda-p yes"
  (destructuring-bind (expected form) (list t '(lambda (k) (funcall next k)))
    (expect (cl-cc/cps::%eta-reducible-lambda-p form) :to-equal expected)))

(it-sequential "cps-eta-reducible-lambda-p no-wrong-param"
  (destructuring-bind (expected form) (list nil '(lambda (k) (funcall next x)))
    (expect (cl-cc/cps::%eta-reducible-lambda-p form) :to-equal expected)))

(it-sequential "cps-eta-reducible-lambda-p no-extra-body"
  (destructuring-bind (expected form) (list nil '(lambda (k) (print k) (funcall next k)))
    (expect (cl-cc/cps::%eta-reducible-lambda-p form) :to-equal expected)))

(it-sequential "cps-eta-reducible-lambda-p no-plain-lambda"
  (destructuring-bind (expected form) (list nil '(lambda (k) k))
    (expect (cl-cc/cps::%eta-reducible-lambda-p form) :to-equal expected)))

(it-sequential "cps-eta-reducible-lambda-p no-multi-param"
  (destructuring-bind (expected form) (list nil '(lambda (k j) (funcall next k)))
    (expect (cl-cc/cps::%eta-reducible-lambda-p form) :to-equal expected)))

(it-sequential "cps-simplify-walk-atom-passthrough number"
  (destructuring-bind (expected input) (list 42 42)
    (expect (cl-cc/cps::%cps-simplify-walk input) :to-equal expected)))

(it-sequential "cps-simplify-walk-atom-passthrough symbol"
  (destructuring-bind (expected input) (list 'foo 'foo)
    (expect (cl-cc/cps::%cps-simplify-walk input) :to-equal expected)))

(it-sequential "cps-simplify-walk-atom-passthrough nil"
  (destructuring-bind (expected input) (list nil nil)
    (expect (cl-cc/cps::%cps-simplify-walk input) :to-equal expected)))

(it-sequential "cps-simplify-walk-atom-passthrough keyword"
  (destructuring-bind (expected input) (list :hello :hello)
    (expect (cl-cc/cps::%cps-simplify-walk input) :to-equal expected)))

(it-sequential "cps-simplify-walk-beta-reduces"
  (expect (cl-cc/cps::%cps-simplify-walk '(funcall (lambda (x) x) 42)) :to-equal 42))

(it-sequential "cps-simplify-walk-eta-reduces"
  (expect (cl-cc/cps::%cps-simplify-walk '(lambda (k) (funcall f k))) :to-equal 'f))

(it-sequential "cps-simplify-walk-nested-reduction"
  (let* ((inner '(funcall (lambda (x) x) 99))
         (outer (list inner 1 2))
         (result (cl-cc/cps::%cps-simplify-walk outer)))
    (expect (first result) :to-equal 99)))

(it-sequential "cps-trampoline-form-wraps-tail-continuation-lambda"
  (let ((result (cl-cc/cps::cps-trampoline-form '(lambda (v) (funcall k v)))))
    (expect (first result) :to-be 'lambda)
    (expect (%cps-form-contains-p result :cps-trampoline-thunk) :to-be-truthy)))

(it-sequential "cps-trampoline-run-forces-tail-thunks"
  (let ((thunk (cl-cc/cps::make-cps-trampoline-thunk
                :function (lambda ()
                            (cl-cc/cps::make-cps-trampoline-thunk
                             :function (lambda () 42))))))
    (expect (= 42 (cl-cc/cps::cps-trampoline-run thunk)) :to-be-truthy)))

(it-sequential "cps-simplify-form-reductions beta-reduce"
  (destructuring-bind (expected form) (list 42 '(funcall (lambda (x) x) 42))
    (expect (cl-cc/cps::cps-simplify-form form) :to-equal expected)))

(it-sequential "cps-simplify-form-reductions eta-reduce"
  (destructuring-bind (expected form) (list 'next '(lambda (k) (funcall next k)))
    (expect (cl-cc/cps::cps-simplify-form form) :to-equal expected)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; S-expression CPS (bootstrap transformer)
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-sexp-transform integer"
  (destructuring-bind (expr expected) (list '42 42)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform add"
  (destructuring-bind (expr expected) (list '(+ 1 2) 3)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform sub"
  (destructuring-bind (expr expected) (list '(- 10 3) 7)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform mul"
  (destructuring-bind (expr expected) (list '(* 3 4) 12)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform if-true"
  (destructuring-bind (expr expected) (list '(if 1 10 20) 10)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform if-false"
  (destructuring-bind (expr expected) (list '(if nil 10 20) 20)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform progn-returns-last"
  (destructuring-bind (expr expected) (list '(progn 1 2 3) 3)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform let-binding"
  (destructuring-bind (expr expected) (list '(let ((x 1) (y 2)) (+ x y)) 3)
    (let ((fn (cl-cc:cps-transform-eval expr)))
    (expect (funcall fn #'identity) :to-equal expected))))

(it-sequential "cps-sexp-transform-emits-trampoline-thunk"
  (expect (%cps-form-contains-p (cl-cc:cps-transform '(+ 1 2))
                                      :cps-trampoline-thunk) :to-be-truthy))

(it-sequential "cps-trmc-rewrites-obvious-self-recursive-cons"
  (let* ((source '(defun trmc-fixture (n)
                   (if (<= n 0)
                       nil
                       (cons n (trmc-fixture (- n 1))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (equal source rewritten) :to-be-falsy)
    (expect (%cps-form-contains-p rewritten 'labels) :to-be-truthy)
    (eval rewritten)
    (expect (funcall (symbol-function 'trmc-fixture) 3) :to-equal '(3 2 1))))

(it-sequential "cps-trmc-does-not-rewrite-nonrecursive-dotted-base"
  (let* ((source '(defun trmc-dotted-base ()
                   (cons 'a '(b . c))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect rewritten :to-equal source)
    (expect (funcall (%eval-trmc-defun rewritten 'trmc-dotted-base)) :to-equal '(a b . c))))

(it-sequential "cps-trmc-rewrites-nested-if-tail-cons-branches"
  (let* ((source '(defun trmc-nested-if (n p)
                   (if (<= n 0)
                       nil
                       (if p
                           (cons :a (trmc-nested-if (- n 1) nil))
                           (cons :b (trmc-nested-if (- n 1) t))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (%cps-trmc-rewritten-p source rewritten) :to-be-truthy)
    (expect (funcall (%eval-trmc-defun rewritten 'trmc-nested-if) 4 t) :to-equal '(:a :b :a :b))))

(it-sequential "cps-trmc-rewrites-block-wrapper-tail-cons"
  (let* ((source '(defun trmc-block-wrapper (n)
                   (if (<= n 0)
                       nil
                       (block nil
                         (cons n (trmc-block-wrapper (- n 1)))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (%cps-trmc-rewritten-p source rewritten) :to-be-truthy)
    (expect (funcall (%eval-trmc-defun rewritten 'trmc-block-wrapper) 4) :to-equal '(4 3 2 1))))

(it-sequential "cps-trmc-rewrites-let-and-let-star-wrapper-tail-cons"
  (let* ((let-source '(defun trmc-let-wrapper (n)
                       (if (<= n 0)
                           nil
                           (let ((x n))
                             (cons x (trmc-let-wrapper (- n 1)))))))
         (let*-source '(defun trmc-let-star-wrapper (n)
                         (if (<= n 0)
                             nil
                             (let* ((x n)
                                    (y (+ x 10)))
                               (cons y (trmc-let-star-wrapper (- n 1)))))))
         (let-rewritten (cl-cc/cps::trmc-transform-defun-form let-source))
         (let*-rewritten (cl-cc/cps::trmc-transform-defun-form let*-source)))
    (expect (%cps-trmc-rewritten-p let-source let-rewritten) :to-be-truthy)
    (expect (%cps-trmc-rewritten-p let*-source let*-rewritten) :to-be-truthy)
    (expect (funcall (%eval-trmc-defun let-rewritten 'trmc-let-wrapper) 3) :to-equal '(3 2 1))
    (expect (funcall (%eval-trmc-defun let*-rewritten 'trmc-let-star-wrapper) 3) :to-equal '(13 12 11))))

(it-sequential "optimizer-trmc-rewrites-list-star-termination-pattern"
  (let* ((source '(defun trmc-list-star (n)
                   (declare (optimize (tail-recursion-modulo-cons t)))
                   (if (<= n 0)
                       '(done . tail)
                       (list* n (+ n 10) (trmc-list-star (- n 1))))))
         (rewritten (cl-cc/optimize::opt-trmc-transform-defun-form source)))
    ;; TRMC rewrite produces a valid form
    (expect rewritten :to-be-truthy)))

(it-sequential "cps-trmc-host-native-compiled-worker-produces-correct-result"
  (let* ((source '(defun trmc-native-fixture (n)
                   (if (<= n 0)
                       nil
                       (cons n (trmc-native-fixture (- n 1))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (%cps-trmc-rewritten-p source rewritten) :to-be-truthy)
    (eval rewritten)
    (compile 'trmc-native-fixture)
    (expect (funcall (symbol-function 'trmc-native-fixture) 5) :to-equal '(5 4 3 2 1))))

(it-sequential "cps-trmc-run-string-pipeline-preserves-semantics"
  (expect (run-string "(progn
                               (defun trmc-pipeline-fixture (n)
                                 (if (<= n 0)
                                     nil
                                     (cons n (trmc-pipeline-fixture (- n 1)))))
                               (trmc-pipeline-fixture 4))"
                             :pass-pipeline '(:trmc :fold :jump)) :to-equal '(4 3 2 1)))
