(in-package :cl-cc/test)



(defun is-compile-string (source &key (target :x86_64))
  "Helper: verify that SOURCE compiles without error."
  (handler-case
      (progn (compile-string source :target target) t)
    (error () nil)))

;; ----------------------------------------------------------------------------
;; setq tests
;; ----------------------------------------------------------------------------

(it-sequential "setq-forms basic"
  (destructuring-bind (source expected) (list "(let ((x 1)) (setq x 42) x)" 42)
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "setq-forms multiple"
  (destructuring-bind (source expected) (list "(let ((x 1) (y 2)) (setq x (+ x y)) x)" 3)
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

;; ----------------------------------------------------------------------------
;; quote tests
;; ----------------------------------------------------------------------------

(it-sequential "quote-integer"
  (expect (is-compile-string "(quote 42)" :target :x86_64) :to-be-truthy)
  (let ((result (run-string "(quote 42)")))
    (expect (= result 42) :to-be-truthy)))

(it-sequential "quote-compile-forms symbol"
  (destructuring-bind (source) (list "(quote foo)")
    (expect (is-compile-string source :target :x86_64) :to-be-truthy)))

(it-sequential "quote-compile-forms list"
  (destructuring-bind (source) (list "(quote (1 2 3))")
    (expect (is-compile-string source :target :x86_64) :to-be-truthy)))

(it-sequential "quote-compile-forms nested"
  (destructuring-bind (source) (list "(quote (a (b c) d))")
    (expect (is-compile-string source :target :x86_64) :to-be-truthy)))

;; ----------------------------------------------------------------------------
;; the tests
;; ----------------------------------------------------------------------------

(it-sequential "the-forms integer"
  (destructuring-bind (source expected) (list "(the integer 42)" 42)
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "the-forms in-expression"
  (destructuring-bind (source expected) (list "(the integer (+ 1 2))" 3)
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "boolean-predicate-branches if-false-branch"
  (destructuring-bind (expected source) (list 20 "(if (= 1 2) 10 20)")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "boolean-predicate-branches cond-second-arm"
  (destructuring-bind (expected source) (list 42 "(cond ((= 1 2) 10) ((= 2 2) 42) (t 0))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "boolean-predicate-branches labels-base-case"
  (destructuring-bind (expected source) (list 1 "(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 0))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "if-condition-clears-tail-position-before-compiling"
  (let ((source "(IF 65 48 (LET ((V0 -83)) V0))"))
    (expect (is-compile-string source :target :vm) :to-be-truthy)
    (expect (is-compile-string source :target :x86_64) :to-be-truthy)))

;; ----------------------------------------------------------------------------
;; block/return-from tests
;; ----------------------------------------------------------------------------

(it-sequential "block-forms normal-return"
  (destructuring-bind (source expected) (list "(block foo 42)" 42)
    (expect (is-compile-string source :target :x86_64) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "block-forms early-return"
  (destructuring-bind (source expected) (list "(block foo (return-from foo 10) 20)" 10)
    (expect (is-compile-string source :target :x86_64) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "block-forms nested"
  (destructuring-bind (source expected) (list "(block outer (block inner (return-from inner 5) 10) 20)" 20)
    (expect (is-compile-string source :target :x86_64) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

;; ----------------------------------------------------------------------------
;; tagbody/go tests
;; ----------------------------------------------------------------------------

(it-sequential "tagbody-simple"
  (expect (is-compile-string "(tagbody start (print 1) (go end) middle (print 2) end (print 3))" :target :vm) :to-be-truthy))

(it-sequential "tagbody-preserves-following-body-forms"
  (expect (= 6 (run-string "(let ((acc 6)) (tagbody start (go end) end) acc)")) :to-be-truthy))

;; ----------------------------------------------------------------------------
;; catch/throw tests
;; ----------------------------------------------------------------------------

(it-sequential "compile-catch-forms basic"
  (destructuring-bind (expected source) (list 42 "(catch 'foo 42)")
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "compile-catch-forms with-throw"
  (destructuring-bind (expected source) (list 99 "(catch 'done (throw 'done 99) 42)")
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "compile-nested-catch inner-tag"
  (destructuring-bind (expected source) (list 10 "(catch 'outer (catch 'inner (throw 'inner 10)))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "compile-nested-catch outer-tag"
  (destructuring-bind (expected source) (list 20 "(catch 'outer (catch 'inner (throw 'outer 20)))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "compile-unwind-protect primary-value"
  (destructuring-bind (expected source) (list 42 "(unwind-protect 42 (print 0))")
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "compile-multiple-value-prog1 primary-value"
  (destructuring-bind (expected source) (list 42 "(multiple-value-prog1 42 (print 1) (print 2))")
    (expect (is-compile-string source :target :vm) :to-be-truthy) (expect (= expected (run-string source)) :to-be-truthy)))

;; ----------------------------------------------------------------------------
;; Run control flow tests
;; ----------------------------------------------------------------------------

(it-sequential "compile-catch-throw-evaluates-operands-once-in-order" (let ((source "(let ((n 0)) (catch (progn (setq n (+ (* 10 n) 1)) (quote done)) (throw (progn (setq n (+ (* 10 n) 2)) (quote done)) (progn (setq n (+ (* 10 n) 3)) n))))")) (expect (= 123 (run-string source)) :to-be-truthy)))

(it-sequential "compile-catch-throw-skips-forms-after-throw" (let ((source "(let ((n 0)) (catch (quote done) (throw (quote done) 7) (setq n 1)) n)")) (expect (= 0 (run-string source)) :to-be-truthy)))

(it-sequential "compile-catch-throw-generates-vm-handler-instructions" (let* ((result (compile-string "(catch (quote done) (throw (quote done) 91))" :target :vm)) (instructions (cl-cc/compile:compilation-result-vm-instructions result))) (expect (find-if (lambda (instruction) (typep instruction (quote cl-cc/vm::vm-establish-catch))) instructions) :to-be-truthy) (expect (find-if (lambda (instruction) (typep instruction (quote cl-cc/vm::vm-throw))) instructions) :to-be-truthy) (expect (member (quote cl-cc/ast:ast-catch) cl-cc/compile:*cps-compile-unsupported-ast-types*) :to-be-falsy) (expect (member (quote cl-cc/ast:ast-throw) cl-cc/compile:*cps-compile-unsupported-ast-types*) :to-be-falsy) (expect (member (quote cl-cc/ast:ast-unwind-protect) cl-cc/compile:*cps-compile-unsupported-ast-types*) :to-be-truthy) (expect (member (quote cl-cc/ast:ast-values) cl-cc/compile:*cps-compile-unsupported-ast-types*) :to-be-truthy)))

(it-sequential "compile-catch-does-not-catch-later-throw" (signals error (run-string "(progn (catch (quote done) 1) (throw (quote done) 2))")))

(defun run-control-flow-tests ()
  (cl-weave:run 'control-flow-tests :reporter :spec))
