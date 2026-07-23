;;;; tests/unit/expand/macros-stdlib-core-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)



(it-sequential "1+-1--expansion 1+"
  (destructuring-bind (form expected) (list '(1+ n) '(+ n 1))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "1+-1--expansion 1-"
  (destructuring-bind (form expected) (list '(1- n) '(- n 1))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "signum-expansion"
  (let* ((result (our-macroexpand-1 '(signum n)))
         (body   (caddr result)))
    (expect 'let :to-be (car result))
    (expect 'cond :to-be (car body))))

(it-sequential "return-expansion with-value"
  (destructuring-bind (form expected) (list '(return v) '(return-from nil v))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "return-expansion no-value"
  (destructuring-bind (form expected) (list '(return) '(return-from nil nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "fr-217-await-non-wasm-synchronous-fallback"
  (let ((cl-cc/expand::*target-backend* nil))
    (expect (our-macroexpand-1 '(await (compute-value))) :to-equal '(compute-value))))

(it-sequential "fr-217-async-handler-non-wasm-handler-case-fallback"
  (let ((cl-cc/expand::*target-backend* nil))
    (expect (our-macroexpand-1
                   '(async-handler (compute-value)
                      (error (e) e))) :to-equal '(handler-case (cl-cc/expand::await (compute-value))
                    (error (e) e)))))

(it-sequential "fr-217-await-wasm-lowers-through-js-await-intrinsic"
  (let ((cl-cc/expand::*target-backend* :wasm32))
    (let ((result (our-macroexpand-1 '(await (compute-promise)))))
      (expect (car result) :to-be 'let)
      (expect (%tree-contains-head-p 'cl-cc/expand::%wasm-promise-reference-p result) :to-be-truthy)
      (expect (%tree-contains-head-p 'cl-cc/expand::%wasm-js-await result) :to-be-truthy))))

(it-sequential "fr-217-async-handler-wasm-lowers-rejection-catch"
  (let ((cl-cc/expand::*target-backend* :wasm32))
    (let ((result (our-macroexpand-1
                   '(async-handler (compute-promise)
                      (error (e) :caught)))))
      (expect (car result) :to-be 'handler-case)
      (expect (%tree-contains-head-p 'cl-cc/expand::%wasm-js-catch result) :to-be-truthy)
      (expect (%tree-contains-head-p 'cl-cc/expand::await result) :to-be-truthy))))
