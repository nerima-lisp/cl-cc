;;;; tests/unit/expand/expander-basic-tests.lisp — Basic expander form tests

(in-package :cl-cc/test)



(it-sequential "expand-apply-named-fn-binary"
  (let ((result (cl-cc/expand::expand-apply-named-fn 'cons 'args)))
    (expect (car result) :to-be 'apply)
    (expect (caadr result) :to-be 'function)
    (expect (cadadr result) :to-be 'cons)
    (expect (third result) :to-be 'args)))

(it-sequential "expand-apply-named-fn-variadic-plus"
  (let ((result (cl-cc/expand::expand-apply-named-fn '+ 'args)))
    (expect (car result) :to-be 'apply)
    (expect (caadr result) :to-be 'function)
    (expect (cadadr result) :to-be '+)
    (expect (third result) :to-be 'args)))

(it-sequential "expand-apply-preserves-function-designator"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(apply #'+ '(1 2 3)))))
    (expect (car result) :to-be 'apply)
    (expect (caadr result) :to-be 'function)
    (expect (cadadr result) :to-be '+)
    (expect (third result) :to-equal '(quote (1 2 3)))))

(it-sequential "expand-apply-expands-dynamic-function-operand"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(apply (funcall 'identity #'+) '(1 2)))))
    (expect (car result) :to-be 'apply)
    (expect (caadr result) :to-be 'identity)))

(it-sequential "expander-function-builtin-wraps-lambda binary"
  (destructuring-bind (name expected-arity) (list 'cons 2)
    (let ((result (assert-expansion-head `(function ,name) 'lambda)))
    (when expected-arity
      (expect (length (second result)) :to-equal expected-arity)))))

(it-sequential "expander-function-builtin-wraps-lambda unary"
  (destructuring-bind (name expected-arity) (list 'car 1)
    (let ((result (assert-expansion-head `(function ,name) 'lambda)))
    (when expected-arity
      (expect (length (second result)) :to-equal expected-arity)))))

(it-sequential "expander-function-builtin-wraps-lambda variadic"
  (destructuring-bind (name expected-arity) (list '+ nil)
    (let ((result (assert-expansion-head `(function ,name) 'lambda)))
    (when expected-arity
      (expect (length (second result)) :to-equal expected-arity)))))

(it-sequential "expander-function-non-builtin-passthrough"
  (let ((result (assert-expansion-head '(function my-user-defined-fn) 'function)))
    (expect (second result) :to-be 'my-user-defined-fn)))

(it-sequential "expander-funcall-quoted-to-direct-call user-fn"
  (destructuring-bind (form expected-head expected-arg) (list '(funcall 'my-unique-test-fn x) 'my-unique-test-fn 'x)
    (let ((result (assert-expansion-head form expected-head)))
    (expect (second result) :to-be expected-arg))))

(it-sequential "expander-funcall-quoted-to-direct-call builtin"
  (destructuring-bind (form expected-head expected-arg) (list '(funcall 'car lst) 'car 'lst)
    (let ((result (assert-expansion-head form expected-head)))
    (expect (second result) :to-be expected-arg))))

(it-sequential "expander-make-hash-table-adjusts-test function"
  (destructuring-bind (form) (list '(make-hash-table :test #'equal))
    (let ((result (assert-expansion-head form 'cl-cc/vm::%make-hash-table-with-test)))
    (expect result :to-be-truthy))))

(it-sequential "expander-make-hash-table-adjusts-test quoted"
  (destructuring-bind (form) (list '(make-hash-table :test 'eql))
    (let ((result (assert-expansion-head form 'cl-cc/vm::%make-hash-table-with-test)))
    (expect result :to-be-truthy))))

(it-sequential "expander-format-literal-single-aesthetic-directive"
  (expect (cl-cc/expand:compiler-macroexpand-all
                 '(format nil "~A" value)) :to-equal '(princ-to-string value)))

(it-sequential "expander-format-literal-supported-directives"
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(format nil "x=~A n=~D~%~~" value count)))
         (printed (format nil "~S" result)))
    (expect (car result) :to-be 'cl-cc/expand::string-concat)
    (expect (search "STRING-CONCAT" printed) :to-be-truthy)
    (expect (search "PRINC-TO-STRING" printed) :to-be-truthy)
    (expect (search "WRITE-TO-STRING" printed) :to-be-truthy)
    (expect (search "*PRINT-BASE*" printed) :to-be-truthy)
    (expect (search (string #\Newline) printed) :to-be-truthy)
    (expect (member "~" result :test #'equal) :to-be-truthy)
    (expect (search "MAKE-STRING-OUTPUT-STREAM" printed) :to-be-falsy)))

(it-sequential "expander-format-literal-unsupported-directive-falls-back"
  (let* ((result (cl-cc/expand:compiler-macroexpand-all
                  '(format nil "~S" value)))
         (printed (format nil "~S" result)))
    (expect (car result) :to-be 'let)
    (expect (search "MAKE-STRING-OUTPUT-STREAM" printed) :to-be-truthy)
    (expect (search "FORMAT" printed) :to-be-truthy)))

(it-sequential "expander-format-literal-extra-args-fall-back"
  (let ((result (cl-cc/expand:compiler-macroexpand-all
                 '(format nil "literal" extra))))
    (expect (car result) :to-be 'let)))
