(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defmacro %assert-failure-message-contains (message-form fragments &body body)
  `(handler-case
       (progn ,@body (assert-false t))
     (test-failure (c)
       (let ((message ,message-form)
             (fragments ,fragments))
         (dolist (fragment fragments)
           (assert-true (search fragment message)))))))

(defmacro assert-failure-message-contains (fragments &body body)
  "Assert that BODY signals TEST-FAILURE whose message includes FRAGMENTS."
  `(%assert-failure-message-contains (test-failure-message c) ,fragments ,@body))

(defmacro assert-failure-message-contains-upcase (fragments &body body)
  "Assert that BODY signals TEST-FAILURE whose uppercased message includes FRAGMENTS."
  `(%assert-failure-message-contains (string-upcase (test-failure-message c))
                                     ,fragments
                                     ,@body))

(defmacro assert-string-contains-all (haystack fragments)
  "Assert that HAYSTACK contains every fragment in FRAGMENTS."
  `(let ((haystack ,haystack)
         (fragments ,fragments))
     (dolist (fragment fragments)
       (assert-true (search fragment haystack)))))

(defmacro assert-string-contains-none (haystack fragments)
  "Assert that HAYSTACK contains none of the fragments in FRAGMENTS."
  `(let ((haystack ,haystack)
         (fragments ,fragments))
     (dolist (fragment fragments)
       (assert-false (search fragment haystack)))))

(deftest-each framework-meta-tree-map-cases
  "%tree-map applies leaf-fn to every atom, rebuilding cons structure unchanged."
  :cases (("atom-identity"    42                   :MAPPED)
          ("atom-transform"   'x                   :MAPPED)
          ("flat-list"        '(a b c)             '(:MAPPED :MAPPED :MAPPED))
          ("nested-list"      '(a (b c))           '(:MAPPED (:MAPPED :MAPPED)))
          ("dotted-pair"      '(a . b)             '(:MAPPED . :MAPPED)))
  (input expected)
  (assert-equal expected (%tree-map input (lambda (atom) (declare (ignore atom)) :MAPPED))))

(deftest framework-meta-substitute-symbol-rewrites-occurrences
  "%substitute-symbol rewrites all occurrences of a symbol without touching unrelated atoms."
  (assert-equal '(let ((y 1)) (+ y z))
                (%substitute-symbol '(let ((x 1)) (+ x z)) 'x 'y)))

(deftest framework-meta-substitute-constant-rewrites-occurrences
  "%substitute-constant rewrites all occurrences of a constant without touching other atoms."
  (assert-equal '(if (= x 1) 1 2)
                (%substitute-constant '(if (= x 0) 0 2) 0 1)))

(deftest-each framework-meta-negate-first-if-condition
  "%negate-first-if-condition wraps the test in NOT, handles nested IF, passes non-IF through."
  :cases (("simple-if"  '(if test then else)         '(if (not test) then else))
          ("nested-if"  '((if inner-test then else)) '((if (not inner-test) then else)))
          ("no-if"      '(+ 1 2)                     '(+ 1 2)))
  (input expected)
  (assert-equal expected (%negate-first-if-condition input)))

(deftest-each framework-meta-return-nil-body
  "%return-nil-body rewrites defun/defmethod body to nil; passes non-binding forms through."
  :cases (("defun-rewrites"     '(defun sample (x) (+ x 1))         '(defun sample (x) nil))
          ("defmethod-rewrites"  '(defmethod draw ((s shape)) (print s)) '(defmethod draw ((s shape)) nil))
          ("non-defun-passthrough" '(if test then else)               '(if test then else)))
  (input expected)
  (assert-equal expected (%return-nil-body input)))

(deftest-each framework-meta-apply-mutation-produces-mutants
  "Mutation operators emit concrete mutant forms for representative inputs."
  :cases (("arithmetic-swap" '(defun sample (x) (+ x 1)) :arithmetic-swap)
          ("condition-negate" '(if test then else) :condition-negate)
          ("boundary-shift" '(< x 10) :boundary-shift)
          ("constant-replace" '(list 0 1) :constant-replace)
          ("return-nil" '(defun sample (x) (+ x 1)) :return-nil))
  (form mutation-type)
  (assert-true (consp (%apply-mutation form mutation-type))))

(deftest framework-meta-read-all-forms-collects-top-level-forms
  "%read-all-forms parses a string into multiple top-level form plists."
  (let ((forms (%read-all-forms "(defun foo () 1) (defun bar () 2)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "FOO" (symbol-name (cadar forms)))
    (assert-eq 'defun (caadr forms))
    (assert-string= "BAR" (symbol-name (cadadr forms)))))

(deftest framework-meta-eval-form-safely-returns-t-or-nil
  "%eval-form-safely returns T for successful evaluation and NIL for signaled errors."
  (assert-true  (%eval-form-safely '(+ 1 2)))
  (assert-false (%eval-form-safely '(error "boom"))))

(deftest framework-meta-binary-assertion-failure-message-has-name-expected-actual
  "A failing assert-equal produces a message with the assertion name, expected, and actual values."
  (assert-failure-message-contains '("assert-equal failed" "expected: 1" "actual: 2")
    (assert-equal 1 2)))

(deftest framework-meta-binary-assertion-evaluates-each-operand-once
  "assert-equal evaluates both the expected and actual operands exactly once."
  (let ((calls 0))
    (handler-case
        (progn
          (assert-equal (progn (incf calls) 1)
                        (progn (incf calls) 2))
          (assert-false t))
      (test-failure ()
        (assert-= 2 calls)))))

(deftest framework-meta-unary-assertion-failure-message-has-name-and-actual
  "A failing assert-null produces a message with the assertion name and actual value."
  (assert-failure-message-contains '("assert-null failed" "actual: :NOT-NIL")
    (assert-null :not-nil)))

(deftest framework-meta-unary-assertion-macroexpands-through-assert-unary
  "assert-null macroexpands to %assert-unary with the null predicate."
  (let ((expanded (macroexpand-1 '(assert-null sample-form))))
    (assert-eq '%assert-unary (first expanded))
    (assert-eq 'null (second expanded))))

(deftest framework-meta-assert-run=-failure-message-is-readable
  "assert-run= produces a readable failure message including the expected value and source form."
  (assert-failure-message-contains '("assert-run=: expected 7" "(+ 1 2)")
    (assert-run= 7 "(+ 1 2)")))

(deftest framework-meta-assert-run=-macroexpands-through-with-run-string-assertion
  "assert-run= macroexpands to %with-run-string-assertion."
  (let ((expanded (macroexpand-1 '(assert-run= 7 "(+ 1 2)"))))
    (assert-eq '%with-run-string-assertion (first expanded))))

(deftest-each framework-meta-deftest-each-rejects-malformed-input
  "deftest-each rejects missing binding lists and mismatched case arity during macro expansion."
  :cases (("missing-binding-list"
           '(deftest-each malformed "doc" :cases (("case" 1))
              (assert-true t)))
          ("mismatched-case-arity"
           '(deftest-each malformed "doc" :cases (("case" 1 2))
              (x)
              (assert-true x))))
  (form)
  (assert-signals error
    (macroexpand-1 form)))

(deftest framework-meta-assert-run=-reports-host-errors-in-failure-message
  "assert-run= reports host-signaled errors as a readable TAP YAML failure."
  (flet ((run-string (expr) (declare (ignore expr)) (error "synthetic host failure")))
      (assert-failure-message-contains '("run-string signaled" "form: (ASSERT-RUN= 1 (boom))")
        (assert-run= 1 "(boom)"))))


(deftest framework-meta-assert-compiles-to-behavior
  "assert-compiles-to succeeds when the requested VM op is present in the compiled stream; gives readable failure when absent."
  (assert-compiles-to "(+ 1 2)" :contains 'vm-const)
    (assert-failure-message-contains '("assert-compiles-to" "VM-SUB")
      (assert-compiles-to "(+ 1 2)" :contains 'vm-sub)))

(deftest framework-meta-assert-evaluates-to-reports-mismatch
  "assert-evaluates-to emits a readable failure when runtime results differ."
    (assert-failure-message-contains '("assert-evaluates-to" "expected 99")
      (assert-evaluates-to "(+ 1 2)" 99)))

(deftest framework-meta-assert-run-string=-failure-on-non-string-result
  "assert-run-string= fails with a readable message when the result is not a string."
    (assert-failure-message-contains '("assert-run-string=")
      (assert-run-string= "3" "(+ 1 2)")))

(deftest framework-meta-assert-macro-expands-to-reports-form-and-mismatch
  "assert-macro-expands-to failure message includes the original form and mismatch detail."
    (assert-failure-message-contains '("assert-macro-expands-to" "(WHEN T 1)")
      (assert-macro-expands-to '(when t 1) '(if nil 1 nil))))

(deftest framework-meta-assert-infers-type-succeeds-for-fixnum
  "assert-infers-type passes when the inferred type matches fixnum."
  (assert-infers-type "42" fixnum))

(deftest framework-meta-assert-infers-type-failure-reports-mismatch
  "assert-infers-type failure message includes the assertion name and the expected type."
    (assert-failure-message-contains '("assert-infers-type" "STRING")
      (assert-infers-type "42" string)))

(deftest-each framework-meta-assertion-name-in-failure-message
  "Failing assertions include their own name in the failure message."
  :cases (("run-true"       (lambda () (assert-run-true "nil"))               "assert-run-true"       nil)
          ("run-false"      (lambda () (assert-run-false "42"))               "assert-run-false"      nil)
          ("run-signals"    (lambda () (assert-run-signals error "42"))       "assert-run-signals"    nil)
          ("output-contains" (lambda () (assert-output-contains "abcdef" "zzz")) "assert-output-contains" "zzz"))
  (thunk name-fragment extra-fragment)
  (assert-failure-message-contains (remove nil (list name-fragment extra-fragment))
    (funcall thunk)))

(deftest framework-meta-defmetamorphic-registers-relation
  "defmetamorphic adds a relation entry to *metamorphic-relations* with the given name."
  (let ((*metamorphic-relations* nil))
    (defmetamorphic test-relation
      :transform (lambda (expr) expr)
      :relation #'equal
      :applicable-when (lambda (expr) (declare (ignore expr)) t))
    (assert-= 1 (length *metamorphic-relations*))
    (assert-eq 'test-relation (getf (first *metamorphic-relations*) :name))))

(deftest framework-meta-mutant-killed-p-returns-true-for-eval-failure
  "%mutant-killed-p returns T when the mutant form signals an error during evaluation."
  (assert-true (%mutant-killed-p '(error "synthetic eval failure")
                                 'cl-cc-unit-suite)))

(deftest framework-meta-coverage-helpers-are-callable
  "enable-coverage and disable-coverage are callable; %print-coverage-report signals with readable message."
  (enable-coverage)
  (disable-coverage)
  (handler-bind ((warning #'muffle-warning))
    (handler-case
        (assert-equal nil (%print-coverage-report nil))
      (error (e)
        (assert-string-contains-all (princ-to-string e) '("Coverage report"))))))

(deftest framework-meta-assert-evaluates-to-stdlib-path
  "assert-evaluates-to supports the stdlib execution path through a shared high-level assertion."
  (assert-evaluates-to "(+ 1 2)" 3 :stdlib t))

(deftest framework-meta-with-reset-repl-state-restores-package
  "with-reset-repl-state restores the caller package after BODY mutates *package*."
  (let ((original-package *package*))
    (with-reset-repl-state
      (setf *package* (find-package :cl-cc/compile))
      (assert-eq (find-package :cl-cc/compile) *package*))
    (assert-eq original-package *package*)))

(deftest framework-meta-with-cleared-hash-table-restores-state
  "with-cleared-hash-table clears a hash table during BODY and restores it afterward."
  (let ((table (make-hash-table :test 'eq)))
    (setf (gethash 'a table) 1
          (gethash 'b table) 2)
    (with-cleared-hash-table (table)
      (assert-= 0 (hash-table-count table))
      (setf (gethash 'c table) 3))
    (assert-= 2 (hash-table-count table))
    (assert-= 1 (gethash 'a table))
    (assert-= 2 (gethash 'b table))
    (assert-null (gethash 'c table))))

(deftest framework-meta-deftest-compile-stdlib-expands-to-shared-assertion
  "deftest-compile stdlib cases macroexpand into assert-evaluates-to instead of duplicating boilerplate."
  (let ((expanded (macroexpand-1
                   '(deftest-compile sample "doc"
                      :cases (("basic" 3 "(+ 1 2)"))
                      :stdlib t))))
    (assert-eq 'progn (car expanded))
    (assert-string-contains-all (prin1-to-string expanded) '("ASSERT-EVALUATES-TO"))))
