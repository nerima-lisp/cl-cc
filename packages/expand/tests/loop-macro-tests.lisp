;;;; tests/unit/expand/loop-macro-tests.lisp — Unit tests for the LOOP macro
;;;;
;;;; Coverage targets:
;;;;   Expansion structure (block/let*/tagbody shape)
;;;;   FOR IN / ON / FROM / ACROSS / = / BEING HASH-KEYS / BEING HASH-VALUES
;;;;   WITH auxiliary binding
;;;;   Accumulation: COLLECT SUM COUNT MAXIMIZE MINIMIZE APPEND NCONC
;;;;   Filtering: WHEN IF UNLESS
;;;;   Control: WHILE UNTIL ALWAYS NEVER THEREIS
;;;;   REPEAT
;;;;   INITIALLY / FINALLY
;;;;   Destructuring in IN and ON
;;;;   Named accumulators (INTO)

(in-package :cl-cc/test)




;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Helper macros — raise the abstraction level for common test patterns.
;;;;
;;;; Design: each macro expands to exactly one deftest.  The description string
;;;; is the contract; the body is the minimal assertion.  Helper names follow:
;;;;   check-loop-equal  — result EQUAL expected
;;;;   check-loop-=      — result = expected  (numeric)
;;;;   check-loop-true   — result is truthy
;;;;   check-loop-false  — result is NIL
;;;;   check-loop-null   — result is NIL (empty accumulation)
;;;;   check-loop-length — (length result) = expected-length  (non-deterministic order)
;;;;   check-loop-expansion — structural test on the macro-expanded form
;;;;   check-loop-signals  — LOOP clause parsing signals an error
;;;;   check-loop-synonym-pair    — pair: canonical + -ING synonym, EQUAL
;;;;   check-loop-=-synonym-pair  — pair: canonical + -ING synonym, =
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmacro check-loop-equal (name description code expected)
  "LOOP runtime test: result is EQUAL to EXPECTED."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (run-string ,code) :to-equal ,expected)))

(defmacro check-loop-= (name description code expected)
  "LOOP runtime test: result is = (numeric) to EXPECTED."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (= ,expected (run-string ,code)) :to-be-truthy)))

(defmacro check-loop-true (name description code)
  "LOOP runtime test: result is truthy."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (run-string ,code) :to-be-truthy)))

(defmacro check-loop-false (name description code)
  "LOOP runtime test: result is NIL."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (run-string ,code) :to-be-falsy)))

(defmacro check-loop-null (name description code)
  "LOOP runtime test: result is NIL (empty list)."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (run-string ,code) :to-be-null)))

(defmacro check-loop-expansion (name description clauses &body assertions)
  "LOOP structural test: macro-expand (loop ,@CLAUSES) then evaluate ASSERTIONS on the result."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (let ((result (our-macroexpand-1 '(loop ,@clauses))))
       (declare (ignorable result))
       ,@assertions)))

(defmacro check-loop-length (name description code expected-length)
  "LOOP runtime test: result is a sequence of EXPECTED-LENGTH elements.
Useful for hash-table tests where element order is non-deterministic."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (expect (= ,expected-length (length (run-string ,code))) :to-be-truthy)))

(defmacro check-loop-signals (name description clauses)
  "LOOP structural test: parsing CLAUSES must signal an error condition.
Exercises error paths in the parser and emitter layers."
  (declare (ignore description))
  `(it-sequential ,(string-downcase (string name))
     (signals error (our-macroexpand-1 '(loop ,@clauses)))))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 1: Expansion structure
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-expansion loop-basic-expands-to-block
  "A bare loop expands to (block nil (let* (...) (tagbody ...)))"
  (do (return 42))
  (expect (consp result) :to-be-truthy)
  (expect (car result) :to-be 'block)
  (expect (cadr result) :to-be nil))

(check-loop-expansion loop-for-in-expands-to-let-tagbody
  "for x in list produces block → nil → let*"
  (for x in '(1 2 3) collect x)
  (expect (car result) :to-be 'block)
  (expect (cadr result) :to-be nil)
  (expect (car (caddr result)) :to-be 'let*))

(check-loop-expansion loop-from-to-expands-to-block
  "for i from 1 to 5 expands to block → nil → let*"
  (for i from 1 to 5 collect i)
  (expect (car result) :to-be 'block)
  (expect (car (caddr result)) :to-be 'let*))

(check-loop-expansion loop-repeat-has-counter-binding
  "repeat N expands with a counter binding in let*"
  (repeat 3 collect t)
  (expect (car result) :to-be 'block)
  (expect (consp (cadr (caddr result))) :to-be-truthy))

(check-loop-expansion loop-while-expands-to-block
  "while cond do ... expands to block"
  (while t do (return 1))
  (expect (car result) :to-be 'block))

(check-loop-expansion loop-initially-finally-expands
  "initially/finally clauses appear in expansion"
  (initially (print 'start) finally (print 'end) repeat 1)
  (expect (car result) :to-be 'block))

(check-loop-expansion loop-across-has-aref-and-length
  "for x across v expansion contains AREF and LENGTH"
  (for x across v collect x)
  (let ((s (format nil "~S" result)))
    (expect (search "AREF" s) :to-be-truthy)
    (expect (search "LENGTH" s) :to-be-truthy)))

(check-loop-expansion loop-across-has-four-bindings
  "for x across v expansion has at least 4 let* bindings (vec len idx var)"
  (for x across v collect x)
  (expect (>= (length (cadr (caddr result))) 4) :to-be-truthy))


;;;; Runtime LOOP behavior tests

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 2: FOR x IN list
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-for-in-collect-transform
  "for x in list collect (* x 2) doubles each element"
  "(loop for x in '(1 2 3) collect (* x 2))"
  '(2 4 6))

(check-loop-null loop-for-in-collect-empty
  "for x in empty list returns nil"
  "(loop for x in '() collect x)")

(check-loop-= loop-for-in-do-sum
  "for x in list do accumulates correctly"
  "(let ((s 0)) (loop for x in '(1 2 3) do (setq s (+ s x))) s)"
  6)

(check-loop-equal loop-for-in-by
  "for x in list by #'cddr steps by two"
  "(loop for x in '(1 2 3 4 5) by #'cddr collect x)"
  '(1 3 5))

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;; Section 3: FOR i FROM n [TO/BELOW m] [BY k]
;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(check-loop-equal loop-from-to-collect
  "for i from 1 to 5 collect i"
  "(loop for i from 1 to 5 collect i)"
  '(1 2 3 4 5))

(check-loop-equal loop-from-below-collect
  "for i from 0 below 4 collect i"
  "(loop for i from 0 below 4 collect i)"
  '(0 1 2 3))

(check-loop-equal loop-from-to-by-collect
  "for i from 0 to 10 by 3 collect i"
  "(loop for i from 0 to 10 by 3 collect i)"
  '(0 3 6 9))

(check-loop-= loop-from-to-sum
  "for i from 1 to 5 sum i = 15"
  "(loop for i from 1 to 5 sum i)"
  15)

(check-loop-null loop-from-empty-range
  "for i from 5 to 1 collects nothing"
  "(loop for i from 5 to 1 collect i)")

;;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
