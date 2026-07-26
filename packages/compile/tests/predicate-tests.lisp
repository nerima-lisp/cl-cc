;;;; tests/unit/expand/predicate-tests.lisp — Predicate tests

(in-package :cl-cc/test)

(defsuite predicate-suite
  :description "Predicate macro expansion tests"
  :parent cl-cc-integration-suite)

(in-suite predicate-suite)

;;; ─── Sequence-dispatch aware expansion accessors ────────────────────────
;;;
;;; The sequence predicates no longer expand straight into a list loop. When the
;;; sequence argument is not a literal, %SEQUENCE-DISPATCH-EXPAND (see
;;; packages/expand/src/macros-hof.lisp) wraps the four per-type bodies in
;;;
;;;   (let ((#:seq <sequence>)) (typecase #:seq (list ...) (string ...)
;;;                                             (vector ...) (otherwise ...)))
;;;
;;; so that these functions accept strings and vectors as ANSI requires, not
;;; just lists. The tests below still care about the shape of the *list* body,
;;; so they reach it through these two helpers instead of asserting on a raw
;;; CADDR that now names the dispatch form.

(defun predicate-expansion-list-body (form)
  "Return the list-sequence body of FORM's one-step macro expansion.

Peels the %SEQUENCE-DISPATCH-EXPAND wrapper when it is present, and returns the
expansion unchanged when the sequence argument was a literal and the macro
therefore selected a path at expansion time. Returns the body itself, so callers
assert on the same form they used to get before the dispatch wrapper existed."
  (let ((expansion (our-macroexpand-1 form)))
    (if (and (consp expansion)
             (eq (car expansion) 'let)
             (consp (caddr expansion))
             (eq (car (caddr expansion)) 'typecase))
        (second (assoc 'list (cddr (caddr expansion))))
        expansion)))

(deftest-each predicate-not-delegates-via-complement
  "Each -NOT predicate expands to the base form with (complement pred) as the first arg."
  :cases (("find-if-not"     'find-if     '(find-if-not pred lst))
          ("position-if-not" 'position-if '(position-if-not pred lst))
          ("count-if-not"    'count-if    '(count-if-not pred lst))
          ("rassoc-if-not"   'rassoc-if   '(rassoc-if-not pred alist))
          ("assoc-if-not"    'assoc-if    '(assoc-if-not pred alist)))
  (base-op form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq base-op (car result))
    (assert-eq 'complement (caadr result))))

(deftest-each find-if-not-runtime
  "FIND-IF-NOT returns first element not satisfying predicate; nil when all satisfy."
  :cases (("found"     "(find-if-not #'oddp '(1 3 4 5 6))"  4)
          ("not-found" "(find-if-not #'numberp '(1 2 3))"   nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest position-if-expansion-structure
  "POSITION-IF's list path binds the predicate in a LET whose body is BLOCK NIL,
so a match can RETURN without scanning the rest of the sequence."
  (let* ((list-body (predicate-expansion-list-body '(position-if pred lst)))
         (body      (caddr list-body)))
    (assert-eq 'let   (car list-body))
    (assert-eq 'block (car body))
    (assert-eq nil    (second body))))

(deftest-each position-if-runtime
  "POSITION-IF returns the 0-based index of first matching element; nil when not found."
  :cases (("found"     "(position-if #'evenp '(1 3 4 7 8))"  2)
          ("not-found" "(position-if #'evenp '(1 3 5))"       nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest-each position-if-not-runtime
  "POSITION-IF-NOT returns index of first element not satisfying predicate; nil when all satisfy."
  :cases (("found"     "(position-if-not #'oddp '(1 3 4 5))"  2)
          ("not-found" "(position-if-not #'oddp '(1 3 5))"    nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest-each count-if-not-runtime
  "COUNT-IF-NOT counts elements not satisfying predicate."
  :cases (("some"  "(count-if-not #'oddp '(1 2 3 4 5))"   2)
          ("empty" "(count-if-not #'numberp '())"           0))
  (form expected)
  (assert-= expected (run-string form)))

(deftest-each remove-if-key-expansion
  "REMOVE-IF and REMOVE-IF-NOT with :key bind the predicate, the key function and
the accumulator separately, so the key form is evaluated once rather than per element."
  :cases (("remove-if"     '(remove-if #'oddp lst :key #'car))
          ("remove-if-not" '(remove-if-not #'evenp lst :key #'car)))
  (form)
  (let ((list-body (predicate-expansion-list-body form)))
    (assert-eq 'let (car list-body))
    (assert-true (> (length (cadr list-body)) 1))))

(deftest find-if-not-with-key
  "FIND-IF-NOT with :key delegates to FIND-IF with complement."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (assert-eq (car result) 'find-if)))

(deftest position-if-with-key
  "POSITION-IF with :key binds the predicate, the key function and the index
separately, so the key form is evaluated once rather than per element."
  (let ((list-body (predicate-expansion-list-body '(position-if #'oddp lst :key #'car))))
    (assert-eq (car list-body) 'let)
    (assert-true (> (length (cadr list-body)) 2))))

(deftest count-if-not-with-key
  "COUNT-IF-NOT with :key delegates to COUNT-IF with complement."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq (car result) 'count-if)))

(deftest-each predicate-if-outer-is-let
  "RASSOC-IF and ASSOC-IF both expand to LET forms binding the predicate."
  :cases (("rassoc-if" '(rassoc-if pred alist))
          ("assoc-if"  '(assoc-if pred alist)))
  (form)
  (assert-eq 'let (car (our-macroexpand-1 form))))

(deftest rassoc-if-body-checks-cdr
  "RASSOC-IF body DOLIST applies predicate to (cdr pair)."
  (let* ((result (our-macroexpand-1 '(rassoc-if pred alist)))
         (dolist-form (caddr result))
         (when-form (second (cdr dolist-form)))
         (and-form (second when-form))
         (funcall-form (third and-form))
         (cdr-arg (caddr funcall-form)))
    (assert-eq (car dolist-form) 'dolist)
    (assert-eq (car funcall-form) 'funcall)
    (assert-eq (car cdr-arg) 'cdr)))


(deftest-each member-if-runtime
  "MEMBER-IF returns the tail starting at first satisfying element; nil when not found."
  :cases (("found"     "(member-if #'evenp '(1 3 4 5 6))"  '(4 5 6))
          ("not-found" "(member-if #'evenp '(1 3 5))"       nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each member-if-not-runtime
  "MEMBER-IF-NOT returns tail starting at first non-matching element; nil when all satisfy."
  :cases (("found"     "(member-if-not #'oddp '(1 3 4 5))"  '(4 5))
          ("not-found" "(member-if-not #'oddp '(1 3 5))"    nil))
  (form expected)
  (assert-equal expected (run-string form)))


(deftest assoc-if-body-is-dolist
  "ASSOC-IF scans an alist with a DOLIST (linear scan), not an index loop."
  (let* ((list-body (predicate-expansion-list-body '(assoc-if pred alist)))
         (body      (caddr list-body)))
    (assert-eq (car body) 'dolist)))


(deftest-each assoc-if-runtime
  "ASSOC-IF returns first pair whose car satisfies predicate; nil when not found."
  :cases (("found"     "(car (assoc-if #'evenp '((1 . 10) (2 . 20) (3 . 30))))"  2)
          ("not-found" "(assoc-if #'evenp '((1 . 10) (3 . 30)))"                  nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each assoc-if-not-runtime
  "ASSOC-IF-NOT returns first pair whose car does NOT satisfy predicate; nil when all satisfy."
  :cases (("found"     "(car (assoc-if-not #'evenp '((2 . 20) (3 . 30))))"  3)
          ("not-found" "(assoc-if-not #'evenp '((2 . 20) (4 . 40)))"         nil))
  (form expected)
  (assert-equal expected (run-string form)))

(deftest-each complement-expansion-structure
  "COMPLEMENT expands to LET+lambda whose body branches on APPLY with IF, so the
inversion follows the VM's truthiness instead of NOT's ANSI test for NIL — a
predicate reached as a function value answers 1/0, which NOT would read as true."
  :cases (("top-is-let"    'let    (lambda (r) (car r)))
          ("inner-lambda"  'lambda (lambda (r) (car (caddr r))))
          ("body-if-head"  'if     (lambda (r) (car (caddr (caddr r)))))
          ("apply-in-if"   'apply  (lambda (r) (car (cadr (caddr (caddr r)))))))
  (expected accessor)
  (assert-eq expected (funcall accessor (our-macroexpand-1 '(complement pred)))))
