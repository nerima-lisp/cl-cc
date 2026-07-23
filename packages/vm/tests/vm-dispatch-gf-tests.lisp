;;;; tests/unit/vm/vm-dispatch-gf-tests.lisp
;;;; Unit tests for EQL specializer helpers in src/vm/vm-dispatch-gf.lisp.
;;;;
;;;; Covers: %eql-specializer-p, %eql-specializer-matches-p,
;;;;   %vm-extract-eql-specializer-keys.
;;;;
;;;; vm-classify-arg and vm-generic-function-p are already covered in
;;;; vm-dispatch-tests.lisp.

(in-package :cl-cc/test)

;;; ─── %eql-specializer-p ──────────────────────────────────────────────────

(it-sequential "eql-specializer-p-true-for-eql-forms eql-integer"
  (destructuring-bind (form) (list '(eql 42))
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-truthy)))

(it-sequential "eql-specializer-p-true-for-eql-forms eql-symbol"
  (destructuring-bind (form) (list '(eql foo))
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-truthy)))

(it-sequential "eql-specializer-p-true-for-eql-forms eql-nil"
  (destructuring-bind (form) (list '(eql nil))
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-truthy)))

(it-sequential "eql-specializer-p-false-for-non-eql symbol"
  (destructuring-bind (form) (list 'integer)
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-falsy)))

(it-sequential "eql-specializer-p-false-for-non-eql integer"
  (destructuring-bind (form) (list 42)
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-falsy)))

(it-sequential "eql-specializer-p-false-for-non-eql other-list"
  (destructuring-bind (form) (list '(and integer))
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-falsy)))

(it-sequential "eql-specializer-p-false-for-non-eql nil"
  (destructuring-bind (form) (list nil)
    (expect (cl-cc/vm::%eql-specializer-p form) :to-be-falsy)))

;;; ─── %eql-specializer-matches-p ──────────────────────────────────────────

(it-sequential "eql-specializer-matches-p-returns-true-for-exact-match"
  (expect (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 42) :to-be-truthy)
  (expect (cl-cc/vm::%eql-specializer-matches-p '(eql foo) 'foo) :to-be-truthy))

(it-sequential "eql-specializer-matches-p-returns-false-for-different-value"
  (expect (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 99) :to-be-falsy))

(it-sequential "eql-specializer-matches-p-returns-false-for-non-eql-form"
  (expect (cl-cc/vm::%eql-specializer-matches-p 'integer 42) :to-be-falsy))

;;; ─── %vm-extract-eql-specializer-keys ───────────────────────────────────

(it-sequential "vm-extract-eql-specializer-keys-bare-eql-form-returns-list"
  (expect (cl-cc/vm::%vm-extract-eql-specializer-keys '(eql 42)) :to-equal '(42)))

(it-sequential "vm-extract-eql-specializer-keys-nested-eql-form-returns-list"
  (expect (cl-cc/vm::%vm-extract-eql-specializer-keys '((eql foo))) :to-equal '(foo)))

(it-sequential "vm-extract-eql-specializer-keys-symbol-returns-nil"
  (expect (cl-cc/vm::%vm-extract-eql-specializer-keys 'integer) :to-be-null))

(it-sequential "vm-extract-eql-specializer-keys-multi-specializer-list-returns-nil"
  (expect (cl-cc/vm::%vm-extract-eql-specializer-keys '(integer string)) :to-be-null))

;;; ─── *method-combination-operators* data table ───────────────────────────

(it-sequential "method-combination-operators-all-known-combinations plus"
  (destructuring-bind (combo) (list '+)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations times"
  (destructuring-bind (combo) (list '*)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations list"
  (destructuring-bind (combo) (list 'list)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations append"
  (destructuring-bind (combo) (list 'append)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations nconc"
  (destructuring-bind (combo) (list 'nconc)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations max"
  (destructuring-bind (combo) (list 'max)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations min"
  (destructuring-bind (combo) (list 'min)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations and"
  (destructuring-bind (combo) (list 'and)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations or"
  (destructuring-bind (combo) (list 'or)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

(it-sequential "method-combination-operators-all-known-combinations progn"
  (destructuring-bind (combo) (list 'progn)
    (expect (assoc combo cl-cc/vm::*method-combination-operators*) :to-be-truthy)))

;;; ─── %resolve-combination-operator ──────────────────────────────────────

(it-sequential "resolve-combination-operator-numeric-folds plus-folds-to-sum"
  (destructuring-bind (combo args expected) (list '+ '(1 2 3) 6)
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-numeric-folds times-folds-to-product"
  (destructuring-bind (combo args expected) (list '* '(2 3 4) 24)
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-numeric-folds max-picks-largest"
  (destructuring-bind (combo args expected) (list 'max '(1 7 3) 7)
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-numeric-folds min-picks-smallest"
  (destructuring-bind (combo args expected) (list 'min '(5 2 9) 2)
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-collection-folds list-collects"
  (destructuring-bind (combo args expected) (list 'list '(1 2 3) '(1 2 3))
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-collection-folds append-merges"
  (destructuring-bind (combo args expected) (list 'append '((a b) (c)) '(a b c))
    (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (expect (apply op args) :to-equal expected))))

(it-sequential "resolve-combination-operator-and-short-circuits-on-nil"
  (let ((and-op (cl-cc/vm::%resolve-combination-operator 'and)))
    (expect (funcall and-op 1 2 3) :to-be-truthy)
    (expect (funcall and-op 1 nil 3) :to-be-falsy)))

(it-sequential "resolve-combination-operator-or-stops-at-first-truthy"
  (let ((or-op (cl-cc/vm::%resolve-combination-operator 'or)))
    (expect (funcall or-op nil 2 nil) :to-be-truthy)
    (expect (funcall or-op nil nil nil) :to-be-falsy)))

(it-sequential "resolve-combination-operator-progn-returns-last-value"
  (let ((progn-op (cl-cc/vm::%resolve-combination-operator 'progn)))
    (expect (funcall progn-op 1 2 99) :to-equal 99)))

(it-sequential "resolve-combination-operator-unknown-signals-error"
  (signals error (cl-cc/vm::%resolve-combination-operator 'unknown-combo)))

;;; ─── %vm-dispatch-key-collect (extracted combination generator) ──────────

(it-sequential "vm-dispatch-key-collect-single-cpl"
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect '((a b c)) nil)))
    (expect (length result) :to-equal 3)
    (expect (member '(a) result :test #'equal) :to-be-truthy)
    (expect (member '(b) result :test #'equal) :to-be-truthy)
    (expect (member '(c) result :test #'equal) :to-be-truthy)))

(it-sequential "vm-dispatch-key-collect-two-cpls"
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect '((x y) (1 2)) nil)))
    (expect (= 4 (length result)) :to-be-truthy)
    (expect (member '(x 1) result :test #'equal) :to-be-truthy)
    (expect (member '(x 2) result :test #'equal) :to-be-truthy)
    (expect (member '(y 1) result :test #'equal) :to-be-truthy)
    (expect (member '(y 2) result :test #'equal) :to-be-truthy)))

(it-sequential "vm-dispatch-key-collect-empty-remaining"
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect nil '(b a))))
    (expect (length result) :to-equal 1)
    (expect (first result) :to-equal '(a b))))
