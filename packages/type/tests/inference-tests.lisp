;;;; tests/unit/type/inference-tests.lisp — Type Inference Registry + Predicate Tests
;;;
;;; Covers functions in src/type/inference.lisp:
;;; - Class/alias registries, type predicate mapping, type guard extraction
;;; - Union narrowing, constant effect table
;;; Inference forms (infer() on AST nodes) → inference-forms-tests.lisp

(in-package :cl-cc/test)

;;; ─── Class Type Registry ──────────────────────────────────────────────────

(it-sequential "infer-class-type-register-returns-slot-list"
  (let ((slots (list (cons 'x cl-cc/type:type-int)
                     (cons 'y cl-cc/type:type-string))))
    (cl-cc/type:register-class-type 'test-class-7891 slots)
    (let ((result (cl-cc/type:lookup-class-type 'test-class-7891)))
      (expect result :to-be-truthy)
      (expect (= 2 (length result)) :to-be-truthy))))

(it-sequential "infer-class-type-lookup-slot-finds-slot-type"
  (cl-cc/type:register-class-type 'test-class-7892
    (list (cons 'name cl-cc/type:type-string)
          (cons 'age cl-cc/type:type-int)))
  (let ((ty (cl-cc/type:lookup-slot-type 'test-class-7892 'age)))
    (expect (cl-cc/type:type-primitive-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum)))

(it-sequential "infer-class-type-lookup-nonexistent-slot-returns-nil"
  (cl-cc/type:register-class-type 'test-class-7893
    (list (cons 'x cl-cc/type:type-int)))
  (expect (cl-cc/type:lookup-slot-type 'test-class-7893 'nonexistent) :to-be-null))

(it-sequential "infer-class-type-lookup-unknown-class-returns-nil"
  (expect (cl-cc/type:lookup-class-type 'completely-unknown-class-xyz) :to-be-null))

;;; ─── Type Alias Registry / Predicate Registry ────────────────────────────

(it-sequential "infer-registry-alias-roundtrip"
  (cl-cc/type:register-type-alias 'test-alias-7891 '(or fixnum string))
  (expect (cl-cc/type:lookup-type-alias 'test-alias-7891) :to-equal '(or fixnum string))
  (expect (cl-cc/type:lookup-type-alias 'no-such-alias-xyz) :to-be-null))

(it-sequential "infer-registry-predicate-roundtrip"
  (cl-cc/type:register-type-predicate 'custom-pred-xyz-7891 cl-cc/type:type-int)
  (let ((ty (cl-cc/type::type-predicate-to-type 'custom-pred-xyz-7891)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be 'fixnum))
  (expect (cl-cc/type::type-predicate-to-type 'foobar-p) :to-be-null))

(it-sequential "infer-global-tables-are-hash-tables predicate-table"
  (destructuring-bind (table) (list cl-cc/type:*type-predicate-table*)
    (expect (hash-table-p table) :to-be-truthy)))

(it-sequential "infer-global-tables-are-hash-tables effect-table"
  (destructuring-bind (table) (list cl-cc/type:*constant-effect-table*)
    (expect (hash-table-p table) :to-be-truthy)))

;;; ─── Constant Effect Table ────────────────────────────────────────────────

(it-sequential "infer-constant-effect-table-entries int-pure"
  (destructuring-bind (ast-type expected-effect-name) (list 'cl-cc:ast-int nil)
    (let ((row (gethash ast-type cl-cc/type:*constant-effect-table*)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                         (cl-cc/type:type-effect-row-effects row))))
      (if expected-effect-name
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy)
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-falsy))))))

(it-sequential "infer-constant-effect-table-entries print-io"
  (destructuring-bind (ast-type expected-effect-name) (list 'cl-cc:ast-print "IO")
    (let ((row (gethash ast-type cl-cc/type:*constant-effect-table*)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                         (cl-cc/type:type-effect-row-effects row))))
      (if expected-effect-name
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy)
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-falsy))))))

(it-sequential "infer-constant-effect-table-entries setq-state"
  (destructuring-bind (ast-type expected-effect-name) (list 'cl-cc:ast-setq "STATE")
    (let ((row (gethash ast-type cl-cc/type:*constant-effect-table*)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                         (cl-cc/type:type-effect-row-effects row))))
      (if expected-effect-name
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-truthy)
          (expect (member expected-effect-name names :key #'symbol-name :test #'string=) :to-be-falsy))))))

;;; ─── type-predicate-to-type ───────────────────────────────────────────────

(it-sequential "infer-predicate-to-type-known numberp"
  (destructuring-bind (pred expected-name) (list 'numberp 'fixnum)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known integerp"
  (destructuring-bind (pred expected-name) (list 'integerp 'fixnum)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known stringp"
  (destructuring-bind (pred expected-name) (list 'stringp 'string)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known symbolp"
  (destructuring-bind (pred expected-name) (list 'symbolp 'symbol)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known consp"
  (destructuring-bind (pred expected-name) (list 'consp 'cons)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known null"
  (destructuring-bind (pred expected-name) (list 'null 'null)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known characterp"
  (destructuring-bind (pred expected-name) (list 'characterp 'character)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

(it-sequential "infer-predicate-to-type-known functionp"
  (destructuring-bind (pred expected-name) (list 'functionp 'function)
    (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (expect ty :to-be-truthy)
    (expect (cl-cc/type:type-primitive-name ty) :to-be expected-name))))

;;; ─── extract-type-guard ───────────────────────────────────────────────────

(it-sequential "infer-extract-type-guard-known-predicates numberp"
  (destructuring-bind (form expected-var expected-type-name) (list '(numberp x) 'x 'fixnum)
    (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (var-name guard-type)
        (cl-cc/type:extract-type-guard ast)
      (expect var-name :to-be expected-var)
      (expect (cl-cc/type:type-primitive-name guard-type) :to-be expected-type-name)))))

(it-sequential "infer-extract-type-guard-known-predicates typep"
  (destructuring-bind (form expected-var expected-type-name) (list '(typep x 'my-class) 'x 'my-class)
    (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (var-name guard-type)
        (cl-cc/type:extract-type-guard ast)
      (expect var-name :to-be expected-var)
      (expect (cl-cc/type:type-primitive-name guard-type) :to-be expected-type-name)))))

(it-sequential "infer-extract-type-guard-returns-nil non-predicate-call"
  (destructuring-bind (form) (list '(foo x))
    (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (v g) (cl-cc/type:extract-type-guard ast)
      (expect v :to-be-null)
      (expect g :to-be-null)))))

(it-sequential "infer-extract-type-guard-returns-nil non-call-integer"
  (destructuring-bind (form) (list '42)
    (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (v g) (cl-cc/type:extract-type-guard ast)
      (expect v :to-be-null)
      (expect g :to-be-null)))))

;;; ─── narrow-union-type ────────────────────────────────────────────────────

(it-sequential "infer-narrow-union-cases removes-member"
  (destructuring-bind (union-type keep-type pred) (list (cl-cc/type:make-type-union
                               (list cl-cc/type:type-int cl-cc/type:type-string cl-cc/type:type-symbol)) cl-cc/type:type-int (lambda (r) (and (cl-cc/type:type-union-p r)
                                               (= 2 (length (cl-cc/type:type-union-types r))))))
    (let ((result (cl-cc/type:narrow-union-type union-type keep-type)))
    (expect (funcall pred result) :to-be-truthy))))

(it-sequential "infer-narrow-union-cases collapses-to-single"
  (destructuring-bind (union-type keep-type pred) (list (cl-cc/type:make-type-union
                                  (list cl-cc/type:type-int cl-cc/type:type-string)) cl-cc/type:type-int (lambda (r) (and (cl-cc/type:type-primitive-p r)
                                                  (eq 'string (cl-cc/type:type-primitive-name r)))))
    (let ((result (cl-cc/type:narrow-union-type union-type keep-type)))
    (expect (funcall pred result) :to-be-truthy))))

(it-sequential "infer-narrow-union-cases non-union-passthrough"
  (destructuring-bind (union-type keep-type pred) (list cl-cc/type:type-int cl-cc/type:type-string (lambda (r) (eq 'fixnum (cl-cc/type:type-primitive-name r))))
    (let ((result (cl-cc/type:narrow-union-type union-type keep-type)))
    (expect (funcall pred result) :to-be-truthy))))
