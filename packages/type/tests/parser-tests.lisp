;;;; tests/unit/type/parser-tests.lisp — Type Parser Tests (primitive/compound/structural)
;;;;
;;;; Tests for src/type/parser.lisp:
;;;; parse-type-specifier, parse-primitive-type, parse-compound-type,
;;;; typed AST nodes, looks-like-type-specifier-p.
;;;; Arrow/quantifier/modal tests → parser-arrow-quantifier-tests.lisp.

(in-package :cl-cc/test)


;;; ─── parse-type-specifier: atoms ─────────────────────────────────────────

(it-sequential "parse-hole-type-specifiers question-mark"
  (destructuring-bind (spec) (list '?)
    (expect (cl-cc/type:type-error-p (cl-cc/type:parse-type-specifier spec)) :to-be-truthy)))

(it-sequential "parse-hole-type-specifiers underscore"
  (destructuring-bind (spec) (list '_)
    (expect (cl-cc/type:type-error-p (cl-cc/type:parse-type-specifier spec)) :to-be-truthy)))

(it-sequential "parse-option-type"
  (let ((ty (cl-cc/type:parse-type-specifier '(option string))))
    (expect (type-union-p ty) :to-be-truthy)
    (expect (length (type-union-types ty)) :to-equal 2)
    (expect (some (lambda (x) (type-equal-p x type-null)) (type-union-types ty)) :to-be-truthy)
    (expect (some (lambda (x) (type-equal-p x type-string)) (type-union-types ty)) :to-be-truthy)))

(it-sequential "looks-like-type-specifier-option"
  (expect (cl-cc/type:looks-like-type-specifier-p '(option fixnum)) :to-be-truthy))

(it-sequential "parse-primitive-symbols nil"
  (destructuring-bind (sym expected) (list nil type-null)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols fixnum"
  (destructuring-bind (sym expected) (list 'fixnum type-int)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols integer"
  (destructuring-bind (sym expected) (list 'integer type-int)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols string"
  (destructuring-bind (sym expected) (list 'string type-string)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols boolean"
  (destructuring-bind (sym expected) (list 'boolean type-bool)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols bool"
  (destructuring-bind (sym expected) (list 'bool type-bool)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols symbol"
  (destructuring-bind (sym expected) (list 'symbol type-symbol)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols character"
  (destructuring-bind (sym expected) (list 'character type-char)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols char"
  (destructuring-bind (sym expected) (list 'char type-char)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols t"
  (destructuring-bind (sym expected) (list 't type-any)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols top"
  (destructuring-bind (sym expected) (list 'top type-any)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-primitive-symbols cons"
  (destructuring-bind (sym expected) (list 'cons type-cons)
    (expect (type-equal-p expected (cl-cc/type:parse-type-specifier sym)) :to-be-truthy)))

(it-sequential "parse-unknown-symbol"
  (let ((ty (cl-cc/type:parse-type-specifier 'my-custom-type)))
    (expect (type-primitive-p ty) :to-be-truthy)
    (expect (type-primitive-name ty) :to-be 'my-custom-type)))

;;; ─── parse-type-specifier: union / intersection ──────────────────────────

(it-sequential "parse-set-type-ops or-two"
  (destructuring-bind (form pred accessor error-p) (list '(or fixnum string) #'type-union-p #'type-union-types nil)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (funcall pred ty) :to-be-truthy)
        (expect (length (funcall accessor ty)) :to-equal 2)))))

(it-sequential "parse-set-type-ops and-compatible"
  (destructuring-bind (form pred accessor error-p) (list '(and fixnum integer) #'type-intersection-p #'type-intersection-types nil)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (funcall pred ty) :to-be-truthy)
        (expect (length (funcall accessor ty)) :to-equal 2)))))

(it-sequential "parse-set-type-ops or-empty-error"
  (destructuring-bind (form pred accessor error-p) (list '(or) nil nil t)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (funcall pred ty) :to-be-truthy)
        (expect (length (funcall accessor ty)) :to-equal 2)))))

(it-sequential "parse-set-type-ops and-empty-error"
  (destructuring-bind (form pred accessor error-p) (list '(and) nil nil t)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (funcall pred ty) :to-be-truthy)
        (expect (length (funcall accessor ty)) :to-equal 2)))))

(it-sequential "parse-set-type-ops and-uninhabited-error"
  (destructuring-bind (form pred accessor error-p) (list '(and fixnum string) nil nil t)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (funcall pred ty) :to-be-truthy)
        (expect (length (funcall accessor ty)) :to-equal 2)))))

;;; ─── parse-type-specifier: function / values ──────────────────────────────

(it-sequential "parse-function-type-cases one-param"
  (destructuring-bind (form expected-nparams expected-ret) (list '(-> fixnum string) 1 type-string)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal expected-nparams)
    (expect (type-equal-p expected-ret (type-arrow-return ty)) :to-be-truthy))))

(it-sequential "parse-function-type-cases multi-param"
  (destructuring-bind (form expected-nparams expected-ret) (list '(-> fixnum string boolean) 2 type-bool)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal expected-nparams)
    (expect (type-equal-p expected-ret (type-arrow-return ty)) :to-be-truthy))))

(it-sequential "parse-type-specifier-wrong-arity-errors arrow-no-param-list"
  (destructuring-bind (form) (list '(-> fixnum))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-type-specifier-wrong-arity-errors list-two-args"
  (destructuring-bind (form) (list '(list fixnum string))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-product-type-forms"
  (let ((ty (cl-cc/type:parse-type-specifier '(values fixnum string))))
    (expect (type-product-p ty) :to-be-truthy)
    (expect (length (type-product-elems ty)) :to-equal 2)))

;;; ─── parse-type-specifier: list / vector / array ─────────────────────────

(it-sequential "parse-collection-type-apps list"
  (destructuring-bind (form expected-fun-name expected-arg) (list '(list fixnum) 'list type-int)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p expected-arg (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps vector"
  (destructuring-bind (form expected-fun-name expected-arg) (list '(vector fixnum) 'vector type-int)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p expected-arg (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps simple-vector"
  (destructuring-bind (form expected-fun-name expected-arg) (list '(simple-vector fixnum) 'vector type-int)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p expected-arg (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps array"
  (destructuring-bind (form expected-fun-name expected-arg) (list '(array string) 'array type-string)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p expected-arg (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps simple-array"
  (destructuring-bind (form expected-fun-name expected-arg) (list '(simple-array string) 'array type-string)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p expected-arg (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-cl-numeric-range-type-specifiers unsigned-byte"
  (destructuring-bind (form) (list '(unsigned-byte 64))
    (expect (type-equal-p type-int (cl-cc/type:parse-type-specifier form)) :to-be-truthy)))

(it-sequential "parse-cl-numeric-range-type-specifiers signed-byte"
  (destructuring-bind (form) (list '(signed-byte 32))
    (expect (type-equal-p type-int (cl-cc/type:parse-type-specifier form)) :to-be-truthy)))

(it-sequential "parse-cl-numeric-range-type-specifiers integer-range"
  (destructuring-bind (form) (list '(integer 0 *))
    (expect (type-equal-p type-int (cl-cc/type:parse-type-specifier form)) :to-be-truthy)))

(it-sequential "parse-cl-numeric-range-type-specifiers mod"
  (destructuring-bind (form) (list '(mod 256))
    (expect (type-equal-p type-int (cl-cc/type:parse-type-specifier form)) :to-be-truthy)))

(it-sequential "parse-collection-type-apps-with-dimensions vector-size"
  (destructuring-bind (form expected-fun-name) (list '(vector (unsigned-byte 8) 4) 'vector)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p type-int (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps-with-dimensions array-dims"
  (destructuring-bind (form expected-fun-name) (list '(array (unsigned-byte 8) (*)) 'array)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p type-int (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-collection-type-apps-with-dimensions simple-array"
  (destructuring-bind (form expected-fun-name) (list '(simple-array (unsigned-byte 8) (*)) 'array)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (expect (type-primitive-name (type-app-fun ty)) :to-be expected-fun-name)
    (expect (type-equal-p type-int (type-app-arg ty)) :to-be-truthy))))

(it-sequential "parse-ansi-function-type-specifier"
  (let ((ty (cl-cc/type:parse-type-specifier '(function (fixnum string) boolean))))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal 2)
    (expect (type-equal-p type-bool (type-arrow-return ty)) :to-be-truthy)))

(it-sequential "parse-compound-type-app-table-covers-five-aliases"
  (let ((table cl-cc/type::*parse-compound-type-app-table*))
    (expect (= 5 (length table)) :to-be-truthy)
    (expect (assoc 'list          table) :to-be-truthy)
    (expect (assoc 'vector        table) :to-be-truthy)
    (expect (assoc 'simple-vector table) :to-be-truthy)
    (expect (assoc 'array         table) :to-be-truthy)
    (expect (assoc 'simple-array  table) :to-be-truthy)))

(it-sequential "parse-compound-multi-arg-table-covers-or-and"
  (let ((table cl-cc/type::*parse-compound-multi-arg-table*))
    (expect (= 2 (length table)) :to-be-truthy)
    (expect (assoc 'or  table) :to-be-truthy)
    (expect (assoc 'and table) :to-be-truthy)))

(it-sequential "parser-graded-arrow-syntax linear-1"
  (destructuring-bind (form expected-mult) (list '(->1 fixnum boolean) :one)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p result) :to-be-truthy)
    (expect (type-arrow-mult result) :to-be expected-mult))))

(it-sequential "parser-graded-arrow-syntax erased-0"
  (destructuring-bind (form expected-mult) (list '(->0 fixnum boolean) :zero)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p result) :to-be-truthy)
    (expect (type-arrow-mult result) :to-be expected-mult))))

(it-sequential "parser-forall-body-keyword"
  (let* ((result (cl-cc/type:parse-type-specifier '(forall a (-> a a)))))
    (expect (type-forall-p result) :to-be-truthy)
    (expect (type-var-name (type-forall-var result)) :to-be 'a)
    (expect (type-arrow-p (type-forall-body result)) :to-be-truthy)))

(it-sequential "parser-bounded-forall-variable"
  (let* ((result (cl-cc/type:parse-type-specifier
                  '(forall (a extends number supertype-of fixnum) a)))
         (var (type-forall-var result)))
    (expect (type-forall-p result) :to-be-truthy)
    (expect (type-var-name var) :to-be 'a)
    (expect (type-equal-p (make-type-primitive :name 'number)
                               (cl-cc/type:type-var-upper-bound var)) :to-be-truthy)
    (expect (type-equal-p type-int (cl-cc/type:type-var-lower-bound var)) :to-be-truthy)))

(it-sequential "parser-quantified-types exists"
  (destructuring-bind (form pred-p get-var get-body body-pred-p) (list '(exists a (values string a)) #'type-exists-p #'type-exists-var #'type-exists-body #'type-product-p)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (funcall pred-p result) :to-be-truthy)
    (expect (type-var-name (funcall get-var result)) :to-be 'a)
    (expect (funcall body-pred-p (funcall get-body result)) :to-be-truthy))))

(it-sequential "parser-quantified-types mu"
  (destructuring-bind (form pred-p get-var get-body body-pred-p) (list '(mu a (or null (values int a))) #'type-mu-p #'type-mu-var #'type-mu-body #'type-union-p)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (funcall pred-p result) :to-be-truthy)
    (expect (type-var-name (funcall get-var result)) :to-be 'a)
    (expect (funcall body-pred-p (funcall get-body result)) :to-be-truthy))))

(it-sequential "parser-record closed"
  (destructuring-bind (form n-fields open-p) (list '(record (name string) (age fixnum)) 2 nil)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-record-p result) :to-be-truthy)
    (expect (= n-fields (length (type-record-fields result))) :to-be-truthy)
    (if open-p
        (expect (type-record-row-var result) :to-be-truthy)
        (expect (type-record-row-var result) :to-be-null)))))

(it-sequential "parser-record open"
  (destructuring-bind (form n-fields open-p) (list `(record (name string) ,(intern "|" :cl-cc/type) rho) 1 t)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-record-p result) :to-be-truthy)
    (expect (= n-fields (length (type-record-fields result))) :to-be-truthy)
    (if open-p
        (expect (type-record-row-var result) :to-be-truthy)
        (expect (type-record-row-var result) :to-be-null)))))

(it-sequential "parser-variant-syntax"
  (let ((result (cl-cc/type:parse-type-specifier '(variant (some fixnum) (none null)))))
    (expect (type-variant-p result) :to-be-truthy)
    (expect (= 2 (length (type-variant-cases result))) :to-be-truthy)
    (expect (type-variant-row-var result) :to-be-null)))

(it-sequential "parser-linear-modal-syntax linear-1"
  (destructuring-bind (form expected-grade) (list '(!1 fixnum) :one)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p result) :to-be-truthy)
    (expect (type-linear-grade result) :to-be expected-grade))))

(it-sequential "parser-linear-modal-syntax omega"
  (destructuring-bind (form expected-grade) (list '(!ω string) :omega)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p result) :to-be-truthy)
    (expect (type-linear-grade result) :to-be expected-grade))))

(it-sequential "parser-linear-modal-syntax erased-0"
  (destructuring-bind (form expected-grade) (list '(!0 boolean) :zero)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p result) :to-be-truthy)
    (expect (type-linear-grade result) :to-be expected-grade))))

(it-sequential "parser-refinement-syntax lambda-pred"
  (destructuring-bind (form expected-pred) (list '(refine fixnum (lambda (x) (> x 0))) t)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-refinement-p result) :to-be-truthy)
    (expect (type-equal-p type-int (type-refinement-base result)) :to-be-truthy)
    (if (eq expected-pred t)
        (expect (type-refinement-predicate result) :to-be-truthy)
        (expect (eq expected-pred
                          (cl-cc/type:type-refinement-predicate result)) :to-be-falsy)))))

(it-sequential "parser-refinement-syntax symbol-pred"
  (destructuring-bind (form expected-pred) (list '(refine fixnum positive-p) 'positive-p)
    (let ((result (cl-cc/type:parse-type-specifier form)))
    (expect (type-refinement-p result) :to-be-truthy)
    (expect (type-equal-p type-int (type-refinement-base result)) :to-be-truthy)
    (if (eq expected-pred t)
        (expect (type-refinement-predicate result) :to-be-truthy)
        (expect (eq expected-pred
                          (cl-cc/type:type-refinement-predicate result)) :to-be-falsy)))))
