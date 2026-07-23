;;;; tests/unit/type/parser-typed-tests.lisp — Type Parser Tests (Row / Typed Parameters)
;;;;
;;;; Continuation of parser-tests.lisp:
;;;; Row types (record/variant), type-app fallback, constraint spec parsing,
;;;; lambda-list parsing, typed parameters, typed AST nodes,
;;;; looks-like-type-specifier-p.

(in-package :cl-cc/test)


(defmacro assert-type-expected-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── Row types: Record / Variant ─────────────────────────────────────────

(it-sequential "parse-record-closed"
  (let ((ty (cl-cc/type:parse-type-specifier '(record (x fixnum) (y string)))))
    (expect (type-record-p ty) :to-be-truthy)
    (expect (length (type-record-fields ty)) :to-equal 2)
    (expect (cl-cc/type:type-record-row-var ty) :to-be-null)))

(it-sequential "parse-record-open"
  (let ((ty (cl-cc/type:parse-type-specifier
             `(record (x fixnum) ,(intern "|" :cl-cc/type) rho))))
    (expect (type-record-p ty) :to-be-truthy)
    (expect (length (type-record-fields ty)) :to-equal 1)
    (expect (not (null (cl-cc/type:type-record-row-var ty))) :to-be-truthy)))

(it-sequential "parse-variant-form"
  (let ((ty (cl-cc/type:parse-type-specifier '(variant (some fixnum) (none null)))))
    (expect (type-variant-p ty) :to-be-truthy)
    (expect (length (cl-cc/type:type-variant-cases ty)) :to-equal 2)))

;;; ─── Type application fallback ───────────────────────────────────────────

(it-sequential "parse-type-app-cases single-arg"
  (destructuring-bind (form expect-nested) (list '(maybe fixnum) nil)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (if expect-nested
        (expect (type-app-p (cl-cc/type:type-app-fun ty)) :to-be-truthy)
        (expect (type-primitive-p (cl-cc/type:type-app-fun ty)) :to-be-truthy)))))

(it-sequential "parse-type-app-cases multi-arg"
  (destructuring-bind (form expect-nested) (list '(f a b) t)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-app-p ty) :to-be-truthy)
    (if expect-nested
        (expect (type-app-p (cl-cc/type:type-app-fun ty)) :to-be-truthy)
        (expect (type-primitive-p (cl-cc/type:type-app-fun ty)) :to-be-truthy)))))

;;; ─── Constraint spec parsing ─────────────────────────────────────────────

(it-sequential "parse-constraint-spec-cases basic"
  (destructuring-bind (input expect-error) (list '(num fixnum) nil)
    (if expect-error
      (signals cl-cc/type:type-parse-error (cl-cc/type::parse-constraint-spec input))
      (let ((c (cl-cc/type::parse-constraint-spec input)))
        (expect (cl-cc/type:type-constraint-p c) :to-be-truthy)
        (expect (cl-cc/type:type-constraint-class-name c) :to-be 'num)))))

(it-sequential "parse-constraint-spec-cases error"
  (destructuring-bind (input expect-error) (list 'num t)
    (if expect-error
      (signals cl-cc/type:type-parse-error (cl-cc/type::parse-constraint-spec input))
      (let ((c (cl-cc/type::parse-constraint-spec input)))
        (expect (cl-cc/type:type-constraint-p c) :to-be-truthy)
        (expect (cl-cc/type:type-constraint-class-name c) :to-be 'num)))))

;;; ─── Lambda list parsing ─────────────────────────────────────────────────

(it-sequential "parse-lambda-list-typed"
  (multiple-value-bind (names types)
      (cl-cc/type:parse-lambda-list-with-types '((x fixnum) (y string)))
    (expect names :to-equal '(x y))
    (expect (length types) :to-equal 2)
    (expect (type-equal-p type-int (first types)) :to-be-truthy)
    (expect (type-equal-p type-string (second types)) :to-be-truthy)))

(it-sequential "parse-lambda-list-untyped"
  (multiple-value-bind (names types)
      (cl-cc/type:parse-lambda-list-with-types '(x y))
    (expect names :to-equal '(x y))
    (expect (type-equal-p type-any (first types)) :to-be-truthy)
    (expect (type-equal-p type-any (second types)) :to-be-truthy)))

(it-sequential "parse-lambda-list-mixed"
  (multiple-value-bind (names types)
      (cl-cc/type:parse-lambda-list-with-types '((x fixnum) y))
    (expect names :to-equal '(x y))
    (expect (type-equal-p type-int (first types)) :to-be-truthy)
    (expect (type-equal-p type-any (second types)) :to-be-truthy)))

(it-sequential "parse-lambda-list-empty"
  (multiple-value-bind (names types)
      (cl-cc/type:parse-lambda-list-with-types nil)
    (expect names :to-be-null)
    (expect types :to-be-null)))

(it-sequential "parse-lambda-list-error"
  (signals cl-cc/type:type-parse-error (cl-cc/type:parse-lambda-list-with-types '(42))))

;;; ─── parse-typed-parameter ───────────────────────────────────────────────

(it-sequential "parse-typed-parameter-cases typed"
  (destructuring-bind (param expected-type) (list '(x fixnum) type-int)
    (let ((result (cl-cc/type:parse-typed-parameter param)))
    (expect (car result) :to-be 'x)
    (expect (type-equal-p expected-type (cdr result)) :to-be-truthy))))

(it-sequential "parse-typed-parameter-cases bare"
  (destructuring-bind (param expected-type) (list 'x type-any)
    (let ((result (cl-cc/type:parse-typed-parameter param)))
    (expect (car result) :to-be 'x)
    (expect (type-equal-p expected-type (cdr result)) :to-be-truthy))))

;;; ─── parse-typed-optional-parameter ──────────────────────────────────────

(it-sequential "parse-optional-parameter-cases typed"
  (destructuring-bind (param expected-type) (list '(x fixnum nil) type-int)
    (let ((result (cl-cc/type:parse-typed-optional-parameter param)))
    (expect (car result) :to-be 'x)
    (expect (type-equal-p expected-type (cdr result)) :to-be-truthy))))

(it-sequential "parse-optional-parameter-cases bare"
  (destructuring-bind (param expected-type) (list 'x type-any)
    (let ((result (cl-cc/type:parse-typed-optional-parameter param)))
    (expect (car result) :to-be 'x)
    (expect (type-equal-p expected-type (cdr result)) :to-be-truthy))))

;;; ─── extract-return-type ─────────────────────────────────────────────────

(it-sequential "extract-return-type-cases with-declare"
  (destructuring-bind (expected body) (list t '((declare (return-type fixnum)) (+ x 1)))
    (assert-type-expected-case expected
    (expect (type-equal-p type-int (cl-cc/type:extract-return-type body)) :to-be-truthy)
    (expect (cl-cc/type:extract-return-type body) :to-be-null))))

(it-sequential "extract-return-type-cases no-declare"
  (destructuring-bind (expected body) (list nil '((+ x 1)))
    (assert-type-expected-case expected
    (expect (type-equal-p type-int (cl-cc/type:extract-return-type body)) :to-be-truthy)
    (expect (cl-cc/type:extract-return-type body) :to-be-null))))

(it-sequential "extract-return-type-cases nil-body"
  (destructuring-bind (expected body) (list nil nil)
    (assert-type-expected-case expected
    (expect (type-equal-p type-int (cl-cc/type:extract-return-type body)) :to-be-truthy)
    (expect (cl-cc/type:extract-return-type body) :to-be-null))))

;;; ─── Typed AST nodes ─────────────────────────────────────────────────────

(it-sequential "parse-typed-defun-basic"
  (let ((node (cl-cc/type:parse-typed-defun '(defun foo ((x fixnum)) (+ x 1)))))
    (expect (cl-cc/type::ast-defun-typed-p node) :to-be-truthy)
    (expect (cl-cc/type:ast-defun-typed-name node) :to-be 'foo)
    (expect (cl-cc/type:ast-defun-typed-params node) :to-equal '(x))
    (expect (length (cl-cc/type:ast-defun-typed-param-types node)) :to-equal 1)
    (expect (type-equal-p type-int (first (cl-cc/type:ast-defun-typed-param-types node))) :to-be-truthy)))

(it-sequential "parse-typed-lambda-basic"
  (let ((node (cl-cc/type:parse-typed-lambda '(lambda ((x fixnum)) (+ x 1)))))
    (expect (cl-cc/type::ast-lambda-typed-p node) :to-be-truthy)
    (expect (cl-cc/type:ast-lambda-typed-params node) :to-equal '(x))
    (expect (length (cl-cc/type:ast-lambda-typed-param-types node)) :to-equal 1)))

;;; ─── looks-like-type-specifier-p ─────────────────────────────────────────

(it-sequential "looks-like-type-specifier-p-cases fixnum"
  (destructuring-bind (expected form) (list t 'fixnum)
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases string"
  (destructuring-bind (expected form) (list t 'string)
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases boolean"
  (destructuring-bind (expected form) (list t 'boolean)
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases question-mark"
  (destructuring-bind (expected form) (list t '?)
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases or-composite"
  (destructuring-bind (expected form) (list t '(or fixnum string))
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases function-type"
  (destructuring-bind (expected form) (list t '(-> fixnum string))
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases values-type"
  (destructuring-bind (expected form) (list t '(values fixnum string))
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

(it-sequential "looks-like-type-specifier-p-cases unknown-symbol"
  (destructuring-bind (expected form) (list nil 'my-random-thing)
    (assert-type-expected-case expected
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-truthy)
    (expect (cl-cc/type:looks-like-type-specifier-p form) :to-be-falsy))))

;;; ─── parse-type-specifier-maybe ──────────────────────────────────────────

(it-sequential "parse-type-specifier-maybe-cases known"
  (destructuring-bind (expected form) (list t 'fixnum)
    (assert-type-expected-case expected
    (expect (type-equal-p type-int (cl-cc/type::parse-type-specifier-maybe form)) :to-be-truthy)
    (expect (cl-cc/type::parse-type-specifier-maybe form) :to-be-null))))

(it-sequential "parse-type-specifier-maybe-cases unknown"
  (destructuring-bind (expected form) (list nil 'my-random-thing)
    (assert-type-expected-case expected
    (expect (type-equal-p type-int (cl-cc/type::parse-type-specifier-maybe form)) :to-be-truthy)
    (expect (cl-cc/type::parse-type-specifier-maybe form) :to-be-null))))

;;; ─── make-type-arrow ─────────────────────────────────────────────────────

(it-sequential "make-type-arrow-basic"
  (let ((ty (cl-cc/type:make-type-arrow (list type-int) type-string)))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal 1)
    (expect (type-equal-p type-string (type-arrow-return ty)) :to-be-truthy)))

;;; ─── Error on non-s-expression ───────────────────────────────────────────

(it-sequential "parse-invalid-atom"
  (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier 42)))
