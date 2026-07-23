;;;; tests/unit/type/parser-arrow-quantifier-tests.lisp — Arrow & Quantifier Parser Tests
;;;;
;;;; Tests for src/type/parser.lisp: arrow multiplicity (->1/->0), bang effects,
;;;; forall/exists/mu quantifiers, type-lambda, qualified types (=>), and graded modal (!1/!0/!W).
;;;; Suite: parser-suite (defined in parser-tests.lisp).

(in-package :cl-cc/test)


;;; ─── Arrow types: ->, ->1, ->0 ──────────────────────────────────────────

(it-sequential "parse-arrow-basic-cases pure"
  (destructuring-bind (form expected-param-count check-effects) (list `(,(intern "->" :cl-cc/type) fixnum string) 1 t)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal expected-param-count)
    (when check-effects
      (expect (type-effect-row-p (type-arrow-effects ty)) :to-be-truthy)))))

(it-sequential "parse-arrow-basic-cases multi-param"
  (destructuring-bind (form expected-param-count check-effects) (list `(,(intern "->" :cl-cc/type) fixnum string boolean) 2 nil)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (length (type-arrow-params ty)) :to-equal expected-param-count)
    (when check-effects
      (expect (type-effect-row-p (type-arrow-effects ty)) :to-be-truthy)))))

(it-sequential "parse-arrow-multiplicity linear"
  (destructuring-bind (arrow-sym expected-mult) (list (intern "->1" :cl-cc/type) :one)
    (let ((ty (cl-cc/type:parse-type-specifier `(,arrow-sym fixnum string))))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-arrow-mult ty) :to-be expected-mult))))

(it-sequential "parse-arrow-multiplicity erased"
  (destructuring-bind (arrow-sym expected-mult) (list (intern "->0" :cl-cc/type) :zero)
    (let ((ty (cl-cc/type:parse-type-specifier `(,arrow-sym fixnum string))))
    (expect (type-arrow-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-arrow-mult ty) :to-be expected-mult))))

(it-sequential "parse-type-specifier-malformed-errors arrow-too-few"
  (destructuring-bind (form) (list `(,(intern "->" :cl-cc/type) fixnum))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-type-specifier-malformed-errors refinement-no-pred"
  (destructuring-bind (form) (list '(refine fixnum))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-type-specifier-malformed-errors record-field-no-type"
  (destructuring-bind (form) (list '(record (x)))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-arrow-with-bang-effects"
  (let ((ty (cl-cc/type:parse-type-specifier
             `(,(intern "->" :cl-cc/type) fixnum string ,(intern "!" :cl-cc/type) io))))
    (expect (type-arrow-p ty) :to-be-truthy)
    (let ((eff (type-arrow-effects ty)))
      (expect (type-effect-row-p eff) :to-be-truthy)
      (expect (> (length (type-effect-row-effects eff)) 0) :to-be-truthy))))

;;; ─── Quantifiers: forall, exists, mu ─────────────────────────────────────

(it-sequential "parse-quantifier-binding-types forall"
  (destructuring-bind (form pred var-fn body-fn) (list '(forall a fixnum) #'type-forall-p #'type-forall-var #'cl-cc/type:type-forall-body)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (funcall pred ty) :to-be-truthy)
    (expect (type-var-p (funcall var-fn ty)) :to-be-truthy)
    (when body-fn
      (expect (type-equal-p type-int (funcall body-fn ty)) :to-be-truthy)))))

(it-sequential "parse-quantifier-binding-types exists"
  (destructuring-bind (form pred var-fn body-fn) (list '(exists a fixnum) #'type-exists-p #'cl-cc/type:type-exists-var nil)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (funcall pred ty) :to-be-truthy)
    (expect (type-var-p (funcall var-fn ty)) :to-be-truthy)
    (when body-fn
      (expect (type-equal-p type-int (funcall body-fn ty)) :to-be-truthy)))))

(it-sequential "parse-quantifier-binding-types mu"
  (destructuring-bind (form pred var-fn body-fn) (list '(mu a fixnum) #'type-mu-p #'cl-cc/type:type-mu-var nil)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (funcall pred ty) :to-be-truthy)
    (expect (type-var-p (funcall var-fn ty)) :to-be-truthy)
    (when body-fn
      (expect (type-equal-p type-int (funcall body-fn ty)) :to-be-truthy)))))

(it-sequential "parse-quantifier-arity-errors forall"
  (destructuring-bind (form) (list '(forall a))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-quantifier-arity-errors exists"
  (destructuring-bind (form) (list '(exists a))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-quantifier-arity-errors mu"
  (destructuring-bind (form) (list '(mu a))
    (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))))

(it-sequential "parse-type-lambda"
  (let ((ty (cl-cc/type:parse-type-specifier '(type-lambda a fixnum))))
    (expect (type-lambda-p ty) :to-be-truthy)
    (expect (type-var-p (type-lambda-var ty)) :to-be-truthy)
    (expect (type-equal-p type-int (type-lambda-body ty)) :to-be-truthy)))

;;; ─── Qualified types: => ─────────────────────────────────────────────────

(it-sequential "parse-qualified-type-cases valid"
  (destructuring-bind (form error-p) (list `(,(intern "=>" :cl-cc/type) (num fixnum) string) nil)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (type-qualified-p ty) :to-be-truthy)
        (expect (length (type-qualified-constraints ty)) :to-equal 1)
        (expect (type-equal-p type-string (cl-cc/type:type-qualified-body ty)) :to-be-truthy)))))

(it-sequential "parse-qualified-type-cases no-body"
  (destructuring-bind (form error-p) (list `(,(intern "=>" :cl-cc/type)) t)
    (if error-p
      (signals cl-cc/type:type-parse-error (cl-cc/type:parse-type-specifier form))
      (let ((ty (cl-cc/type:parse-type-specifier form)))
        (expect (type-qualified-p ty) :to-be-truthy)
        (expect (length (type-qualified-constraints ty)) :to-equal 1)
        (expect (type-equal-p type-string (cl-cc/type:type-qualified-body ty)) :to-be-truthy)))))

;;; ─── Graded modal types ─────────────────────────────────────────────────

(it-sequential "parse-graded-modal-types one"
  (destructuring-bind (form expected-grade) (list `(,(intern "!1" :cl-cc/type) fixnum) :one)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-linear-grade ty) :to-be expected-grade))))

(it-sequential "parse-graded-modal-types zero"
  (destructuring-bind (form expected-grade) (list `(,(intern "!0" :cl-cc/type) fixnum) :zero)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-linear-grade ty) :to-be expected-grade))))

(it-sequential "parse-graded-modal-types omega"
  (destructuring-bind (form expected-grade) (list `(,(intern "!W" :cl-cc/type) fixnum) :omega)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-linear-grade ty) :to-be expected-grade))))

(it-sequential "parse-graded-modal-types explicit"
  (destructuring-bind (form expected-grade) (list `(,(intern "!"  :cl-cc/type) 1 fixnum) :one)
    (let ((ty (cl-cc/type:parse-type-specifier form)))
    (expect (type-linear-p ty) :to-be-truthy)
    (expect (cl-cc/type:type-linear-grade ty) :to-be expected-grade))))
