;;;; tests/pbt/macro-pbt-hygiene-tests.lisp — Idempotency, Hygiene, Uniqueness, Validity
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.
;;;
;;; Note: this is the same set of properties as macro-pbt-advanced-tests.lisp,
;;; distinguished only by the -hygiene-pbt name suffix. Both are migrated
;;; as-is rather than deduplicated.

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

;;; Property: Macro Expansion Idempotency

(cl-weave:describe "macro expansion idempotency properties (hygiene)"

  ;; These three iterate a fixed table of forms rather than generating any, so
  ;; they stay plain IT tests.
  (cl-weave:it "Fully expanding representative WHEN forms twice gives the same result."
    (dolist (form '((when t body)
                    (when flag body1 body2)
                    (when (= x 0) (print 1))))
      (let ((exp1 (cl-cc:our-macroexpand form)))
        (assert-equal exp1 (cl-cc:our-macroexpand exp1)))))

  (cl-weave:it "Fully expanding representative UNLESS forms twice gives the same result."
    (dolist (form '((unless t body)
                    (unless flag body1 body2)
                    (unless (= x 0) (print 1))))
      (let ((exp1 (cl-cc:our-macroexpand form)))
        (assert-equal exp1 (cl-cc:our-macroexpand exp1)))))

  (cl-weave:it "Fully expanding representative AND forms twice gives the same result."
    (dolist (form '((and a b)
                    (and a b c)
                    (and (= x 0) flag (print 1))))
      (let ((exp1 (cl-cc:our-macroexpand form)))
        (assert-equal exp1 (cl-cc:our-macroexpand exp1)))))

  (cl-weave:it-property "macroexpand-idempotent-or-hygiene-pbt"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 4)))
    (let ((exp1 (cl-cc:our-macroexpand `(or ,@args))))
      (cl-weave:expect (cl-cc:our-macroexpand exp1) :to-equal exp1)))

  (cl-weave:it-property "macroexpand-idempotent-let-star-hygiene-pbt"
      ((bindings (gen-binding-list :min-length 1 :max-length 3))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 2)))
    (let ((exp1 (cl-cc:our-macroexpand `(let* ,bindings ,@body))))
      (cl-weave:expect (cl-cc:our-macroexpand exp1) :to-equal exp1))))

;;; Property: Macro Hygiene (Gensym Usage)

(cl-weave:describe "macro hygiene properties (hygiene)"

  (cl-weave:it-property "prog1-hygiene-hygiene-pbt"
      ((first-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy)))

  (cl-weave:it-property "prog2-hygiene-hygiene-pbt"
      ((first-form (gen-body-form))
       (second-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy)))

  (cl-weave:it-property "or-hygiene-hygiene-pbt"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 4)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(or ,@args))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy)))

  (cl-weave:it-property "psetq-hygiene-hygiene-pbt"
      ((pairs (cl-weave:gen-list (gen-binding-pair) :min-length 1 :max-length 3)))
    (let* ((flat-pairs (apply #'append pairs))
           (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy)))

  (cl-weave:it-property "mvsq-hygiene-hygiene-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 3))
       (form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy)))

  (cl-weave:it-property "mvl-hygiene-hygiene-pbt"
      ((form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
      (cl-weave:expect (form-contains-gensym-p expanded) :to-be-truthy))))

;;; Property: Unique Gensym per Expansion

(cl-weave:describe "unique gensym per expansion properties (hygiene)"

  (cl-weave:it-property "prog1-unique-gensym-per-expansion-hygiene-pbt"
      ((first-form (gen-body-form))
       (body (gen-body-form)))
    (let* ((form `(prog1 ,first-form ,body))
           (exp1 (cl-cc:our-macroexpand-1 form))
           (exp2 (cl-cc:our-macroexpand-1 form)))
      (cl-weave:expect (eq (caar (second exp1)) (caar (second exp2))) :to-be-falsy)))

  (cl-weave:it-property "or-unique-gensym-per-expansion-hygiene-pbt"
      ((a (gen-body-form))
       (b (gen-body-form)))
    (let* ((form `(or ,a ,b))
           (exp1 (cl-cc:our-macroexpand-1 form))
           (exp2 (cl-cc:our-macroexpand-1 form)))
      (cl-weave:expect (eq (caar (second exp1)) (caar (second exp2))) :to-be-falsy))))

;;; Property: Expansion Structure Validity

(cl-weave:describe "expansion structure validity properties (hygiene)"

  (cl-weave:it-property "when-valid-lisp-form-hygiene-pbt"
      ((test (gen-test-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,@body))))
      (cl-weave:expect (car expanded) :to-satisfy #'symbolp)
      (cl-weave:expect (cdr expanded) :to-satisfy #'listp)))

  (cl-weave:it-property "and-valid-lisp-form-hygiene-pbt"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 5)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
      (cl-weave:expect (or (atom expanded)
                           (and (symbolp (car expanded))
                                (listp (cdr expanded))))
                       :to-be-truthy)))

  (cl-weave:it-property "let-star-valid-lisp-form-hygiene-pbt"
      ((bindings (gen-binding-list :min-length 0 :max-length 4))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,@body))))
      (cl-weave:expect (car expanded) :to-satisfy #'symbolp)
      (cl-weave:expect (cdr expanded) :to-satisfy #'listp))))

;;; Property: Macroexpand-All Recursiveness

(cl-weave:describe "macroexpand-all recursiveness properties (hygiene)"

  (cl-weave:it-property "macroexpand-all-reaches-all-subforms-hygiene-pbt"
      ((test (gen-test-form))
       (body1 (gen-body-form))
       (body2 (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(when (when ,test ,body1) ,body2))))
      (cl-weave:expect (form-contains-symbol-p 'when expanded) :to-be-falsy)))

  (cl-weave:it-property "macroexpand-all-preserves-structure-hygiene-pbt"
      ((x (cl-weave:gen-integer :min 1 :max 10))
       (y (cl-weave:gen-integer :min 1 :max 10)))
    (let ((expanded (cl-cc:our-macroexpand-all `(+ ,x (* ,y 2)))))
      (cl-weave:expect (car expanded) :to-be '+)
      (cl-weave:expect (second expanded) :to-equal x))))

;;; Property: Environment Interaction

(cl-weave:describe "macroexpand environment properties (hygiene)"

  (cl-weave:it-property "macroexpand-ignores-nil-env-hygiene-pbt"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((form `(when ,test ,body)))
      (cl-weave:expect (cl-cc:our-macroexpand-1 form nil)
                       :to-equal (cl-cc:our-macroexpand-1 form))))

  (cl-weave:it-property "macroexpand-env-optional-hygiene-pbt"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((form `(when ,test ,body)))
      (cl-weave:expect (cl-cc:our-macroexpand-1 form)
                       :to-equal (cl-cc:our-macroexpand-1 form nil)))))
