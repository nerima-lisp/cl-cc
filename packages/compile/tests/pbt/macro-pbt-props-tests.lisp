;;;; tests/pbt/macro-pbt-props-tests.lisp — Macro Expansion Properties
;;;
;;; Expressed with cl-weave's NATIVE property API. Each body asserts through
;;; CL-WEAVE:EXPECT rather than returning a boolean: IT-PROPERTY decides
;;; pass/fail from a *signaled* condition (RUN-PROPERTY wraps the body in
;;; PROPERTY-FAILURE-CONDITION, which only catches ERROR) and discards the
;;; return value, so a bare boolean body would report PASS even when false.

(in-package :cl-cc/pbt)


;;; Property: WHEN Macro Expansion

(cl-weave:describe "WHEN macro expansion properties"

  (cl-weave:it-property "when-expands-to-if-with-progn"
      ((test (gen-test-form))
       (body1 (gen-body-form))
       (body2 (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body1 ,body2))))
      (cl-weave:expect (car expanded) :to-be 'if)
      (cl-weave:expect expanded :to-have-length 4)
      (cl-weave:expect (car (third expanded)) :to-be 'progn)
      (cl-weave:expect (fourth expanded) :to-be-null)))

  (cl-weave:it-property "when-single-body-still-has-progn"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body))))
      (cl-weave:expect (car expanded) :to-be 'if)
      (cl-weave:expect (car (third expanded)) :to-be 'progn)))

  (cl-weave:it-property "when-preserves-test-form"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test ,body))))
      (cl-weave:expect (second expanded) :to-equal test)))

  (cl-weave:it-property "when-no-body-still-valid"
      ((test (gen-test-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(when ,test))))
      (cl-weave:expect (car expanded) :to-be 'if)
      (cl-weave:expect (fourth expanded) :to-be-null))))

;;; Property: UNLESS Macro Expansion

(cl-weave:describe "UNLESS macro expansion properties"

  (cl-weave:it-property "unless-expands-to-if-with-progn"
      ((test (gen-test-form))
       (body1 (gen-body-form))
       (body2 (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(unless ,test ,body1 ,body2))))
      (cl-weave:expect (car expanded) :to-be 'if)
      (cl-weave:expect expanded :to-have-length 4)
      (cl-weave:expect (third expanded) :to-be-null)
      (cl-weave:expect (car (fourth expanded)) :to-be 'progn)))

  (cl-weave:it-property "unless-preserves-test-form"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(unless ,test ,body))))
      (cl-weave:expect (second expanded) :to-equal test)))

  (cl-weave:it-property "unless-swaps-branch-order"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((unless-exp (cl-cc:our-macroexpand-1 `(unless ,test ,body)))
          (when-exp (cl-cc:our-macroexpand-1 `(when ,test ,body))))
      (cl-weave:expect (second unless-exp) :to-equal (second when-exp))
      (cl-weave:expect (third unless-exp) :to-be-null)
      (cl-weave:expect (fourth when-exp) :to-be-null))))

;;; Property: COND Macro Expansion

(cl-weave:describe "COND macro expansion properties"

  (cl-weave:it "cond-empty-returns-nil"
    (cl-weave:expect (cl-cc:our-macroexpand-1 '(cond)) :to-be-null))

  (cl-weave:it-property "cond-single-clause-is-if"
      ((test (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(cond (,test ,body)))))
      (cl-weave:expect (car expanded) :to-be 'if)))

  (cl-weave:it-property "cond-preserves-test-order"
      ((tests (cl-weave:gen-list (gen-test-form) :min-length 2 :max-length 4)))
    (let* ((clauses (mapcar (lambda (tst) `(,tst :result)) tests))
           (expanded (cl-cc:our-macroexpand `(cond ,@clauses))))
      (labels ((extract-first-test (form)
                 (when (and (consp form) (eq (car form) 'if))
                   (second form))))
        (cl-weave:expect (extract-first-test expanded) :to-equal (first tests)))))

  (cl-weave:it-property "cond-multiple-clauses-nested-if"
      ((clauses (gen-cond-clauses :min-length 2 :max-length 5)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(cond ,@clauses))))
      (cl-weave:expect (car expanded) :to-be-one-of '(if or))
      (cl-weave:expect (some (lambda (sub)
                               (and (consp sub) (eq (car sub) 'cond)))
                             (cdr expanded))
                       :to-be-truthy))))

;;; Property: AND Macro Expansion

(cl-weave:describe "AND macro expansion properties"

  (cl-weave:it "and-empty-returns-t"
    (cl-weave:expect (cl-cc:our-macroexpand-1 '(and)) :to-be t))

  (cl-weave:it-property "and-single-returns-arg"
      ((arg (gen-body-form)))
    (cl-weave:expect (cl-cc:our-macroexpand-1 `(and ,arg)) :to-equal arg))

  (cl-weave:it-property "and-multiple-is-if"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 5)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(and ,@args))))
      (cl-weave:expect (car expanded) :to-be 'if)
      ;; The then-branch recurses into (and ...) except for the two-argument
      ;; base case, which expands it away.
      (let ((then-part (third expanded)))
        (cl-weave:expect (or (and (consp then-part) (eq (car then-part) 'and))
                             (= (length args) 2))
                         :to-be-truthy))
      (cl-weave:expect (fourth expanded) :to-be-null)))

  (cl-weave:it-property "and-full-expansion-no-and"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 4)))
    (let ((expanded (cl-cc:our-macroexpand-all `(and ,@args))))
      (cl-weave:expect (form-contains-symbol-p 'and expanded) :to-be-falsy))))

;;; Property: OR Macro Expansion

(cl-weave:describe "OR macro expansion properties"

  (cl-weave:it "or-empty-returns-nil"
    (cl-weave:expect (cl-cc:our-macroexpand-1 '(or)) :to-be-null))

  (cl-weave:it-property "or-single-returns-arg"
      ((arg (gen-body-form)))
    (cl-weave:expect (cl-cc:our-macroexpand-1 `(or ,arg)) :to-equal arg))

  (cl-weave:it-property "or-multiple-introduces-gensym"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 5)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(or ,@args))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-have-length 1)
      (cl-weave:expect (caar (second expanded)) :to-satisfy #'symbolp)))

  (cl-weave:it-property "or-full-expansion-no-or"
      ((args (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 4)))
    (let ((expanded (cl-cc:our-macroexpand-all `(or ,@args))))
      (cl-weave:expect (form-contains-symbol-p 'or expanded) :to-be-falsy))))

;;; Property: LET* Macro Expansion

(cl-weave:describe "LET* macro expansion properties"

  (cl-weave:it-property "let-star-empty-is-progn"
      ((body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* () ,@body))))
      (cl-weave:expect (car expanded) :to-be 'progn)
      (cl-weave:expect (cdr expanded) :to-equal body)))

  (cl-weave:it-property "let-star-single-is-let"
      ((binding (gen-binding-pair))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* (,binding) ,body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-equal (list binding))))

  (cl-weave:it-property "let-star-multiple-nested"
      ((bindings (gen-binding-list :min-length 2 :max-length 4))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings ,body))))
      (labels ((count-nested-lets (form depth)
                 (if (and (consp form) (eq (car form) 'let))
                     (count-nested-lets (third form) (1+ depth))
                     depth)))
        (cl-weave:expect (count-nested-lets expanded 0) :to-be (length bindings)))))

  (cl-weave:it-property "let-star-preserves-binding-order"
      ((bindings (gen-binding-list :min-length 2 :max-length 3))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (caar (second expanded)) :to-equal (caar bindings)))))

;;; Property: PROG1 and PROG2 Macro Expansion

(cl-weave:describe "PROG1/PROG2 macro expansion properties"

  (cl-weave:it-property "prog1-introduces-result-variable"
      ((first-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-have-length 1)
      (cl-weave:expect (caar (second expanded)) :to-satisfy #'symbolp)))

  (cl-weave:it-property "prog1-returns-first-value"
      ((first-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (car (last expanded)) :to-be (caar (second expanded)))))

  (cl-weave:it-property "prog2-structure"
      ((first-form (gen-body-form))
       (second-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'progn)
      (cl-weave:expect (second expanded) :to-equal first-form)
      (cl-weave:expect (car (third expanded)) :to-be 'let)))

  (cl-weave:it-property "prog2-returns-second-value"
      ((first-form (gen-body-form))
       (second-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((let-form (third (cl-cc:our-macroexpand-1
                            `(prog2 ,first-form ,second-form ,@body)))))
      (cl-weave:expect (car let-form) :to-be 'let)
      (cl-weave:expect (car (last let-form)) :to-be (caar (second let-form))))))
