;;;; tests/pbt/macro-pbt-binding-tests.lisp — PBT for LET*/PROG1/PROG2/SETF/PSETQ/MVB/MVS/MVL/DEFUN
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.
;;;
;;; Note: these properties duplicate macro-pbt-props-tests.lisp's LET*/PROG1/
;;; PROG2 block and macro-pbt-mv-tests.lisp's SETF/PSETQ/MV/DEFUN block, with
;;; a -binding-pbt name suffix. They are migrated as-is rather than deduplicated.

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

(cl-weave:describe "LET* macro expansion properties (binding)"

  (cl-weave:it-property "let-star-empty-is-progn-binding-pbt"
      ((body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* () ,@body))))
      (cl-weave:expect (car expanded) :to-be 'progn)
      (cl-weave:expect (cdr expanded) :to-equal body)))

  (cl-weave:it-property "let-star-single-is-let-binding-pbt"
      ((binding (gen-binding-pair))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* (,binding) ,body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-equal (list binding))))

  (cl-weave:it-property "let-star-multiple-nested-binding-pbt"
      ((bindings (gen-binding-list :min-length 2 :max-length 4))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings ,body))))
      (labels ((count-nested-lets (form depth)
                 (if (and (consp form) (eq (car form) 'let))
                     (count-nested-lets (third form) (1+ depth))
                     depth)))
        (cl-weave:expect (count-nested-lets expanded 0) :to-be (length bindings)))))

  (cl-weave:it-property "let-star-preserves-binding-order-binding-pbt"
      ((bindings (gen-binding-list :min-length 2 :max-length 3))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(let* ,bindings ,body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (caar (second expanded)) :to-equal (caar bindings)))))

;;; Property: PROG1 and PROG2 Macro Expansion

(cl-weave:describe "PROG1/PROG2 macro expansion properties (binding)"

  (cl-weave:it-property "prog1-introduces-result-variable-binding-pbt"
      ((first-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-have-length 1)
      (cl-weave:expect (caar (second expanded)) :to-satisfy #'symbolp)))

  (cl-weave:it-property "prog1-returns-first-value-binding-pbt"
      ((first-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog1 ,first-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (car (last expanded)) :to-be (caar (second expanded)))))

  (cl-weave:it-property "prog2-structure-binding-pbt"
      ((first-form (gen-body-form))
       (second-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(prog2 ,first-form ,second-form ,@body))))
      (cl-weave:expect (car expanded) :to-be 'progn)
      (cl-weave:expect (second expanded) :to-equal first-form)
      (cl-weave:expect (car (third expanded)) :to-be 'let)))

  (cl-weave:it-property "prog2-returns-second-value-binding-pbt"
      ((first-form (gen-body-form))
       (second-form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 2)))
    (let ((let-form (third (cl-cc:our-macroexpand-1
                            `(prog2 ,first-form ,second-form ,@body)))))
      (cl-weave:expect (car let-form) :to-be 'let)
      (cl-weave:expect (car (last let-form)) :to-be (caar (second let-form))))))

;;; Property: SETF and PSETQ Macro Expansion

(cl-weave:describe "SETF/PSETQ macro expansion properties (binding)"

  (cl-weave:it-property "setf-symbol-is-setq-binding-pbt"
      ((var (gen-pbt-symbol "VAR"))
       (val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(setf ,var ,val))))
      (cl-weave:expect (car expanded) :to-be 'setq)
      (cl-weave:expect (second expanded) :to-equal var)
      (cl-weave:expect (third expanded) :to-equal val)))

  (cl-weave:it "psetq-empty-is-nil-binding-pbt"
    (cl-weave:expect (cl-cc:our-macroexpand-1 '(psetq)) :to-be-null))

  (cl-weave:it-property "psetq-introduces-temps-binding-pbt"
      ((pairs (cl-weave:gen-list (gen-binding-pair) :min-length 1 :max-length 4)))
    (let* ((flat-pairs (apply #'append pairs))
           (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
      (cl-weave:expect (car expanded) :to-be 'let)
      ;; Number of temp bindings should equal number of pairs
      (cl-weave:expect (second expanded) :to-have-length (length pairs))))

  (cl-weave:it-property "psetq-preserves-values-binding-pbt"
      ((pairs (cl-weave:gen-list (gen-binding-pair) :min-length 1 :max-length 3)))
    (let* ((flat-pairs (apply #'append pairs))
           (values (loop for (nil val) on flat-pairs by #'cddr collect val))
           (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (mapcar #'second (second expanded)) :to-equal values))))

;;; Property: MULTIPLE-VALUE-BIND / -SETQ / -LIST Macro Expansion

(cl-weave:describe "MULTIPLE-VALUE-* macro expansion properties (binding)"

  (cl-weave:it-property "mvb-uses-canonical-binding-form-binding-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,@body))))
      (cl-weave:expect (car expanded) :to-be-one-of '(let let*))))

  (cl-weave:it-property "mvb-preserves-variables-binding-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,body))))
      (cl-weave:expect (car expanded) :to-be-one-of '(let let*))))

  ;; Asserts nothing about the expansion: the original body was
  ;; (progn expanded vars form t), a constant T referencing its bindings only
  ;; to silence unused-variable warnings. Kept as a "does not signal" check
  ;; rather than given an invented assertion.
  (cl-weave:it-property "mvb-preserves-form-binding-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 3))
       (form (gen-body-form)))
    (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form))
    t)

  (cl-weave:it-property "mvsq-uses-multiple-value-list-binding-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (let ((binding (car (second expanded))))
        (cl-weave:expect (car (second binding)) :to-be 'multiple-value-list))))

  (cl-weave:it-property "mvsq-has-setq-for-each-var-binding-pbt"
      ((vars (gen-variable-list :min-length 1 :max-length 4))
       (form (gen-body-form)))
    (let* ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form)))
           (body (cddr expanded)))
      (cl-weave:expect (count-symbols-in-form 'setq body) :to-be (length vars))))

  (cl-weave:it-property "mvl-uses-multiple-value-call-binding-pbt"
      ((form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
      (cl-weave:expect (or (form-contains-symbol-p 'multiple-value-call expanded)
                           (form-contains-symbol-p 'multiple-value-list expanded)
                           (eq (car expanded) 'list))
                       :to-be-truthy)))

  (cl-weave:it-property "mvl-accumulates-into-list-binding-pbt"
      ((form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
      (cl-weave:expect (form-contains-symbol-p 'nreverse expanded) :to-be-truthy))))

;;; Property: DEFUN Macro Expansion

(cl-weave:describe "DEFUN macro expansion properties (binding)"

  (cl-weave:it-property "defun-uses-setf-fdefinition-binding-pbt"
      ((name (gen-pbt-symbol "FN"))
       (params (gen-variable-list :min-length 0 :max-length 4))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body))))
      (cl-weave:expect (car expanded) :to-be 'setf)
      (cl-weave:expect (car (second expanded)) :to-be 'fdefinition)))

  (cl-weave:it-property "defun-creates-lambda-binding-pbt"
      ((name (gen-pbt-symbol "FN"))
       (params (gen-variable-list :min-length 0 :max-length 4))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((lambda-form (third (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body)))))
      (cl-weave:expect (car lambda-form) :to-be 'lambda)
      (cl-weave:expect (second lambda-form) :to-equal params)))

  (cl-weave:it "DEFUN/C expands with explicit pre/post contract checks."
    (let ((expanded-1
            (cl-cc:our-macroexpand-1
             '(defun/c add1-positive-pbtb (x)
                :requires (> x 0)
                :ensures (= result (+ x 1))
                (+ x 1)))))
      (assert-eq 'defun (car expanded-1))
      (assert-eq 'add1-positive-pbtb (cadr expanded-1))
      (assert-equal '(x) (caddr expanded-1))
      (assert-true
       (some (lambda (form) (and (consp form) (eq (car form) 'unless)))
             (cdddr expanded-1)))
      (assert-true
       (some (lambda (form) (and (consp form) (eq (car form) 'let)))
             (cdddr expanded-1))))))
