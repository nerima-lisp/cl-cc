;;;; tests/pbt/macro-pbt-mv-tests.lisp — SETF, PSETQ, MVB, MVSQ, MVL, DEFUN, Nested
;;;
;;; Expressed with cl-weave's NATIVE property API; bodies assert through
;;; CL-WEAVE:EXPECT because IT-PROPERTY detects a signaled condition and
;;; ignores the body's return value.

(in-package :cl-cc/pbt)

(in-suite macro-pbt-suite)

;;; Property: SETF Macro Expansion

(cl-weave:describe "SETF/PSETQ macro expansion properties"

  (cl-weave:it-property "setf-symbol-is-setq"
      ((var (gen-pbt-symbol "VAR"))
       (val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(setf ,var ,val))))
      (cl-weave:expect (car expanded) :to-be 'setq)
      (cl-weave:expect (second expanded) :to-equal var)
      (cl-weave:expect (third expanded) :to-equal val)))

  (cl-weave:it "psetq-empty-is-nil-mv-pbt"
    (cl-weave:expect (cl-cc:our-macroexpand-1 '(psetq)) :to-be-null))

  (cl-weave:it-property "psetq-introduces-temps"
      ((pairs (cl-weave:gen-list (gen-binding-pair) :min-length 1 :max-length 4)))
    (let* ((flat-pairs (apply #'append pairs))
           (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (second expanded) :to-have-length (length pairs))))

  (cl-weave:it-property "psetq-preserves-values"
      ((pairs (cl-weave:gen-list (gen-binding-pair) :min-length 1 :max-length 3)))
    (let* ((flat-pairs (apply #'append pairs))
           (values (loop for (nil val) on flat-pairs by #'cddr collect val))
           (expanded (cl-cc:our-macroexpand-1 `(psetq ,@flat-pairs))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (mapcar #'second (second expanded)) :to-equal values))))

;;; Property: MULTIPLE-VALUE-BIND / -SETQ / -LIST Macro Expansion

(cl-weave:describe "MULTIPLE-VALUE-* macro expansion properties"

  ;; The original file defined mvb-uses-canonical-binding-path twice (once at
  ;; line 47, once at line 103) with the same body and a different docstring.
  ;; Under DEFPROPERTY the second DEFTEST of that symbol replaced the first, so
  ;; it only ever counted once; IT-PROPERTY names are strings and both would
  ;; have registered, silently adding a test. Only one definition is kept.
  (cl-weave:it-property "mvb-uses-canonical-binding-path"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form))
       (body (cl-weave:gen-list (gen-body-form) :min-length 0 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,@body))))
      (cl-weave:expect (car expanded) :to-be-one-of '(let let*))))

  ;; mvb-preserves-variables and mvb-preserves-form assert nothing about the
  ;; expansion: their bodies were (progn expanded vars form body t), a constant
  ;; T that merely referenced the bindings to silence unused-variable warnings.
  ;; They are preserved as-is — checking only that expansion does not signal —
  ;; rather than given invented assertions, since what they were meant to
  ;; verify is not recoverable from the code.
  (cl-weave:it-property "mvb-preserves-variables"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form))
       (body (gen-body-form)))
    (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form ,body))
    t)

  (cl-weave:it-property "mvb-preserves-form"
      ((vars (gen-variable-list :min-length 1 :max-length 3))
       (form (gen-body-form)))
    (cl-cc:our-macroexpand-1 `(multiple-value-bind ,vars ,form))
    t)

  (cl-weave:it-property "mvsq-uses-multiple-value-list"
      ((vars (gen-variable-list :min-length 1 :max-length 5))
       (form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (let ((binding (car (second expanded))))
        (cl-weave:expect (car (second binding)) :to-be 'multiple-value-list))))

  (cl-weave:it-property "mvsq-has-setq-for-each-var"
      ((vars (gen-variable-list :min-length 1 :max-length 4))
       (form (gen-body-form)))
    (let* ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-setq ,vars ,form)))
           (body (cddr expanded)))
      (cl-weave:expect (count-symbols-in-form 'setq body) :to-be (length vars))))

  (cl-weave:it-property "mvl-uses-canonical-mv-list-path"
      ((form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
      (cl-weave:expect (or (form-contains-symbol-p 'multiple-value-call expanded)
                           (form-contains-symbol-p 'multiple-value-list expanded)
                           (eq (car expanded) 'list))
                       :to-be-truthy)))

  (cl-weave:it-property "mvl-accumulates-into-list"
      ((form (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(multiple-value-list ,form))))
      (cl-weave:expect (form-contains-symbol-p 'nreverse expanded) :to-be-truthy))))

;;; Property: DEFUN Macro Expansion

(cl-weave:describe "DEFUN macro expansion properties"

  (cl-weave:it-property "defun-uses-setf-fdefinition"
      ((name (gen-pbt-symbol "FN"))
       (params (gen-variable-list :min-length 0 :max-length 4))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body))))
      (cl-weave:expect (car expanded) :to-be 'setf)
      (cl-weave:expect (car (second expanded)) :to-be 'fdefinition)))

  (cl-weave:it-property "defun-creates-lambda"
      ((name (gen-pbt-symbol "FN"))
       (params (gen-variable-list :min-length 0 :max-length 4))
       (body (cl-weave:gen-list (gen-body-form) :min-length 1 :max-length 3)))
    (let ((lambda-form (third (cl-cc:our-macroexpand-1 `(defun ,name ,params ,@body)))))
      (cl-weave:expect (car lambda-form) :to-be 'lambda)
      (cl-weave:expect (second lambda-form) :to-equal params)))

  (cl-weave:it "DEFUN/C expands with explicit pre/post contract checks."
    (let ((expanded-1
            (cl-cc:our-macroexpand-1
             '(defun/c add1-positive-pbtmv (x)
                :requires (> x 0)
                :ensures (= result (+ x 1))
                (+ x 1)))))
      (assert-eq 'defun (car expanded-1))
      (assert-eq 'add1-positive-pbtmv (cadr expanded-1))
      (assert-equal '(x) (caddr expanded-1))
      (assert-true
       (some (lambda (form) (and (consp form) (eq (car form) 'unless)))
             (cdddr expanded-1)))
      (assert-true
       (some (lambda (form) (and (consp form) (eq (car form) 'let)))
             (cdddr expanded-1))))))

;;; Property: Nested Macro Expansion

(cl-weave:describe "nested macro expansion properties (mv)"

  (cl-weave:it-property "nested-when-in-let-star"
      ((var (gen-pbt-symbol "X"))
       (val (cl-weave:gen-integer :min -10 :max 10))
       (test (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(let* ((,var ,val)) (when ,test ,body)))))
      (cl-weave:expect (car expanded) :to-be 'let)
      (cl-weave:expect (form-contains-symbol-p 'when expanded) :to-be-falsy)
      (cl-weave:expect (form-contains-symbol-p 'let* expanded) :to-be-falsy)))

  (cl-weave:it-property "nested-cond-in-and"
      ((test1 (gen-test-form))
       (test2 (gen-test-form))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(and ,test1 (cond ((,test2 ,body)))))))
      (cl-weave:expect (form-contains-symbol-p 'cond expanded) :to-be-falsy)
      (cl-weave:expect (form-contains-symbol-p 'and expanded) :to-be-falsy)))

  (cl-weave:it-property "nested-let-star-in-let-star"
      ((bindings1 (gen-binding-list :min-length 1 :max-length 2))
       (bindings2 (gen-binding-list :min-length 1 :max-length 2))
       (body (gen-body-form)))
    (let ((expanded (cl-cc:our-macroexpand-all `(let* ,bindings1 (let* ,bindings2 ,body)))))
      (cl-weave:expect (form-contains-symbol-p 'let* expanded) :to-be-falsy)
      (cl-weave:expect (car expanded) :to-be 'let)))

  (cl-weave:it-property "nested-or-in-prog1"
      ((args1 (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 3))
       (args2 (cl-weave:gen-list (gen-body-form) :min-length 2 :max-length 3)))
    (let ((expanded (cl-cc:our-macroexpand-all `(prog1 (or ,@args1) (or ,@args2)))))
      (cl-weave:expect (form-contains-symbol-p 'or expanded) :to-be-falsy)
      (cl-weave:expect (car expanded) :to-be 'let))))
