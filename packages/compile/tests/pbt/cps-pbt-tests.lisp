(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;; ----------------------------------------------------------------------------
;; CPS Transformation Property-Based Tests
;; ----------------------------------------------------------------------------
;; Verify that CPS conversion produces the expected continuation-passing shape.
;; Expressed with cl-weave's NATIVE property API: a local data-table macro
;; (define-cps-shape-properties) emits one cl-weave:it-property per row, so the
;; table stays pure data while cl-weave owns generation/shrinking/running.
;;
;; Each emitted body asserts through CL-WEAVE:EXPECT with :TO-SATISFY.
;; IT-PROPERTY decides pass/fail from a *signaled* condition — RUN-PROPERTY
;; wraps the body in PROPERTY-FAILURE-CONDITION, which only catches ERROR — and
;; discards the body's return value, so emitting a bare (funcall #'PREDICATE ...)
;; would report PASS regardless of what the predicate returned. :TO-SATISFY is
;; preferred over :TO-BE-TRUTHY on the predicate's result because it reports the
;; offending CPS form itself rather than just "false".

;; ----------------------------------------------------------------------------
;; Test Helpers
;; ----------------------------------------------------------------------------

(defun cps-to-sexp (ast)
  "Convert AST to S-expression."
  (ast-to-sexp ast))

(defun cps-of-expr (expr)
  "Apply CPS transformation to an expression."
  (cps-transform expr))

(defun cps-lambda-form-p (cps-expr)
  "Return T when CPS-EXPR is a lambda form."
  (and (consp cps-expr)
       (eq (car cps-expr) 'lambda)))

(defun cps-continuation-p (cps-expr)
  "Check if CPS expression is a lambda with continuation parameter.
   Returns T if CPS-EXPR has the form (lambda (k) ...)."
  (and (cps-lambda-form-p cps-expr)
       (consp (second cps-expr))
       (symbolp (car (second cps-expr)))
       (= (length (second cps-expr)) 1)))

(defun cps-lambda-single-parameter-p (cps-expr)
  "Return T when CPS-EXPR is a single-parameter lambda."
  (and (cps-lambda-form-p cps-expr)
       (consp (second cps-expr))
       (= (length (second cps-expr)) 1)))

(defun get-continuation-name (cps-expr)
  "Extract the continuation parameter name from a CPS expression."
  (and (cps-continuation-p cps-expr)
       (car (second cps-expr))))

(defun cps-continuation-name-k-p (cps-expr)
  "Return T when the continuation parameter is named K."
  (and (cps-continuation-p cps-expr)
       (string= (symbol-name (get-continuation-name cps-expr)) "K")))

(defun cps-find-continuation-application (form var)
  "Find a (FUNCALL <symbol> VAR) subform of FORM, or NIL if there is none.
Walks CAR and CDR separately so an improper subform cannot break the search."
  (when (consp form)
    (if (and (eq (car form) 'funcall)
             (consp (cdr form))
             (consp (cddr form))
             (null (cdddr form))
             (symbolp (second form))
             (eq (third form) var))
        form
        (or (cps-find-continuation-application (car form) var)
            (cps-find-continuation-application (cdr form) var)))))

(defmacro define-cps-shape-properties (&rest specs)
  "Emit one cl-weave:it-property per (NAME (VAR GEN...) EXPR CHECK) row.
The row is pure data: NAME labels the property, (VAR GEN...) is a flat
variable/generator table, EXPR is CPS-transformed, and CHECK is the shape
predicate the result must satisfy."
  `(progn
     ,@(mapcar
        (lambda (spec)
          (destructuring-bind (name bindings expr check) spec
            `(cl-weave:it-property ,(string-downcase (symbol-name name))
                 ,(loop for (var gen) on bindings by #'cddr collect (list var gen))
               (cl-weave:expect (cps-transform ,expr) :to-satisfy #',check))))
        specs)))

(cl-weave:describe "CPS transformation shape properties"

  (define-cps-shape-properties
    (cps-constant-introduces-continuation
        (n (cl-weave:gen-integer :min -1000 :max 1000))
        n
        cps-continuation-p)

    (cps-variable-preserves-symbol
        (sym (cl-weave:gen-symbol))
        sym
        cps-continuation-p)

    (cps-continuation-is-named-k
        (n (cl-weave:gen-integer :min -1000 :max 1000))
        n
        cps-continuation-name-k-p)

    (cps-addition-produces-lambda
        (a (cl-weave:gen-integer :min -100 :max 100)
         b (cl-weave:gen-integer :min -100 :max 100))
        `(+ ,a ,b)
        cps-lambda-form-p)

    (cps-subtraction-produces-lambda
        (a (cl-weave:gen-integer :min -100 :max 100)
         b (cl-weave:gen-integer :min -100 :max 100))
        `(- ,a ,b)
        cps-lambda-form-p)

    (cps-multiplication-produces-lambda
        (a (cl-weave:gen-integer :min -100 :max 100)
         b (cl-weave:gen-integer :min -100 :max 100))
        `(* ,a ,b)
        cps-lambda-form-p)

    (cps-if-produces-lambda
        (cond-val (cl-weave:gen-member '(-1 0 1))
         then-val (cl-weave:gen-integer :min -100 :max 100)
         else-val (cl-weave:gen-integer :min -100 :max 100))
        `(if ,cond-val ,then-val ,else-val)
        cps-lambda-form-p)

    (cps-let-produces-lambda
        (val1 (cl-weave:gen-integer :min -100 :max 100))
        `(let ((x ,val1)) x)
        cps-lambda-form-p)

    (cps-progn-produces-lambda
        (vals (cl-weave:gen-list (cl-weave:gen-integer :min -10 :max 10)
                                 :min-length 1 :max-length 3))
        `(progn ,@vals)
        cps-lambda-form-p)

    (cps-lambda-has-one-parameter
        (n (cl-weave:gen-integer :min -100 :max 100))
        n
        cps-lambda-single-parameter-p)

    (cps-nested-addition-produces-lambda
        (a (cl-weave:gen-integer :min -50 :max 50)
         b (cl-weave:gen-integer :min -50 :max 50)
         c (cl-weave:gen-integer :min -50 :max 50))
        `(+ (+ ,a ,b) ,c)
        cps-lambda-form-p)

    (cps-print-produces-lambda
        (val (cl-weave:gen-integer :min -100 :max 100))
        `(print ,val)
        cps-continuation-p))

  ;; A simple variable CPS-transforms to an application of the continuation to
  ;; that variable.
  ;;
  ;; This assertion had to be rewritten, not merely made live. The original
  ;; took (caddr cps-result) and required it to BE the (funcall k var) form.
  ;; CPS-TRANSFORM wraps the continuation application in a tail-call
  ;; trampoline, so a variable now transforms to
  ;;
  ;;   (lambda (k)
  ;;     (labels ((cps-trampoline-run-internal (value) ...))
  ;;       (cps-trampoline-run-internal (funcall k VAR))))
  ;;
  ;; making (caddr cps-result) the LABELS form for every possible input. The
  ;; old assertion could therefore never hold — it was stale rather than
  ;; merely unchecked, and only survived because the vacuous body discarded it.
  ;; The invariant that actually holds is that the continuation parameter is
  ;; applied to the variable *somewhere* in the body, which is also stronger
  ;; than the original: it pins the callee to the lambda's own continuation
  ;; parameter rather than accepting any FUNCALL.
  (cl-weave:it-property "cps-identity-for-simple-variable"
      ((sym (cl-weave:gen-symbol)))
    (let* ((cps-result (cps-transform sym))
           (continuation (car (second cps-result)))
           (application (cps-find-continuation-application cps-result sym)))
      (cl-weave:expect application :to-be-truthy)
      (cl-weave:expect (second application) :to-be continuation))))
