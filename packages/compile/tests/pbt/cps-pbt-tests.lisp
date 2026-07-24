(in-package :cl-cc/pbt)

;; ----------------------------------------------------------------------------
;; CPS Transformation Property-Based Tests
;; ----------------------------------------------------------------------------
;; Verify that CPS conversion produces the expected continuation-passing shape.
;; Expressed with cl-weave's NATIVE property API: a local data-table macro
;; (define-cps-shape-properties) emits one cl-weave:it-property per row, so the
;; table stays pure data while cl-weave owns generation/shrinking/running.

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
               (funcall #',check (cps-transform ,expr)))))
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

  ;; A simple variable CPS-transforms to a (funcall k var) body.
  (cl-weave:it-property "cps-identity-for-simple-variable"
      ((sym (cl-weave:gen-symbol)))
    (let* ((cps-result (cps-transform sym))
           (body (caddr cps-result)))
      (and (consp body)
           (eq (car body) 'funcall)
           (eq (third body) sym)))))
