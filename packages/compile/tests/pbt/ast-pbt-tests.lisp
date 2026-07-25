;;;; tests/pbt/ast-pbt-tests.lisp - Property-Based Tests for AST Roundtrip
;;;
;;; Expressed with cl-weave's NATIVE property API (cl-weave:describe +
;;; it-property + gen-integer/gen-member/gen-list) rather than the home-grown
;;; cl-cc/pbt DEFTEST + FOR-ALL + GEN-FN combination.
;;;
;;; The bodies keep their cl-cc/test ASSERT-* calls, which are backed by
;;; CL-WEAVE:FAIL and therefore signal on failure — the only thing IT-PROPERTY
;;; detects, since it ignores the body's return value.
;;;
;;; Several of the originals bundled two or three independent FOR-ALL blocks
;;; into one DEFTEST (hence the "cases" in their names). Those stay bundled as
;;; a single IT-PROPERTY whose binding list is the union of the blocks', so the
;;; suite keeps one test per original test. Every generated case now exercises
;;; all of the bundled checks rather than each running its own iterations.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

(defun %ast-roundtrip (ast)
  "Convert AST to sexp and parse it back."
  (lower-sexp-to-ast (ast-to-sexp ast)))

(cl-weave:describe "AST sexp roundtrip properties"

  ;;; ── Atomic Nodes ──────────────────────────────────────────────────────────

  (cl-weave:it-property
      "Integer value and variable name are both preserved through sexp roundtrip."
      ((value (cl-weave:gen-integer :min -10000 :max 10000))
       (name  (gen-pbt-symbol "VAR")))
    (let ((ast2 (%ast-roundtrip (make-ast-int :value value))))
      (assert-type ast-int ast2)
      (assert-= value (ast-int-value ast2)))
    (let ((ast2 (%ast-roundtrip (make-ast-var :name name))))
      (assert-type ast-var ast2)
      (assert-eq name (ast-var-name ast2))))

  (cl-weave:it-property
      "Quoted values (mixed/symbol/nested-list) are preserved through sexp roundtrip."
      ((value (cl-weave:gen-member '(nil t 42 "string" (a b c))))
       (sym   (gen-pbt-symbol "SYM")))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value value))))
      (assert-type ast-quote ast2)
      (assert-equal value (ast-quote-value ast2)))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value sym))))
      (assert-type ast-quote ast2)
      (assert-eq sym (ast-quote-value ast2)))
    (let* ((nested '(a (b c) d))
           (ast2 (%ast-roundtrip (make-ast-quote :value nested))))
      (assert-type ast-quote ast2)
      (assert-equal nested (ast-quote-value ast2))))

  ;;; ── Compound Nodes ────────────────────────────────────────────────────────

  (cl-weave:it-property
      "Binary operation (op, lhs, rhs) is preserved through sexp roundtrip."
      ((op      (cl-weave:gen-member '(+ - *)))
       (lhs-val (cl-weave:gen-integer :min -100 :max 100))
       (rhs-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((ast  (make-ast-binop :op op
                                 :lhs (make-ast-int :value lhs-val)
                                 :rhs (make-ast-int :value rhs-val)))
           (ast2 (%ast-roundtrip ast)))
      (assert-type ast-binop ast2)
      (assert-eq   op      (ast-binop-op ast2))
      (assert-type ast-int (ast-binop-lhs ast2))
      (assert-type ast-int (ast-binop-rhs ast2))
      (assert-=    lhs-val (ast-int-value (ast-binop-lhs ast2)))
      (assert-=    rhs-val (ast-int-value (ast-binop-rhs ast2)))))

  (cl-weave:it-property
      "Conditional (cond/then/else) is preserved through sexp roundtrip."
      ((cond-val (cl-weave:gen-integer :min 0 :max 1))
       (then-val (cl-weave:gen-integer :min -100 :max 100))
       (else-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((ast  (make-ast-if :cond (make-ast-int :value cond-val)
                              :then (make-ast-int :value then-val)
                              :else (make-ast-int :value else-val)))
           (ast2 (%ast-roundtrip ast)))
      (assert-type ast-if  ast2)
      (assert-type ast-int (ast-if-cond ast2))
      (assert-type ast-int (ast-if-then ast2))
      (assert-type ast-int (ast-if-else ast2))
      (assert-=    cond-val (ast-int-value (ast-if-cond ast2)))
      (assert-=    then-val (ast-int-value (ast-if-then ast2)))
      (assert-=    else-val (ast-int-value (ast-if-else ast2)))))

  (cl-weave:it-property
      "Sequence of integer forms is preserved (type and value) through roundtrip."
      ((vals (cl-weave:gen-list (cl-weave:gen-integer :min -100 :max 100)
                                :min-length 1 :max-length 5)))
    (let* ((ast  (make-ast-progn :forms (mapcar (lambda (v) (make-ast-int :value v)) vals)))
           (ast2 (%ast-roundtrip ast)))
      (assert-type ast-progn ast2)
      (assert-=    (length vals) (length (ast-progn-forms ast2)))
      (assert-true (every (lambda (f) (typep f 'ast-int)) (ast-progn-forms ast2)))
      (assert-equal vals (mapcar #'ast-int-value (ast-progn-forms ast2)))))

  (cl-weave:it-property "Print expression is preserved through sexp roundtrip."
      ((value (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-print :expr (make-ast-int :value value)))))
      (assert-type ast-print ast2)
      (assert-type ast-int   (ast-print-expr ast2))
      (assert-=    value     (ast-int-value (ast-print-expr ast2)))))

  ;;; ── Binding Forms ─────────────────────────────────────────────────────────

  ;; The empty-bindings half of this property currently fails: (let () BODY)
  ;; round-trips through AST-TO-SEXP as a two-element LET, which
  ;; LOWER-SEXP-TO-AST then reads as a *named* let and rejects with "named let
  ;; requires name, bindings and body". That is a pre-existing failure, not one
  ;; introduced by this migration, and it is left failing deliberately rather
  ;; than weakened to green.
  (cl-weave:it-property
      "Let binding: one binding/two-form body preserved; empty bindings preserve single body form."
      ((var-name (gen-pbt-symbol "VAR"))
       (init-val (cl-weave:gen-integer :min -100 :max 100))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((ast  (make-ast-let
                  :bindings (list (cons var-name (make-ast-int :value init-val)))
                  :body     (list (make-ast-var :name var-name)
                                  (make-ast-int :value body-val))))
           (ast2 (%ast-roundtrip ast)))
      (assert-type ast-let ast2)
      (assert-=    1        (length (ast-let-bindings ast2)))
      (assert-eq   var-name (car   (first (ast-let-bindings ast2))))
      (assert-=    init-val (ast-int-value (cdr (first (ast-let-bindings ast2)))))
      (assert-=    2        (length (ast-let-body ast2))))
    (let ((ast2 (%ast-roundtrip (make-ast-let :bindings nil
                                              :body (list (make-ast-int :value body-val))))))
      (assert-type ast-let ast2)
      (assert-null (ast-let-bindings ast2))
      (assert-=    1        (length (ast-let-body ast2)))
      (assert-=    body-val (ast-int-value (first (ast-let-body ast2))))))

  (cl-weave:it-property
      "Lambda params and single-form body are preserved through sexp roundtrip."
      ((params   (cl-weave:gen-list (gen-pbt-symbol "ARG")
                                    :min-length 0 :max-length 4))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-lambda :params params
                                                 :body (list (make-ast-int :value body-val))))))
      (assert-type  ast-lambda ast2)
      (assert-equal params    (ast-lambda-params ast2))
      (assert-=     1         (length (ast-lambda-body ast2)))
      (assert-=     body-val  (ast-int-value (first (ast-lambda-body ast2))))))

  (cl-weave:it-property
      "Flet binding (name, params, body) is preserved through sexp roundtrip."
      ((fn-name  (gen-pbt-symbol "FN"))
       (param    (gen-pbt-symbol "ARG"))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((ast  (make-ast-flet
                  :bindings (list (list* fn-name (list param)
                                         (list (make-ast-int :value body-val))))
                  :body     (list (make-ast-var :name fn-name))))
           (ast2 (%ast-roundtrip ast)))
      (assert-type  ast-flet ast2)
      (assert-=     1        (length (ast-flet-bindings ast2)))
      (assert-eq    fn-name  (first  (first (ast-flet-bindings ast2))))
      (assert-equal (list param) (second (first (ast-flet-bindings ast2))))
      (assert-=     body-val (ast-int-value (third (first (ast-flet-bindings ast2)))))))

  (cl-weave:it-property
      "Labels with two bindings preserves both function names through sexp roundtrip."
      ((fn1-name (gen-pbt-symbol "FN1"))
       (fn2-name (gen-pbt-symbol "FN2"))
       (param    (gen-pbt-symbol "ARG"))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast  (make-ast-labels
                  :bindings (list (list* fn1-name (list param) (list body-ast))
                                  (list* fn2-name (list param) (list body-ast)))
                  :body     (list (make-ast-var :name fn1-name))))
           (ast2 (%ast-roundtrip ast)))
      (assert-type ast-labels ast2)
      (assert-=    2       (length (ast-labels-bindings ast2)))
      (assert-eq   fn1-name (first (first  (ast-labels-bindings ast2))))
      (assert-eq   fn2-name (first (second (ast-labels-bindings ast2))))))

  ;;; ── Control Flow ──────────────────────────────────────────────────────────

  (cl-weave:it-property
      "Block and return-from nodes each preserve name and body through sexp roundtrip."
      ((name        (gen-pbt-symbol "BLOCK"))
       (body-val    (cl-weave:gen-integer :min -100 :max 100))
       (return-name (gen-pbt-symbol "BLOCK"))
       (value       (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-block :name name
                                                :body (list (make-ast-int :value body-val))))))
      (assert-type ast-block ast2)
      (assert-eq   name     (ast-block-name ast2))
      (assert-=    1        (length (ast-block-body ast2)))
      (assert-=    body-val (ast-int-value (first (ast-block-body ast2)))))
    (let ((ast2 (%ast-roundtrip (make-ast-return-from :name  return-name
                                                      :value (make-ast-int :value value)))))
      (assert-type ast-return-from ast2)
      (assert-eq   return-name (ast-return-from-name ast2))
      (assert-type ast-int (ast-return-from-value ast2))
      (assert-=    value  (ast-int-value (ast-return-from-value ast2)))))

  (cl-weave:it-property
      "Tagbody preserves tag entries; go preserves tag (symbol and integer) through roundtrip."
      ((tag-val (cl-weave:gen-integer :min 0 :max 100))
       (tag     (gen-pbt-symbol "TAG")))
    (let ((ast2 (%ast-roundtrip
                 (make-ast-tagbody :tags (list (cons tag-val (list (make-ast-var :name 'x))))))))
      (assert-type ast-tagbody ast2)
      (assert-false (null (ast-tagbody-tags ast2))))
    (let ((ast2 (%ast-roundtrip (make-ast-go :tag tag))))
      (assert-type ast-go ast2)
      (assert-eq   tag (ast-go-tag ast2)))
    (let ((ast2 (%ast-roundtrip (make-ast-go :tag 42))))
      (assert-type ast-go ast2)
      (assert-=    42 (ast-go-tag ast2)))))
