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
      (expect (typep ast2 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value ast2)) :to-be-truthy))
    (let ((ast2 (%ast-roundtrip (make-ast-var :name name))))
      (expect (typep ast2 'ast-var) :to-be-truthy)
      (expect (ast-var-name ast2) :to-be name)))

  (cl-weave:it-property
      "Quoted values (mixed/symbol/nested-list) are preserved through sexp roundtrip."
      ((value (cl-weave:gen-member '(nil t 42 "string" (a b c))))
       (sym   (gen-pbt-symbol "SYM")))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value value))))
      (expect (typep ast2 'ast-quote) :to-be-truthy)
      (expect (ast-quote-value ast2) :to-equal value))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value sym))))
      (expect (typep ast2 'ast-quote) :to-be-truthy)
      (expect (ast-quote-value ast2) :to-be sym))
    (let* ((nested '(a (b c) d))
           (ast2 (%ast-roundtrip (make-ast-quote :value nested))))
      (expect (typep ast2 'ast-quote) :to-be-truthy)
      (expect (ast-quote-value ast2) :to-equal nested)))

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
      (expect (typep ast2 'ast-binop) :to-be-truthy)
      (expect (ast-binop-op ast2) :to-be op)
      (expect (typep (ast-binop-lhs ast2) 'ast-int) :to-be-truthy)
      (expect (typep (ast-binop-rhs ast2) 'ast-int) :to-be-truthy)
      (expect (= lhs-val (ast-int-value (ast-binop-lhs ast2))) :to-be-truthy)
      (expect (= rhs-val (ast-int-value (ast-binop-rhs ast2))) :to-be-truthy)))

  (cl-weave:it-property
      "Conditional (cond/then/else) is preserved through sexp roundtrip."
      ((cond-val (cl-weave:gen-integer :min 0 :max 1))
       (then-val (cl-weave:gen-integer :min -100 :max 100))
       (else-val (cl-weave:gen-integer :min -100 :max 100)))
    (let* ((ast  (make-ast-if :cond (make-ast-int :value cond-val)
                              :then (make-ast-int :value then-val)
                              :else (make-ast-int :value else-val)))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-if) :to-be-truthy)
      (expect (typep (ast-if-cond ast2) 'ast-int) :to-be-truthy)
      (expect (typep (ast-if-then ast2) 'ast-int) :to-be-truthy)
      (expect (typep (ast-if-else ast2) 'ast-int) :to-be-truthy)
      (expect (= cond-val (ast-int-value (ast-if-cond ast2))) :to-be-truthy)
      (expect (= then-val (ast-int-value (ast-if-then ast2))) :to-be-truthy)
      (expect (= else-val (ast-int-value (ast-if-else ast2))) :to-be-truthy)))

  (cl-weave:it-property
      "Sequence of integer forms is preserved (type and value) through roundtrip."
      ((vals (cl-weave:gen-list (cl-weave:gen-integer :min -100 :max 100)
                                :min-length 1 :max-length 5)))
    (let* ((ast  (make-ast-progn :forms (mapcar (lambda (v) (make-ast-int :value v)) vals)))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-progn) :to-be-truthy)
      (expect (= (length vals) (length (ast-progn-forms ast2))) :to-be-truthy)
      (expect (every (lambda (f) (typep f 'ast-int)) (ast-progn-forms ast2)) :to-be-truthy)
      (expect (mapcar #'ast-int-value (ast-progn-forms ast2)) :to-equal vals)))

  (cl-weave:it-property "Print expression is preserved through sexp roundtrip."
      ((value (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-print :expr (make-ast-int :value value)))))
      (expect (typep ast2 'ast-print) :to-be-truthy)
      (expect (typep (ast-print-expr ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-print-expr ast2))) :to-be-truthy)))

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
      (expect (typep ast2 'ast-let) :to-be-truthy)
      (expect (= 1 (length (ast-let-bindings ast2))) :to-be-truthy)
      (expect (car   (first (ast-let-bindings ast2))) :to-be var-name)
      (expect (= init-val (ast-int-value (cdr (first (ast-let-bindings ast2))))) :to-be-truthy)
      (expect (= 2 (length (ast-let-body ast2))) :to-be-truthy))
    (let ((ast2 (%ast-roundtrip (make-ast-let :bindings nil
                                              :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-let) :to-be-truthy)
      (expect (ast-let-bindings ast2) :to-be-null)
      (expect (= 1 (length (ast-let-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-let-body ast2)))) :to-be-truthy)))

  (cl-weave:it-property
      "Lambda params and single-form body are preserved through sexp roundtrip."
      ((params   (cl-weave:gen-list (gen-pbt-symbol "ARG")
                                    :min-length 0 :max-length 4))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-lambda :params params
                                                 :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-lambda) :to-be-truthy)
      (expect (ast-lambda-params ast2) :to-equal params)
      (expect (= 1 (length (ast-lambda-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-lambda-body ast2)))) :to-be-truthy)))

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
      (expect (typep ast2 'ast-flet) :to-be-truthy)
      (expect (= 1 (length (ast-flet-bindings ast2))) :to-be-truthy)
      (expect (first  (first (ast-flet-bindings ast2))) :to-be fn-name)
      (expect (second (first (ast-flet-bindings ast2))) :to-equal (list param))
      (expect (= body-val (ast-int-value (third (first (ast-flet-bindings ast2))))) :to-be-truthy)))

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
      (expect (typep ast2 'ast-labels) :to-be-truthy)
      (expect (= 2 (length (ast-labels-bindings ast2))) :to-be-truthy)
      (expect (first (first  (ast-labels-bindings ast2))) :to-be fn1-name)
      (expect (first (second (ast-labels-bindings ast2))) :to-be fn2-name)))

  ;;; ── Control Flow ──────────────────────────────────────────────────────────

  (cl-weave:it-property
      "Block and return-from nodes each preserve name and body through sexp roundtrip."
      ((name        (gen-pbt-symbol "BLOCK"))
       (body-val    (cl-weave:gen-integer :min -100 :max 100))
       (return-name (gen-pbt-symbol "BLOCK"))
       (value       (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-block :name name
                                                :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-block) :to-be-truthy)
      (expect (ast-block-name ast2) :to-be name)
      (expect (= 1 (length (ast-block-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-block-body ast2)))) :to-be-truthy))
    (let ((ast2 (%ast-roundtrip (make-ast-return-from :name  return-name
                                                      :value (make-ast-int :value value)))))
      (expect (typep ast2 'ast-return-from) :to-be-truthy)
      (expect (ast-return-from-name ast2) :to-be return-name)
      (expect (typep (ast-return-from-value ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-return-from-value ast2))) :to-be-truthy)))

  (cl-weave:it-property
      "Tagbody preserves tag entries; go preserves tag (symbol and integer) through roundtrip."
      ((tag-val (cl-weave:gen-integer :min 0 :max 100))
       (tag     (gen-pbt-symbol "TAG")))
    (let ((ast2 (%ast-roundtrip
                 (make-ast-tagbody :tags (list (cons tag-val (list (make-ast-var :name 'x))))))))
      (expect (typep ast2 'ast-tagbody) :to-be-truthy)
      (expect (null (ast-tagbody-tags ast2)) :to-be-falsy))
    (let ((ast2 (%ast-roundtrip (make-ast-go :tag tag))))
      (expect (typep ast2 'ast-go) :to-be-truthy)
      (expect (ast-go-tag ast2) :to-be tag))
    (let ((ast2 (%ast-roundtrip (make-ast-go :tag 42))))
      (expect (typep ast2 'ast-go) :to-be-truthy)
      (expect (= 42 (ast-go-tag ast2)) :to-be-truthy))))
