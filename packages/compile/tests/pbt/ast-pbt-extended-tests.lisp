;;;; tests/pbt/ast-pbt-extended-tests.lisp — PBT: assignment, multiple-values, dynamic control, calls, types, nested
;;;
;;; Expressed with cl-weave's NATIVE property API (cl-weave:describe +
;;; it-property + gen-integer/gen-member/gen-list) rather than the home-grown
;;; cl-cc/pbt DEFTEST + FOR-ALL + GEN-FN combination.
;;;
;;; The bodies keep their cl-cc/test ASSERT-* calls. Those are backed by
;;; CL-WEAVE:FAIL (see packages/testing-framework/src/framework-assertions.lisp),
;;; so they signal on failure, which is what IT-PROPERTY detects — it decides
;;; pass/fail from a signaled condition, not from the body's return value.

(in-package :cl-cc/pbt)

(cl-weave:describe "AST sexp roundtrip properties (extended)"

  ;;; ── Assignment ────────────────────────────────────────────────────────────

  (cl-weave:it-property "Setq variable and value are preserved through sexp roundtrip."
      ((var (gen-pbt-symbol "VAR"))
       (value (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-setq :var   var
                                               :value (make-ast-int :value value)))))
      (expect (typep ast2 'ast-setq) :to-be-truthy)
      (expect (ast-setq-var ast2) :to-be var)
      (expect (typep (ast-setq-value ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-setq-value ast2))) :to-be-truthy)))

  ;;; ── Multiple Values ───────────────────────────────────────────────────────

  (cl-weave:it-property "Multiple-value-call (func + one arg) is preserved through sexp roundtrip."
      ((fn-name (gen-pbt-symbol "FN"))
       (arg-val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-multiple-value-call
                                 :func (make-ast-var :name fn-name)
                                 :args (list (make-ast-int :value arg-val))))))
      (expect (typep ast2 'ast-multiple-value-call) :to-be-truthy)
      (expect (typep (ast-mv-call-func ast2) 'ast-var) :to-be-truthy)
      (expect (ast-var-name (ast-mv-call-func ast2)) :to-be fn-name)
      (expect (= 1 (length (ast-mv-call-args ast2))) :to-be-truthy)))

  (cl-weave:it-property "Multiple-value-prog1 (first + one form) is preserved through sexp roundtrip."
      ((first-val (cl-weave:gen-integer :min -100 :max 100))
       (form-val  (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-multiple-value-prog1
                                 :first (make-ast-int :value first-val)
                                 :forms (list (make-ast-int :value form-val))))))
      (expect (typep ast2 'ast-multiple-value-prog1) :to-be-truthy)
      (expect (typep (ast-mv-prog1-first ast2) 'ast-int) :to-be-truthy)
      (expect (= first-val (ast-int-value (ast-mv-prog1-first ast2))) :to-be-truthy)
      (expect (= 1 (length (ast-mv-prog1-forms ast2))) :to-be-truthy)))

  ;;; ── Dynamic Control ───────────────────────────────────────────────────────

  (cl-weave:it-property "Catch (keyword tag + one body form) is preserved through sexp roundtrip."
      ((tag      (gen-pbt-keyword "TAG"))
       (body-val (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-catch :tag  (make-ast-var :name tag)
                                                :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-catch) :to-be-truthy)
      (expect (typep (ast-catch-tag ast2) 'ast-var) :to-be-truthy)
      (expect (= 1 (length (ast-catch-body ast2))) :to-be-truthy)))

  (cl-weave:it-property "Throw (tag and value) is preserved through sexp roundtrip."
      ((tag   (gen-pbt-keyword "TAG"))
       (value (cl-weave:gen-integer :min -1000 :max 1000)))
    (let ((ast2 (%ast-roundtrip (make-ast-throw :tag   (make-ast-var :name tag)
                                                :value (make-ast-int :value value)))))
      (expect (typep ast2 'ast-throw) :to-be-truthy)
      (expect (typep (ast-throw-tag ast2) 'ast-var) :to-be-truthy)
      (expect (typep (ast-throw-value ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-throw-value ast2))) :to-be-truthy)))

  (cl-weave:it-property "Unwind-protect (protected form + one cleanup form) is preserved through sexp roundtrip."
      ((protected-val (cl-weave:gen-integer :min -100 :max 100))
       (cleanup-val   (cl-weave:gen-integer :min -100 :max 100)))
    (let ((ast2 (%ast-roundtrip (make-ast-unwind-protect
                                 :protected (make-ast-int :value protected-val)
                                 :cleanup   (list (make-ast-int :value cleanup-val))))))
      (expect (typep ast2 'ast-unwind-protect) :to-be-truthy)
      (expect (typep (ast-unwind-protected ast2) 'ast-int) :to-be-truthy)
      (expect (= protected-val (ast-int-value (ast-unwind-protected ast2))) :to-be-truthy)
      (expect (= 1 (length (ast-unwind-cleanup ast2))) :to-be-truthy)))

  ;;; ── Function Calls and References ─────────────────────────────────────────

  (cl-weave:it-property "Function call (name + N integer args) is preserved through sexp roundtrip."
      ((fn-name (gen-pbt-symbol "FN"))
       (args    (cl-weave:gen-list (cl-weave:gen-integer :min -100 :max 100)
                                   :min-length 1 :max-length 5)))
    (let ((ast2 (%ast-roundtrip (make-ast-call :func fn-name
                                               :args (mapcar (lambda (v) (make-ast-int :value v))
                                                             args)))))
      (expect (typep ast2 'ast-call) :to-be-truthy)
      (expect (= (length args) (length (ast-call-args ast2))) :to-be-truthy)
      (expect (mapcar #'ast-int-value (ast-call-args ast2)) :to-equal args)))

  (cl-weave:it-property "Function reference with symbol name is preserved through sexp roundtrip."
      ((name (gen-pbt-symbol "FN")))
    (let ((ast2 (%ast-roundtrip (make-ast-function :name name))))
      (expect (typep ast2 'ast-function) :to-be-truthy)
      (expect (ast-function-name ast2) :to-be name)))

  (cl-weave:it "Function reference with setf name is preserved through sexp roundtrip."
    (let* ((name '(setf accessor))
           (ast2 (%ast-roundtrip (make-ast-function :name name))))
      (expect (typep ast2 'ast-function) :to-be-truthy)
      (expect (ast-function-name ast2) :to-equal name)))

  ;;; ── Type Declarations ─────────────────────────────────────────────────────

  (cl-weave:it-property "The type declaration (type tag + integer value) is preserved through sexp roundtrip."
      ((value (cl-weave:gen-integer :min -1000 :max 1000))
       (type  (cl-weave:gen-member '(integer fixnum number))))
    (let ((ast2 (%ast-roundtrip (make-ast-the :type  type
                                              :value (make-ast-int :value value)))))
      (expect (typep ast2 'ast-the) :to-be-truthy)
      (expect (ast-the-type ast2) :to-be type)
      (expect (typep (ast-the-value ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-the-value ast2))) :to-be-truthy)))

  ;;; ── Nested Structure ──────────────────────────────────────────────────────

  (cl-weave:it-property "Nested if (outer/inner) preserves all five integer values through sexp roundtrip."
      ((v1 (cl-weave:gen-integer :min 0 :max 1))
       (v2 (cl-weave:gen-integer :min 0 :max 1))
       (v3 (cl-weave:gen-integer :min -10 :max 10))
       (v4 (cl-weave:gen-integer :min -10 :max 10))
       (v5 (cl-weave:gen-integer :min -10 :max 10)))
    (let* ((inner-if (make-ast-if :cond (make-ast-int :value v2)
                                  :then (make-ast-int :value v3)
                                  :else (make-ast-int :value v4)))
           (ast2     (%ast-roundtrip
                      (make-ast-if :cond (make-ast-int :value v1)
                                   :then inner-if
                                   :else (make-ast-int :value v5)))))
      (expect (typep (ast-if-then ast2) 'ast-if) :to-be-truthy)
      (expect (= v1 (ast-int-value (ast-if-cond ast2))) :to-be-truthy)
      (expect (= v2 (ast-int-value (ast-if-cond (ast-if-then ast2)))) :to-be-truthy)
      (expect (= v3 (ast-int-value (ast-if-then (ast-if-then ast2)))) :to-be-truthy)
      (expect (= v4 (ast-int-value (ast-if-else (ast-if-then ast2)))) :to-be-truthy)
      (expect (= v5 (ast-int-value (ast-if-else ast2))) :to-be-truthy)))

  (cl-weave:it-property "Nested binop (* (+ v1 v2) v3) preserves op, nesting, and values through roundtrip."
      ((v1 (cl-weave:gen-integer :min -10 :max 10))
       (v2 (cl-weave:gen-integer :min -10 :max 10))
       (v3 (cl-weave:gen-integer :min -10 :max 10)))
    (let* ((inner (make-ast-binop :op  '+
                                  :lhs (make-ast-int :value v1)
                                  :rhs (make-ast-int :value v2)))
           (ast2  (%ast-roundtrip (make-ast-binop :op  '*
                                                  :lhs inner
                                                  :rhs (make-ast-int :value v3)))))
      (expect (typep ast2 'ast-binop) :to-be-truthy)
      (expect (ast-binop-op ast2) :to-be '*)
      (expect (typep (ast-binop-lhs ast2) 'ast-binop) :to-be-truthy)
      (expect (ast-binop-op (ast-binop-lhs ast2)) :to-be '+)
      (expect (= v1 (ast-int-value (ast-binop-lhs (ast-binop-lhs ast2)))) :to-be-truthy)
      (expect (= v2 (ast-int-value (ast-binop-rhs (ast-binop-lhs ast2)))) :to-be-truthy)
      (expect (= v3 (ast-int-value (ast-binop-rhs ast2))) :to-be-truthy)))

  (cl-weave:it-property "Nested let (outer binds var1, inner binds var2) preserves both through sexp roundtrip."
      ((var1 (gen-pbt-symbol "X"))
       (var2 (gen-pbt-symbol "Y"))
       (v1   (cl-weave:gen-integer :min -10 :max 10))
       (v2   (cl-weave:gen-integer :min -10 :max 10)))
    (let* ((inner-let (make-ast-let
                       :bindings (list (cons var2 (make-ast-int :value v2)))
                       :body     (list (make-ast-var :name var2))))
           (ast2      (%ast-roundtrip
                       (make-ast-let
                        :bindings (list (cons var1 (make-ast-int :value v1)))
                        :body     (list inner-let)))))
      (expect (typep ast2 'ast-let) :to-be-truthy)
      (expect (car (first (ast-let-bindings ast2))) :to-be var1)
      (expect (= v1 (ast-int-value (cdr (first (ast-let-bindings ast2))))) :to-be-truthy)
      (expect (= 1 (length (ast-let-body ast2))) :to-be-truthy)
      (expect (typep (first (ast-let-body ast2)) 'ast-let) :to-be-truthy)
      (expect (car (first (ast-let-bindings (first (ast-let-body ast2))))) :to-be var2))))
