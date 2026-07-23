;;;; tests/pbt/ast-pbt-tests.lisp - Property-Based Tests for AST Roundtrip
(in-package :cl-cc/pbt)


(defun %ast-roundtrip (ast)
  "Convert AST to sexp and parse it back."
  (lower-sexp-to-ast (ast-to-sexp ast)))

;;; ── Atomic Nodes ────────────────────────────────────────────────────────────

(it-sequential "ast-atomic-roundtrip-cases"
  (for-all ((value (gen-fn (gen-integer :min -10000 :max 10000))))
    (let ((ast2 (%ast-roundtrip (make-ast-int :value value))))
      (expect (typep ast2 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value ast2)) :to-be-truthy)))
  (for-all ((name (gen-fn (gen-symbol :package nil :prefix "VAR"))))
    (let ((ast2 (%ast-roundtrip (make-ast-var :name name))))
      (expect (typep ast2 'ast-var) :to-be-truthy)
      (expect (ast-var-name ast2) :to-be name))))

(it-sequential "ast-quote-roundtrip"
  (for-all ((value (gen-fn (gen-one-of '(nil t 42 "string" (a b c))))))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value value))))
      (expect (typep ast2 'ast-quote) :to-be-truthy)
      (expect (ast-quote-value ast2) :to-equal value)))
  (for-all ((sym (gen-fn (gen-symbol :package nil :prefix "SYM"))))
    (let ((ast2 (%ast-roundtrip (make-ast-quote :value sym))))
      (expect (typep ast2 'ast-quote) :to-be-truthy)
      (expect (ast-quote-value ast2) :to-be sym)))
  (let* ((value '(a (b c) d))
         (ast2 (%ast-roundtrip (make-ast-quote :value value))))
    (expect (typep ast2 'ast-quote) :to-be-truthy)
    (expect (ast-quote-value ast2) :to-equal value)))

;;; ── Compound Nodes ──────────────────────────────────────────────────────────

(it-sequential "ast-binop-roundtrip"
  (for-all ((op      (gen-fn (gen-one-of '(+ - *))))
            (lhs-val (gen-fn (gen-integer :min -100 :max 100)))
            (rhs-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((ast  (make-ast-binop :op op
                                 :lhs (make-ast-int :value lhs-val)
                                 :rhs (make-ast-int :value rhs-val)))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-binop) :to-be-truthy)
      (expect (ast-binop-op ast2) :to-be op)
      (expect (typep (ast-binop-lhs ast2) 'ast-int) :to-be-truthy)
      (expect (typep (ast-binop-rhs ast2) 'ast-int) :to-be-truthy)
      (expect (= lhs-val (ast-int-value (ast-binop-lhs ast2))) :to-be-truthy)
      (expect (= rhs-val (ast-int-value (ast-binop-rhs ast2))) :to-be-truthy))))

(it-sequential "ast-if-roundtrip"
  (for-all ((cond-val (gen-fn (gen-integer :min 0 :max 1)))
            (then-val (gen-fn (gen-integer :min -100 :max 100)))
            (else-val (gen-fn (gen-integer :min -100 :max 100))))
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
      (expect (= else-val (ast-int-value (ast-if-else ast2))) :to-be-truthy))))

(it-sequential "ast-progn-roundtrip"
  (for-all ((vals (gen-fn (gen-list-of (gen-integer :min -100 :max 100)
                                       :min-length 1 :max-length 5))))
    (let* ((ast  (make-ast-progn :forms (mapcar (lambda (v) (make-ast-int :value v)) vals)))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-progn) :to-be-truthy)
      (expect (= (length vals) (length (ast-progn-forms ast2))) :to-be-truthy)
      (expect (every (lambda (f) (typep f 'ast-int)) (ast-progn-forms ast2)) :to-be-truthy)
      (expect (mapcar #'ast-int-value (ast-progn-forms ast2)) :to-equal vals))))

(it-sequential "ast-print-roundtrip"
  (for-all ((value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let ((ast2 (%ast-roundtrip (make-ast-print :expr (make-ast-int :value value)))))
      (expect (typep ast2 'ast-print) :to-be-truthy)
      (expect (typep (ast-print-expr ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-print-expr ast2))) :to-be-truthy))))

;;; ── Binding Forms ───────────────────────────────────────────────────────────

(it-sequential "ast-let-roundtrip"
  (for-all ((var-name  (gen-fn (gen-symbol :package nil :prefix "VAR")))
            (init-val  (gen-fn (gen-integer :min -100 :max 100)))
            (body-val  (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((ast  (make-ast-let
                  :bindings (list (cons var-name (make-ast-int :value init-val)))
                  :body     (list (make-ast-var :name var-name)
                                  (make-ast-int :value body-val))))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-let) :to-be-truthy)
      (expect (= 1 (length (ast-let-bindings ast2))) :to-be-truthy)
      (expect (car   (first (ast-let-bindings ast2))) :to-be var-name)
      (expect (= init-val (ast-int-value (cdr (first (ast-let-bindings ast2))))) :to-be-truthy)
      (expect (= 2 (length (ast-let-body ast2))) :to-be-truthy)))
  (for-all ((body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-let :bindings nil
                                              :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-let) :to-be-truthy)
      (expect (ast-let-bindings ast2) :to-be-null)
      (expect (= 1 (length (ast-let-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-let-body ast2)))) :to-be-truthy))))

(it-sequential "ast-lambda-roundtrip"
  (for-all ((params   (gen-fn (gen-list-of (gen-symbol :package nil :prefix "ARG")
                                           :min-length 0 :max-length 4)))
            (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-lambda :params params
                                                 :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-lambda) :to-be-truthy)
      (expect (ast-lambda-params ast2) :to-equal params)
      (expect (= 1 (length (ast-lambda-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-lambda-body ast2)))) :to-be-truthy))))

(it-sequential "ast-flet-roundtrip"
  (for-all ((fn-name  (gen-fn (gen-symbol :package nil :prefix "FN")))
            (param    (gen-fn (gen-symbol :package nil :prefix "ARG")))
            (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((ast  (make-ast-flet
                  :bindings (list (list* fn-name (list param)
                                         (list (make-ast-int :value body-val))))
                  :body     (list (make-ast-var :name fn-name))))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-flet) :to-be-truthy)
      (expect (= 1 (length (ast-flet-bindings ast2))) :to-be-truthy)
      (expect (first  (first (ast-flet-bindings ast2))) :to-be fn-name)
      (expect (second (first (ast-flet-bindings ast2))) :to-equal (list param))
      (expect (= body-val (ast-int-value (third (first (ast-flet-bindings ast2))))) :to-be-truthy))))

(it-sequential "ast-labels-roundtrip"
  (for-all ((fn1-name (gen-fn (gen-symbol :package nil :prefix "FN1")))
            (fn2-name (gen-fn (gen-symbol :package nil :prefix "FN2")))
            (param    (gen-fn (gen-symbol :package nil :prefix "ARG")))
            (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let* ((body-ast (make-ast-int :value body-val))
           (ast  (make-ast-labels
                  :bindings (list (list* fn1-name (list param) (list body-ast))
                                  (list* fn2-name (list param) (list body-ast)))
                  :body     (list (make-ast-var :name fn1-name))))
           (ast2 (%ast-roundtrip ast)))
      (expect (typep ast2 'ast-labels) :to-be-truthy)
      (expect (= 2 (length (ast-labels-bindings ast2))) :to-be-truthy)
      (expect (first (first  (ast-labels-bindings ast2))) :to-be fn1-name)
      (expect (first (second (ast-labels-bindings ast2))) :to-be fn2-name))))

;;; ── Control Flow ────────────────────────────────────────────────────────────

(it-sequential "ast-block-return-from-cases"
  (for-all ((name     (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
            (body-val (gen-fn (gen-integer :min -100 :max 100))))
    (let ((ast2 (%ast-roundtrip (make-ast-block :name name
                                                :body (list (make-ast-int :value body-val))))))
      (expect (typep ast2 'ast-block) :to-be-truthy)
      (expect (ast-block-name ast2) :to-be name)
      (expect (= 1 (length (ast-block-body ast2))) :to-be-truthy)
      (expect (= body-val (ast-int-value (first (ast-block-body ast2)))) :to-be-truthy)))
  (for-all ((name  (gen-fn (gen-symbol :package nil :prefix "BLOCK")))
            (value (gen-fn (gen-integer :min -1000 :max 1000))))
    (let ((ast2 (%ast-roundtrip (make-ast-return-from :name  name
                                                      :value (make-ast-int :value value)))))
      (expect (typep ast2 'ast-return-from) :to-be-truthy)
      (expect (ast-return-from-name ast2) :to-be name)
      (expect (typep (ast-return-from-value ast2) 'ast-int) :to-be-truthy)
      (expect (= value (ast-int-value (ast-return-from-value ast2))) :to-be-truthy))))

(it-sequential "ast-tagbody-go-cases"
  (for-all ((tag-val (gen-fn (gen-integer :min 0 :max 100))))
    (let ((ast2 (%ast-roundtrip
                 (make-ast-tagbody :tags (list (cons tag-val (list (make-ast-var :name 'x))))))))
      (expect (typep ast2 'ast-tagbody) :to-be-truthy)
      (expect (null (ast-tagbody-tags ast2)) :to-be-falsy)))
  (for-all ((tag (gen-fn (gen-symbol :package nil :prefix "TAG"))))
    (let ((ast2 (%ast-roundtrip (make-ast-go :tag tag))))
      (expect (typep ast2 'ast-go) :to-be-truthy)
      (expect (ast-go-tag ast2) :to-be tag)))
  (let ((ast2 (%ast-roundtrip (make-ast-go :tag 42))))
    (expect (typep ast2 'ast-go) :to-be-truthy)
    (expect (= 42 (ast-go-tag ast2)) :to-be-truthy)))

