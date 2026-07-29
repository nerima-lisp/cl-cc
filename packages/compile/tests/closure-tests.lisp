;;;; tests/unit/compile/closure-tests.lisp — Unit tests for find-free-variables
;;;;
;;;; Tests the free variable analysis on hand-constructed AST nodes.
;;;; Each test builds an AST directly (no parsing) and checks the result
;;;; of find-free-variables against the expected set of free variables.

(in-package :cl-cc/test)


;;; ─── Literals ─────────────────────────────────────────────────────────────

(it-sequential "free-vars-atomic-forms int"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-int   :value 42))
    (expect (cl-cc/compile::find-free-variables node) :to-equal nil)))

(it-sequential "free-vars-atomic-forms quote"
  (destructuring-bind (node) (list (cl-cc/ast:make-ast-quote :value '(a b c)))
    (expect (cl-cc/compile::find-free-variables node) :to-equal nil)))

;;; ─── Simple references ────────────────────────────────────────────────────

(it-sequential "free-vars-single-var-is-free"
  (expect (cl-cc/compile::find-free-variables
                      (cl-cc/ast:make-ast-var :name 'x)) :to-equal '(x)))

(it-sequential "free-vars-binop-two-distinct-vars"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-binop
                  :op '+
                  :lhs (cl-cc/ast:make-ast-var :name 'x)
                  :rhs (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-list-contains result '(x y) :length 2)))

(it-sequential "free-vars-same-var-deduplicated"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-binop
      :op '+
      :lhs (cl-cc/ast:make-ast-var :name 'x)
      :rhs (cl-cc/ast:make-ast-var :name 'x))) :to-equal '(x)))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(it-sequential "free-vars-let-bound-var-not-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
      :body     (list (cl-cc/ast:make-ast-var :name 'x)))) :to-equal nil))

(it-sequential "free-vars-let-binding-expr-is-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-var :name 'y)))
      :body     (list (cl-cc/ast:make-ast-var :name 'x)))) :to-equal '(y)))

(it-sequential "free-vars-let-unbound-body-var-is-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
      :body     (list (cl-cc/ast:make-ast-binop
                       :op '+
                       :lhs (cl-cc/ast:make-ast-var :name 'x)
                       :rhs (cl-cc/ast:make-ast-var :name 'z))))) :to-equal '(z)))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(it-sequential "free-vars-lambda-all-params-bound"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x y)
      :body (list (cl-cc/ast:make-ast-binop
                   :op '+
                   :lhs (cl-cc/ast:make-ast-var :name 'x)
                   :rhs (cl-cc/ast:make-ast-var :name 'y))))) :to-equal nil))

(it-sequential "free-vars-lambda-outer-var-is-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x)
      :body (list (cl-cc/ast:make-ast-binop
                   :op '+
                   :lhs (cl-cc/ast:make-ast-var :name 'x)
                   :rhs (cl-cc/ast:make-ast-var :name 'z))))) :to-equal '(z)))

(it-sequential "free-vars-lambda-rest-param-shadows-body"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x)
      :rest-param 'rest
      :body (list (cl-cc/ast:make-ast-var :name 'rest)))) :to-equal nil))

(it-sequential "free-vars-lambda-optional-param-shadows-body"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params nil
      :optional-params (list (list 'a (cl-cc/ast:make-ast-int :value 0)))
      :body (list (cl-cc/ast:make-ast-var :name 'a)))) :to-equal nil))

(it-sequential "free-vars-optional-param-default-is-free-var"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc/ast:make-ast-var :name 'z)))
                  :body (list (cl-cc/ast:make-ast-var :name 'a))))))
    (expect result :to-equal '(z))))

(it-sequential "free-vars-keyword-param-is-not-free-in-body"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc/ast:make-ast-var :name 'k))))))
    (expect result :to-equal nil)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(it-sequential "free-vars-nested-let"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-let
                  :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
                  :body (list
                         (cl-cc/ast:make-ast-let
                          :bindings (list (cons 'y (cl-cc/ast:make-ast-var :name 'x)))
                          :body (list (cl-cc/ast:make-ast-binop
                                       :op '+
                                       :lhs (cl-cc/ast:make-ast-var :name 'y)
                                       :rhs (cl-cc/ast:make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (expect result :to-equal '(w))))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(it-sequential "free-vars-defun-shadows-params"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc/ast:make-ast-binop
                               :op '+
                               :lhs (cl-cc/ast:make-ast-var :name 'a)
                               :rhs (cl-cc/ast:make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (expect result :to-equal '(c))))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(it-sequential "free-vars-setq"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-setq
                  :var 'x
                  :value (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-list-contains result '(x y) :length 2)))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(it-sequential "free-vars-call-symbol-func"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-call
      :func 'foo
      :args (list (cl-cc/ast:make-ast-var :name 'x)))) :to-equal '(x)))

(it-sequential "free-vars-call-ast-func-node"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-var :name 'f)
                  :args (list (cl-cc/ast:make-ast-var :name 'x))))))
    (assert-list-contains result '(f x) :length 2)))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(it-sequential "free-vars-flet-bound-name-not-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-flet
      :bindings (list (list 'my-fn '(a) (cl-cc/ast:make-ast-var :name 'a)))
      :body (list (cl-cc/ast:make-ast-call
                   :func 'my-fn
                   :args (list (cl-cc/ast:make-ast-int :value 1)))))) :to-equal nil))

(it-sequential "free-vars-labels-outer-var-in-binding-is-free"
  (expect (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-labels
      :bindings (list (list 'rec '(n) (cl-cc/ast:make-ast-var :name 'limit)))
      :body (list (cl-cc/ast:make-ast-call
                   :func 'rec
                   :args (list (cl-cc/ast:make-ast-int :value 0)))))) :to-equal '(limit)))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(it-sequential "free-vars-if-collects-all-branches"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'p)
                  :then (cl-cc/ast:make-ast-var :name 'x)
                  :else (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-list-contains result '(p x y) :length 3)))

(it-sequential "free-vars-progn-collects-all-forms"
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-progn
                  :forms (list (cl-cc/ast:make-ast-var :name 'a)
                               (cl-cc/ast:make-ast-var :name 'b))))))
    (assert-list-contains result '(a b) :length 2)))

;;; ─── %escape-add-kind / %escape-merge-kinds (extracted pure helpers) ─────

(it-sequential "escape-add-kind-deduplicates fresh-add"
  (destructuring-bind (kind acc expected) (list :return nil '(:return))
    (expect (cl-cc/ast::%escape-add-kind kind acc) :to-equal expected)))

(it-sequential "escape-add-kind-deduplicates already-there"
  (destructuring-bind (kind acc expected) (list :return '(:return) '(:return))
    (expect (cl-cc/ast::%escape-add-kind kind acc) :to-equal expected)))

(it-sequential "escape-add-kind-deduplicates second-kind"
  (destructuring-bind (kind acc expected) (list :capture '(:return) '(:capture :return))
    (expect (cl-cc/ast::%escape-add-kind kind acc) :to-equal expected)))

(it-sequential "escape-merge-kinds-merges-multiple-lists"
  (expect (cl-cc/ast::%escape-merge-kinds nil nil) :to-equal nil)
  (let ((result (cl-cc/ast::%escape-merge-kinds '(:return) '(:return :capture))))
    (assert-list-contains result '(:return :capture) :length 2)))

;;; ─── %count-ast-calls (extracted recursive helper) ───────────────────────

(it-sequential "count-ast-calls-direct-match-returns-one"
  (let ((node (cl-cc/ast:make-ast-call
               :func 'my-fn
               :args (list (cl-cc/ast:make-ast-int :value 1)))))
    (expect (= 1 (cl-cc/ast::%count-ast-calls node 'my-fn)) :to-be-truthy)))

(it-sequential "count-ast-calls-no-match-returns-zero"
  (let ((node (cl-cc/ast:make-ast-call
               :func 'other-fn
               :args (list (cl-cc/ast:make-ast-int :value 1)))))
    (expect (= 0 (cl-cc/ast::%count-ast-calls node 'my-fn)) :to-be-truthy)))

(it-sequential "count-ast-calls-counts-across-nested-children"
  (let* ((inner (cl-cc/ast:make-ast-call :func 'my-fn :args nil))
         (outer (cl-cc/ast:make-ast-progn :forms (list inner inner))))
    (expect (= 2 (cl-cc/ast::%count-ast-calls outer 'my-fn)) :to-be-truthy)))

;;; ─── %escape-mentions-node-p / %escape-mentions-forms-p ─────────────────

(it-sequential "escape-mentions-node-p-cases match"
  (destructuring-bind (node binding expected) (list (cl-cc/ast:make-ast-var :name 'x) 'x t)
    (if expected
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-truthy)
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-falsy))))

(it-sequential "escape-mentions-node-p-cases no-match"
  (destructuring-bind (node binding expected) (list (cl-cc/ast:make-ast-var :name 'y) 'x nil)
    (if expected
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-truthy)
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-falsy))))

(it-sequential "escape-mentions-node-p-cases literal"
  (destructuring-bind (node binding expected) (list (cl-cc/ast:make-ast-int :value 1) 'x nil)
    (if expected
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-truthy)
      (expect (cl-cc/ast::%escape-mentions-node-p node binding) :to-be-falsy))))

(it-sequential "escape-mentions-forms-p-returns-true-when-a-form-matches"
  (expect (cl-cc/ast::%escape-mentions-forms-p
                (list (cl-cc/ast:make-ast-var :name 'x)) 'x) :to-be-truthy))

(it-sequential "escape-mentions-forms-p-returns-false-when-no-form-matches"
  (expect (cl-cc/ast::%escape-mentions-forms-p
                 (list (cl-cc/ast:make-ast-int :value 1)) 'x) :to-be-falsy))

(it-sequential "escape-mentions-forms-p-returns-false-for-nil-list"
  (expect (cl-cc/ast::%escape-mentions-forms-p nil 'x) :to-be-falsy))

;;; ─── %escape-classify-children ──────────────────────────────────────────

(it-sequential "escape-classify-children-returns-nil-when-no-child-matches"
  (let* ((child  (cl-cc/ast:make-ast-int :value 42))
         (parent (cl-cc/ast:make-ast-progn :forms (list child))))
    (expect (cl-cc/ast::%escape-classify-children parent 'x nil) :to-be-null)))

(it-sequential "escape-classify-children-reports-return-when-child-is-matching-var"
  (let* ((child  (cl-cc/ast:make-ast-var :name 'x))
         (parent (cl-cc/ast:make-ast-progn :forms (list child))))
    (let ((kinds (cl-cc/ast::%escape-classify-children parent 'x nil)))
      (expect (member :return kinds) :to-be-truthy))))

;;; ─── %escape-capture-kinds ───────────────────────────────────────────────

(it-sequential "escape-capture-kinds-returns-nil-when-body-does-not-reference-binding"
  (let ((body (list (cl-cc/ast:make-ast-int :value 0))))
    (expect (cl-cc/ast::%escape-capture-kinds body 'x nil) :to-be-null)))

(it-sequential "escape-capture-kinds-returns-capture-when-body-references-binding"
  (let ((body (list (cl-cc/ast:make-ast-var :name 'x))))
    (let ((kinds (cl-cc/ast::%escape-capture-kinds body 'x nil)))
      (expect (member :capture kinds) :to-be-truthy))))

;;; ─── %escape-classify / binding-escape-kinds-in-body ────────────────────

(it-sequential "escape-classify-matching-var-yields-return"
  (let ((node (cl-cc/ast:make-ast-var :name 'x)))
    (expect (cl-cc/ast::%escape-classify node 'x nil) :to-equal '(:return))))

(it-sequential "escape-classify-non-matching-var-yields-nil"
  (let ((node (cl-cc/ast:make-ast-var :name 'y)))
    (expect (cl-cc/ast::%escape-classify node 'x nil) :to-be-null)))

(it-sequential "binding-escape-kinds-detects-capture-via-lambda"
  (let* ((body (list (cl-cc/ast:make-ast-lambda
                      :params '(z)
                      :body   (list (cl-cc/ast:make-ast-var :name 'x))))))
    (let ((kinds (cl-cc/compile::binding-escape-kinds-in-body body 'x)))
      (expect (member :capture kinds) :to-be-truthy))))

(it-sequential "binding-escape-kinds-returns-nil-for-empty-body"
  (expect (cl-cc/compile::binding-escape-kinds-in-body nil 'x) :to-be-null))
