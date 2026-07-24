;;;; tests/unit/ast/ast-tests.lisp — AST Node Roundtrip Tests
;;;;
;;;; Verifies that ast-to-sexp and lower-sexp-to-ast are inverses:
;;;;   AST -> sexp -> AST preserves structural identity.
;;;;
;;;; Each case provides (ast-node verify-fn) where verify-fn checks
;;;; the relevant structural property of the reconstructed node.

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; Helper: shared roundtrip combinator
;;; ─────────────────────────────────────────────────────────────────────────

(defun %ast-roundtrip (ast)
  "Convert AST → sexp → AST and return the reconstructed node."
  (cl-cc:lower-sexp-to-ast (cl-cc:ast-to-sexp ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Primitive nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-primitives int"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-int :value 42) (lambda (ast2) (expect (= 42 (cl-cc:ast-int-value ast2)) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-primitives var"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-var :name 'x) (lambda (ast2) (expect (cl-cc:ast-var-name ast2) :to-be 'x)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-primitives hole"
  (destructuring-bind (ast verify) (list (cl-cc/ast:make-ast-hole) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-hole) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-primitives quote-atom"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-quote :value 'hello) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-quote) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-primitives quote-list"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-quote :value '(x y)) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-quote) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Expression nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-expressions binop"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-binop :op '+
                          :lhs (cl-cc:make-ast-int :value 1)
                          :rhs (cl-cc:make-ast-int :value 2)) (lambda (ast2) (expect (cl-cc:ast-binop-op ast2) :to-be '+)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-expressions if"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value 1)
                       :then (cl-cc:make-ast-int :value 2)
                       :else (cl-cc:make-ast-int :value 3)) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-if) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-expressions progn"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-progn :forms (list (cl-cc:make-ast-int :value 1)
                                       (cl-cc:make-ast-int :value 2))) (lambda (ast2) (expect (= 2 (length (cl-cc:ast-progn-forms ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-expressions setq"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-setq :var 'x :value (cl-cc:make-ast-int :value 42)) (lambda (ast2) (expect (cl-cc:ast-setq-var ast2) :to-be 'x)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Binding nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-bindings let"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-let :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                        :body     (list (cl-cc:make-ast-var :name 'x))) (lambda (ast2) (expect (= 1 (length (cl-cc:ast-let-bindings ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-bindings lambda"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-lambda :params (list 'x)
                           :body   (list (cl-cc:make-ast-var :name 'x))) (lambda (ast2) (expect (= 1 (length (cl-cc:ast-lambda-params ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-bindings flet"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-flet
     :bindings (list (list 'double '(x)
                           (cl-cc:make-ast-binop
                            :op '* :lhs (cl-cc:make-ast-int :value 2)
                            :rhs (cl-cc:make-ast-var :name 'x))))
     :body (list (cl-cc:make-ast-int :value 1))) (lambda (ast2) (expect (= 1 (length (cl-cc:ast-flet-bindings ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-bindings labels"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-labels
     :bindings (list (list 'id '(x) (cl-cc:make-ast-var :name 'x)))
     :body (list (cl-cc:make-ast-int :value 1))) (lambda (ast2) (expect (= 1 (length (cl-cc:ast-labels-bindings ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Control flow nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-control-flow block"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-block :name 'loop
                          :body (list (cl-cc:make-ast-int :value 1))) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-block) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-control-flow return-from"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-return-from :name 'loop
                                :value (cl-cc:make-ast-int :value 42)) (lambda (ast2) (expect (cl-cc:ast-return-from-name ast2) :to-be 'loop)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-control-flow tagbody"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-tagbody :tags (list (cons 'start (list (cl-cc:make-ast-int :value 1))))) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-tagbody) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-control-flow go"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-go :tag 'start) (lambda (ast2) (expect (cl-cc:ast-go-tag ast2) :to-be 'start)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Exception handling nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-exceptions catch"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-catch :tag  (cl-cc:make-ast-var :name 'my-tag)
                          :body (list (cl-cc:make-ast-int :value 42))) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-catch) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-exceptions throw"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-throw :tag   (cl-cc:make-ast-var :name 'my-tag)
                          :value (cl-cc:make-ast-int :value 42)) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-throw) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-exceptions unwind-protect"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-unwind-protect :protected (cl-cc:make-ast-int :value 1)
                                   :cleanup   (list (cl-cc:make-ast-int :value 0))) (lambda (ast2) (expect (typep ast2 'cl-cc:ast-unwind-protect) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Multiple-values nodes
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-roundtrip-multiple-values values"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)
                                        (cl-cc:make-ast-int :value 2))) (lambda (ast2) (expect (= 2 (length (cl-cc:ast-values-forms ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

(it-sequential "ast-roundtrip-multiple-values multiple-value-bind"
  (destructuring-bind (ast verify) (list (cl-cc:make-ast-multiple-value-bind
     :vars        '(a b)
     :values-form (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)
                                                      (cl-cc:make-ast-int :value 2)))
     :body        (list (cl-cc:make-ast-var :name 'a))) (lambda (ast2) (expect (= 2 (length (cl-cc:ast-mvb-vars ast2))) :to-be-truthy)))
    (funcall verify (%ast-roundtrip ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Source location utility
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "ast-location-string-cases full"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-int :value 1 :source-file "foo.lisp" :source-line 10 :source-column 5) "foo.lisp:10:5")
    (expect (cl-cc:ast-location-string node) :to-equal expected)))

(it-sequential "ast-location-string-cases file-line"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-int :value 1 :source-file "foo.lisp" :source-line 3) "foo.lisp:3")
    (expect (cl-cc:ast-location-string node) :to-equal expected)))

(it-sequential "ast-location-string-cases unknown"
  (destructuring-bind (node expected) (list (cl-cc:make-ast-int :value 1) "<unknown location>")
    (expect (cl-cc:ast-location-string node) :to-equal expected)))

(it-sequential "ast-error-signals-condition"
  (let ((node (cl-cc:make-ast-int :value 1 :source-file "t.lisp" :source-line 1)))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc:ast-error node "test error ~A" 42)) (cl-cc:ast-compilation-error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))

(it-sequential "ast-node-namespace-and-imports-metadata"
  (let ((node (cl-cc:make-ast-int :value 1
                                  :namespace "App\\Example"
                                  :imports '("Vendor\\Library"))))
    (expect (cl-cc/ast:ast-namespace node) :to-equal "App\\Example")
    (expect (cl-cc/ast:ast-imports node) :to-equal '("Vendor\\Library"))
    (setf (cl-cc/ast:ast-namespace node) "App\\Updated"
          (cl-cc/ast:ast-imports node) '("Vendor\\Other"))
    (expect (cl-cc/ast:ast-namespace node) :to-equal "App\\Updated")
    (expect (cl-cc/ast:ast-imports node) :to-equal '("Vendor\\Other"))))
