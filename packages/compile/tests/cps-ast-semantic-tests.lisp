;;;; tests/unit/compile/cps-ast-semantic-tests.lisp — CPS AST semantic evaluation tests

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — semantic (evaluable forms)
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-ast-binop add"
  (destructuring-bind (op lhs rhs expected) (list '+ 3 4 7)
    (let ((ast (cl-cc:make-ast-binop :op op
                                   :lhs (cl-cc:make-ast-int :value lhs)
                                   :rhs (cl-cc:make-ast-int :value rhs))))
    (expect (= expected (run-cps-ast ast)) :to-be-truthy))))

(it-sequential "cps-ast-binop sub"
  (destructuring-bind (op lhs rhs expected) (list '- 9 4 5)
    (let ((ast (cl-cc:make-ast-binop :op op
                                   :lhs (cl-cc:make-ast-int :value lhs)
                                   :rhs (cl-cc:make-ast-int :value rhs))))
    (expect (= expected (run-cps-ast ast)) :to-be-truthy))))

(it-sequential "cps-ast-binop mul"
  (destructuring-bind (op lhs rhs expected) (list '* 3 4 12)
    (let ((ast (cl-cc:make-ast-binop :op op
                                   :lhs (cl-cc:make-ast-int :value lhs)
                                   :rhs (cl-cc:make-ast-int :value rhs))))
    (expect (= expected (run-cps-ast ast)) :to-be-truthy))))

(it-sequential "cps-ast-if-branch truthy-takes-then"
  (destructuring-bind (cond-val then-val else-val expected) (list 1 10 20 10)
    (let ((ast (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value cond-val)
                                :then (cl-cc:make-ast-int :value then-val)
                                :else (cl-cc:make-ast-int :value else-val))))
    (expect (= expected (run-cps-ast ast)) :to-be-truthy))))

(it-sequential "cps-ast-if-branch nil-takes-else"
  (destructuring-bind (cond-val then-val else-val expected) (list nil 10 20 20)
    (let ((ast (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value cond-val)
                                :then (cl-cc:make-ast-int :value then-val)
                                :else (cl-cc:make-ast-int :value else-val))))
    (expect (= expected (run-cps-ast ast)) :to-be-truthy))))

(it-sequential "cps-evaluable-forms integer"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-int :value 42) 42)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms progn"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-progn
                           :forms (list (cl-cc:make-ast-int :value 1)
                                        (cl-cc:make-ast-int :value 2)
                                        (cl-cc:make-ast-int :value 99))) 99)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms let"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-let
                           :bindings (list (cons 'x (cl-cc:make-ast-int :value 3))
                                           (cons 'y (cl-cc:make-ast-int :value 4)))
                           :body (list (cl-cc:make-ast-binop
                                        :op '+
                                        :lhs (cl-cc:make-ast-var :name 'x)
                                        :rhs (cl-cc:make-ast-var :name 'y)))) 7)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms print"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-print :expr (cl-cc:make-ast-int :value 42)) 42)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms quote-symbol"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-quote :value 'hello) 'hello)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms quote-list"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-quote :value '(1 2 3)) '(1 2 3))
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-evaluable-forms the"
  (destructuring-bind (ast expected) (list (cl-cc:make-ast-the :type 'integer
                                              :value (cl-cc:make-ast-int :value 7)) 7)
    (expect (run-cps-ast ast) :to-equal expected)))

(it-sequential "cps-ast-setq-returns-value"
  (let ((setq-ast (cl-cc:make-ast-setq
                   :var 'cl-cc-test-setq-var
                   :value (cl-cc:make-ast-int :value 55))))
    ;; Bind the target var so SBCL doesn't complain about an unbound special
    (let ((cl-cc-test-setq-var nil))
      (declare (special cl-cc-test-setq-var))
      (handler-bind ((warning #'muffle-warning))
        (let ((result (run-cps-ast setq-ast)))
          (expect (= 55 result) :to-be-truthy)
          (expect (= 55 cl-cc-test-setq-var) :to-be-truthy))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — structural ("is it a CPS lambda?")
;;; These forms are transformed correctly but cannot be trivially evaluated
;;; because they involve non-local control (block, go, throw) or closures.
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "cps-ast-structural-shape lambda"
  (destructuring-bind (ast) (list (cl-cc:make-ast-lambda
     :params '(x)
     :body (list (cl-cc:make-ast-int :value 1))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape block"
  (destructuring-bind (ast) (list (cl-cc:make-ast-block
     :name 'b
     :body (list (cl-cc:make-ast-int :value 1))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape return-from"
  (destructuring-bind (ast) (list (cl-cc:make-ast-return-from
     :name 'b
     :value (cl-cc:make-ast-int :value 1)))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape tagbody"
  (destructuring-bind (ast) (list (cl-cc:make-ast-tagbody
     :tags (list (cons 'tag1 (list (cl-cc:make-ast-int :value 1))))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape go"
  (destructuring-bind (ast) (list (cl-cc:make-ast-go :tag 'tag1))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape catch"
  (destructuring-bind (ast) (list (cl-cc:make-ast-catch
     :tag  (cl-cc:make-ast-var :name 'my-tag)
     :body (list (cl-cc:make-ast-int :value 42))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape throw"
  (destructuring-bind (ast) (list (cl-cc:make-ast-throw
     :tag   (cl-cc:make-ast-var :name 'my-tag)
     :value (cl-cc:make-ast-int :value 42)))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape unwind-protect"
  (destructuring-bind (ast) (list (cl-cc:make-ast-unwind-protect
     :protected (cl-cc:make-ast-int :value 42)
     :cleanup   (list (cl-cc:make-ast-int :value 0))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape flet"
  (destructuring-bind (ast) (list (cl-cc:make-ast-flet
     :bindings (list (list 'double '(x)
                           (cl-cc:make-ast-binop
                            :op '*
                            :lhs (cl-cc:make-ast-int :value 2)
                            :rhs (cl-cc:make-ast-var :name 'x))))
     :body (list (cl-cc:make-ast-int :value 1))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))

(it-sequential "cps-ast-structural-shape labels"
  (destructuring-bind (ast) (list (cl-cc:make-ast-labels
     :bindings (list (list 'id '(x) (cl-cc:make-ast-var :name 'x)))
     :body (list (cl-cc:make-ast-int :value 1))))
    (expect (is-cps-lambda (cl-cc:cps-transform-ast* ast)) :to-be-truthy)))
