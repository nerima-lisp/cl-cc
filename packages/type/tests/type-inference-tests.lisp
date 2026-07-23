(in-package :cl-cc/test)


;;; Type Inference Tests

(it-sequential "infer-forms-return-type-int integer-literal"
  (destructuring-bind (form) (list '42)
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int binop-addition"
  (destructuring-bind (form) (list '(+ 1 2))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int binop-nested"
  (destructuring-bind (form) (list '(+ (* 2 3) (- 4 1)))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int let-simple"
  (destructuring-bind (form) (list '(let ((x 42)) x))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int let-multi-binop"
  (destructuring-bind (form) (list '(let ((x 10) (y 20)) (+ x y)))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int function-call"
  (destructuring-bind (form) (list '(let ((f (lambda (x) (+ x 1)))) (f 5)))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int print-expr"
  (destructuring-bind (form) (list '(print 42))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int progn-last"
  (destructuring-bind (form) (list '(progn 1 2 3))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-forms-return-type-int let-poly-identity"
  (destructuring-bind (form) (list '(let ((id (lambda (x) x))) (id 42)))
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))))

(it-sequential "infer-env-and-control-flow variable-from-env"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((ast (lower-sexp-to-ast 'x))
                    (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
               (multiple-value-bind (ty subst) (infer ast env)
                 (declare (ignore subst))
                 (assert-type-equal ty type-int)))))
    (reset-type-vars!) (funcall verify)))

(it-sequential "infer-env-and-control-flow if-expression"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
                    (env (type-env-extend 'cond-var
                                          (type-to-scheme type-bool)
                                          (type-env-empty))))
               (multiple-value-bind (ty subst) (infer ast env)
                 (declare (ignore subst))
                 (assert-type-equal ty type-int)))))
    (reset-type-vars!) (funcall verify)))

(it-sequential "infer-type-error-signals unbound-var"
  (destructuring-bind (verify) (list (lambda ()
             (let ((ast (lower-sexp-to-ast 'undefined-var)))
               (signals unbound-variable-error (infer-with-env ast)))))
    (reset-type-vars!) (funcall verify)))

(it-sequential "infer-type-error-signals typed-hole"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((ast (lower-sexp-to-ast '(+ x _)))
                    (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
               (signals cl-cc/type::typed-hole-error (infer ast env)))))
    (reset-type-vars!) (funcall verify)))

(it-sequential "infer-lambda"
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (typep ty 'type-arrow) :to-be-truthy)
      (expect (= 1 (length (type-arrow-params ty))) :to-be-truthy)))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (expect (typep ty 'type-arrow) :to-be-truthy)
      (assert-type-equal (type-arrow-return ty) type-int)
      (assert-type-equal (first (type-arrow-params ty)) type-int))))


(it-sequential "infer-quote-type symbol"
  (destructuring-bind (form expected-type) (list '(quote hello) type-symbol)
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type)))))

(it-sequential "infer-quote-type integer"
  (destructuring-bind (form expected-type) (list '(quote 42) type-int)
    (reset-type-vars!) (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type)))))

;;; ─── syntactic-value-p (value restriction) ──────────────────────────────────

(it-sequential "infer-syntactic-value-p-truthy int"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-int      :value 42))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-truthy var"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-var       :name 'x))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-truthy lambda"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-lambda    :params '(x) :body nil))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-truthy quote"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-quote     :value 'foo))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-truthy function"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-function  :name 'f))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-truthy hole"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-hole))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-truthy)))

(it-sequential "infer-syntactic-value-p-falsy call"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-call   :func 'f :args nil))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))

(it-sequential "infer-syntactic-value-p-falsy binop"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-binop  :op '+ :lhs (cl-cc/ast:make-ast-int :value 1)
                                                        :rhs (cl-cc/ast:make-ast-int :value 2)))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))

(it-sequential "infer-syntactic-value-p-falsy if"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-if     :cond (cl-cc/ast:make-ast-var :name 'c)
                                                 :then (cl-cc/ast:make-ast-int :value 1)
                                                 :else (cl-cc/ast:make-ast-int :value 2)))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))

(it-sequential "infer-syntactic-value-p-falsy let"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-let    :bindings nil :body nil))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))

(it-sequential "infer-syntactic-value-p-falsy progn"
  (destructuring-bind (ast) (list (cl-cc/ast:make-ast-progn  :forms nil))
    (expect (cl-cc/type:syntactic-value-p ast) :to-be-falsy)))

;;; ─── infer-if type narrowing ──────────────────────────────────────────────────

(it-sequential "infer-if-narrows-type-in-then-branch"
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if (numberp x) (+ x 1) 0)))
         (env (type-env-extend 'x
               (make-type-scheme nil
                 (make-type-union (list type-int type-string)))
               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (expect ty :to-be-truthy))))

(it-sequential "infer-if-no-narrowing-without-predicate"
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if flag 1 2)))
         (env (type-env-extend 'flag (type-to-scheme type-bool) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

;;; Generalization / Instantiation Tests

(it-sequential "generalize-and-scheme-ops nil-env-quantifies-all"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((v  (fresh-type-var :name 'a))
                    (fn (make-type-arrow-raw :params (list v) :return v))
                    (s  (generalize nil fn)))
               (expect (typep s 'type-scheme) :to-be-truthy)
               (expect (= 1 (length (type-scheme-quantified-vars s))) :to-be-truthy))))
    (funcall verify)))

(it-sequential "generalize-and-scheme-ops non-nil-env-excludes"
  (destructuring-bind (verify) (list (lambda ()
              (let* ((v1   (fresh-type-var :name 'a))
                     (v2   (fresh-type-var :name 'b))
                     (fn   (make-type-arrow-raw :params (list v1) :return v2))
                     (env  (type-env-extend 'x (type-to-scheme v1) (type-env-empty)))
                     (s    (generalize env fn)))
                (expect (= 1 (length (type-scheme-quantified-vars s))) :to-be-truthy)
               (expect (type-var-equal-p (first (type-scheme-quantified-vars s)) v2) :to-be-truthy))))
    (funcall verify)))

(it-sequential "generalize-and-scheme-ops instantiate-fresh-vars"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((v (fresh-type-var :name 'a))
                    (fn-type (make-type-arrow-raw :params (list v) :return v))
                    (scheme (make-type-scheme (list v) fn-type))
                    (inst (instantiate scheme)))
               (expect (typep inst 'type-arrow) :to-be-truthy)
               (let ((new-param (first (type-arrow-params inst)))
                     (new-ret (type-arrow-return inst)))
                 (expect (typep new-param 'type-var) :to-be-truthy)
                 (expect (type-var-equal-p new-param v) :to-be-falsy)
                 (expect (type-var-equal-p new-param new-ret) :to-be-truthy)))))
    (funcall verify)))

(it-sequential "generalize-and-scheme-ops monomorphic-scheme"
  (destructuring-bind (verify) (list (lambda ()
             (let ((scheme (type-to-scheme type-int)))
               (expect (typep scheme 'type-scheme) :to-be-truthy)
               (expect (type-scheme-quantified-vars scheme) :to-be-null)
               (assert-type-equal (type-scheme-type scheme) type-int))))
    (funcall verify)))
