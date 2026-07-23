;;;; tests/unit/type/representation-tests.lisp — Type Representation Tests
;;;;
;;;; Tests for src/type/representation.lisp:
;;;; Advanced type nodes, type-equal-p branches, type-free-vars,
;;;; type-env operations, and type-constructor encoding.

(in-package :cl-cc/test)



;;; ─── type-equal-p: product ─────────────────────────────────────────────────

(it-sequential "type-equal-product-same-elements-is-true"
  (expect (type-equal-p (make-type-product :elems (list type-int type-string))
                              (make-type-product :elems (list type-int type-string))) :to-be-truthy))

(it-sequential "type-equal-product-different-length-is-false"
  (expect (type-equal-p (make-type-product :elems (list type-int))
                               (make-type-product :elems (list type-int type-string))) :to-be-falsy))

(it-sequential "type-equal-product-different-element-is-false"
  (expect (type-equal-p (make-type-product :elems (list type-int type-int))
                               (make-type-product :elems (list type-int type-string))) :to-be-falsy))

(it-sequential "type-equal-forall-same-body-is-true"
  (let ((v (fresh-type-var :name "a")))
    (expect (type-equal-p
                  (make-type-forall :var v :body (make-type-arrow (list v) type-int))
                  (make-type-forall :var v :body (make-type-arrow (list v) type-int))) :to-be-truthy)))

(it-sequential "type-equal-forall-different-body-is-false"
  (let ((v (fresh-type-var :name "a")))
    (expect (type-equal-p
                   (make-type-forall :var v :body type-int)
                   (make-type-forall :var v :body type-string)) :to-be-falsy)))

(it-sequential "type-equal-exists-and-mu-same-is-true"
  (let ((v (fresh-type-var :name "a")))
    (expect (type-equal-p (make-type-exists :var v :body type-int)
                                (make-type-exists :var v :body type-int)) :to-be-truthy)
    (expect (type-equal-p (make-type-mu :var v :body type-int)
                                (make-type-mu :var v :body type-int)) :to-be-truthy)))

;;; ─── type-equal-p: union / intersection ────────────────────────────────────

(it-sequential "type-equal-union-intersection-same union"
  (destructuring-bind (a b) (list (make-type-union        (list type-int type-string)) (make-type-union        (list type-int type-string)))
    (expect (type-equal-p a b) :to-be-truthy)))

(it-sequential "type-equal-union-intersection-same intersection"
  (destructuring-bind (a b) (list (make-type-intersection (list type-int type-any)) (make-type-intersection (list type-int type-any)))
    (expect (type-equal-p a b) :to-be-truthy)))

;;; ─── type-equal-p: type-app / type-linear ──────────────────────────────────

(it-sequential "type-equal-type-app-same-is-true"
  (expect (type-equal-p (make-type-app :fun type-int :arg type-string)
                              (make-type-app :fun type-int :arg type-string)) :to-be-truthy))

(it-sequential "type-equal-type-app-different-arg-is-false"
  (expect (type-equal-p (make-type-app :fun type-int :arg type-string)
                               (make-type-app :fun type-int :arg type-int)) :to-be-falsy))

(it-sequential "type-equal-linear-same-grade-is-true"
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l2 (make-type-linear :base type-int :grade :one)))
    (expect (type-equal-p l1 l2) :to-be-truthy)))

(it-sequential "type-equal-linear-different-grade-is-false"
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l3 (make-type-linear :base type-int :grade :omega)))
    (expect (type-equal-p l1 l3) :to-be-falsy)))

;;; ─── type-equal-p: effect-row / effect-op ──────────────────────────────────

(it-sequential "type-equal-effect-rows-and-ops"
  (expect (type-equal-p +pure-effect-row+ +pure-effect-row+) :to-be-truthy)
  (expect (type-equal-p +pure-effect-row+ +io-effect-row+) :to-be-falsy)
  (expect (type-equal-p (make-type-effect-op :name 'io    :args nil)
                               (make-type-effect-op :name 'io    :args nil)) :to-be-truthy)
  (expect (type-equal-p (make-type-effect-op :name 'io    :args nil)
                               (make-type-effect-op :name 'state :args nil)) :to-be-falsy))

;;; ─── type-equal-p: constraint / qualified ──────────────────────────────────

(it-sequential "type-equal-constraint-and-qualified"
  (expect (type-equal-p (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)
                               (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)) :to-be-truthy)
  (expect (type-equal-p (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)
                               (cl-cc/type:make-type-constraint :class-name 'ord :type-arg type-int)) :to-be-falsy)
  (let* ((tc (cl-cc/type:make-type-constraint :class-name 'eq :type-arg type-int))
         (q1 (make-type-qualified :constraints (list tc) :body type-int))
         (q2 (make-type-qualified :constraints (list tc) :body type-int)))
    (expect (type-equal-p q1 q2) :to-be-truthy)))

;;; ─── type-equal-p: error sentinel and rigid vars ────────────────────────────

(it-sequential "type-equal-error-node-never-equal"
  (let ((e (make-type-error :message "test")))
    (expect (type-equal-p e e) :to-be-falsy)
    (expect (type-equal-p e type-int) :to-be-falsy)))

(it-sequential "type-equal-rigid-var-equal-only-to-itself"
  (let ((r  (fresh-rigid-var "a"))
        (r1 (fresh-rigid-var "a"))
        (r2 (fresh-rigid-var "b")))
    (expect (type-equal-p r r) :to-be-truthy)
    (expect (type-equal-p r1 r2) :to-be-falsy)))

;;; ─── type-free-vars ────────────────────────────────────────────────────────

(it-sequential "free-vars-primitive-has-no-free-vars"
  (expect (type-free-vars type-int) :to-be-null))

(it-sequential "free-vars-type-var-is-its-own-free-var"
  (let* ((v (fresh-type-var :name "a"))
         (fvs (type-free-vars v)))
    (expect (length fvs) :to-equal 1)
    (expect (type-var-equal-p v (first fvs)) :to-be-truthy)))

(it-sequential "free-vars-binding-forms-remove-bound-var"
  (let ((v (fresh-type-var :name "a")))
    (expect (type-free-vars (make-type-forall :var v :body v)) :to-be-null)
    (expect (type-free-vars (make-type-exists :var v :body v)) :to-be-null)
    (expect (type-free-vars (make-type-mu :var v :body v)) :to-be-null)))

(it-sequential "free-vars-count-cases arrow"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v1 (fresh-type-var :name "a")) (v2 (fresh-type-var :name "b")))
                            (make-type-arrow (list v1) v2))) 2)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases product"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v1 (fresh-type-var :name "a")) (v2 (fresh-type-var :name "b")))
                            (make-type-product :elems (list v1 type-int v2)))) 2)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases record"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v (fresh-type-var :name "a")) (rv (fresh-type-var :name "rho")))
                            (make-type-record :fields (list (cons 'x v)) :row-var rv))) 2)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases variant"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v (fresh-type-var :name "a")))
                            (make-type-variant :cases (list (cons 'x v)) :row-var nil))) 1)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases linear"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v (fresh-type-var :name "a")))
                            (make-type-linear :base v :grade :one))) 1)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases type-app"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((v1 (fresh-type-var :name "f")) (v2 (fresh-type-var :name "a")))
                            (make-type-app :fun v1 :arg v2))) 2)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases effect-row"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let ((rv (fresh-type-var :name "ε")))
                            (make-type-effect-row :effects nil :row-var rv))) 1)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

(it-sequential "free-vars-count-cases qualified"
  (destructuring-bind (make-ty expected-count) (list (lambda ()
                          (let* ((v (fresh-type-var :name "a"))
                                 (tc (cl-cc/type:make-type-constraint :class-name 'eq :type-arg v)))
                            (make-type-qualified :constraints (list tc) :body v))) 1)
    (expect (length (type-free-vars (funcall make-ty))) :to-equal expected-count)))

;;; ─── Type environment operations ───────────────────────────────────────────

(it-sequential "type-env-lookup-in-empty-returns-nil"
  (let ((env (type-env-empty)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type:type-env-lookup 'x env)
      (expect scheme :to-be-null)
      (expect found-p :to-be-falsy))))

(it-sequential "type-env-extend-and-lookup-finds-scheme"
  (let* ((env  (type-env-empty))
         (env2 (cl-cc/type:type-env-extend 'x (type-to-scheme type-int) env)))
    (multiple-value-bind (result found-p)
        (cl-cc/type:type-env-lookup 'x env2)
      (expect found-p :to-be-truthy)
      (expect (type-equal-p type-int (cl-cc/type:type-scheme-type result)) :to-be-truthy))))

(it-sequential "type-env-extend*-adds-multiple-bindings"
  (let* ((env (type-env-empty))
         (bindings (list (cons 'x (type-to-scheme type-int))
                         (cons 'y (type-to-scheme type-string))))
         (env2 (cl-cc/type:type-env-extend* bindings env)))
    (expect (nth-value 1 (cl-cc/type:type-env-lookup 'x env2)) :to-be-truthy)
    (expect (nth-value 1 (cl-cc/type:type-env-lookup 'y env2)) :to-be-truthy)))

(it-sequential "type-env-free-vars-collects-from-bindings"
  (let* ((v (fresh-type-var :name "a"))
         (env (cl-cc/type:type-env-extend 'x (type-to-scheme v) (type-env-empty)))
         (fvs (cl-cc/type:type-env-free-vars env)))
    (expect (length fvs) :to-equal 1)))

;;; ─── Constructor and printing cases ────────────────────────────────────────

(it-sequential "type-var-constructor-name-is-preserved"
  (let ((v (cl-cc/type:fresh-type-var 'x)))
    (expect (type-var-p v) :to-be-truthy)
    (expect (type-var-name v) :to-be 'x)))

(it-sequential "type-constructor-name-and-args"
  (let ((tc (cl-cc/type:make-type-constructor 'list (list type-int))))
    (expect (cl-cc/type:type-constructor-p tc) :to-be-truthy)
    (expect (cl-cc/type:type-constructor-name tc) :to-be 'list)
    (expect (length (cl-cc/type:type-constructor-args tc)) :to-equal 1)))

(it-sequential "type-specifier-parse-unparse-roundtrip"
  (let* ((ty (cl-cc/type:parse-type-specifier '(Option string)))
         (spec (cl-cc/type:unparse-type ty)))
    (expect (first spec) :to-be 'Option)
    (expect (type-equal-p ty (cl-cc/type:parse-type-specifier spec)) :to-be-truthy)))

(it-sequential "type-arrow-default-mult-is-omega"
  (let ((f (make-type-arrow (list type-int) type-string)))
    (expect (type-arrow-p f) :to-be-truthy)
    (expect (type-arrow-mult f) :to-be :omega)))

(it-sequential "type-to-string-returns-string-for-all-forms"
  (expect (stringp (type-to-string type-int)) :to-be-truthy)
  (expect (stringp (type-to-string cl-cc/type:+type-unknown+)) :to-be-truthy)
  (expect (stringp (type-to-string nil)) :to-be-truthy)
  (expect (search "->" (type-to-string (make-type-arrow (list type-int) type-string))) :to-be-truthy))
