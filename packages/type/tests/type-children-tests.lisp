;;;; tests/unit/type/type-children-tests.lisp — Type Children & Bound-Var Tests
;;;;
;;;; Tests for type-children and type-bound-var in src/type/representation.lisp.
;;;; Verifies the structural data layer that enables data/logic separation
;;;; in type-free-vars, type-occurs-p, and zonk.

(in-package :cl-cc/test)

(defmacro assert-when-present (value form)
  `(when ,value ,form))



;;; ─── type-children: leaf types return nil ──────────────────────────────────

(it-sequential "type-children-atomic-leaf-types int"
  (destructuring-bind (leaf-type) (list type-int)
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types string"
  (destructuring-bind (leaf-type) (list type-string)
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types bool"
  (destructuring-bind (leaf-type) (list type-bool)
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types var"
  (destructuring-bind (leaf-type) (list (fresh-type-var :name "a"))
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types rigid"
  (destructuring-bind (leaf-type) (list (fresh-rigid-var "r"))
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types error"
  (destructuring-bind (leaf-type) (list (make-type-error :message "test"))
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

(it-sequential "type-children-atomic-leaf-types nil"
  (destructuring-bind (leaf-type) (list nil)
    (expect (cl-cc/type:type-children leaf-type) :to-be-null)))

;;; ─── type-children: compound types ─────────────────────────────────────────

(it-sequential "type-children-pure-arrow-yields-params-and-return"
  (let* ((arr (make-type-arrow (list type-int type-string) type-bool))
         (ch  (cl-cc/type:type-children arr)))
    (expect (length ch) :to-equal 3)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)
    (expect (type-equal-p type-string (second ch)) :to-be-truthy)
    (expect (type-equal-p type-bool (third ch)) :to-be-truthy)))

(it-sequential "type-children-arrow-with-effects-has-effect-row-as-child"
  (let* ((arr (make-type-arrow (list type-int) type-bool :effects +io-effect-row+))
         (ch  (cl-cc/type:type-children arr)))
    (expect (length ch) :to-equal 3)
    (expect (type-effect-row-p (third ch)) :to-be-truthy)))

(it-sequential "type-children-product"
  (let* ((p  (make-type-product :elems (list type-int type-string type-bool)))
         (ch (cl-cc/type:type-children p)))
    (expect (length ch) :to-equal 3)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)))

(it-sequential "type-children-binary-collection-types union"
  (destructuring-bind (ty) (list (make-type-union (list type-int type-string)))
    (expect (length (cl-cc/type:type-children ty)) :to-equal 2)))

(it-sequential "type-children-binary-collection-types intersection"
  (destructuring-bind (ty) (list (make-type-intersection (list type-int type-string)))
    (expect (length (cl-cc/type:type-children ty)) :to-equal 2)))

(it-sequential "type-children-record-variants closed"
  (destructuring-bind (open-p expected-count second-type-or-nil) (list nil 2 type-string)
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (fields (if open-p
                     (list (cons :x type-int))
                     (list (cons :x type-int) (cons :y type-string))))
         (r  (make-type-record :fields fields :row-var rv))
         (ch (cl-cc/type:type-children r)))
    (expect (length ch) :to-equal expected-count)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy))
    (assert-when-present (not open-p)
      (expect (type-equal-p second-type-or-nil (second ch)) :to-be-truthy)))))

(it-sequential "type-children-record-variants open"
  (destructuring-bind (open-p expected-count second-type-or-nil) (list t 2 nil)
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (fields (if open-p
                     (list (cons :x type-int))
                     (list (cons :x type-int) (cons :y type-string))))
         (r  (make-type-record :fields fields :row-var rv))
         (ch (cl-cc/type:type-children r)))
    (expect (length ch) :to-equal expected-count)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy))
    (assert-when-present (not open-p)
      (expect (type-equal-p second-type-or-nil (second ch)) :to-be-truthy)))))

(it-sequential "type-children-variant-variants closed"
  (destructuring-bind (open-p) (list nil)
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (cases (if open-p
                    (list (cons :ok type-int))
                    (list (cons :some type-int) (cons :none type-null))))
         (v  (make-type-variant :cases cases :row-var rv))
         (ch (cl-cc/type:type-children v)))
    (expect (length ch) :to-equal 2)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy)))))

(it-sequential "type-children-variant-variants open"
  (destructuring-bind (open-p) (list t)
    (let* ((rv (when open-p (fresh-type-var :name "rho")))
         (cases (if open-p
                    (list (cons :ok type-int))
                    (list (cons :some type-int) (cons :none type-null))))
         (v  (make-type-variant :cases cases :row-var rv))
         (ch (cl-cc/type:type-children v)))
    (expect (length ch) :to-equal 2)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy)))))

(it-sequential "type-children-quantifier-return-body forall"
  (destructuring-bind (ty expected-body) (list (make-type-forall :var (fresh-type-var :name "a") :body type-int) type-int)
    (let ((ch (cl-cc/type:type-children ty)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p expected-body (first ch)) :to-be-truthy))))

(it-sequential "type-children-quantifier-return-body exists"
  (destructuring-bind (ty expected-body) (list (make-type-exists :var (fresh-type-var :name "a") :body type-string) type-string)
    (let ((ch (cl-cc/type:type-children ty)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p expected-body (first ch)) :to-be-truthy))))

(it-sequential "type-children-app"
  (let* ((app (make-type-app :fun type-int :arg type-string))
         (ch  (cl-cc/type:type-children app)))
    (expect (length ch) :to-equal 2)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)
    (expect (type-equal-p type-string (second ch)) :to-be-truthy)))

(it-sequential "type-children-lambda-and-mu"
  (let ((a (fresh-type-var :name "a")))
    (let ((ch (cl-cc/type:type-children (cl-cc/type:make-type-lambda :var a :body type-int))))
      (expect (length ch) :to-equal 1)
      (expect (type-equal-p type-int (first ch)) :to-be-truthy))
    (let ((ch (cl-cc/type:type-children (make-type-mu :var a :body type-int))))
      (expect (length ch) :to-equal 1))))

(it-sequential "type-children-wrapper-types refinement"
  (destructuring-bind (ty) (list (cl-cc/type:make-type-refinement :base type-int :predicate #'numberp))
    (let ((ch (cl-cc/type:type-children ty)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy))))

(it-sequential "type-children-wrapper-types linear"
  (destructuring-bind (ty) (list (make-type-linear :base type-int :grade :one))
    (let ((ch (cl-cc/type:type-children ty)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy))))

(it-sequential "type-children-wrapper-types capability"
  (destructuring-bind (ty) (list (cl-cc/type:make-type-capability :base type-int :cap 'read))
    (let ((ch (cl-cc/type:type-children ty)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy))))

(it-sequential "type-children-effect-row-variants closed"
  (destructuring-bind (open-p expected-count) (list nil 1)
    (let* ((rv  (when open-p (fresh-type-var :name "rho")))
         (eff (make-type-effect-op :name 'io :args nil))
         (row (make-type-effect-row :effects (list eff) :row-var rv))
         (ch  (cl-cc/type:type-children row)))
    (expect (length ch) :to-equal expected-count)
    (expect (cl-cc/type:type-effect-op-p (first ch)) :to-be-truthy)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy)))))

(it-sequential "type-children-effect-row-variants open"
  (destructuring-bind (open-p expected-count) (list t 2)
    (let* ((rv  (when open-p (fresh-type-var :name "rho")))
         (eff (make-type-effect-op :name 'io :args nil))
         (row (make-type-effect-row :effects (list eff) :row-var rv))
         (ch  (cl-cc/type:type-children row)))
    (expect (length ch) :to-equal expected-count)
    (expect (cl-cc/type:type-effect-op-p (first ch)) :to-be-truthy)
    (assert-when-present open-p
      (expect (type-var-p (second ch)) :to-be-truthy)))))

(it-sequential "type-children-effect-op-cases no-args"
  (destructuring-bind (name args expected-count) (list 'io nil nil)
    (let* ((eff (make-type-effect-op :name name :args args))
         (ch  (cl-cc/type:type-children eff)))
    (assert-when-present expected-count
      (progn (expect (length ch) :to-equal expected-count)
             (expect (type-equal-p type-int (first ch)) :to-be-truthy)))
    (assert-when-present (not expected-count)
      (expect ch :to-be-null)))))

(it-sequential "type-children-effect-op-cases with-args"
  (destructuring-bind (name args expected-count) (list 'state (list type-int) 1)
    (let* ((eff (make-type-effect-op :name name :args args))
         (ch  (cl-cc/type:type-children eff)))
    (assert-when-present expected-count
      (progn (expect (length ch) :to-equal expected-count)
             (expect (type-equal-p type-int (first ch)) :to-be-truthy)))
    (assert-when-present (not expected-count)
      (expect ch :to-be-null)))))

(it-sequential "type-children-handler-has-three-children"
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h   (cl-cc/type:make-type-handler :effect eff :input type-int :output type-string))
         (ch  (cl-cc/type:type-children h)))
    (expect (length ch) :to-equal 3)))

(it-sequential "type-children-constraint-has-one-child"
  (let* ((c  (cl-cc/type:make-type-constraint :class-name 'eq :type-arg type-int))
         (ch (cl-cc/type:type-children c)))
    (expect (length ch) :to-equal 1)
    (expect (type-equal-p type-int (first ch)) :to-be-truthy)))

(it-sequential "type-children-qualified-has-two-children"
  (let* ((c  (cl-cc/type:make-type-constraint :class-name 'eq :type-arg type-int))
         (q  (make-type-qualified :constraints (list c) :body type-string))
         (ch (cl-cc/type:type-children q)))
    (expect (length ch) :to-equal 2)
    (expect (type-equal-p type-string (second ch)) :to-be-truthy)))

;;; ─── type-bound-var ────────────────────────────────────────────────────────

(it-sequential "type-bound-var-binding-types forall"
  (destructuring-bind (ty) (list (let ((a (fresh-type-var :name "a"))) (make-type-forall :var a :body type-int)))
    (expect (type-var-p (cl-cc/type:type-bound-var ty)) :to-be-truthy)))

(it-sequential "type-bound-var-binding-types exists"
  (destructuring-bind (ty) (list (let ((a (fresh-type-var :name "a"))) (make-type-exists :var a :body type-int)))
    (expect (type-var-p (cl-cc/type:type-bound-var ty)) :to-be-truthy)))

(it-sequential "type-bound-var-binding-types lambda"
  (destructuring-bind (ty) (list (let ((a (fresh-type-var :name "a"))) (cl-cc/type:make-type-lambda :var a :body type-int)))
    (expect (type-var-p (cl-cc/type:type-bound-var ty)) :to-be-truthy)))

(it-sequential "type-bound-var-binding-types mu"
  (destructuring-bind (ty) (list (let ((a (fresh-type-var :name "a"))) (make-type-mu :var a :body type-int)))
    (expect (type-var-p (cl-cc/type:type-bound-var ty)) :to-be-truthy)))

(it-sequential "type-bound-var-non-binding-cases primitive"
  (destructuring-bind (ty) (list type-int)
    (expect (cl-cc/type:type-bound-var ty) :to-be-null)))

(it-sequential "type-bound-var-non-binding-cases var"
  (destructuring-bind (ty) (list (fresh-type-var :name "a"))
    (expect (cl-cc/type:type-bound-var ty) :to-be-null)))

(it-sequential "type-bound-var-non-binding-cases arrow"
  (destructuring-bind (ty) (list (make-type-arrow (list type-int) type-bool))
    (expect (cl-cc/type:type-bound-var ty) :to-be-null)))

(it-sequential "type-bound-var-non-binding-cases product"
  (destructuring-bind (ty) (list (make-type-product :elems (list type-int)))
    (expect (cl-cc/type:type-bound-var ty) :to-be-null)))

(it-sequential "type-bound-var-non-binding-cases union"
  (destructuring-bind (ty) (list (make-type-union (list type-int type-string)))
    (expect (cl-cc/type:type-bound-var ty) :to-be-null)))

;;; ─── Integration: type-free-vars still works correctly ─────────────────────

(it-sequential "type-free-vars-via-children-simple"
  (let* ((a   (fresh-type-var :name "a"))
         (arr (make-type-arrow (list a) type-int)))
    (expect (length (type-free-vars arr)) :to-equal 1)
    (expect (type-var-equal-p a (first (type-free-vars arr))) :to-be-truthy)))

(it-sequential "type-free-vars-forall-bound-var-excluded"
  (let* ((a (fresh-type-var :name "a"))
         (f (make-type-forall :var a :body a)))
    (expect (type-free-vars f) :to-be-null)))

(it-sequential "type-free-vars-forall-only-unbound-vars-are-free"
  (let* ((a (fresh-type-var :name "a"))
         (b (fresh-type-var :name "b"))
         (f (make-type-forall :var a :body (make-type-arrow (list a) b))))
    (expect (length (type-free-vars f)) :to-equal 1)
    (expect (type-var-equal-p b (first (type-free-vars f))) :to-be-truthy)))

(it-sequential "type-free-vars-via-children-record"
  (let* ((a  (fresh-type-var :name "a"))
         (rv (fresh-type-var :name "rho"))
         (r  (make-type-record :fields (list (cons :x a)) :row-var rv)))
    (expect (length (type-free-vars r)) :to-equal 2)))

(it-sequential "type-free-vars-via-children-mu"
  (let* ((a (fresh-type-var :name "a"))
         (b (fresh-type-var :name "b"))
         (m (make-type-mu :var a :body (make-type-product :elems (list a b)))))
    (expect (length (type-free-vars m)) :to-equal 1)
    (expect (type-var-equal-p b (first (type-free-vars m))) :to-be-truthy)))

(it-sequential "type-free-vars-via-children-nested"
  (let* ((a (fresh-type-var :name "a"))
         (b (fresh-type-var :name "b"))
         (ty (make-type-union
              (list (make-type-arrow (list a) type-int)
                    (make-type-product :elems (list b type-string))))))
    (expect (length (type-free-vars ty)) :to-equal 2)))

;;; ─── Integration: type-occurs-p still works correctly ──────────────────────

(it-sequential "type-occurs-p-direct-arrow-and-absent"
  (let ((a (fresh-type-var :name "a"))
        (b (fresh-type-var :name "b"))
        (s (make-substitution)))
    (expect (type-occurs-p a a s) :to-be-truthy)
    (expect (type-occurs-p a (make-type-arrow (list a) type-int) s) :to-be-truthy)
    (expect (type-occurs-p b (make-type-arrow (list a) type-int) s) :to-be-falsy)))

(it-sequential "type-occurs-p-follows-subst-chain"
  (let* ((a (fresh-type-var :name "a"))
         (b (fresh-type-var :name "b"))
         (subst (make-substitution)))
    (subst-extend! b a subst)
    (expect (type-occurs-p a b subst) :to-be-truthy)))

(it-sequential "type-occurs-p-finds-var-in-nested-union"
  (let* ((a (fresh-type-var :name "a"))
         (ty (make-type-union
              (list type-int
                    (make-type-product :elems
                      (list type-string
                            (make-type-arrow (list a) type-bool)))))))
    (expect (type-occurs-p a ty (make-substitution)) :to-be-truthy)))
