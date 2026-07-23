;;;; tests/type-2026-nodes-tests.lisp - 2026 Type System Node and Substitution API Tests
;;;;
;;;; Covers: 2026 type node extensions (rigid vars, product/variant, exists/mu, HKT app,
;;;; record, arrow-mult, linear), ANSI upgrade helpers, and hash-table substitution API.

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New Type Node Tests (direct new API)
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "type-rigid-var-identity-and-uniqueness"
  (let ((r1 (fresh-rigid-var 'a))
        (r2 (fresh-rigid-var 'a)))
    (expect (type-rigid-p r1) :to-be-truthy)
    (expect (type-rigid-p r2) :to-be-truthy)
    (expect (type-rigid-equal-p r1 r2) :to-be-falsy)
    (expect (type-rigid-equal-p r1 r1) :to-be-truthy)
    (expect (type-rigid-name r1) :to-be 'a)))

(it-sequential "type-product-and-variant-creation"
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (expect (type-product-p pair) :to-be-truthy)
    (expect (= 2 (length (type-product-elems pair))) :to-be-truthy)
    (expect (type-equal-p type-int    (first  (type-product-elems pair))) :to-be-truthy)
    (expect (type-equal-p type-string (second (type-product-elems pair))) :to-be-truthy))
  (let ((v (make-type-variant :cases (list (cons 'some type-int) (cons 'none type-null))
                              :row-var nil)))
    (expect (type-variant-p v) :to-be-truthy)
    (expect (= 2 (length (type-variant-cases v))) :to-be-truthy)
    (expect (type-variant-row-var v) :to-be-null)))

(it-sequential "type-forall-body-keyword-aliases-type"
  (let* ((a  (fresh-type-var :name 'a))
         (fn (make-type-arrow (list a) a))
         (fa (make-type-forall :var a :body fn)))
    (expect (type-forall-p fa) :to-be-truthy)
    (expect (type-var-equal-p a (type-forall-var fa)) :to-be-truthy)
    (expect (type-equal-p fn (type-forall-body fa)) :to-be-truthy)))

(it-sequential "type-exists-and-mu-creation"
  (let* ((a    (fresh-type-var :name 'a))
         (pair (make-type-product :elems (list type-string a)))
         (ex   (make-type-exists :var a :knd nil :body pair)))
    (expect (type-exists-p ex) :to-be-truthy)
    (expect (type-var-equal-p a (type-exists-var ex)) :to-be-truthy)
    (expect (type-product-p (type-exists-body ex)) :to-be-truthy))
  (let* ((a  (fresh-type-var :name 'a))
         (mu (make-type-mu :var a
                           :body (make-type-union
                                  (list type-null
                                        (make-type-product :elems (list type-int a)))))))
    (expect (type-mu-p mu) :to-be-truthy)
    (expect (type-var-equal-p a (type-mu-var mu)) :to-be-truthy)
    (expect (type-union-p (type-mu-body mu)) :to-be-truthy)))

(it-sequential "type-hkt-app-creation"
  (let* ((list-con (make-type-primitive :name 'list))
         (list-int (make-type-app :fun list-con :arg type-int)))
    (expect (type-app-p list-int) :to-be-truthy)
    (expect (type-primitive-p (type-app-fun list-int)) :to-be-truthy)
    (expect (type-equal-p type-int (type-app-arg list-int)) :to-be-truthy)))

(it-sequential "type-record-open-closed closed"
  (destructuring-bind (open-p expected-field-count) (list nil 2)
    (let* ((rv  (when open-p (fresh-type-var :name 'rho)))
         (fields (if open-p
                     (list (cons 'name type-string))
                     (list (cons 'name type-string) (cons 'age type-int))))
         (rec (make-type-record :fields fields :row-var rv)))
    (expect (type-record-p rec) :to-be-truthy)
    (expect (= expected-field-count (length (type-record-fields rec))) :to-be-truthy)
    (if open-p
        (expect (type-var-p (type-record-row-var rec)) :to-be-truthy)
        (expect (type-record-row-var rec) :to-be-null)))))

(it-sequential "type-record-open-closed open"
  (destructuring-bind (open-p expected-field-count) (list t 1)
    (let* ((rv  (when open-p (fresh-type-var :name 'rho)))
         (fields (if open-p
                     (list (cons 'name type-string))
                     (list (cons 'name type-string) (cons 'age type-int))))
         (rec (make-type-record :fields fields :row-var rv)))
    (expect (type-record-p rec) :to-be-truthy)
    (expect (= expected-field-count (length (type-record-fields rec))) :to-be-truthy)
    (if open-p
        (expect (type-var-p (type-record-row-var rec)) :to-be-truthy)
        (expect (type-record-row-var rec) :to-be-null)))))


(it-sequential "type-arrow-mult-cases linear-one"
  (destructuring-bind (param-t ret-t effs mult) (list type-int type-int +pure-effect-row+ :one)
    (let ((arr (make-type-arrow-raw :params (list param-t) :return ret-t :effects effs :mult mult)))
    (expect (type-arrow-p arr) :to-be-truthy)
    (expect (type-arrow-mult arr) :to-be mult))))

(it-sequential "type-arrow-mult-cases erased-zero"
  (destructuring-bind (param-t ret-t effs mult) (list type-bool type-null nil :zero)
    (let ((arr (make-type-arrow-raw :params (list param-t) :return ret-t :effects effs :mult mult)))
    (expect (type-arrow-p arr) :to-be-truthy)
    (expect (type-arrow-mult arr) :to-be mult))))


(it-sequential "type-linear-creation linear-one"
  (destructuring-bind (base grade) (list type-int :one)
    (let ((lin (make-type-linear :base base :grade grade)))
    (expect (type-linear-p lin) :to-be-truthy)
    (expect (type-linear-grade lin) :to-be grade)
    (expect (type-equal-p base (type-linear-base lin)) :to-be-truthy))))

(it-sequential "type-linear-creation erased-zero"
  (destructuring-bind (base grade) (list type-string :zero)
    (let ((lin (make-type-linear :base base :grade grade)))
    (expect (type-linear-p lin) :to-be-truthy)
    (expect (type-linear-grade lin) :to-be grade)
    (expect (type-equal-p base (type-linear-base lin)) :to-be-truthy))))

(it-sequential "type-linear-creation unrestricted"
  (destructuring-bind (base grade) (list type-bool :omega)
    (let ((lin (make-type-linear :base base :grade grade)))
    (expect (type-linear-p lin) :to-be-truthy)
    (expect (type-linear-grade lin) :to-be grade)
    (expect (type-equal-p base (type-linear-base lin)) :to-be-truthy))))

(it-sequential "upgraded-array-and-complex-part-types"
  (let ((bit-upgraded (upgraded-array-element-type 'bit))
        (char-upgraded (upgraded-array-element-type 'character))
        (fallback-upgraded (upgraded-array-element-type '(or fixnum string)))
        (complex-part (upgraded-complex-part-type 'complex)))
    (expect (type-equal-p (cl-cc/type:parse-type-specifier 'bit) bit-upgraded) :to-be-truthy)
    (expect (type-equal-p (cl-cc/type:parse-type-specifier 'character) char-upgraded) :to-be-truthy)
    (expect (type-equal-p type-any fallback-upgraded) :to-be-truthy)
    (expect (type-equal-p (cl-cc/type:parse-type-specifier 'real) complex-part) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Hash-Table Substitution API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "subst-empty-has-no-bindings"
  (let ((s (make-substitution)))
    (expect (substitution-p s) :to-be-truthy)
    (expect (= 0 (substitution-generation s)) :to-be-truthy)
    (let ((v (fresh-type-var)))
      (multiple-value-bind (bound found) (subst-lookup v s)
        (declare (ignore bound))
        (expect found :to-be-falsy)))))

(it-sequential "subst-extend-is-functional"
  (let* ((v  (fresh-type-var))
         (s0 (make-substitution))
         (s1 (subst-extend v type-int s0)))
    (multiple-value-bind (b f) (subst-lookup v s0) (declare (ignore b)) (expect f :to-be-falsy))
    (multiple-value-bind (bound found) (subst-lookup v s1)
      (expect found :to-be-truthy)
      (expect (type-equal-p type-int bound) :to-be-truthy))
    (expect (> (substitution-generation s1) (substitution-generation s0)) :to-be-truthy)))

(it-sequential "subst-extend!-mutates-in-place"
  (let* ((v (fresh-type-var))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (multiple-value-bind (bound found) (subst-lookup v s)
      (expect found :to-be-truthy)
      (expect (type-equal-p type-string bound) :to-be-truthy))))

(it-sequential "subst-advanced-operations"
  (let* ((v1  (fresh-type-var))
         (v2  (fresh-type-var))
         (s1  (subst-extend v1 type-int (make-substitution)))
         (s2  (subst-extend v2 v1 (make-substitution)))
         (s12 (subst-compose s1 s2)))
    (expect (type-equal-p type-int (zonk v2 s12)) :to-be-truthy))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) v))
         (s  (subst-extend v type-bool (make-substitution)))
         (r  (zonk fn s)))
    (expect (type-arrow-p r) :to-be-truthy)
    (expect (type-equal-p type-bool (first (type-arrow-params r))) :to-be-truthy)
    (expect (type-equal-p type-bool (type-arrow-return r)) :to-be-truthy))
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) type-int))
         (s  (make-substitution))
         (w  (fresh-type-var)))
    (expect (type-occurs-p v fn s) :to-be-truthy)
    (expect (type-occurs-p v type-int s) :to-be-falsy)
    (expect (type-occurs-p w fn s) :to-be-falsy)))
