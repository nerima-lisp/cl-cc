;;;; tests/type-tests.lisp - Type System Tests
;;;;
;;;; Comprehensive tests for the HM type system including:
;;;; - Type representation (primitives, variables, functions)
;;;; - Unification (with occurs check)
;;;; - Type inference (Algorithm W)
;;;; - Generalization and instantiation (let-polymorphism)

(in-package :cl-cc/test)


;;; Type Representation Tests

(it-sequential "type-repr-primitive-is-type-primitive int"
  (destructuring-bind (tp) (list type-int)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-is-type-primitive string"
  (destructuring-bind (tp) (list type-string)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-is-type-primitive bool"
  (destructuring-bind (tp) (list type-bool)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-is-type-primitive symbol"
  (destructuring-bind (tp) (list type-symbol)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-is-type-primitive null"
  (destructuring-bind (tp) (list type-null)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-is-type-primitive any"
  (destructuring-bind (tp) (list type-any)
    (expect (typep tp 'type-primitive) :to-be-truthy)))

(it-sequential "type-repr-primitive-name int"
  (destructuring-bind (tp expected-name) (list type-int 'fixnum)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-primitive-name string"
  (destructuring-bind (tp expected-name) (list type-string 'string)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-primitive-name bool"
  (destructuring-bind (tp expected-name) (list type-bool 'boolean)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-primitive-name symbol"
  (destructuring-bind (tp expected-name) (list type-symbol 'symbol)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-primitive-name null"
  (destructuring-bind (tp expected-name) (list type-null 'null)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-primitive-name any"
  (destructuring-bind (tp expected-name) (list type-any 't)
    (expect (type-primitive-name tp) :to-be expected-name)))

(it-sequential "type-repr-variable-and-function-creation"
  (let ((v1 (fresh-type-var :name 'a))
        (v2 (fresh-type-var :name 'b)))
    (expect (typep v1 'type-var) :to-be-truthy)
    (expect (typep v2 'type-var) :to-be-truthy)
    (expect (= (type-var-id v1) (type-var-id v2)) :to-be-falsy)
    (expect (type-var-name v1) :to-be 'a)
    (expect (type-var-name v2) :to-be 'b))
  (let ((fn-type (make-type-arrow-raw
                  :params (list type-int type-int)
                  :return type-int)))
    (expect (typep fn-type 'type-arrow) :to-be-truthy)
    (expect (= 2 (length (type-arrow-params fn-type))) :to-be-truthy)
    (expect (type-arrow-return fn-type) :to-be type-int)))

(it-sequential "type-repr-equality-and-strings primitive-same"
  (destructuring-bind (verify) (list (lambda ()
             (assert-type-equal type-int type-int)))
    (funcall verify)))

(it-sequential "type-repr-equality-and-strings primitive-distinct"
  (destructuring-bind (verify) (list (lambda ()
             (expect (type-equal-p type-int type-string) :to-be-falsy)))
    (funcall verify)))

(it-sequential "type-repr-equality-and-strings variable-self"
  (destructuring-bind (verify) (list (lambda ()
             (let ((v (fresh-type-var)))
               (assert-type-equal v v))))
    (funcall verify)))

(it-sequential "type-repr-equality-and-strings function-equal"
  (destructuring-bind (verify) (list (lambda ()
             (let ((fn1 (make-type-arrow-raw :params (list type-int) :return type-int))
                   (fn2 (make-type-arrow-raw :params (list type-int) :return type-int)))
               (assert-type-equal fn1 fn2))))
    (funcall verify)))

(it-sequential "type-repr-equality-and-strings function-different"
  (destructuring-bind (verify) (list (lambda ()
             (let ((fn1 (make-type-arrow-raw :params (list type-int)    :return type-int))
                   (fn3 (make-type-arrow-raw :params (list type-string) :return type-int)))
               (expect (type-equal-p fn1 fn3) :to-be-falsy))))
    (funcall verify)))

(it-sequential "type-repr-equality-and-strings arrow-to-string"
  (destructuring-bind (verify) (list (lambda ()
             (let ((fn (make-type-arrow-raw :params (list type-int) :return type-int)))
               (expect (type-to-string fn) :to-equal "FIXNUM -> FIXNUM"))))
    (funcall verify)))

(it-sequential "type-repr-primitive-type-to-string int"
  (destructuring-bind (expected type) (list "FIXNUM" type-int)
    (expect (type-to-string type) :to-equal expected)))

(it-sequential "type-repr-primitive-type-to-string string"
  (destructuring-bind (expected type) (list "STRING" type-string)
    (expect (type-to-string type) :to-equal expected)))

(it-sequential "type-repr-primitive-type-to-string bool"
  (destructuring-bind (expected type) (list "BOOLEAN" type-bool)
    (expect (type-to-string type) :to-equal expected)))

(it-sequential "type-repr-primitive-type-to-string unknown"
  (destructuring-bind (expected type) (list "?" cl-cc/type:+type-unknown+)
    (expect (type-to-string type) :to-equal expected)))


(it-sequential "type-repr-unknown-type"
  (expect (type-equal-p cl-cc/type:+type-unknown+ cl-cc/type:+type-unknown+) :to-be-falsy)
  (expect (type-error-p cl-cc/type:+type-unknown+) :to-be-truthy))

;;; Unification Tests

(it-sequential "unify-primitive same"
  (destructuring-bind (should-unify a b) (list t type-int type-int)
    (if should-unify
      (assert-unifies a b)
      (assert-not-unifies a b))))

(it-sequential "unify-primitive different"
  (destructuring-bind (should-unify a b) (list nil type-int type-string)
    (if should-unify
      (assert-unifies a b)
      (assert-not-unifies a b))))

(it-sequential "unify-advanced-cases variable-binds"
  (destructuring-bind (verify) (list (lambda ()
             (let ((v (fresh-type-var)))
               (multiple-value-bind (result ok) (type-unify v type-int)
                 (expect ok :to-be-truthy)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (expect found :to-be-truthy)
                   (assert-type-equal binding type-int))))
             (assert-unifies (fresh-type-var) (fresh-type-var))
             (let ((v (fresh-type-var)))
               (assert-unifies v v))))
    (funcall verify)))

(it-sequential "unify-advanced-cases structural-binding"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((v   (fresh-type-var))
                    (fn1 (make-type-arrow-raw :params (list v) :return type-int))
                    (fn2 (make-type-arrow-raw :params (list type-string) :return type-int)))
               (multiple-value-bind (result ok) (type-unify fn1 fn2)
                 (expect ok :to-be-truthy)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (expect found :to-be-truthy)
                   (assert-type-equal binding type-string))))))
    (funcall verify)))

(it-sequential "unify-advanced-cases failure-cases"
  (destructuring-bind (verify) (list (lambda ()
             (assert-not-unifies
              (make-type-arrow-raw :params (list type-int) :return type-int)
              (make-type-arrow-raw :params (list type-int type-int) :return type-int))
             (let* ((v  (fresh-type-var))
                    (fn (make-type-arrow-raw :params (list v) :return type-int)))
               (assert-not-unifies v fn))))
    (funcall verify)))

(it-sequential "unify-advanced-cases subst-chains"
  (destructuring-bind (verify) (list (lambda ()
             (let* ((v1       (fresh-type-var))
                    (v2       (fresh-type-var))
                    (s1       (subst-extend v1 type-int (make-substitution)))
                    (s2       (subst-extend v2 v1 (make-substitution)))
                    (composed (subst-compose s1 s2)))
               (assert-type-equal type-int (zonk v2 composed)))
             (let* ((v1 (fresh-type-var))
                    (v2 (fresh-type-var)))
               (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
                 (expect ok1 :to-be-truthy)
                 (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
                   (expect ok2 :to-be-truthy)
                   (assert-type-equal type-int (zonk v1 subst2)))))))
    (funcall verify)))

(it-sequential "unify-lists success"
  (destructuring-bind (expected a b) (list t (list type-int type-string) (list type-int type-string))
    (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (expect (not (null ok)) :to-equal expected))))

(it-sequential "unify-lists type-mismatch"
  (destructuring-bind (expected a b) (list nil (list type-int type-string) (list type-string type-int))
    (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (expect (not (null ok)) :to-equal expected))))

(it-sequential "unify-lists length-mismatch"
  (destructuring-bind (expected a b) (list nil (list type-int) (list type-int type-string))
    (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (expect (not (null ok)) :to-equal expected))))

(it-sequential "unify-type-error-fails unknown-int"
  (destructuring-bind (a b) (list cl-cc/type:+type-unknown+ type-int)
    (assert-not-unifies a b)))

(it-sequential "unify-type-error-fails string-unknown"
  (destructuring-bind (a b) (list type-string cl-cc/type:+type-unknown+)
    (assert-not-unifies a b)))

(it-sequential "unify-type-error-fails unknown-unknown"
  (destructuring-bind (a b) (list cl-cc/type:+type-unknown+ cl-cc/type:+type-unknown+)
    (assert-not-unifies a b)))
