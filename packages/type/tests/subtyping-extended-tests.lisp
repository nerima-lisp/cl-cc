;;;; tests/unit/type/subtyping-extended-tests.lisp — Extended Subtyping and Lattice Tests
;;;;
;;;; Additional coverage for is-subtype-p, type-join, and type-meet beyond the
;;;; base cases in subtyping-tests.lisp. Depends on subtyping-tests.lisp being
;;;; loaded first (via ASDF :serial t) for the prim helper and suite definition.

(in-package :cl-cc/test)


(defmacro assert-subtyping-expected-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── is-subtype-p — extended reflexivity and hierarchy ───────────────────────

(it-sequential "is-subtype-reflexive int"
  (destructuring-bind (tp) (list type-int)
    (expect (cl-cc/type:is-subtype-p tp tp) :to-be-truthy)))

(it-sequential "is-subtype-reflexive string"
  (destructuring-bind (tp) (list type-string)
    (expect (cl-cc/type:is-subtype-p tp tp) :to-be-truthy)))

(it-sequential "is-subtype-reflexive bool"
  (destructuring-bind (tp) (list type-bool)
    (expect (cl-cc/type:is-subtype-p tp tp) :to-be-truthy)))

(it-sequential "is-subtype-reflexive null"
  (destructuring-bind (tp) (list type-null)
    (expect (cl-cc/type:is-subtype-p tp tp) :to-be-truthy)))

(it-sequential "is-subtype-primitive-hierarchy fixnum<integer"
  (destructuring-bind (name1 name2 expected) (list 'fixnum 'integer t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy integer<rational"
  (destructuring-bind (name1 name2 expected) (list 'integer 'rational t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy rational<real"
  (destructuring-bind (name1 name2 expected) (list 'rational 'real t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy float<real"
  (destructuring-bind (name1 name2 expected) (list 'float 'real t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy real<number"
  (destructuring-bind (name1 name2 expected) (list 'real 'number t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy fixnum<number"
  (destructuring-bind (name1 name2 expected) (list 'fixnum 'number t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy number<t"
  (destructuring-bind (name1 name2 expected) (list 'number 't t)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-primitive-hierarchy int not<string"
  (destructuring-bind (name1 name2 expected) (list 'fixnum 'string nil)
    (assert-subtyping-expected-case expected
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-truthy)
    (expect (cl-cc/type:type-name-subtype-p name1 name2) :to-be-falsy))))

(it-sequential "is-subtype-of-top-type int"
  (destructuring-bind (tp) (list type-int)
    (expect (cl-cc/type:is-subtype-p tp type-any) :to-be-truthy)))

(it-sequential "is-subtype-of-top-type string"
  (destructuring-bind (tp) (list type-string)
    (expect (cl-cc/type:is-subtype-p tp type-any) :to-be-truthy)))

(it-sequential "is-subtype-of-top-type bool"
  (destructuring-bind (tp) (list type-bool)
    (expect (cl-cc/type:is-subtype-p tp type-any) :to-be-truthy)))

(it-sequential "is-subtype-unknown-gradual"
  (let ((unk cl-cc/type:+type-unknown+))
    (expect (cl-cc/type:is-subtype-p unk  type-int) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p type-int unk) :to-be-truthy)))

(it-sequential "is-subtype-union-right int in or-int-string"
  (destructuring-bind (tp expected) (list type-int t)
    (let ((u (make-type-union (list type-int type-string))))
    (assert-subtyping-expected-case expected
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-truthy)
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-falsy)))))

(it-sequential "is-subtype-union-right string in or"
  (destructuring-bind (tp expected) (list type-string t)
    (let ((u (make-type-union (list type-int type-string))))
    (assert-subtyping-expected-case expected
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-truthy)
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-falsy)))))

(it-sequential "is-subtype-union-right bool not in"
  (destructuring-bind (tp expected) (list type-bool nil)
    (let ((u (make-type-union (list type-int type-string))))
    (assert-subtyping-expected-case expected
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-truthy)
      (expect (cl-cc/type:is-subtype-p tp u) :to-be-falsy)))))

(it-sequential "is-subtype-function-contravariant-params"
  (let ((f1 (make-type-arrow (list (make-type-primitive :name 'number)) type-string))
        (f2 (make-type-arrow (list (make-type-primitive :name 'fixnum))  type-string)))
    (expect (cl-cc/type:is-subtype-p f1 f2) :to-be-truthy)))

;;; ─── type-join / type-meet — extended lattice coverage ──────────────────────

(it-sequential "type-join-meet-equal-types join-int"
  (destructuring-bind (op tp) (list #'cl-cc/type:type-join type-int)
    (expect (type-equal-p tp (funcall op tp tp)) :to-be-truthy)))

(it-sequential "type-join-meet-equal-types join-string"
  (destructuring-bind (op tp) (list #'cl-cc/type:type-join type-string)
    (expect (type-equal-p tp (funcall op tp tp)) :to-be-truthy)))

(it-sequential "type-join-meet-equal-types join-bool"
  (destructuring-bind (op tp) (list #'cl-cc/type:type-join type-bool)
    (expect (type-equal-p tp (funcall op tp tp)) :to-be-truthy)))

(it-sequential "type-join-meet-equal-types meet-int"
  (destructuring-bind (op tp) (list #'cl-cc/type:type-meet type-int)
    (expect (type-equal-p tp (funcall op tp tp)) :to-be-truthy)))

(it-sequential "type-join-meet-equal-types meet-string"
  (destructuring-bind (op tp) (list #'cl-cc/type:type-meet type-string)
    (expect (type-equal-p tp (funcall op tp tp)) :to-be-truthy)))

(it-sequential "type-lattice-join-meet-subtype join"
  (destructuring-bind (op expected-name) (list #'cl-cc/type:type-join 'integer)
    (let* ((fixnum-t  (make-type-primitive :name 'fixnum))
         (integer-t (make-type-primitive :name 'integer))
         (result    (funcall op fixnum-t integer-t)))
    (expect (type-primitive-p result) :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected-name))))

(it-sequential "type-lattice-join-meet-subtype meet"
  (destructuring-bind (op expected-name) (list #'cl-cc/type:type-meet 'fixnum)
    (let* ((fixnum-t  (make-type-primitive :name 'fixnum))
         (integer-t (make-type-primitive :name 'integer))
         (result    (funcall op fixnum-t integer-t)))
    (expect (type-primitive-p result) :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected-name))))

(it-sequential "type-join-incompatible-makes-union"
  (let* ((fixnum-t (make-type-primitive :name 'fixnum))
         (string-t (make-type-primitive :name 'string))
         (result   (cl-cc/type:type-join fixnum-t string-t)))
    (expect (type-primitive-p result) :to-be-truthy)
    (expect (type-primitive-name result) :to-be 't)))

(it-sequential "type-join-with-unknown-is-other"
  (let* ((unk cl-cc/type:+type-unknown+)
         (result (cl-cc/type:type-join unk type-int)))
    (expect (type-equal-p type-int result) :to-be-truthy)))

(it-sequential "type-meet-incompatible-makes-intersection"
  (let ((result (cl-cc/type:type-meet type-int type-string)))
    (expect (type-intersection-p result) :to-be-truthy)
    (expect (= 2 (length (type-intersection-types result))) :to-be-truthy)))
