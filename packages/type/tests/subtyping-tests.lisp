;;;; tests/unit/type/subtyping-tests.lisp — Subtyping Relation Tests
;;;;
;;;; Tests for src/type/subtyping.lisp:
;;;; type-name-subtype-p, is-subtype-p, find-common-supertype,
;;;; type-join, type-meet.

(in-package :cl-cc/test)



;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun prim (name) (make-type-primitive :name name))

;;; ─── type-name-subtype-p (CL hierarchy table lookup) ─────────────────────────

(it-sequential "subtype-name-reflexive fixnum"
  (destructuring-bind (name) (list 'fixnum)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-reflexive integer"
  (destructuring-bind (name) (list 'integer)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-reflexive number"
  (destructuring-bind (name) (list 'number)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-reflexive string"
  (destructuring-bind (name) (list 'string)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-reflexive symbol"
  (destructuring-bind (name) (list 'symbol)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-reflexive t"
  (destructuring-bind (name) (list 't)
    (expect (cl-cc/type:type-name-subtype-p name name) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain fixnum<:integer"
  (destructuring-bind (sub super) (list 'fixnum 'integer)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain fixnum<:number"
  (destructuring-bind (sub super) (list 'fixnum 'number)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain integer<:rational"
  (destructuring-bind (sub super) (list 'integer 'rational)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain integer<:real"
  (destructuring-bind (sub super) (list 'integer 'real)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain rational<:real"
  (destructuring-bind (sub super) (list 'rational 'real)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain real<:number"
  (destructuring-bind (sub super) (list 'real 'number)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain float<:real"
  (destructuring-bind (sub super) (list 'float 'real)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-numeric-chain number<:t"
  (destructuring-bind (sub super) (list 'number 't)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-not-subtype integer-not<:fixnum"
  (destructuring-bind (sub super) (list 'integer 'fixnum)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-falsy)))

(it-sequential "subtype-name-not-subtype string-not<:number"
  (destructuring-bind (sub super) (list 'string 'number)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-falsy)))

(it-sequential "subtype-name-not-subtype symbol-not<:integer"
  (destructuring-bind (sub super) (list 'symbol 'integer)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-falsy)))

(it-sequential "subtype-name-not-subtype number-not<:string"
  (destructuring-bind (sub super) (list 'number 'string)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-falsy)))

(it-sequential "subtype-name-list-hierarchy null<:list"
  (destructuring-bind (sub super) (list 'null 'list)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-list-hierarchy null<:sequence"
  (destructuring-bind (sub super) (list 'null 'sequence)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-list-hierarchy cons<:list"
  (destructuring-bind (sub super) (list 'cons 'list)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-list-hierarchy list<:sequence"
  (destructuring-bind (sub super) (list 'list 'sequence)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-list-hierarchy string<:sequence"
  (destructuring-bind (sub super) (list 'string 'sequence)
    (expect (cl-cc/type:type-name-subtype-p sub super) :to-be-truthy)))

(it-sequential "subtype-name-everything-subtype-of-t"
  (dolist (entry cl-cc/type:*subtype-table*)
    (expect (cl-cc/type:type-name-subtype-p (car entry) 't) :to-be-truthy)))

;;; ─── is-subtype-p (structural subtyping) ────────────────────────────────────

(it-sequential "subtype-primitive-and-gradual-typing"
  (expect (cl-cc/type:is-subtype-p type-int     type-int) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p type-int     type-any) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p cl-cc/type:+type-unknown+ type-int) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p type-string cl-cc/type:+type-unknown+) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p type-string type-int) :to-be-falsy))

(it-sequential "is-subtype-p-basic-wrapper"
  (expect (cl-cc/type:is-subtype-p type-int type-any) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p type-string type-int) :to-be-falsy))

(it-sequential "subtypep-wrapper-returns-two-values"
  (multiple-value-bind (ok surep)
      (cl-cc/type:subtypep 'fixnum 'integer)
    (expect ok :to-be-truthy)
    (expect surep :to-be-truthy)))

;;; ─── Union subtyping ────────────────────────────────────────────────────────

(it-sequential "subtype-union-is-subtype-of-any"
  (let ((u (make-type-union (list type-int type-string))))
    (expect (cl-cc/type:is-subtype-p u type-any) :to-be-truthy)))

(it-sequential "subtype-union-not-subtype-of-member"
  (let ((u (make-type-union (list type-int type-string))))
    (expect (cl-cc/type:is-subtype-p u type-int) :to-be-falsy)))

(it-sequential "subtype-member-is-subtype-of-union"
  (let ((u (make-type-union (list type-int type-string))))
    (expect (cl-cc/type:is-subtype-p type-int u) :to-be-truthy)))

;;; ─── Intersection subtyping ─────────────────────────────────────────────────

(it-sequential "subtype-intersection-is-subtype-of-left-member"
  (let ((i-int-str (make-type-intersection (list type-int type-string))))
    (expect (cl-cc/type:is-subtype-p i-int-str type-int) :to-be-truthy)))

(it-sequential "subtype-type-not-subtype-of-intersection"
  (let ((i-int-str (make-type-intersection (list type-int type-string))))
    (expect (cl-cc/type:is-subtype-p type-int i-int-str) :to-be-falsy)))

(it-sequential "subtype-type-is-subtype-of-intersection-with-any"
  (let ((i-int-any (make-type-intersection (list type-int type-any))))
    (expect (cl-cc/type:is-subtype-p type-int i-int-any) :to-be-truthy)))

;;; ─── Record / variant structural subtyping ─────────────────────────────────

(it-sequential "subtype-record-width-and-field-types"
  (let ((wide   (make-type-record :fields (list (cons 'x type-int)
                                                 (cons 'y type-string))
                                  :row-var nil))
        (narrow (make-type-record :fields (list (cons 'x type-int))
                                  :row-var nil)))
    (expect (cl-cc/type:is-subtype-p wide narrow) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p narrow wide) :to-be-falsy)))

(it-sequential "subtype-class-shape-satisfies-has-slots"
  (let ((cl-cc/type:*class-type-registry* (make-hash-table :test #'eq)))
    (cl-cc/type:register-class-type
     'point
     (list (cons 'x type-int)
           (cons 'y type-int)
           (cons 'label type-string)))
    (expect (cl-cc/type:is-subtype-p (make-type-primitive :name 'point)
                              (cl-cc/type:parse-type-specifier '(has-slots :x :y))) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p (make-type-primitive :name 'point)
                              (cl-cc/type:parse-type-specifier '(has-slots :z))) :to-be-falsy)))

(it-sequential "subtype-class-shape-satisfies-registered-protocol"
  (let ((cl-cc/type:*class-type-registry* (make-hash-table :test #'eq))
        (cl-cc/type:*class-method-type-registry* (make-hash-table :test #'eq))
        (cl-cc/type:*protocol-type-registry* (make-hash-table :test #'eq)))
    (cl-cc/type:register-protocol-type 'drawable '(draw))
    (cl-cc/type:register-class-type
     'sprite
     (list (cons 'x type-int)))
    (cl-cc/type:register-class-method-type
     'sprite 'draw (make-type-primitive :name 'function))
    (expect (cl-cc/type:is-subtype-p (make-type-primitive :name 'sprite)
                              (cl-cc/type:parse-type-specifier '(protocol drawable))) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p (make-type-primitive :name 'point)
                              (cl-cc/type:parse-type-specifier '(protocol drawable))) :to-be-falsy)))

(it-sequential "subtype-variant-width-and-case-types"
  (let ((small (make-type-variant :cases (list (cons 'ok type-int)) :row-var nil))
        (large (make-type-variant :cases (list (cons 'ok type-int)
                                                (cons 'err type-string))
                                  :row-var nil)))
    (expect (cl-cc/type:is-subtype-p small large) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p large small) :to-be-falsy)))

(it-sequential "subtype-refinement-to-base"
  (let ((refined (cl-cc/type:make-type-refinement :base type-int :predicate #'plusp)))
    (expect (cl-cc/type:is-subtype-p refined type-int) :to-be-truthy)
    (expect (cl-cc/type:is-subtype-p type-int refined) :to-be-falsy)))

;;; ─── Function subtyping (contravariant params, covariant return) ────────────

(it-sequential "subtype-function-variance"
  (expect (cl-cc/type:is-subtype-p
                (make-type-arrow (list type-int) type-string)
                (make-type-arrow (list type-int) type-string)) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p      ; covariant return
                (make-type-arrow (list type-int) type-int)
                (make-type-arrow (list type-int) type-any)) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p      ; contravariant params
                (make-type-arrow (list type-any) type-int)
                (make-type-arrow (list type-int) type-int)) :to-be-truthy)
  (expect (cl-cc/type:is-subtype-p
                 (make-type-arrow (list type-int) type-int)
                 (make-type-arrow (list type-any) type-int)) :to-be-falsy))

;;; ─── find-common-supertype ──────────────────────────────────────────────────

(it-sequential "common-supertype-numeric fixnum-integer"
  (destructuring-bind (n1 n2 expected) (list 'fixnum 'integer 'integer)
    (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (expect result :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected))))

(it-sequential "common-supertype-numeric fixnum-float"
  (destructuring-bind (n1 n2 expected) (list 'fixnum 'float 'real)
    (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (expect result :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected))))

(it-sequential "common-supertype-numeric integer-string"
  (destructuring-bind (n1 n2 expected) (list 'integer 'string 't)
    (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (expect result :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected))))

(it-sequential "common-supertype-numeric fixnum-fixnum"
  (destructuring-bind (n1 n2 expected) (list 'fixnum 'fixnum 'fixnum)
    (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (expect result :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected))))

(it-sequential "common-supertype-numeric null-cons"
  (destructuring-bind (n1 n2 expected) (list 'null 'cons 'list)
    (let ((result (cl-cc/type:find-common-supertype n1 n2)))
    (expect result :to-be-truthy)
    (expect (type-primitive-name result) :to-be expected))))

;;; ─── type-join (LUB) ────────────────────────────────────────────────────────

(it-sequential "type-lattice-identity-same join"
  (destructuring-bind (op) (list :join)
    (let ((result (if (eq op :join)
                    (cl-cc/type:type-join type-int type-int)
                    (cl-cc/type:type-meet type-int type-int))))
    (expect (type-equal-p type-int result) :to-be-truthy))))

(it-sequential "type-lattice-identity-same meet"
  (destructuring-bind (op) (list :meet)
    (let ((result (if (eq op :join)
                    (cl-cc/type:type-join type-int type-int)
                    (cl-cc/type:type-meet type-int type-int))))
    (expect (type-equal-p type-int result) :to-be-truthy))))

(it-sequential "type-join-subtype-yields-larger"
  (let ((fixnum-t (prim 'fixnum))
        (int-t    (prim 'integer)))
    (expect (type-equal-p int-t (cl-cc/type:type-join fixnum-t int-t)) :to-be-truthy)))

(it-sequential "type-join-unrelated-yields-common-supertype"
  (let ((fixnum-t (prim 'fixnum))
        (float-t  (prim 'float)))
    (let ((result (cl-cc/type:type-join fixnum-t float-t)))
      (expect (type-primitive-p result) :to-be-truthy)
      (expect (type-primitive-name result) :to-be 'real))))

(it-sequential "type-join-unknown-yields-other-type"
  (expect (type-equal-p type-string (cl-cc/type:type-join cl-cc/type:+type-unknown+ type-string)) :to-be-truthy))

(it-sequential "type-join-int-and-string-returns-truthy"
  (expect (cl-cc/type:type-join type-int type-string) :to-be-truthy))

;;; ─── type-meet (GLB) ────────────────────────────────────────────────────────

(it-sequential "type-meet-subtype-yields-smaller"
  (let ((fixnum-t (prim 'fixnum))
        (int-t    (prim 'integer)))
    (expect (type-equal-p fixnum-t (cl-cc/type:type-meet fixnum-t int-t)) :to-be-truthy)))

(it-sequential "type-meet-unknown-yields-unknown"
  (expect (cl-cc/type:type-unknown-p (cl-cc/type:type-meet cl-cc/type:+type-unknown+ type-string)) :to-be-truthy))

(it-sequential "type-meet-unrelated-yields-intersection"
  (expect (type-intersection-p (cl-cc/type:type-meet type-int type-string)) :to-be-truthy))
