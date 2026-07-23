;;;; tests/unit/type/kind-tests.lisp — Kind System Tests
;;;;
;;;; Tests for src/type/kind.lisp:
;;;; kind structs, kind-fun, kind-equal-p, kind-to-string, kind variables.

(in-package :cl-cc/test)



;;; ─── kind struct predicates ──────────────────────────────────────────────────

(it-sequential "kind-singleton-predicates type"
  (destructuring-bind (pred-fn kind) (list #'kind-type-p +kind-type+)
    (expect (funcall pred-fn kind) :to-be-truthy)))

(it-sequential "kind-singleton-predicates effect"
  (destructuring-bind (pred-fn kind) (list #'kind-effect-p +kind-effect+)
    (expect (funcall pred-fn kind) :to-be-truthy)))

(it-sequential "kind-singleton-predicates constraint"
  (destructuring-bind (pred-fn kind) (list #'cl-cc/type:kind-constraint-p +kind-constraint+)
    (expect (funcall pred-fn kind) :to-be-truthy)))

(it-sequential "kind-singleton-predicates multiplicity"
  (destructuring-bind (pred-fn kind) (list #'cl-cc/type:kind-multiplicity-p +kind-multiplicity+)
    (expect (funcall pred-fn kind) :to-be-truthy)))

(it-sequential "kind-row-singletons row-type"
  (destructuring-bind (row-kind elem-pred) (list +kind-row-type+ #'kind-type-p)
    (expect (kind-row-p row-kind) :to-be-truthy) (expect (funcall elem-pred (kind-row-elem row-kind)) :to-be-truthy)))

(it-sequential "kind-row-singletons row-effect"
  (destructuring-bind (row-kind elem-pred) (list +kind-row-effect+ #'kind-effect-p)
    (expect (kind-row-p row-kind) :to-be-truthy) (expect (funcall elem-pred (kind-row-elem row-kind)) :to-be-truthy)))

;;; ─── kind-fun (arrow construction) ──────────────────────────────────────────

(it-sequential "kind-fun-builds-arrow"
  (let ((k (kind-fun +kind-type+ +kind-type+)))
    (expect (kind-arrow-p k) :to-be-truthy)
    (expect (kind-type-p (kind-arrow-from k)) :to-be-truthy)
    (expect (kind-type-p (kind-arrow-to k)) :to-be-truthy)))

(it-sequential "kind-fun-nested"
  (let ((k (kind-fun +kind-type+ (kind-fun +kind-type+ +kind-type+))))
    (expect (kind-arrow-p k) :to-be-truthy)
    (expect (kind-type-p (kind-arrow-from k)) :to-be-truthy)
    (expect (kind-arrow-p (kind-arrow-to k)) :to-be-truthy)))

;;; ─── kind variables ─────────────────────────────────────────────────────────

(it-sequential "kind-var-properties"
  (let ((kv  (fresh-kind-var))
        (kv1 (fresh-kind-var))
        (kv2 (fresh-kind-var))
        (kvn (fresh-kind-var "test")))
    (expect (kind-var-p kv) :to-be-truthy)
    (expect (kind-var-equal-p kv1 kv2) :to-be-falsy)
    (expect (kind-var-equal-p kv kv) :to-be-truthy)
    (expect (kind-var-p kvn) :to-be-truthy)
    (expect (cl-cc/type:kind-var-name kvn) :to-equal "test")))

;;; ─── kind-equal-p ───────────────────────────────────────────────────────────

(it-sequential "kind-equal-same type-type"
  (destructuring-bind (k1 k2) (list +kind-type+ +kind-type+)
    (expect (kind-equal-p k1 k2) :to-be-truthy)))

(it-sequential "kind-equal-same effect-effect"
  (destructuring-bind (k1 k2) (list +kind-effect+ +kind-effect+)
    (expect (kind-equal-p k1 k2) :to-be-truthy)))

(it-sequential "kind-equal-same constraint"
  (destructuring-bind (k1 k2) (list +kind-constraint+ +kind-constraint+)
    (expect (kind-equal-p k1 k2) :to-be-truthy)))

(it-sequential "kind-equal-same multiplicity"
  (destructuring-bind (k1 k2) (list +kind-multiplicity+ +kind-multiplicity+)
    (expect (kind-equal-p k1 k2) :to-be-truthy)))

(it-sequential "kind-not-equal-different type-vs-effect"
  (destructuring-bind (k1 k2) (list +kind-type+ +kind-effect+)
    (expect (kind-equal-p k1 k2) :to-be-falsy)))

(it-sequential "kind-not-equal-different type-vs-row"
  (destructuring-bind (k1 k2) (list +kind-type+ +kind-row-type+)
    (expect (kind-equal-p k1 k2) :to-be-falsy)))

(it-sequential "kind-not-equal-different effect-vs-row"
  (destructuring-bind (k1 k2) (list +kind-effect+ +kind-row-effect+)
    (expect (kind-equal-p k1 k2) :to-be-falsy)))

(it-sequential "kind-equal-arrow-and-row"
  (expect (kind-equal-p (kind-fun +kind-type+ +kind-type+)
                               (kind-fun +kind-type+ +kind-type+)) :to-be-truthy)
  (expect (kind-equal-p (kind-fun +kind-type+ +kind-effect+)
                               (kind-fun +kind-type+ +kind-type+)) :to-be-falsy)
  (expect (kind-equal-p +kind-row-type+ +kind-row-type+) :to-be-truthy)
  (expect (kind-equal-p +kind-row-type+ +kind-row-effect+) :to-be-falsy))

;;; ─── kind-to-string ─────────────────────────────────────────────────────────

(it-sequential "kind-to-string-values star"
  (destructuring-bind (expected kind) (list "*" +kind-type+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-values effect"
  (destructuring-bind (expected kind) (list "Effect" +kind-effect+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-values constraint"
  (destructuring-bind (expected kind) (list "Constraint" +kind-constraint+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-values multiplicity"
  (destructuring-bind (expected kind) (list "Multiplicity" +kind-multiplicity+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-values row-star"
  (destructuring-bind (expected kind) (list "Row *" +kind-row-type+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-values row-effect"
  (destructuring-bind (expected kind) (list "Row Effect" +kind-row-effect+)
    (expect (kind-to-string kind) :to-equal expected)))

(it-sequential "kind-to-string-computed-kinds"
  (expect (kind-to-string (kind-fun +kind-type+ +kind-type+)) :to-equal "* -> *")
  (let ((inner (kind-fun +kind-type+ +kind-type+)))
    (expect (kind-to-string (kind-fun inner +kind-type+)) :to-equal "(* -> *) -> *"))
  (let ((kv (fresh-kind-var "foo")))
    (expect (kind-to-string kv) :to-equal "kfoo")))

(it-sequential "kind-type-singleton"
  (expect (kind-type-p +kind-type+) :to-be-truthy)
  (expect (kind-node-p +kind-type+) :to-be-truthy)
  (expect (kind-equal-p +kind-type+ +kind-type+) :to-be-truthy)
  (expect (kind-equal-p (make-kind-type) (make-kind-type)) :to-be-truthy))

(it-sequential "kind-arrow-creation"
  (let ((list-kind (kind-fun +kind-type+ +kind-type+)))
    (expect (kind-arrow-p list-kind) :to-be-truthy)
    (expect (kind-equal-p (kind-arrow-from list-kind) +kind-type+) :to-be-truthy)
    (expect (kind-equal-p (kind-arrow-to list-kind)   +kind-type+) :to-be-truthy)
    (let ((fix-kind (kind-fun list-kind +kind-type+)))
      (expect (kind-arrow-p fix-kind) :to-be-truthy)
      (expect (kind-equal-p (kind-arrow-from fix-kind) list-kind) :to-be-truthy))))

(it-sequential "kind-effect-row-singletons"
  (expect (kind-effect-p +kind-effect+) :to-be-truthy)
  (expect (kind-row-p +kind-row-type+) :to-be-truthy)
  (expect (kind-row-p +kind-row-effect+) :to-be-truthy)
  (expect (kind-equal-p (kind-row-elem +kind-row-type+)   +kind-type+) :to-be-truthy)
  (expect (kind-equal-p (kind-row-elem +kind-row-effect+) +kind-effect+) :to-be-truthy))

(it-sequential "kind-equal-p-basic *=*"
  (destructuring-bind (should-be-equal k1 k2) (list t +kind-type+ +kind-type+)
    (if should-be-equal
      (expect (kind-equal-p k1 k2) :to-be-truthy)
      (expect (kind-equal-p k1 k2) :to-be-falsy))))

(it-sequential "kind-equal-p-basic Eff=Eff"
  (destructuring-bind (should-be-equal k1 k2) (list t +kind-effect+ +kind-effect+)
    (if should-be-equal
      (expect (kind-equal-p k1 k2) :to-be-truthy)
      (expect (kind-equal-p k1 k2) :to-be-falsy))))

(it-sequential "kind-equal-p-basic *≠Eff"
  (destructuring-bind (should-be-equal k1 k2) (list nil +kind-type+ +kind-effect+)
    (if should-be-equal
      (expect (kind-equal-p k1 k2) :to-be-truthy)
      (expect (kind-equal-p k1 k2) :to-be-falsy))))

(it-sequential "kind-var-fresh-and-equality"
  (let ((k1 (fresh-kind-var 'k))
        (k2 (fresh-kind-var 'k)))
    (expect (kind-var-p k1) :to-be-truthy)
    (expect (kind-var-p k2) :to-be-truthy)
    (expect (kind-var-equal-p k1 k2) :to-be-falsy)
    (expect (kind-var-equal-p k1 k1) :to-be-truthy))
  (expect (kind-equal-p (kind-fun +kind-type+ +kind-type+)
                               (kind-fun +kind-type+ +kind-type+)) :to-be-truthy)
  (expect (kind-equal-p (kind-fun +kind-type+ +kind-effect+)
                               (kind-fun +kind-type+ +kind-type+)) :to-be-falsy))
