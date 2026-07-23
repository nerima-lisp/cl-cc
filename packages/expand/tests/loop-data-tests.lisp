;;;; tests/unit/expand/loop-data-tests.lisp — LOOP data layer unit tests

(in-package :cl-cc/test)



(it-sequential "loop-boundary-keywords-contain-core-markers for"
  (destructuring-bind (kw) (list "FOR")
    (expect (member kw cl-cc/expand::*loop-boundary-keywords* :test #'string=) :to-be-truthy)))

(it-sequential "loop-boundary-keywords-contain-core-markers collect"
  (destructuring-bind (kw) (list "COLLECT")
    (expect (member kw cl-cc/expand::*loop-boundary-keywords* :test #'string=) :to-be-truthy)))

(it-sequential "loop-boundary-keywords-contain-core-markers finally"
  (destructuring-bind (kw) (list "FINALLY")
    (expect (member kw cl-cc/expand::*loop-boundary-keywords* :test #'string=) :to-be-truthy)))

(it-sequential "loop-boundary-keywords-contain-core-markers named"
  (destructuring-bind (kw) (list "NAMED")
    (expect (member kw cl-cc/expand::*loop-boundary-keywords* :test #'string=) :to-be-truthy)))

(it-sequential "loop-vacuous-truth-conditions-are-the-expected-two-symbols"
  (expect cl-cc/expand::*loop-vacuous-truth-conditions* :to-equal '(:always :never)))

(it-sequential "loop-accum-keyword-table-maps-canonical-and-ing-forms collect"
  (destructuring-bind (kw expected) (list "COLLECT" :collect)
    (expect (cdr (assoc kw cl-cc/expand::*loop-accum-keyword-table* :test #'string=)) :to-be expected)))

(it-sequential "loop-accum-keyword-table-maps-canonical-and-ing-forms collecting"
  (destructuring-bind (kw expected) (list "COLLECTING" :collect)
    (expect (cdr (assoc kw cl-cc/expand::*loop-accum-keyword-table* :test #'string=)) :to-be expected)))

(it-sequential "loop-accum-keyword-table-maps-canonical-and-ing-forms summing"
  (destructuring-bind (kw expected) (list "SUMMING" :sum)
    (expect (cdr (assoc kw cl-cc/expand::*loop-accum-keyword-table* :test #'string=)) :to-be expected)))

(it-sequential "loop-accum-keyword-table-maps-canonical-and-ing-forms nconcing"
  (destructuring-bind (kw expected) (list "NCONCING" :nconc)
    (expect (cdr (assoc kw cl-cc/expand::*loop-accum-keyword-table* :test #'string=)) :to-be expected)))

(it-sequential "loop-hash-keyword-tables-map-both-spellings hash-keys"
  (destructuring-bind (kw table expected) (list "HASH-KEYS" cl-cc/expand::*loop-hash-iter-keywords* :hash-keys)
    (expect (cdr (assoc kw table :test #'string=)) :to-be expected)))

(it-sequential "loop-hash-keyword-tables-map-both-spellings hash-values"
  (destructuring-bind (kw table expected) (list "HASH-VALUE" cl-cc/expand::*loop-hash-iter-keywords* :hash-values)
    (expect (cdr (assoc kw table :test #'string=)) :to-be expected)))

(it-sequential "loop-hash-keyword-tables-map-both-spellings using-key"
  (destructuring-bind (kw table expected) (list "HASH-KEY" cl-cc/expand::*loop-using-keywords* :hash-key)
    (expect (cdr (assoc kw table :test #'string=)) :to-be expected)))

(it-sequential "loop-hash-keyword-tables-map-both-spellings using-value"
  (destructuring-bind (kw table expected) (list "HASH-VALUE" cl-cc/expand::*loop-using-keywords* :hash-value)
    (expect (cdr (assoc kw table :test #'string=)) :to-be expected)))

(it-sequential "loop-emitter-dispatch-tables-are-hash-tables iter"
  (destructuring-bind (table) (list cl-cc/expand::*loop-iter-emitters*)
    (expect (hash-table-p table) :to-be-truthy) (expect (hash-table-test table) :to-be 'eq)))

(it-sequential "loop-emitter-dispatch-tables-are-hash-tables acc"
  (destructuring-bind (table) (list cl-cc/expand::*loop-acc-emitters*)
    (expect (hash-table-p table) :to-be-truthy) (expect (hash-table-test table) :to-be 'eq)))

(it-sequential "loop-emitter-dispatch-tables-are-hash-tables condition"
  (destructuring-bind (table) (list cl-cc/expand::*loop-condition-emitters*)
    (expect (hash-table-p table) :to-be-truthy) (expect (hash-table-test table) :to-be 'eq)))
