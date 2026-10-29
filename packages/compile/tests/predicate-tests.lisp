;;;; tests/unit/expand/predicate-tests.lisp — Predicate tests

(in-package :cl-cc/test)

(it-sequential "find-if-not-runtime found"
  (destructuring-bind (form expected) (list "(find-if-not #'oddp '(1 3 4 5 6))" 4)
    (expect (run-string form) :to-equal expected)))

(it-sequential "find-if-not-runtime not-found"
  (destructuring-bind (form expected) (list "(find-if-not #'numberp '(1 2 3))" nil)
    (expect (run-string form) :to-equal expected)))

(it-sequential "position-if-runtime found"
  (destructuring-bind (form expected) (list "(position-if #'evenp '(1 3 4 7 8))" 2)
    (expect (run-string form) :to-equal expected)))

(it-sequential "position-if-runtime not-found"
  (destructuring-bind (form expected) (list "(position-if #'evenp '(1 3 5))" nil)
    (expect (run-string form) :to-equal expected)))


(it-sequential "position-if-not-runtime found"
  (destructuring-bind (form expected) (list "(position-if-not #'oddp '(1 3 4 5))" 2)
    (expect (run-string form) :to-equal expected)))

(it-sequential "position-if-not-runtime not-found"
  (destructuring-bind (form expected) (list "(position-if-not #'oddp '(1 3 5))" nil)
    (expect (run-string form) :to-equal expected)))


(it-sequential "count-if-not-runtime some"
  (destructuring-bind (form expected) (list "(count-if-not #'oddp '(1 2 3 4 5))" 2)
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "count-if-not-runtime empty"
  (destructuring-bind (form expected) (list "(count-if-not #'numberp '())" 0)
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "member-if-runtime found"
  (destructuring-bind (form expected) (list "(member-if #'evenp '(1 3 4 5 6))" '(4 5 6))
    (expect (run-string form) :to-equal expected)))

(it-sequential "member-if-runtime not-found"
  (destructuring-bind (form expected) (list "(member-if #'evenp '(1 3 5))" nil)
    (expect (run-string form) :to-equal expected)))

(it-sequential "member-if-not-runtime found"
  (destructuring-bind (form expected) (list "(member-if-not #'oddp '(1 3 4 5))" '(4 5))
    (expect (run-string form) :to-equal expected)))

(it-sequential "member-if-not-runtime not-found"
  (destructuring-bind (form expected) (list "(member-if-not #'oddp '(1 3 5))" nil)
    (expect (run-string form) :to-equal expected)))


(it-sequential "assoc-if-runtime found"
  (destructuring-bind (form expected) (list "(car (assoc-if #'evenp '((1 . 10) (2 . 20) (3 . 30))))" 2)
    (expect (run-string form) :to-equal expected)))

(it-sequential "assoc-if-runtime not-found"
  (destructuring-bind (form expected) (list "(assoc-if #'evenp '((1 . 10) (3 . 30)))" nil)
    (expect (run-string form) :to-equal expected)))

(it-sequential "assoc-if-not-runtime found"
  (destructuring-bind (form expected) (list "(car (assoc-if-not #'evenp '((2 . 20) (3 . 30))))" 3)
    (expect (run-string form) :to-equal expected)))

(it-sequential "assoc-if-not-runtime not-found"
  (destructuring-bind (form expected) (list "(assoc-if-not #'evenp '((2 . 20) (4 . 40)))" nil)
    (expect (run-string form) :to-equal expected)))

