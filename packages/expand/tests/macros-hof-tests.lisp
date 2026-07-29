;;;; tests/unit/expand/macros-hof-tests.lisp
;;;; Coverage tests for src/expand/macros-hof.lisp
;;;;
;;;; Macros tested: mapcar, every, some, notany, notevery, find,
;;;; position, count, mapcan, stable-sort

(in-package :cl-cc/test)



;;; ── HOF macros ───────────────────────────────────────────────────────────────

(it-sequential "hof-macro-outer-is-let mapcar"
  (destructuring-bind (form) (list '(mapcar fn lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "hof-macro-outer-is-let every"
  (destructuring-bind (form) (list '(every pred lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "hof-macro-outer-is-let some"
  (destructuring-bind (form) (list '(some pred lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "hof-macro-outer-is-let remove-if"
  (destructuring-bind (form) (list '(remove-if pred lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "hof-macro-outer-is-let remove-if-not"
  (destructuring-bind (form) (list '(remove-if-not pred lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "mapcar-body-contains-dolist"
  (let ((result (our-macroexpand-1 '(mapcar fn lst))))
    (expect (%tree-contains-head-p 'dolist result) :to-be-truthy)))

(it-sequential "every-short-circuits-on-false"
  (let ((result (our-macroexpand-1 '(every pred lst))))
    (expect (%tree-contains-head-p 'block result) :to-be-truthy)
    (expect (%tree-contains-head-p 'dolist result) :to-be-truthy)))

(it-sequential "notany-notevery-negation notany"
  (destructuring-bind (form expected) (list '(notany   pred lst) '(not (some  pred lst)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "notany-notevery-negation notevery"
  (destructuring-bind (form expected) (list '(notevery pred lst) '(not (every pred lst)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "find-no-keys-is-eql-loop"
  (let ((result (our-macroexpand-1 '(find item lst))))
    (expect (%tree-contains-head-p 'block result) :to-be-truthy)
    (expect (%tree-contains-head-p 'dolist result) :to-be-truthy)))

(it-sequential "sequence-search-macro-outer-is-let position"
  (destructuring-bind (form) (list '(position item lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "sequence-search-macro-outer-is-let count"
  (destructuring-bind (form) (list '(count item lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "sequence-search-macro-outer-is-let mapcan"
  (destructuring-bind (form) (list '(mapcan fn lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "stable-sort-delegates-to-sort"
  (expect '(sort lst pred) :to-equal (our-macroexpand-1 '(stable-sort lst pred))))
