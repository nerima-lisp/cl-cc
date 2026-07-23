;;;; tests/unit/expand/macros-sequence-tests.lisp
;;;; Coverage tests for src/expand/macros-sequence.lisp

(in-package :cl-cc/test)



(it-sequential "reduce-outer-is-let without-initial-value"
  (destructuring-bind (form) (list '(reduce #'+ lst))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "reduce-outer-is-let with-initial-value"
  (destructuring-bind (form) (list '(reduce #'+ lst :initial-value 0))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "reduce-without-initial-value-has-inner-let"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst)))
          (inner  (caddr result)))
    (expect (%tree-contains-head-p 'let inner) :to-be-truthy)))

(it-sequential "reduce-with-initial-value-has-loop"
  (let* ((result (our-macroexpand-1 '(reduce #'+ lst :initial-value 0)))
          (body   (cddr result)))
    (expect (%tree-contains-head-p 'loop body) :to-be-truthy)))

(it-sequential "substitute-expansion"
  (let* ((result   (our-macroexpand-1 '(substitute new old seq)))
         (dolist-f (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (car dolist-f) :to-be 'dolist)))

(it-sequential "predicate-sequence-outer-is-let substitute-if"
  (destructuring-bind (form) (list '(substitute-if new pred seq))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "predicate-sequence-outer-is-let substitute-if-not"
  (destructuring-bind (form) (list '(substitute-if-not new pred seq))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "predicate-sequence-outer-is-let delete-if"
  (destructuring-bind (form) (list '(delete-if pred seq))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "predicate-sequence-outer-is-let delete-if-not"
  (destructuring-bind (form) (list '(delete-if-not pred seq))
    (expect 'let :to-be (car (our-macroexpand-1 form)))))

(it-sequential "nsubstitute-delegates-to-substitute-variant nsubstitute"
  (destructuring-bind (form expected) (list '(nsubstitute new old seq) '(substitute new old seq))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "nsubstitute-delegates-to-substitute-variant nsubstitute-if"
  (destructuring-bind (form expected) (list '(nsubstitute-if new pred seq) '(substitute-if new pred seq))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "nsubstitute-delegates-to-substitute-variant nsubstitute-if-not"
  (destructuring-bind (form expected) (list '(nsubstitute-if-not new pred seq) '(substitute-if-not new pred seq))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "delete-delegates-to-remove"
  (let ((result (our-macroexpand-1 '(delete item seq))))
    (expect 'remove :to-be (car result))))

(it-sequential "delete-duplicates-delegates-to-remove-duplicates"
  (expect '(remove-duplicates seq) :to-equal (our-macroexpand-1 '(delete-duplicates seq))))

(it-sequential "copy-seq-delegates-to-copy-list"
  (let ((result (our-macroexpand-1 '(copy-seq seq))))
    (expect (car result) :to-be 'let)))

(it-sequential "fill-expansion"
  (let* ((result (our-macroexpand-1 '(fill seq item)))
         (body   (cddr result)))
    (expect (car result) :to-be 'let*)
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'if))) body) :to-be-truthy)))

(it-sequential "mismatch-expansion"
  (let* ((result (our-macroexpand-1 '(mismatch seq1 seq2)))
         (let-f  (caddr result)))
    (expect (car result) :to-be 'block)
    (expect (car let-f) :to-be 'let)))


(it-sequential "last-expands-to-let*-nthcdr"
  (let ((result (our-macroexpand-1 '(last xs 2))))
    (expect (car result) :to-be 'let*)
    (expect (car (caddr result)) :to-be 'nthcdr)))

(it-sequential "butlast-expands-to-let*-when"
  (let ((result (our-macroexpand-1 '(butlast xs 2))))
    (expect (car result) :to-be 'let*)
    (expect (car (caddr result)) :to-be 'when)))

(it-sequential "nbutlast-delegates-to-butlast"
  (expect '(butlast xs 2) :to-equal (our-macroexpand-1 '(nbutlast xs 2))))

(it-sequential "search-expansion"
  (let ((result (our-macroexpand-1 '(search '(1 2) xs))))
    (expect (car result) :to-be 'let*)
    (expect (search "BLOCK" (string-upcase (write-to-string result))) :to-be-truthy)))

(it-sequential "progv-expansion"
  (let* ((result   (our-macroexpand-1 '(progv syms vals (do-stuff))))
         (bindings (second result))
         (body     (caddr result)))
    (expect (car result) :to-be 'let*)
    (expect (= 3 (length bindings)) :to-be-truthy)
    (expect (car body) :to-be 'unwind-protect)))

(it-sequential "sort-with-key-structure"
  (let* ((result      (our-macroexpand-1 '(sort lst pred :key #'car)))
          (bindings    (second result))
          (body        (cddr result)))
    (expect 'let* :to-be (car result))
    (expect (= (length bindings) 5) :to-be-truthy)
    (expect (some (lambda (form)
                         (and (consp form) (eq (car form) 'loop)))
                       body) :to-be-truthy)))

(it-sequential "stable-sort-with-key-delegates-to-sort-with-key"
  (let ((result (our-macroexpand-1 '(stable-sort lst pred :key #'car))))
    (expect 'sort :to-be (car result))
    (expect '(pred :key #'car) :to-equal (cddr result))))
