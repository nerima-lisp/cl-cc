;;;; tests/unit/expand/predicate-tests.lisp — Predicate tests

(in-package :cl-cc/test)



(it-sequential "predicate-not-delegates-via-complement find-if-not"
  (destructuring-bind (base-op form) (list 'find-if '(find-if-not pred lst))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be base-op)
    (expect (caadr result) :to-be 'complement))))

(it-sequential "predicate-not-delegates-via-complement position-if-not"
  (destructuring-bind (base-op form) (list 'position-if '(position-if-not pred lst))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be base-op)
    (expect (caadr result) :to-be 'complement))))

(it-sequential "predicate-not-delegates-via-complement count-if-not"
  (destructuring-bind (base-op form) (list 'count-if '(count-if-not pred lst))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be base-op)
    (expect (caadr result) :to-be 'complement))))

(it-sequential "predicate-not-delegates-via-complement rassoc-if-not"
  (destructuring-bind (base-op form) (list 'rassoc-if '(rassoc-if-not pred alist))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be base-op)
    (expect (caadr result) :to-be 'complement))))

(it-sequential "predicate-not-delegates-via-complement assoc-if-not"
  (destructuring-bind (base-op form) (list 'assoc-if '(assoc-if-not pred alist))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be base-op)
    (expect (caadr result) :to-be 'complement))))

(it-sequential "find-if-not-runtime found"
  (destructuring-bind (form expected) (list "(find-if-not #'oddp '(1 3 4 5 6))" 4)
    (expect (run-string form) :to-equal expected)))

(it-sequential "find-if-not-runtime not-found"
  (destructuring-bind (form expected) (list "(find-if-not #'numberp '(1 2 3))" nil)
    (expect (run-string form) :to-equal expected)))

(it-sequential "position-if-expansion-structure"
  (let* ((result (our-macroexpand-1 '(position-if pred lst)))
         (body   (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (car body) :to-be 'block)
    (expect (second body) :to-be nil)))

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

(it-sequential "remove-if-key-expansion remove-if"
  (destructuring-bind (form) (list '(remove-if #'oddp lst :key #'car))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (> (length (cadr result)) 1) :to-be-truthy))))

(it-sequential "remove-if-key-expansion remove-if-not"
  (destructuring-bind (form) (list '(remove-if-not #'evenp lst :key #'car))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let)
    (expect (> (length (cadr result)) 1) :to-be-truthy))))

(it-sequential "find-if-not-with-key"
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (expect 'find-if :to-be (car result))))

(it-sequential "position-if-with-key"
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (expect 'let :to-be (car result))
    (expect (> (length (cadr result)) 2) :to-be-truthy)))

(it-sequential "count-if-not-with-key"
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (expect 'count-if :to-be (car result))))

(it-sequential "predicate-if-outer-is-let rassoc-if"
  (destructuring-bind (form) (list '(rassoc-if pred alist))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "predicate-if-outer-is-let assoc-if"
  (destructuring-bind (form) (list '(assoc-if pred alist))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "rassoc-if-body-checks-cdr"
  (let* ((result (our-macroexpand-1 '(rassoc-if pred alist)))
         (dolist-form (caddr result))
         (when-form (second (cdr dolist-form)))
         (and-form (second when-form))
         (funcall-form (third and-form))
         (cdr-arg (caddr funcall-form)))
    (expect 'dolist :to-be (car dolist-form))
    (expect 'funcall :to-be (car funcall-form))
    (expect 'cdr :to-be (car cdr-arg))))


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


(it-sequential "assoc-if-body-is-dolist"
  (let* ((result (our-macroexpand-1 '(assoc-if pred alist)))
         (body   (caddr result)))
    (expect 'dolist :to-be (car body))))


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

(it-sequential "complement-expansion-structure top-is-let"
  (destructuring-bind (expected accessor) (list 'let (lambda (r) (car r)))
    (expect (funcall accessor (our-macroexpand-1 '(complement pred))) :to-be expected)))

(it-sequential "complement-expansion-structure inner-lambda"
  (destructuring-bind (expected accessor) (list 'lambda (lambda (r) (car (caddr r))))
    (expect (funcall accessor (our-macroexpand-1 '(complement pred))) :to-be expected)))

(it-sequential "complement-expansion-structure body-not-head"
  (destructuring-bind (expected accessor) (list 'not (lambda (r) (car (caddr (caddr r)))))
    (expect (funcall accessor (our-macroexpand-1 '(complement pred))) :to-be expected)))

(it-sequential "complement-expansion-structure apply-in-not"
  (destructuring-bind (expected accessor) (list 'apply (lambda (r) (caadr (caddr (caddr r)))))
    (expect (funcall accessor (our-macroexpand-1 '(complement pred))) :to-be expected)))
