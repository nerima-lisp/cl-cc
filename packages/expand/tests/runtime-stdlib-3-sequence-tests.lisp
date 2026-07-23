;;;; runtime-stdlib-3-sequence-tests.lisp — FR-1076 sequence completion

(in-package :cl-cc/test)



(it-sequential "sequence-every-some-accept-multiple-sequences"
  (expect (%tree-contains-head-p 'apply (our-macroexpand-1 '(every #'< '(1 2) '(3 4)))) :to-be-truthy)
  (expect (%tree-contains-head-p 'apply (our-macroexpand-1 '(some #'= '(1 2) '(0 2)))) :to-be-truthy)
  (expect (car (our-macroexpand-1 '(notany #'= '(1) '(2)))) :to-be 'not)
  (expect (car (our-macroexpand-1 '(notevery #'< '(1) '(2)))) :to-be 'not))

(it-sequential "sequence-substitute-count-from-end-expands"
  (let ((expanded (our-macroexpand-1 '(substitute 9 1 '(1 2 1) :count 1 :from-end t))))
    (expect (car expanded) :to-be 'let*))
  (expect '(substitute 9 1 xs :count 1 :from-end t) :to-equal (our-macroexpand-1 '(nsubstitute 9 1 xs :count 1 :from-end t))))

(it-sequential "sequence-mismatch-start-end-expands"
  (let ((expanded (our-macroexpand-1 '(mismatch xs ys :start1 1 :end1 3 :start2 2 :end2 4))))
    (expect (car expanded) :to-be 'let)))

(it-sequential "sequence-remove-duplicates-from-end-expands"
  (let ((expanded (our-macroexpand-1 '(remove-duplicates xs :from-end t))))
    (expect (car expanded) :to-be 'let)))

(it-sequential "sequence-remove-duplicates-vector-expands-to-coerce"
  (let ((expanded (our-macroexpand-1 '(remove-duplicates #(1 2 1)))))
    (expect (%tree-contains-head-p 'coerce expanded) :to-be-truthy)))

(it-sequential "sequence-concatenate-coerces-inputs-for-list-vector"
  (let ((list-exp (our-macroexpand-1 '(concatenate 'list #(1 2) '(3))))
        (vec-exp  (our-macroexpand-1 '(concatenate 'vector '(1) #(2)))))
    (expect (%tree-contains-head-p 'coerce list-exp) :to-be-truthy)
    (expect (%tree-contains-head-p 'coerce vec-exp) :to-be-truthy)))

(it-sequential "loop-arithmetic-iota-and-type-hints"
  (let ((expanded (our-macroexpand-1 '(loop for i from 0 below n collect i))))
    (expect (member (symbol-name (car expanded)) '("IOTA") :test #'string=) :to-be-truthy))
  (let ((typed (our-macroexpand-1 '(loop for i of-type fixnum from 0 below n sum i))))
    (expect (%tree-contains-head-p 'the typed) :to-be-truthy)))
