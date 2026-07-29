;;;; tests/unit/expand/macros-stdlib-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp (remaining forms)

(in-package :cl-cc/test)



(it-sequential "with-open-stream-expansion"
  (let ((result (our-macroexpand-1 '(with-open-stream (s stream) (write-char #\x s)))))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be 'unwind-protect)))

(it-sequential "prog-expansion"
  (let ((prog-result (our-macroexpand-1 '(prog ((x 1)) x)))
        (prog*-result (our-macroexpand-1 '(prog* ((x 1)) x))))
    (expect (car prog-result) :to-be 'block)
    (expect (car (caddr prog-result)) :to-be 'let)
    (expect (car prog*-result) :to-be 'block)
    (expect (car (caddr prog*-result)) :to-be 'let*)))

(it-sequential "fr-839-make-iterator-and-next-over-list"
  (let ((iterator (cl-cc/expand:make-iterator '(a b))))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (expect value :to-be 'a)
      (expect morep :to-be-truthy))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (expect value :to-be 'b)
      (expect morep :to-be-truthy))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (expect value :to-be-null)
      (expect morep :to-be-falsy))))

(it-sequential "fr-839-doiterator-expansion-uses-with-iterator"
  (let ((result (our-macroexpand-1 '(doiterator (x '(1 2) :done) (print x)))))
    (expect (car result) :to-be 'cl-cc/expand:with-iterator)
    (expect (search "ITERATOR-NEXT" (prin1-to-string result)) :to-be-truthy)))
