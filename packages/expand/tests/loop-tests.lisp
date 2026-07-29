;;;; tests/unit/expand/loop-tests.lisp — LOOP generator layer unit tests

(in-package :cl-cc/test)



(it-sequential "loop-build-return-forms-prefers-result-form"
  (expect (cl-cc/expand::%loop-build-return-forms '(foo bar) nil nil) :to-equal '((values foo bar)))
  (expect (cl-cc/expand::%loop-build-return-forms '(foo) nil nil) :to-equal '(foo)))

(it-sequential "loop-build-return-forms-falls-back-to-accumulators-and-vacuous-truth"
  (expect (cl-cc/expand::%loop-build-return-forms nil '(b a) nil) :to-equal '((values a b)))
  (expect (cl-cc/expand::%loop-build-return-forms nil nil '((:always t))) :to-equal '(t))
  (expect (cl-cc/expand::%loop-build-return-forms nil nil nil) :to-be-null))

(it-sequential "loop-replace-finish-substitutes-loop-finish-recursively"
  (expect (cl-cc/expand::%loop-replace-finish
                 '((loop-finish) '(loop-finish) (if x (loop-finish) y))
                 'done) :to-equal '((go done) (quote (loop-finish)) (if x (go done) y))))
