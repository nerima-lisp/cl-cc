;;;; packages/expand/tests/match-tests.lisp --- FR-779/FR-780 MATCH tests

(in-package :cl-cc/test)



(defun %match-test-eval (form)
  (eval (our-macroexpand-all form)))

(it-sequential "match-literal-variable-and-wildcard-patterns"
  :tags
  '(:fr-779)
  (expect (%match-test-eval '(match 1
                                     (0 :zero)
                                     (1 :one)
                                     (_ :other))) :to-equal :one)
  (expect (%match-test-eval '(match 41
                                     (0 :zero)
                                     (x (+ x 1)))) :to-equal 42)
  (expect (%match-test-eval '(match :unknown
                                     (:known :known)
                                     (_ :fallback))) :to-equal :fallback))

(it-sequential "match-cons-list-and-vector-patterns"
  :tags
  '(:fr-779)
  (expect (%match-test-eval '(match '(1 . 2)
                                     ((cons x y) (+ x y))
                                     (_ :no))) :to-equal 3)
  (expect (%match-test-eval '(match '(1 2 3)
                                     ((list a b c) (+ a b c))
                                     (_ :no))) :to-equal 6)
  (expect (%match-test-eval '(match #(4 5)
                                     ((vector a b) (+ a b))
                                     (_ :no))) :to-equal 9))

(it-sequential "match-type-and-guard-patterns"
  :tags
  '(:fr-779)
  (expect (%match-test-eval '(match 5
                                     ((type integer n) (* n 2))
                                     (_ :no))) :to-equal 10)
  (expect (%match-test-eval '(match 4
                                     ((when (type integer n)
                                        (and (> n 0) (evenp n)))
                                      :positive-even)
                                     (_ :no))) :to-equal :positive-even)
  (expect (%match-test-eval '(match 1
                                     ((or 0 1 2) :small)
                                     (_ :large))) :to-equal :small)
  (expect (%match-test-eval '(match '(a b)
                                     ((and (type cons) (cons head tail))
                                      (declare (ignore head tail))
                                      :non-empty-list)
                                     (_ :no))) :to-equal :non-empty-list))

(it-sequential "match-non-exhaustive-pattern-signals-compiler-style-warning"
  :tags
  '(:fr-780)
  (let ((seen nil))
    (handler-bind ((cl-cc/expand:match-exhaustiveness-warning
                     (lambda (warning)
                       (setf seen warning)
                       (muffle-warning))))
      (our-macroexpand-1 '(match x
                           (0 :zero)
                           (1 :one))))
    (expect seen :to-be-truthy)
    (expect (typep seen 'cl-cc/vm:compiler-style-warning) :to-be-truthy)
    (expect (cl-cc/vm:compiler-diagnostic-error-code seen) :to-equal "W0780")))

(it-sequential "match-exhaustive-pattern-does-not-warn"
  :tags
  '(:fr-780)
  (let ((warnings nil))
    (handler-bind ((cl-cc/expand:match-exhaustiveness-warning
                     (lambda (warning)
                       (push warning warnings)
                       (muffle-warning))))
      (our-macroexpand-1 '(match x
                           (0 :zero)
                           (_ :other))))
    (expect warnings :to-be-null)))

(it-sequential "match-unreachable-pattern-signals-warning"
  :tags
  '(:fr-780)
  (let ((warnings nil))
    (handler-bind ((cl-cc/expand:match-unreachable-pattern-warning
                     (lambda (warning)
                       (push warning warnings)
                       (muffle-warning)))
                   (cl-cc/expand:match-exhaustiveness-warning #'muffle-warning))
      (our-macroexpand-1 '(match x
                           (1 :first)
                           (1 :duplicate)
                           (_ :other)
                           (2 :after-wildcard))))
    (expect (= 2 (length warnings)) :to-be-truthy)
    (expect (every (lambda (warning)
                          (equal "W0781"
                                 (cl-cc/vm:compiler-diagnostic-error-code warning)))
                        warnings) :to-be-truthy)))
