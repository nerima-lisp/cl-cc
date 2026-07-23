;;;; tests/unit/expand/macros-filesystem-tests.lisp
;;;; Coverage tests for src/expand/macros-filesystem.lisp

(in-package :cl-cc/test)



(it-sequential "time-expansion"
  (let ((result (our-macroexpand-1 '(time (+ 1 2)))))
    (expect (car result) :to-be 'let*)
    (expect (car (second (first (second result)))) :to-be 'get-universal-time)
    (expect (car (third result)) :to-be 'format)))
