;;;; tests/unit/expand/macros-hof-search-tests.lisp
;;;; Coverage tests for src/expand/macros-hof-search.lisp
;;;;
;;;; Macros tested: position-if, position-if-not, count-if, count-if-not,
;;;; find-if-not, assoc, assoc-if, assoc-if-not, rassoc-if, rassoc-if-not

(in-package :cl-cc/test)


;;; ── position-if / position-if-not ──────────────────────────────────────────

(it-sequential "position-if-without-key-uses-block"
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst))))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-head-p 'block result) :to-be-truthy)))

(it-sequential "position-if-with-key-expands-to-let"
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (expect (car result) :to-be 'let)))

(it-sequential "position-if-not-delegates-to-complement"
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst))))
    (expect (car result) :to-be 'position-if)
    (expect (caadr result) :to-equal 'complement)))

(it-sequential "position-if-not-with-key-forwards-key"
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst :key #'car))))
    (expect (car result) :to-be 'position-if)
    (expect (member :key result) :to-be-truthy)))

;;; ── count-if / count-if-not ─────────────────────────────────────────────────

(it-sequential "count-if-without-key-uses-dolist"
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst))))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-head-p 'dolist result) :to-be-truthy)))

(it-sequential "count-if-with-key-expands-to-let"
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst :key #'car))))
    (expect (car result) :to-be 'let)))

(it-sequential "count-if-not-delegates-to-complement"
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst))))
    (expect (car result) :to-be 'count-if)
    (expect (caadr result) :to-equal 'complement)))

(it-sequential "count-if-not-with-key-forwards-key"
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (expect (car result) :to-be 'count-if)
    (expect (member :key result) :to-be-truthy)))

;;; ── find-if-not ─────────────────────────────────────────────────────────────

(it-sequential "find-if-not-delegates-to-complement"
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst))))
    (expect (car result) :to-be 'find-if)
    (expect (caadr result) :to-equal 'complement)))

(it-sequential "find-if-not-with-key-forwards-key"
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (expect (car result) :to-be 'find-if)
    (expect (member :key result) :to-be-truthy)))

;;; ── assoc / assoc-if / assoc-if-not ────────────────────────────────────────

(it-sequential "assoc-without-test-uses-block"
  (let ((result (our-macroexpand-1 '(assoc item alist))))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-head-p 'block result) :to-be-truthy)))

(it-sequential "assoc-with-test-also-uses-block"
  (let ((result (our-macroexpand-1 '(assoc item alist :test #'equal))))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-head-p 'block result) :to-be-truthy)))

(it-sequential "assoc-if-uses-dolist"
  (let ((result (our-macroexpand-1 '(assoc-if #'oddp alist))))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-head-p 'dolist result) :to-be-truthy)))

(it-sequential "assoc-if-not-delegates-to-complement"
  (let ((result (our-macroexpand-1 '(assoc-if-not #'oddp alist))))
    (expect (car result) :to-be 'assoc-if)
    (expect (caadr result) :to-equal 'complement)))

;;; ── rassoc-if / rassoc-if-not ───────────────────────────────────────────────

(it-sequential "rassoc-if-uses-dolist"
  (let ((result (our-macroexpand-1 '(rassoc-if #'oddp alist))))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be 'dolist)))

(it-sequential "rassoc-if-not-delegates-to-complement"
  (let ((result (our-macroexpand-1 '(rassoc-if-not #'oddp alist))))
    (expect (car result) :to-be 'rassoc-if)
    (expect (caadr result) :to-equal 'complement)))
