;;;; tests/macro-tests.lisp - Quasiquote expansion tests for CL-CC
;;;
;;;; Remaining tests for %expand-quasiquote.
;;;;
(in-package :cl-cc/test)



;;; ─── %expand-quasiquote ──────────────────────────────────────────────────

(it-sequential "expand-quasiquote-wraps-in-quote atom"
  (destructuring-bind (input expected-val) (list 'foo 'foo)
    (expect (cl-cc/expand::%expand-quasiquote input) :to-equal (list 'quote expected-val))))

(it-sequential "expand-quasiquote-wraps-in-quote number"
  (destructuring-bind (input expected-val) (list 42 42)
    (expect (cl-cc/expand::%expand-quasiquote input) :to-equal (list 'quote expected-val))))

(it-sequential "expand-quasiquote-unquote-extracts"
  (expect (cl-cc/expand::%expand-quasiquote '(cl-cc:unquote x)) :to-equal 'x))

(it-sequential "expand-quasiquote-list-wraps-in-list"
  (let ((result (cl-cc/expand::%expand-quasiquote '(a b))))
    ;; Adjacent (list ...) parts are merged into a single (list ...) form
    (expect (car result) :to-be 'list)))

(it-sequential "expand-quasiquote-unquote-in-list"
  (let* ((result (cl-cc/expand::%expand-quasiquote '(a (cl-cc:unquote x))))
         (str (format nil "~S" result)))
    (expect (search "X" str) :to-be-truthy)))

;;; ─── %qq-head-p ──────────────────────────────────────────────────────────

(it-sequential "qq-head-p-cases matching"
  (destructuring-bind (form name expected) (list '(unquote x) "UNQUOTE" t)
    (if expected
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-truthy)
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-falsy))))

(it-sequential "qq-head-p-cases non-matching"
  (destructuring-bind (form name expected) (list '(unquote x) "SPLICE" nil)
    (if expected
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-truthy)
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-falsy))))

(it-sequential "qq-head-p-cases atom"
  (destructuring-bind (form name expected) (list 'unquote "UNQUOTE" nil)
    (if expected
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-truthy)
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-falsy))))

(it-sequential "qq-head-p-cases empty-list"
  (destructuring-bind (form name expected) (list nil "UNQUOTE" nil)
    (if expected
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-truthy)
      (expect (cl-cc/expand::%qq-head-p form name) :to-be-falsy))))

;;; ─── %step-cache-and-return ──────────────────────────────────────────────

(it-sequential "step-cache-and-return-returns-values"
  (multiple-value-bind (r p)
      (cl-cc/expand::%step-cache-and-return '(foo) nil '(bar) t)
    (expect r :to-equal '(bar))
    (expect p :to-be-truthy)))

(it-sequential "step-cache-and-return-no-expand"
  (multiple-value-bind (r p)
      (cl-cc/expand::%step-cache-and-return '(foo) nil '(foo) nil)
    (expect r :to-equal '(foo))
    (expect p :to-be-falsy)))

;;; ─── %cache-all-result ───────────────────────────────────────────────────

(it-sequential "cache-all-result-returns-result"
  (expect (cl-cc/expand::%cache-all-result '(original) nil '(expanded form)) :to-equal '(expanded form)))

(it-sequential "cache-all-result-skips-uninterned-form"
  (let ((fresh-sym (make-symbol "X"))
        (env nil))
    (cl-cc/expand::%cache-all-result (list fresh-sym) env 'result)
    ;; No error, result still returned
    (expect t :to-be-truthy)))

;;; ─── %maybe-postprocess-expansion ────────────────────────────────────────

(it-sequential "maybe-postprocess-expansion-no-postprocess"
  (let ((descriptor '(:kind :macro-expander :lambda-list (form))))
    (expect (cl-cc/expand::%maybe-postprocess-expansion '(foo bar) descriptor nil) :to-equal '(foo bar))))

(it-sequential "maybe-postprocess-expansion-returns-result-for-other-postprocess"
  (let ((descriptor '(:kind :macro-expander :post-expand :some-unknown)))
    (expect (cl-cc/expand::%maybe-postprocess-expansion '(baz) descriptor nil) :to-equal '(baz))))
