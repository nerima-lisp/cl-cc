;;;; packages/cps/tests/cps-trmc-tests.lisp
;;;; Unit tests for the TRMC (Tail Recursion Modulo Cons) transformation
;;;; functions defined in packages/cps/src/cps.lisp.
;;;;
;;;; These tests exercise the transformation functions directly, not via
;;;; run-string or the full compilation pipeline. (Migrated to cl-weave native
;;;; it-sequential/expect.)

(in-package :cl-cc/test)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers (local to this file)
;;; ─────────────────────────────────────────────────────────────────────────

(defun %trmc-contains-p (form sym)
  "Return T if SYM appears anywhere in FORM (recursive tree walk)."
  (cond ((eq form sym) t)
        ((consp form) (or (%trmc-contains-p (car form) sym)
                          (%trmc-contains-p (cdr form) sym)))
        (t nil)))

(defun %trmc-rewritten-p (source rewritten)
  "Return T when REWRITTEN has the accumulator-worker shape expected of TRMC output."
  (and (not (equal source rewritten))
       (%trmc-contains-p rewritten 'labels)
       (or (%trmc-contains-p rewritten 'nreverse)
           (%trmc-contains-p rewritten 'nreconc))))

;;; Test 1 — Simple cons tail-recursive pattern produces labels/accumulator

(it-sequential "cps-trmc-simple-cons-is-transformed"
  (let* ((source '(defun trmc-simple (n)
                   (if (zerop n)
                       nil
                       (cons n (trmc-simple (1- n))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (%trmc-rewritten-p source rewritten) :to-be-truthy)
    (expect (first rewritten) :to-equal 'defun)
    (expect (second rewritten) :to-equal 'trmc-simple)
    (eval rewritten)
    (expect (funcall (symbol-function 'trmc-simple) 3) :to-equal '(3 2 1))))

;;; Test 2 — Pure tail recursion (no cons) is NOT transformed

(it-sequential "cps-trmc-pure-tail-recursion-not-transformed"
  (let* ((source '(defun trmc-sum (n acc)
                   (if (zerop n)
                       acc
                       (trmc-sum (1- n) (+ acc n)))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect rewritten :to-equal source)
    (expect (%trmc-contains-p rewritten 'labels) :to-be-falsy)))

;;; Test 3 — Multi-head cons chain (nested cons) is transformed correctly

(it-sequential "cps-trmc-multi-head-cons-chain-transformed"
  (let* ((source '(defun trmc-two-heads (n)
                   (if (zerop n)
                       nil
                       (cons n (cons (- n) (trmc-two-heads (1- n)))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (expect (%trmc-rewritten-p source rewritten) :to-be-truthy)
    (eval rewritten)
    (expect (funcall (symbol-function 'trmc-two-heads) 2) :to-equal '(2 -2 1 -1))))

;;; Test 4 — Idempotency: applying the transform twice gives the same result

(it-sequential "cps-trmc-idempotent"
  (let* ((source '(defun trmc-idem (n)
                   (if (zerop n)
                       nil
                       (cons n (trmc-idem (1- n))))))
         (once   (cl-cc/cps::trmc-transform-defun-form source))
         (twice  (cl-cc/cps::trmc-transform-defun-form once)))
    (expect (%trmc-rewritten-p source once) :to-be-truthy)
    (expect twice :to-equal once)))

;;; Test 5 — Edge case: *enable-trmc* nil disables the transformation

(it-sequential "cps-trmc-disabled-by-parameter"
  (let ((source '(defun trmc-disabled (n)
                  (if (zerop n)
                      nil
                      (cons n (trmc-disabled (1- n)))))))
    (let ((cl-cc/cps:*enable-trmc* nil))
      (expect (cl-cc/cps::trmc-transform-defun-form source) :to-equal source))))

;;; Test 6 — Edge cases: lambda-lists containing non-symbol elements are skipped
;;; (was deftest-each; expanded to one it-sequential per case)

(it-sequential "cps-trmc-non-simple-lambda-list-skipped optional-with-default"
  (let ((source '(defun trmc-opt (n &optional (acc nil))
                  (if (zerop n) acc (cons n (trmc-opt (1- n)))))))
    (expect (cl-cc/cps::trmc-transform-defun-form source) :to-equal source)))

(it-sequential "cps-trmc-non-simple-lambda-list-skipped key-with-default"
  (let ((source '(defun trmc-key (n &key (step 1))
                  (if (zerop n) nil (cons n (trmc-key (- n step)))))))
    (expect (cl-cc/cps::trmc-transform-defun-form source) :to-equal source)))

(it-sequential "cps-trmc-non-simple-lambda-list-skipped not-a-defun"
  (let ((source '(defmacro trmc-mac (n)
                  `(cons ,n (trmc-mac (1- ,n))))))
    (expect (cl-cc/cps::trmc-transform-defun-form source) :to-equal source)))
