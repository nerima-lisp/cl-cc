;;;; tests/unit/type/multiplicity-tests.lisp — Multiplicity System Tests
;;;;
;;;; Tests for src/type/multiplicity.lisp:
;;;; multiplicity-p, mult-add (semiring join), mult-mul (semiring scale),
;;;; mult-leq (ordering), mult-to-string.

(in-package :cl-cc/test)




(defmacro assert-multiplicity-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── multiplicity-p ─────────────────────────────────────────────────────────

(it-sequential "mult-valid-grades zero"
  (destructuring-bind (expected grade) (list t :zero)
    (expect (multiplicity-p grade) :to-be-truthy)))

(it-sequential "mult-valid-grades one"
  (destructuring-bind (expected grade) (list t :one)
    (expect (multiplicity-p grade) :to-be-truthy)))

(it-sequential "mult-valid-grades omega"
  (destructuring-bind (expected grade) (list t :omega)
    (expect (multiplicity-p grade) :to-be-truthy)))

(it-sequential "mult-invalid-grades nil"
  (destructuring-bind (val) (list nil)
    (expect (multiplicity-p val) :to-be-falsy)))

(it-sequential "mult-invalid-grades integer"
  (destructuring-bind (val) (list 0)
    (expect (multiplicity-p val) :to-be-falsy)))

(it-sequential "mult-invalid-grades string"
  (destructuring-bind (val) (list "one")
    (expect (multiplicity-p val) :to-be-falsy)))

(it-sequential "mult-invalid-grades keyword"
  (destructuring-bind (val) (list :two)
    (expect (multiplicity-p val) :to-be-falsy)))

;;; ─── mult-add (semiring join) ───────────────────────────────────────────────

(it-sequential "mult-add-identity zero-left"
  (destructuring-bind (q1 q2 expected) (list :zero :zero :zero)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-identity one-left"
  (destructuring-bind (q1 q2 expected) (list :one :zero :one)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-identity omega-left"
  (destructuring-bind (q1 q2 expected) (list :omega :zero :omega)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-identity zero-right"
  (destructuring-bind (q1 q2 expected) (list :zero :zero :zero)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-identity one-right"
  (destructuring-bind (q1 q2 expected) (list :zero :one :one)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-identity omega-right"
  (destructuring-bind (q1 q2 expected) (list :zero :omega :omega)
    (expect (mult-add q1 q2) :to-be expected)))

(it-sequential "mult-add-idempotent zero"
  (destructuring-bind (grade expected) (list :zero :zero)
    (expect (mult-add grade grade) :to-be expected)))

(it-sequential "mult-add-idempotent one"
  (destructuring-bind (grade expected) (list :one :one)
    (expect (mult-add grade grade) :to-be expected)))

(it-sequential "mult-add-idempotent omega"
  (destructuring-bind (grade expected) (list :omega :omega)
    (expect (mult-add grade grade) :to-be expected)))

(it-sequential "mult-add-one-omega"
  (expect (mult-add :one :omega) :to-be :omega)
  (expect (mult-add :omega :one) :to-be :omega))

;;; ─── mult-mul (semiring scale) ──────────────────────────────────────────────

(it-sequential "mult-mul-zero-absorbs zero-zero"
  (destructuring-bind (q1 q2 expected) (list :zero :zero :zero)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-zero-absorbs zero-one"
  (destructuring-bind (q1 q2 expected) (list :zero :one :zero)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-zero-absorbs zero-omega"
  (destructuring-bind (q1 q2 expected) (list :zero :omega :zero)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-zero-absorbs one-zero"
  (destructuring-bind (q1 q2 expected) (list :one :zero :zero)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-zero-absorbs omega-zero"
  (destructuring-bind (q1 q2 expected) (list :omega :zero :zero)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-one-identity one-one"
  (destructuring-bind (q1 q2 expected) (list :one :one :one)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-one-identity one-omega"
  (destructuring-bind (q1 q2 expected) (list :one :omega :omega)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-one-identity omega-one"
  (destructuring-bind (q1 q2 expected) (list :omega :one :omega)
    (expect (mult-mul q1 q2) :to-be expected)))

(it-sequential "mult-mul-omega-omega"
  (expect (mult-mul :omega :omega) :to-be :omega))

;;; ─── mult-leq (ordering) ────────────────────────────────────────────────────

(it-sequential "mult-leq-reflexive zero"
  (destructuring-bind (grade) (list :zero)
    (expect (mult-leq grade grade) :to-be-truthy)))

(it-sequential "mult-leq-reflexive one"
  (destructuring-bind (grade) (list :one)
    (expect (mult-leq grade grade) :to-be-truthy)))

(it-sequential "mult-leq-reflexive omega"
  (destructuring-bind (grade) (list :omega)
    (expect (mult-leq grade grade) :to-be-truthy)))

(it-sequential "mult-leq-zero-bottom zero-one"
  (destructuring-bind (upper) (list :one)
    (expect (mult-leq :zero upper) :to-be-truthy)))

(it-sequential "mult-leq-zero-bottom zero-omega"
  (destructuring-bind (upper) (list :omega)
    (expect (mult-leq :zero upper) :to-be-truthy)))

(it-sequential "mult-leq-omega-top zero-omega"
  (destructuring-bind (lower) (list :zero)
    (expect (mult-leq lower :omega) :to-be-truthy)))

(it-sequential "mult-leq-omega-top one-omega"
  (destructuring-bind (lower) (list :one)
    (expect (mult-leq lower :omega) :to-be-truthy)))

(it-sequential "mult-leq-false-cases one-not-leq-zero"
  (destructuring-bind (lower upper) (list :one :zero)
    (expect (mult-leq lower upper) :to-be-falsy)))

(it-sequential "mult-leq-false-cases omega-not-leq-one"
  (destructuring-bind (lower upper) (list :omega :one)
    (expect (mult-leq lower upper) :to-be-falsy)))

;;; ─── mult-to-string ─────────────────────────────────────────────────────────

(it-sequential "mult-to-string-values zero"
  (destructuring-bind (expected grade) (list "0" :zero)
    (expect (mult-to-string grade) :to-equal expected)))

(it-sequential "mult-to-string-values one"
  (destructuring-bind (expected grade) (list "1" :one)
    (expect (mult-to-string grade) :to-equal expected)))

(it-sequential "mult-to-string-values omega"
  (destructuring-bind (expected grade) (list "ω" :omega)
    (expect (mult-to-string grade) :to-equal expected)))

(it-sequential "multiplicity-p-recognition zero-valid"
  (destructuring-bind (val expected) (list :zero t)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-p-recognition one-valid"
  (destructuring-bind (val expected) (list :one t)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-p-recognition omega-valid"
  (destructuring-bind (val expected) (list :omega t)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-p-recognition two-invalid"
  (destructuring-bind (val expected) (list :two nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-p-recognition nil-invalid"
  (destructuring-bind (val expected) (list nil nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-p-recognition int-invalid"
  (destructuring-bind (val expected) (list 1 nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (cl-cc/type:multiplicity-p val) :to-be-truthy)
      (expect (cl-cc/type:multiplicity-p val) :to-be-falsy))))

(it-sequential "multiplicity-grade-constants zero"
  (destructuring-bind (grade expected-kw) (list +mult-zero+ :zero)
    (expect (multiplicity-p grade) :to-be-truthy) (expect grade :to-be expected-kw)))

(it-sequential "multiplicity-grade-constants one"
  (destructuring-bind (grade expected-kw) (list +mult-one+ :one)
    (expect (multiplicity-p grade) :to-be-truthy) (expect grade :to-be expected-kw)))

(it-sequential "multiplicity-grade-constants omega"
  (destructuring-bind (grade expected-kw) (list +mult-omega+ :omega)
    (expect (multiplicity-p grade) :to-be-truthy) (expect grade :to-be expected-kw)))

(it-sequential "multiplicity-add 0+0=0"
  (destructuring-bind (a b expected) (list :zero :zero :zero)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add 0+1=1"
  (destructuring-bind (a b expected) (list :zero :one :one)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add 1+0=1"
  (destructuring-bind (a b expected) (list :one :zero :one)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add 0+ω=ω"
  (destructuring-bind (a b expected) (list :zero :omega :omega)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add 1+1=1"
  (destructuring-bind (a b expected) (list :one :one :one)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add 1+ω=ω"
  (destructuring-bind (a b expected) (list :one :omega :omega)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-add ω+ω=ω"
  (destructuring-bind (a b expected) (list :omega :omega :omega)
    (expect (mult-add a b) :to-be expected)))

(it-sequential "multiplicity-mul 0*0=0"
  (destructuring-bind (a b expected) (list :zero :zero :zero)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul 0*1=0"
  (destructuring-bind (a b expected) (list :zero :one :zero)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul 0*ω=0"
  (destructuring-bind (a b expected) (list :zero :omega :zero)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul 1*0=0"
  (destructuring-bind (a b expected) (list :one :zero :zero)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul 1*1=1"
  (destructuring-bind (a b expected) (list :one :one :one)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul 1*ω=ω"
  (destructuring-bind (a b expected) (list :one :omega :omega)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul ω*0=0"
  (destructuring-bind (a b expected) (list :omega :zero :zero)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul ω*1=ω"
  (destructuring-bind (a b expected) (list :omega :one :omega)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-mul ω*ω=ω"
  (destructuring-bind (a b expected) (list :omega :omega :omega)
    (expect (mult-mul a b) :to-be expected)))

(it-sequential "multiplicity-leq 0≤0"
  (destructuring-bind (a b expected) (list :zero :zero t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq 0≤1"
  (destructuring-bind (a b expected) (list :zero :one t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq 0≤ω"
  (destructuring-bind (a b expected) (list :zero :omega t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq 1≤1"
  (destructuring-bind (a b expected) (list :one :one t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq 1≤ω"
  (destructuring-bind (a b expected) (list :one :omega t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq ω≤ω"
  (destructuring-bind (a b expected) (list :omega :omega t)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq 1≰0"
  (destructuring-bind (a b expected) (list :one :zero nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq ω≰1"
  (destructuring-bind (a b expected) (list :omega :one nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-leq ω≰0"
  (destructuring-bind (a b expected) (list :omega :zero nil)
    (assert-multiplicity-boolean-case
      expected
      (expect (mult-leq a b) :to-be-truthy)
      (expect (mult-leq a b) :to-be-falsy))))

(it-sequential "multiplicity-to-string zero"
  (destructuring-bind (grade expected-str) (list :zero "0")
    (expect (mult-to-string grade) :to-equal expected-str)))

(it-sequential "multiplicity-to-string one"
  (destructuring-bind (grade expected-str) (list :one "1")
    (expect (mult-to-string grade) :to-equal expected-str)))

(it-sequential "multiplicity-to-string omega"
  (destructuring-bind (grade expected-str) (list :omega "ω")
    (expect (mult-to-string grade) :to-equal expected-str)))
