;;;; tests/unit/type/exhaustiveness-tests.lisp
;;;; FR-1903: Exhaustiveness / Coverage Checking Tests
;;;;
;;;; Tests for check-typecase-exhaustiveness, check-etypecase-completeness,
;;;; useful-typecase-arms, and typecase-arm-subsumed-p.

(in-package :cl-cc/test)

(defmacro assert-exhaustiveness-expected-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── typecase-arm-subsumed-p ──────────────────────────────────────────────

(it-sequential "exhaustiveness-arm-subsumed-p-cases int-by-T"
  (destructuring-bind (arm-type covered expected) (list 'integer '(t) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases string-by-T"
  (destructuring-bind (arm-type covered expected) (list 'string '(t) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases fixnum-by-T"
  (destructuring-bind (arm-type covered expected) (list 'fixnum '(t) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases integer-by-exact"
  (destructuring-bind (arm-type covered expected) (list 'integer '(integer) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases string-in-list"
  (destructuring-bind (arm-type covered expected) (list 'string '(symbol integer string) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases fixnum-by-integer"
  (destructuring-bind (arm-type covered expected) (list 'fixnum '(integer) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases cons-by-list"
  (destructuring-bind (arm-type covered expected) (list 'cons '(list) t)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases string-not-int"
  (destructuring-bind (arm-type covered expected) (list 'string '(integer) nil)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases symbol-not-in"
  (destructuring-bind (arm-type covered expected) (list 'symbol '(integer string) nil)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

(it-sequential "exhaustiveness-arm-subsumed-p-cases int-no-arms"
  (destructuring-bind (arm-type covered expected) (list 'integer nil nil)
    (assert-exhaustiveness-expected-case expected
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-truthy)
    (expect (cl-cc/type:typecase-arm-subsumed-p arm-type covered) :to-be-falsy))))

;;; ─── check-typecase-exhaustiveness ───────────────────────────────────────

(it-sequential "exhaustiveness-basic-coverage-cases with-t"
  (destructuring-bind (arms expected-exhaustive) (list '(integer string t) t)
    (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (assert-exhaustiveness-expected-case expected-exhaustive
      (expect exhaustive-p :to-be-truthy)
      (expect exhaustive-p :to-be-falsy))
    (expect unreachable :to-be-null)
    (expect warnings :to-be-null))))

(it-sequential "exhaustiveness-basic-coverage-cases without-t"
  (destructuring-bind (arms expected-exhaustive) (list '(integer string) nil)
    (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (assert-exhaustiveness-expected-case expected-exhaustive
      (expect exhaustive-p :to-be-truthy)
      (expect exhaustive-p :to-be-falsy))
    (expect unreachable :to-be-null)
    (expect warnings :to-be-null))))

(it-sequential "exhaustiveness-detects-duplicate-arm"
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer integer t))
    (expect exhaustive-p :to-be-truthy)
    (expect (= 1 (length unreachable)) :to-be-truthy)
    (expect (= 1 (first unreachable)) :to-be-truthy)        ; second 'integer (index 1) is unreachable
    (expect (= 1 (length warnings)) :to-be-truthy)))

(it-sequential "exhaustiveness-detects-subsumed-arm"
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer fixnum t))
    (expect exhaustive-p :to-be-truthy)
    (expect (member 1 unreachable) :to-be-truthy)    ; fixnum (index 1) subsumed by integer
    (expect (> (length warnings) 0) :to-be-truthy)))

(it-sequential "exhaustiveness-t-after-t-is-unreachable"
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer t t))
    (expect exhaustive-p :to-be-truthy)
    (expect (member 2 unreachable) :to-be-truthy)    ; second T (index 2) unreachable
    (expect (> (length warnings) 0) :to-be-truthy)))

(it-sequential "exhaustiveness-boundary-cases single-T"
  (destructuring-bind (arms expected-exhaustive) (list '(t) t)
    (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (assert-exhaustiveness-expected-case expected-exhaustive
      (expect exhaustive-p :to-be-truthy)
      (expect exhaustive-p :to-be-falsy))
    (expect unreachable :to-be-null)
    (expect warnings :to-be-null))))

(it-sequential "exhaustiveness-boundary-cases empty"
  (destructuring-bind (arms expected-exhaustive) (list '() nil)
    (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (assert-exhaustiveness-expected-case expected-exhaustive
      (expect exhaustive-p :to-be-truthy)
      (expect exhaustive-p :to-be-falsy))
    (expect unreachable :to-be-null)
    (expect warnings :to-be-null))))

;;; ─── check-etypecase-completeness ────────────────────────────────────────

(it-sequential "etypecase-completeness-with-t-is-exhaustive"
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-etypecase-completeness '(integer string t))
    (expect exhaustive-p :to-be-truthy)
    (expect unreachable :to-be-null)
    (expect warnings :to-be-null)))

(it-sequential "etypecase-completeness-without-t-warns"
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-etypecase-completeness '(integer string))
    (expect exhaustive-p :to-be-falsy)
    (expect unreachable :to-be-null)
    (expect (= 1 (length warnings)) :to-be-truthy)
    (expect (search "etypecase" (first warnings)) :to-be-truthy)))

;;; ─── useful-typecase-arms ─────────────────────────────────────────────────

(it-sequential "useful-arms-removes-subsumed"
  (let ((useful (cl-cc/type:useful-typecase-arms '(integer fixnum string t))))
    ;; fixnum is subsumed by integer, so result is (integer string t)
    (expect (= 3 (length useful)) :to-be-truthy)
    (expect (member 'fixnum useful) :to-be-falsy)))

(it-sequential "useful-arms-keeps-all-distinct"
  (let ((useful (cl-cc/type:useful-typecase-arms '(integer string symbol))))
    (expect (= 3 (length useful)) :to-be-truthy)))

(it-sequential "useful-arms-empty-input"
  (expect (cl-cc/type:useful-typecase-arms '()) :to-be-null))
