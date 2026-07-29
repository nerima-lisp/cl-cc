;;;; tests/unit/expand/macros-sequence-fold-tests.lisp
;;;; Runtime behavior tests for src/expand/macros-sequence-fold.lisp
;;;;
;;;; Covers: reduce, nsubstitute, nsubstitute-if, nsubstitute-if-not,
;;;;   map-into, merge, last, butlast, nbutlast, search.
;;;;
;;;; Tests verify actual computed results (not expansion structure).
;;;; Expansion-level tests are in macros-sequence-tests.lisp.

(in-package :cl-cc/test)

;;; ─── reduce ─────────────────────────────────────────────────────────────────

(it-sequential "reduce-basic-operations sum"
  (destructuring-bind (form expected) (list "(reduce #'+ '(1 2 3 4 5))" 15)
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "reduce-basic-operations product"
  (destructuring-bind (form expected) (list "(reduce #'* '(1 2 3 4))" 24)
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "reduce-basic-operations with-iv"
  (destructuring-bind (form expected) (list "(reduce #'+ '(1 2 3) :initial-value 10)" 16)
    (expect (= expected (run-string form)) :to-be-truthy)))

;; Pre-existing base failures (error identically on the pre-migration deftest
;; baseline): REDUCE over an empty list with :initial-value, and over a
;; single-element list, error in the compile→VM run-string pipeline. Base bug,
;; unrelated to the cl-weave migration; needs a runtime/stdlib fix.
(it-sequential "reduce-basic-operations empty-iv" (expect (= 0 (run-string "(reduce #'+ '() :initial-value 0)")) :to-be-truthy))

(it-sequential "reduce-basic-operations single" (expect (= 42 (run-string "(reduce #'+ '(42))")) :to-be-truthy))

(it-sequential "reduce-basic-operations max"
  (destructuring-bind (form expected) (list "(reduce #'max '(3 1 4 1 5 9 2 6))" 9)
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "reduce-from-end"
  (let ((left-result  (run-string "(reduce #'- '(10 3 2 1))"))
        (right-result (run-string "(reduce #'- '(10 3 2 1) :from-end t)")))
    (expect (not (= left-result right-result)) :to-be-truthy)))

(it-sequential "reduce-with-key"
  (expect (= 9 (run-string "(reduce #'+ '(\"foo\" \"ba\" \"quux\") :key #'length)" :stdlib t)) :to-be-truthy))

;;; ─── last ───────────────────────────────────────────────────────────────────

(it-sequential "last-returns-last-n-conses last-1"
  (destructuring-bind (form expected-str) (list "(last '(1 2 3))" "(3)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "last-returns-last-n-conses last-2"
  (destructuring-bind (form expected-str) (list "(last '(1 2 3) 2)" "(2 3)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "last-returns-last-n-conses last-3"
  (destructuring-bind (form expected-str) (list "(last '(1 2 3) 3)" "(1 2 3)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "last-returns-last-n-conses last-0"
  (destructuring-bind (form expected-str) (list "(last '(1 2 3) 0)" "nil")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "last-returns-last-n-conses singleton"
  (destructuring-bind (form expected-str) (list "(last '(42))" "(42)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

;;; ─── butlast ────────────────────────────────────────────────────────────────

(it-sequential "butlast-removes-last-n default-1"
  (destructuring-bind (form expected-str) (list "(butlast '(1 2 3))" "(1 2)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "butlast-removes-last-n explicit-2"
  (destructuring-bind (form expected-str) (list "(butlast '(1 2 3) 2)" "(1)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "butlast-removes-last-n all"
  (destructuring-bind (form expected-str) (list "(butlast '(1 2 3) 3)" "nil")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "butlast-removes-last-n over"
  (destructuring-bind (form expected-str) (list "(butlast '(1 2 3) 5)" "nil")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

;;; ─── nbutlast ───────────────────────────────────────────────────────────────
;;; nbutlast delegates directly to butlast via macro expansion.
;;; Expansion-level coverage is in macros-sequence-tests.lisp.
;;; Runtime behavior is guaranteed by butlast runtime tests above.

;;; ─── nsubstitute ────────────────────────────────────────────────────────────

(it-sequential "nsubstitute-family by-value"
  (destructuring-bind (expected form) (list '(1 99 3 99 5) "(nsubstitute 99 2 '(1 2 3 2 5))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "nsubstitute-family if-oddp"
  (destructuring-bind (expected form) (list '(0 2 0 4 0) "(nsubstitute-if 0 #'oddp '(1 2 3 4 5))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "nsubstitute-family if-not-oddp"
  (destructuring-bind (expected form) (list '(1 0 3 0 5) "(nsubstitute-if-not 0 #'oddp '(1 2 3 4 5))")
    (expect (run-string form :stdlib t) :to-equal expected)))

;;; ─── merge ──────────────────────────────────────────────────────────────────

(it-sequential "merge-sorted-sequences basic"
  (destructuring-bind (form expected-str) (list "(merge 'list '(1 3 5) '(2 4 6) #'<)" "(1 2 3 4 5 6)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "merge-sorted-sequences empty-l1"
  (destructuring-bind (form expected-str) (list "(merge 'list '() '(1 2 3) #'<)" "(1 2 3)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "merge-sorted-sequences empty-l2"
  (destructuring-bind (form expected-str) (list "(merge 'list '(1 2 3) '() #'<)" "(1 2 3)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "merge-sorted-sequences both-empty"
  (destructuring-bind (form expected-str) (list "(merge 'list '() '() #'<)" "nil")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

;;; ─── merge :key (FR-452) ─────────────────────────────────────────────────

(it-sequential "merge-with-key car-key"
  (destructuring-bind (expected-str form) (list "((1 . :a) (2 . :c) (3 . :b) (4 . :d))" "(merge 'list '((1 . :a) (3 . :b)) '((2 . :c) (4 . :d)) #'< :key #'car)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "merge-with-key cdr-key"
  (destructuring-bind (expected-str form) (list "((:a . 1) (:c . 2) (:b . 3) (:d . 4))" "(merge 'list '((:a . 1) (:b . 3)) '((:c . 2) (:d . 4)) #'< :key #'cdr)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

(it-sequential "merge-with-key nil-key"
  (destructuring-bind (expected-str form) (list "(1 2 3 4 5 6)" "(merge 'list '(1 3 5) '(2 4 6) #'< :key nil)")
    (let ((result (run-string form :stdlib t))
        (expected (read-from-string expected-str)))
    (expect result :to-equal expected))))

;;; ─── search ─────────────────────────────────────────────────────────────────

(it-sequential "search-finds-subsequence found-start"
  (destructuring-bind (form expected) (list "(search '(1 2) '(1 2 3 4))" 0)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence found-middle"
  (destructuring-bind (form expected) (list "(search '(2 3) '(1 2 3 4))" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence found-end"
  (destructuring-bind (form expected) (list "(search '(3 4) '(1 2 3 4))" 2)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence not-found"
  (destructuring-bind (form expected) (list "(search '(5 6) '(1 2 3 4))" nil)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence empty-pattern"
  (destructuring-bind (form expected) (list "(search '() '(1 2 3))" 0)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence empty-from-end"
  (destructuring-bind (form expected) (list "(search '() '(1 2 3) :from-end t)" 3)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence with-test"
  (destructuring-bind (form expected) (list "(search '(#\\B) '(#\\a #\\B #\\c) :test #'char-equal)" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence with-test-not"
  (destructuring-bind (form expected) (list "(search '(1) '(2 1) :test-not #'=)" 0)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence with-key"
  (destructuring-bind (form expected) (list "(search '(\"bb\" \"ccc\") '(\"a\" \"bb\" \"ccc\") :key #'length)" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence dynamic-nil-key"
  (destructuring-bind (form expected) (list "(let ((k nil)) (search '(2) '(1 2 3) :key k))" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence dynamic-nil-end1"
  (destructuring-bind (form expected) (list "(let ((e nil)) (search '(2 3) '(1 2 3) :end1 e))" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence dynamic-nil-end2"
  (destructuring-bind (form expected) (list "(let ((e nil)) (search '(2 3) '(1 2 3) :end2 e))" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence start2"
  (destructuring-bind (form expected) (list "(search '(2 3) '(0 2 3 2 3) :start2 2)" 3)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence end2"
  (destructuring-bind (form expected) (list "(search '(2 3) '(0 2 3 2 3) :end2 3)" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence start1-end1"
  (destructuring-bind (form expected) (list "(search '(0 2 3 9) '(1 2 3 4) :start1 1 :end1 3)" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence from-end"
  (destructuring-bind (form expected) (list "(search '(2 3) '(1 2 3 2 3) :from-end t)" 3)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence from-end-bounds"
  (destructuring-bind (form expected) (list "(search '(2 3) '(1 2 3 2 3 2 3) :end2 5 :from-end t)" 3)
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "search-finds-subsequence vector"
  (destructuring-bind (form expected) (list "(search #(2 3) #(1 2 3 4))" 1)
    (expect (run-string form :stdlib t) :to-equal expected)))

;;; ─── map-into ───────────────────────────────────────────────────────────────

(it-sequential "map-into-behavior fills-dest"
  (destructuring-bind (expected form) (list '(2 4 6) "(let ((dest (list 0 0 0)))
                                        (map-into dest #'(lambda (x) (* x 2)) '(1 2 3)))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "map-into-behavior returns-dest"
  (destructuring-bind (expected form) (list '(2 3) "(let ((d (list 0 0))) (map-into d #'1+ '(1 2)) d)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "map-into-behavior two-sources"
  (destructuring-bind (expected form) (list '(11 22 33) "(let ((d (list 0 0 0)))
                                          (map-into d #'+ '(1 2 3) '(10 20 30)))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "map-into-behavior shortest-source"
  (destructuring-bind (expected form) (list '(11 22 0 0) "(let ((d (list 0 0 0 0)))
                                             (map-into d #'+ '(1 2 3) '(10 20))
                                             d)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "map-into-behavior zero-sources"
  (destructuring-bind (expected form) (list '(:filled :filled) "(let ((d (list 0 0)))
                                                (map-into d #'(lambda () :filled))
                                                d)")
    (expect (run-string form :stdlib t) :to-equal expected)))
