;;;; tests/unit/runtime/runtime-tests-2.lisp — Runtime Library Unit Tests (Part 2)
;;;;
;;;; Continuation of runtime-tests.lisp:
;;;; list ops, array ops, arithmetic helpers, numeric predicates, and comparisons.

(in-package :cl-cc/test)


;;; ─── List Operations ───────────────────────────────────────────────────────

(defun %prepare-rt-copy-list-fixture ()
  (let ((base '(1 2 3)))
    (values base (cl-cc/runtime:rt-copy-list base))))

(it-sequential "rt-cons-creation-and-mutation"
  (let ((c (cl-cc/runtime:rt-cons 1 2)))
    (expect (= 1 (cl-cc/runtime:rt-car c)) :to-be-truthy)
    (expect (= 2 (cl-cc/runtime:rt-cdr c)) :to-be-truthy)
    (cl-cc/runtime:rt-rplaca c 10)
    (cl-cc/runtime:rt-rplacd c 20)
    (expect (= 10 (cl-cc/runtime:rt-car c)) :to-be-truthy)
    (expect (= 20 (cl-cc/runtime:rt-cdr c)) :to-be-truthy)))

(it-sequential "rt-copy-list-cow-read-path"
  (multiple-value-bind (base copy) (%prepare-rt-copy-list-fixture)
    (declare (ignore base))
    (expect (= 1 (cl-cc/runtime:rt-car copy)) :to-be-truthy)
    (expect (cl-cc/runtime:rt-cdr copy) :to-equal '(2 3))
    (expect (= 3 (cl-cc/runtime:rt-list-length copy)) :to-be-truthy)))

(it-sequential "rt-copy-list-cow-write-does-not-mutate-base-list"
  (multiple-value-bind (base copy) (%prepare-rt-copy-list-fixture)
    (cl-cc/runtime:rt-rplaca copy 99)
    (expect (= 99 (cl-cc/runtime:rt-car copy)) :to-be-truthy)
    (expect (= 1 (car base)) :to-be-truthy)))

(it-sequential "rt-list-push-pop"
  (multiple-value-bind (head tail)
      (cl-cc/runtime:rt-pop-list '(a b c))
    (expect head :to-be 'a)
    (expect tail :to-equal '(b c)))
  (expect (cl-cc/runtime:rt-push-list 'x '(a b)) :to-equal '(x a b)))

(it-sequential "rt-endp-convention nil-end"
  (destructuring-bind (input expected) (list nil 1)
    (expect (= expected (cl-cc/runtime:rt-endp input)) :to-be-truthy)))

(it-sequential "rt-endp-convention non-empty"
  (destructuring-bind (input expected) (list '(1) 0)
    (expect (= expected (cl-cc/runtime:rt-endp input)) :to-be-truthy)))

(it-sequential "rt-equal-convention equal"
  (destructuring-bind (a b expected) (list '(1 2) '(1 2) 1)
    (expect (= expected (cl-cc/runtime:rt-equal a b)) :to-be-truthy)))

(it-sequential "rt-equal-convention not-equal"
  (destructuring-bind (a b expected) (list '(1 2) '(1 3) 0)
    (expect (= expected (cl-cc/runtime:rt-equal a b)) :to-be-truthy)))

;;; ─── Array Operations ─────────────────────────────────────────────────────

(it-sequential "rt-make-array-creation"
  (let ((a5 (cl-cc/runtime:rt-make-array 5))
        (a3 (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (expect (= 5 (cl-cc/runtime:rt-array-length a5)) :to-be-truthy)
    (expect (= 0 (cl-cc/runtime:rt-aref a3 0)) :to-be-truthy)))

(it-sequential "rt-array-mutation-ops"
  (let ((a (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (cl-cc/runtime:rt-aset a 1 42)
    (expect (= 42 (cl-cc/runtime:rt-aref a 1)) :to-be-truthy))
  (let ((v (vector 1 2 3)))
    (expect (= 2 (cl-cc/runtime:rt-svref v 1)) :to-be-truthy)
    (cl-cc/runtime:rt-svset v 1 99)
    (expect (= 99 (cl-cc/runtime:rt-svref v 1)) :to-be-truthy))
  (let ((bv (make-array 4 :element-type 'bit :initial-element 0)))
    (cl-cc/runtime:rt-bit-set bv 2 1)
    (expect (= 1 (cl-cc/runtime:rt-bit-access bv 2)) :to-be-truthy)
    (expect (= 0 (cl-cc/runtime:rt-bit-access bv 0)) :to-be-truthy)))

;;; ─── Arithmetic Helpers ────────────────────────────────────────────────────

(it-sequential "rt-basic-arithmetic add"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-add 3 4 7)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-basic-arithmetic sub"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-sub 3 4 -1)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-basic-arithmetic mul"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-mul 3 4 12)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-basic-arithmetic div"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-div 5 2 5/2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-basic-arithmetic mod"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-mod 7 3 1)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-basic-arithmetic rem"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-rem 7 3 1)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-unary-arithmetic neg"
  (destructuring-bind (fn input expected) (list #'cl-cc/runtime:rt-neg 5 -5)
    (expect (= expected (funcall fn input)) :to-be-truthy)))

(it-sequential "rt-unary-arithmetic abs"
  (destructuring-bind (fn input expected) (list #'cl-cc/runtime:rt-abs -5 5)
    (expect (= expected (funcall fn input)) :to-be-truthy)))

(it-sequential "rt-unary-arithmetic inc"
  (destructuring-bind (fn input expected) (list #'cl-cc/runtime:rt-inc 5 6)
    (expect (= expected (funcall fn input)) :to-be-truthy)))

(it-sequential "rt-unary-arithmetic dec"
  (destructuring-bind (fn input expected) (list #'cl-cc/runtime:rt-dec 5 4)
    (expect (= expected (funcall fn input)) :to-be-truthy)))

(it-sequential "rt-unary-arithmetic lognot"
  (destructuring-bind (fn input expected) (list #'cl-cc/runtime:rt-lognot 42 -43)
    (expect (= expected (funcall fn input)) :to-be-truthy)))

(it-sequential "rt-not-convention nil"
  (destructuring-bind (input expected) (list nil 1)
    (expect (= expected (cl-cc/runtime:rt-not input)) :to-be-truthy)))

(it-sequential "rt-not-convention true"
  (destructuring-bind (input expected) (list t 0)
    (expect (= expected (cl-cc/runtime:rt-not input)) :to-be-truthy)))

(it-sequential "rt-not-convention integer"
  (destructuring-bind (input expected) (list 42 0)
    (expect (= expected (cl-cc/runtime:rt-not input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates evenp-t"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-evenp 4 1)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates evenp-f"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-evenp 3 0)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates oddp-t"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-oddp 3 1)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates oddp-f"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-oddp 4 0)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates zerop-t"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-zerop 0 1)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates zerop-f"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-zerop 1 0)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates plusp-t"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-plusp 5 1)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates plusp-f"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-plusp -1 0)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates minusp-t"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-minusp -1 1)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

(it-sequential "rt-numeric-predicates minusp-f"
  (destructuring-bind (pred-fn input expected) (list #'cl-cc/runtime:rt-minusp 1 0)
    (expect (= expected (funcall pred-fn input)) :to-be-truthy)))

;;; ─── Comparisons ───────────────────────────────────────────────────────────

(it-sequential "rt-comparisons lt-t"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-lt 1 2 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons lt-f"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-lt 2 1 0)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons gt-t"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-gt 2 1 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons gt-f"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-gt 1 2 0)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons le-eq"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-le 2 2 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons ge-eq"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-ge 2 2 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons num-eq-t"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-num-eq 5 5 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons num-eq-f"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-num-eq 5 6 0)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons eq-t"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-eq :a :a 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons eq-f"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-eq :a :b 0)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons eql-t"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-eql 42 42 1)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

(it-sequential "rt-comparisons eql-f"
  (destructuring-bind (cmp-fn a b expected) (list #'cl-cc/runtime:rt-eql 42 43 0)
    (expect (= expected (funcall cmp-fn a b)) :to-be-truthy)))

;;; ─── Runtime Region Operations ─────────────────────────────────────────────

(it-sequential "rt-region-lifetime-guards-references"
  (let (escaped-ref)
    (cl-cc/runtime:rt-with-region (region)
      (expect (cl-cc/runtime:rt-region-active-p region) :to-be-truthy)
      (let ((ref (cl-cc/runtime:rt-region-alloc region 42)))
        (setf escaped-ref ref)
        (expect (cl-cc/runtime:rt-region-ref-valid-p ref) :to-be-truthy)
        (expect (= 42 (cl-cc/runtime:rt-region-deref ref)) :to-be-truthy)))
    (expect (cl-cc/runtime:rt-region-ref-valid-p escaped-ref) :to-be-falsy)
    (signals error (cl-cc/runtime:rt-region-deref escaped-ref))))

(it-sequential "rt-region-bump-pointer-accounting"
  (let ((r (cl-cc/runtime:rt-make-region)))
    (expect (= 0 (cl-cc/runtime:rt-region-used r)) :to-be-truthy)
    (let ((cap (cl-cc/runtime:rt-region-capacity r)))
      (expect (> cap 0) :to-be-truthy)
      (cl-cc/runtime:rt-region-alloc r :a)
      (cl-cc/runtime:rt-region-alloc r :b)
      (expect (= 2 (cl-cc/runtime:rt-region-used r)) :to-be-truthy)
      (cl-cc/runtime:rt-close-region r)
      (expect (= 0 (cl-cc/runtime:rt-region-used r)) :to-be-truthy))))
