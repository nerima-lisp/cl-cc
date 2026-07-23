;;;; packages/javascript/tests/js-runtime-core-tests.lisp
;;;;
;;;; Core type coercion, equality, truthiness, for-of/in, try-catch, Object
;;;; basic operations, and bitwise operators.
;;;;
;;;; Helper functions (%jr-arr, %jr-list, %jr-set) are defined here
;;;; and available to all subsequently-loaded js-runtime-* test files.

(in-package :cl-cc/test)

;;; ─── Shared helpers (used across all js-runtime-* test files) ─────────────────

(defun %jr-arr (&rest els)
  "Build a JS array (adjustable vector) from ELS."
  (apply #'cl-cc/javascript::%js-make-array els))

(defun %jr-list (vec)
  "Coerce a JS array (vector) to a CL list for easy comparison."
  (coerce vec 'list))

(defun %jr-set (&rest vals)
  "Build a JS Set from VALS in insertion order."
  (let ((s (cl-cc/javascript::%js-make-set)))
    (dolist (v vals s)
      (cl-cc/javascript::%js-set-add s v))))

;;; ─── typeof ──────────────────────────────────────────────────────────────────

(it-sequential "js-rt-typeof number"
  (destructuring-bind (value expected) (list 42 "number")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof string"
  (destructuring-bind (value expected) (list "hi" "string")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof bool-true"
  (destructuring-bind (value expected) (list t "boolean")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof bool-nil"
  (destructuring-bind (value expected) (list nil "boolean")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof undefined"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-undefined+ "undefined")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof null"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-null+ "object")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof array"
  (destructuring-bind (value expected) (list (%jr-arr 1 2) "object")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

;;; ─── ToString ────────────────────────────────────────────────────────────────

(it-sequential "js-rt-to-string integer"
  (destructuring-bind (value expected) (list 42 "42")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string true"
  (destructuring-bind (value expected) (list t "true")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string false"
  (destructuring-bind (value expected) (list nil "false")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string null"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-null+ "null")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string undefined"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-undefined+ "undefined")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string nan"
  (destructuring-bind (value expected) (list :js-nan "NaN")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

;;; ─── Strict equality ────────────────────────────────────────────────────────

(it-sequential "js-rt-strict-eq same-num"
  (destructuring-bind (a b expected) (list 3 3 t)
    (expect (cl-cc/javascript::%js-strict-eq a b) :to-equal expected)))

(it-sequential "js-rt-strict-eq diff-num"
  (destructuring-bind (a b expected) (list 3 4 nil)
    (expect (cl-cc/javascript::%js-strict-eq a b) :to-equal expected)))

(it-sequential "js-rt-strict-eq same-str"
  (destructuring-bind (a b expected) (list "a" "a" t)
    (expect (cl-cc/javascript::%js-strict-eq a b) :to-equal expected)))

(it-sequential "js-rt-strict-eq diff-str"
  (destructuring-bind (a b expected) (list "a" "b" nil)
    (expect (cl-cc/javascript::%js-strict-eq a b) :to-equal expected)))

;;; ─── Truthiness ──────────────────────────────────────────────────────────────

(it-sequential "js-rt-truthy truthy-num"
  (destructuring-bind (value expected) (list 1 t)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy truthy-str"
  (destructuring-bind (value expected) (list "x" t)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy falsy-zero"
  (destructuring-bind (value expected) (list 0 nil)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy falsy-str"
  (destructuring-bind (value expected) (list "" nil)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy falsy-nil"
  (destructuring-bind (value expected) (list nil nil)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy falsy-undef"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-undefined+ nil)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

(it-sequential "js-rt-truthy falsy-null"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-null+ nil)
    (expect (cl-cc/javascript::%js-truthy value) :to-equal expected)))

;;; ─── Loose equality ──────────────────────────────────────────────────────────

(it-sequential "js-rt-loose-eq identity"
  (destructuring-bind (a b expected) (list 1 1 t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq null-undef"
  (destructuring-bind (a b expected) (list cl-cc/javascript::+js-null+ cl-cc/javascript::+js-undefined+ t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq undef-null"
  (destructuring-bind (a b expected) (list cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-null+ t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq num-str"
  (destructuring-bind (a b expected) (list 3 "3" t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq str-num"
  (destructuring-bind (a b expected) (list "3" 3 t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq true-1"
  (destructuring-bind (a b expected) (list t 1 t)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq nan-nan"
  (destructuring-bind (a b expected) (list :js-nan :js-nan nil)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

(it-sequential "js-rt-loose-eq mismatch"
  (destructuring-bind (a b expected) (list 1 2 nil)
    (expect (cl-cc/javascript::%js-loose-eq a b) :to-equal expected)))

;;; ─── Bitwise operators ───────────────────────────────────────────────────────

(it-sequential "js-rt-bitwise-or simple"
  (destructuring-bind (a b expected) (list #b1010 #b1100 #b1110)
    (expect (= expected (cl-cc/javascript::%js-bitwise-or a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-or neg"
  (destructuring-bind (a b expected) (list -1 0 -1)
    (expect (= expected (cl-cc/javascript::%js-bitwise-or a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-or zero"
  (destructuring-bind (a b expected) (list 0 0 0)
    (expect (= expected (cl-cc/javascript::%js-bitwise-or a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-and simple"
  (destructuring-bind (a b expected) (list #b1010 #b1100 #b1000)
    (expect (= expected (cl-cc/javascript::%js-bitwise-and a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-and zero"
  (destructuring-bind (a b expected) (list 5 0 0)
    (expect (= expected (cl-cc/javascript::%js-bitwise-and a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-and same"
  (destructuring-bind (a b expected) (list 7 7 7)
    (expect (= expected (cl-cc/javascript::%js-bitwise-and a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-xor simple"
  (destructuring-bind (a b expected) (list #b1010 #b1100 #b0110)
    (expect (= expected (cl-cc/javascript::%js-bitwise-xor a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-xor same"
  (destructuring-bind (a b expected) (list 7 7 0)
    (expect (= expected (cl-cc/javascript::%js-bitwise-xor a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-xor zero"
  (destructuring-bind (a b expected) (list 0 5 5)
    (expect (= expected (cl-cc/javascript::%js-bitwise-xor a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-not zero"
  (destructuring-bind (x expected) (list 0 -1)
    (expect (= expected (cl-cc/javascript::%js-bitwise-not x)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-not neg-one"
  (destructuring-bind (x expected) (list -1 0)
    (expect (= expected (cl-cc/javascript::%js-bitwise-not x)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-not one"
  (destructuring-bind (x expected) (list 1 -2)
    (expect (= expected (cl-cc/javascript::%js-bitwise-not x)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full shl-2"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-left 2 2 8)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full shl-1"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-left 1 4 16)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full shl-0"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-left 5 0 5)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full shr-pos"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-right 8 2 2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full shr-neg"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-right -8 2 -2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full ushr-neg"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-unsigned-shift-right -8 2 1073741822)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops-full ushr-pos"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-unsigned-shift-right 8 2 2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

;;; ─── Object basic operations ─────────────────────────────────────────────────

(it-sequential "js-rt-object-make-access"
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (expect (= 1 (cl-cc/javascript::%js-get-prop o "a")) :to-be-truthy)
    (expect (= 2 (cl-cc/javascript::%js-get-prop o "b")) :to-be-truthy)))

(it-sequential "js-rt-object-keys-values"
  (let* ((o  (cl-cc/javascript::%js-make-object "x" 10 "y" 20))
         (ks (sort (%jr-list (cl-cc/javascript::%js-object-keys   o)) #'string<))
         (vs (sort (%jr-list (cl-cc/javascript::%js-object-values o)) #'<)))
    (expect ks :to-equal '("x" "y"))
    (expect vs :to-equal '(10 20))))

(it-sequential "js-rt-get-prop-array"
  (let ((a (%jr-arr 10 20 30)))
    (expect (= 3 (cl-cc/javascript::%js-get-prop a "length")) :to-be-truthy)
    (expect (= 20 (cl-cc/javascript::%js-get-prop a 1)) :to-be-truthy)))

(it-sequential "js-rt-object-prop-false-value"
  (let ((obj (cl-cc/javascript::%js-make-object "flag" nil)))
    (expect (cl-cc/javascript::%js-get-prop obj "flag") :to-be-falsy)
    (expect (cl-cc/javascript::%js-get-prop obj "absent") :to-be cl-cc/javascript::+js-undefined+)))

;;; ─── for-of / for-in ─────────────────────────────────────────────────────────

(it-sequential "js-rt-for-of array"
  (destructuring-bind (iterable expected) (list (%jr-arr 10 20 30) '(10 20 30))
    (let ((seen nil))
    (cl-cc/javascript::%js-for-of
     iterable
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (expect (nreverse seen) :to-equal expected))))

(it-sequential "js-rt-for-of string"
  (destructuring-bind (iterable expected) (list "hi" '("h" "i"))
    (let ((seen nil))
    (cl-cc/javascript::%js-for-of
     iterable
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (expect (nreverse seen) :to-equal expected))))

(it-sequential "js-rt-for-in-object"
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2))
        (keys nil))
    (cl-cc/javascript::%js-for-in o (lambda (k &rest _) (declare (ignore _)) (push k keys)))
    (expect (sort keys #'string<) :to-equal '("a" "b"))))

(it-sequential "js-rt-for-in-skips-accessor-keys"
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1 "b" 2))
        (keys nil))
    (setf (gethash "__get_foo" obj) (lambda () 99))
    (setf (gethash "__set_foo" obj) (lambda (v) v))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-for-in
     obj (lambda (k &rest _) (declare (ignore _)) (push k keys)))
    (expect (sort keys #'string<) :to-equal '("a" "b"))))

;;; ─── ToNumber coercions ──────────────────────────────────────────────────────

(it-sequential "js-rt-to-number integer"
  (destructuring-bind (value expected) (list 42 42.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number true"
  (destructuring-bind (value expected) (list t 1.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number false"
  (destructuring-bind (value expected) (list nil 0.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number null"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-null+ 0.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number empty-str"
  (destructuring-bind (value expected) (list "" 0.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number num-str"
  (destructuring-bind (value expected) (list "3.14" 3.14d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number int-str"
  (destructuring-bind (value expected) (list "42" 42.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number trimmed"
  (destructuring-bind (value expected) (list "  7  " 7.0d0)
    (expect (= expected (cl-cc/javascript::%js-to-number value)) :to-be-truthy)))

(it-sequential "js-rt-to-number-nan-str"
  (expect (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-to-number "abc")) :to-be-truthy))

;;; ─── typeof: extended types ──────────────────────────────────────────────────

(it-sequential "js-rt-typeof-extended function"
  (destructuring-bind (value expected) (list (lambda () nil) "function")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof-extended object-ht"
  (destructuring-bind (value expected) (list (cl-cc/javascript::%js-make-ht) "object")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof-extended bigint"
  (destructuring-bind (value expected) (list (cl-cc/javascript::%make-js-bigint 5) "bigint")
    (expect (cl-cc/javascript::%js-typeof value) :to-equal expected)))

(it-sequential "js-rt-typeof-callable-object"
  (let ((fn-obj (cl-cc/javascript::%js-make-ht)))
    (setf (gethash "__call__" fn-obj) (lambda () nil))
    (expect (cl-cc/javascript::%js-typeof fn-obj) :to-equal "function")))

;;; ─── Relational operators ────────────────────────────────────────────────────

(it-sequential "js-rt-relational-num lt-true"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-lt 1 2 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num lt-false"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-lt 2 1 nil)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num gt-true"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-gt 5 3 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num gt-false"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-gt 3 5 nil)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num le-eq"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-le 3 3 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num le-less"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-le 2 3 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num ge-eq"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-ge 3 3 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-num ge-more"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-ge 4 3 t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-string lt-abc"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-lt "a" "b" t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-string gt-abc"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-gt "b" "a" t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-string lt-same"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-lt "a" "a" nil)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-string le-same"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-le "a" "a" t)
    (expect (funcall fn a b) :to-equal expected)))

(it-sequential "js-rt-relational-nan-always-false"
  (let ((nan cl-cc/javascript::+js-nan+))
    (expect (cl-cc/javascript::%js-lt nan 1) :to-be-falsy)
    (expect (cl-cc/javascript::%js-gt nan 1) :to-be-falsy)
    (expect (cl-cc/javascript::%js-le nan 1) :to-be-falsy)
    (expect (cl-cc/javascript::%js-ge nan 1) :to-be-falsy)))

;;; ─── Nullish coalescing ──────────────────────────────────────────────────────

(it-sequential "js-rt-nullish-coalesce null-rhs"
  (destructuring-bind (lhs rhs expected) (list cl-cc/javascript::+js-null+ "default" "default")
    (expect (cl-cc/javascript::%js-nullish-coalesce lhs rhs) :to-equal expected)))

(it-sequential "js-rt-nullish-coalesce undef-rhs"
  (destructuring-bind (lhs rhs expected) (list cl-cc/javascript::+js-undefined+ "default" "default")
    (expect (cl-cc/javascript::%js-nullish-coalesce lhs rhs) :to-equal expected)))

(it-sequential "js-rt-nullish-coalesce false-lhs"
  (destructuring-bind (lhs rhs expected) (list nil "default" nil)
    (expect (cl-cc/javascript::%js-nullish-coalesce lhs rhs) :to-equal expected)))

(it-sequential "js-rt-nullish-coalesce zero-lhs"
  (destructuring-bind (lhs rhs expected) (list 0 "default" 0)
    (expect (cl-cc/javascript::%js-nullish-coalesce lhs rhs) :to-equal expected)))

(it-sequential "js-rt-nullish-coalesce str-lhs"
  (destructuring-bind (lhs rhs expected) (list "x" "default" "x")
    (expect (cl-cc/javascript::%js-nullish-coalesce lhs rhs) :to-equal expected)))

;;; ─── instanceof ──────────────────────────────────────────────────────────────

(it-sequential "js-rt-instanceof-true"
  (let* ((klass (cl-cc/javascript::%js-make-class nil nil))
         (obj   (cl-cc/javascript::%js-new klass)))
    (expect (cl-cc/javascript::%js-instanceof obj klass) :to-be-truthy)))

(it-sequential "js-rt-instanceof-false"
  (let* ((klass-a (cl-cc/javascript::%js-make-class nil nil))
         (klass-b (cl-cc/javascript::%js-make-class nil nil))
         (obj     (cl-cc/javascript::%js-new klass-a)))
    (expect (cl-cc/javascript::%js-instanceof obj klass-b) :to-be-falsy)))

;;; ─── try-catch-finally ───────────────────────────────────────────────────────

(it-sequential "js-rt-try-catch"
  (let ((caught nil))
    (cl-cc/javascript::%js-try-catch-finally
     (lambda () (cl-cc/javascript::%js-throw "err"))
     (lambda (v) (setf caught v))
     nil)
    (expect caught :to-equal "err")))

(it-sequential "js-rt-try-finally-runs"
  (let ((ran nil))
    (cl-cc/javascript::%js-try-catch-finally
     (lambda () 42)
     nil
     (lambda () (setf ran t)))
    (expect ran :to-be-truthy)))

;;; ─── JS + operator (add / string-concat) ────────────────────────────────────

(it-sequential "js-rt-add-numeric"
  (expect (= 7 (cl-cc/javascript::%js-add 3 4)) :to-be-truthy))

(it-sequential "js-rt-add-string-concat str-str"
  (destructuring-bind (a b expected) (list "a" "b" "ab")
    (expect (cl-cc/javascript::%js-add a b) :to-equal expected)))

(it-sequential "js-rt-add-string-concat num-str"
  (destructuring-bind (a b expected) (list 1 "x" "1x")
    (expect (cl-cc/javascript::%js-add a b) :to-equal expected)))

(it-sequential "js-rt-add-string-concat str-num"
  (destructuring-bind (a b expected) (list "x" 1 "x1")
    (expect (cl-cc/javascript::%js-add a b) :to-equal expected)))

;;; ─── JS /, %, ** arithmetic operators ───────────────────────────────────────

(it-sequential "js-rt-arithmetic-ops div-float"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-divide 10 4 2.5d0)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-arithmetic-ops mod-pos"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-mod 10 3 1)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-arithmetic-ops mod-neg"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-mod -5 3 -2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-arithmetic-ops pow-two"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-pow 2 10 1024)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-divide-by-zero-infinity"
  (expect (cl-cc/javascript::%js-float-infinity-p (cl-cc/javascript::%js-divide 1 0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-divide 0 0)) :to-be-truthy))

(it-sequential "js-rt-mod-zero-denominator-nan"
  (expect (cl-cc/javascript::%js-nan-p (cl-cc/javascript::%js-mod 5 0)) :to-be-truthy))

;;; ─── Property: delete / in / optional-chain ──────────────────────────────────

(it-sequential "js-rt-delete-property"
  (let ((o (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (cl-cc/javascript::%js-delete o "a")
    (expect (cl-cc/javascript::%js-get-prop o "a") :to-be cl-cc/javascript::+js-undefined+)
    (expect (= 2 (cl-cc/javascript::%js-get-prop o "b")) :to-be-truthy)))

(it-sequential "js-rt-in-operator present"
  (destructuring-bind (key expected) (list "a" t)
    (let ((o (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-in key o) :to-equal expected))))

(it-sequential "js-rt-in-operator absent"
  (destructuring-bind (key expected) (list "z" nil)
    (let ((o (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-in key o) :to-equal expected))))

(it-sequential "js-rt-optional-chain-present"
  (let ((o (cl-cc/javascript::%js-make-object "x" 42)))
    (expect (= 42 (cl-cc/javascript::%js-optional-chain o "x")) :to-be-truthy)))

(it-sequential "js-rt-optional-chain-null-undefined"
  (expect (cl-cc/javascript::%js-optional-chain cl-cc/javascript::+js-null+ "x") :to-be cl-cc/javascript::+js-undefined+)
  (expect (cl-cc/javascript::%js-optional-chain cl-cc/javascript::+js-undefined+ "x") :to-be cl-cc/javascript::+js-undefined+))
