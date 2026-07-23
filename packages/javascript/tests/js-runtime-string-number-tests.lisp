;;;; packages/javascript/tests/js-runtime-string-number-tests.lisp
;;;;
;;;; String prototype methods, Math (basic and transcendental), Number.prototype
;;;; methods, number format helpers, predicates (isNaN/isFinite/isInteger),
;;;; and parseInt/parseFloat.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)

;;; ─── String ──────────────────────────────────────────────────────────────────

(it-sequential "js-rt-string-slice"
  (expect (cl-cc/javascript::%js-string-slice "hello" 1 4) :to-equal "ell")
  (expect (cl-cc/javascript::%js-string-slice "hello" -2) :to-equal "lo"))

(it-sequential "js-rt-string-search"
  (expect (cl-cc/javascript::%js-string-includes  "hello" "ell") :to-be-truthy)
  (expect (cl-cc/javascript::%js-string-includes  "hello" "xyz") :to-be-falsy)
  (expect (= 2 (cl-cc/javascript::%js-string-index-of  "hello" "l")) :to-be-truthy)
  (expect (= -1 (cl-cc/javascript::%js-string-index-of  "hello" "z")) :to-be-truthy))

(it-sequential "js-rt-string-index-of-position-coercion"
  (expect (= 2 (cl-cc/javascript::%js-string-index-of "ababa" "a" "2")) :to-be-truthy)
  (expect (= 3 (cl-cc/javascript::%js-string-index-of "abcabc" "a" 2.8d0)) :to-be-truthy)
  (expect (= 0 (cl-cc/javascript::%js-string-index-of
               "abc" "a" cl-cc/javascript::+js-undefined+)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-string-includes "abc" "a" 1) :to-be-falsy))

(it-sequential "js-rt-string-case upper"
  (destructuring-bind (fn input expected) (list #'cl-cc/javascript::%js-string-to-upper-case "hello" "HELLO")
    (expect (funcall fn input) :to-equal expected)))

(it-sequential "js-rt-string-case lower"
  (destructuring-bind (fn input expected) (list #'cl-cc/javascript::%js-string-to-lower-case "HELLO" "hello")
    (expect (funcall fn input) :to-equal expected)))

(it-sequential "js-rt-string-repeat three-times"
  (destructuring-bind (s n expected) (list "ab" 3 "ababab")
    (expect (cl-cc/javascript::%js-string-repeat s n) :to-equal expected)))

(it-sequential "js-rt-string-repeat zero-times"
  (destructuring-bind (s n expected) (list "ab" 0 "")
    (expect (cl-cc/javascript::%js-string-repeat s n) :to-equal expected)))

(it-sequential "js-rt-string-split-empty-sep"
  (expect (%jr-list (cl-cc/javascript::%js-string-split "abc" "")) :to-equal '("a" "b" "c")))

(it-sequential "js-rt-string-starts-ends-with starts-t"
  (destructuring-bind (fn s sub expected) (list #'cl-cc/javascript::%js-string-starts-with "hello" "hel" t)
    (expect (funcall fn s sub) :to-equal expected)))

(it-sequential "js-rt-string-starts-ends-with starts-f"
  (destructuring-bind (fn s sub expected) (list #'cl-cc/javascript::%js-string-starts-with "hello" "ell" nil)
    (expect (funcall fn s sub) :to-equal expected)))

(it-sequential "js-rt-string-starts-ends-with ends-t"
  (destructuring-bind (fn s sub expected) (list #'cl-cc/javascript::%js-string-ends-with "hello" "llo" t)
    (expect (funcall fn s sub) :to-equal expected)))

(it-sequential "js-rt-string-starts-ends-with ends-f"
  (destructuring-bind (fn s sub expected) (list #'cl-cc/javascript::%js-string-ends-with "hello" "hel" nil)
    (expect (funcall fn s sub) :to-equal expected)))

(it-sequential "js-rt-string-pad pad-start"
  (destructuring-bind (fn s len expected) (list #'cl-cc/javascript::%js-string-pad-start "5" 3 "005")
    (expect (funcall fn s len "0") :to-equal expected)))

(it-sequential "js-rt-string-pad pad-end"
  (destructuring-bind (fn s len expected) (list #'cl-cc/javascript::%js-string-pad-end "5" 3 "500")
    (expect (funcall fn s len "0") :to-equal expected)))

(it-sequential "js-rt-string-trim"
  (expect (cl-cc/javascript::%js-string-trim       "  hello  ") :to-equal "hello")
  (expect (cl-cc/javascript::%js-string-trim-start "  hello  ") :to-equal "hello  ")
  (expect (cl-cc/javascript::%js-string-trim-end   "  hello  ") :to-equal "  hello"))

(it-sequential "js-rt-string-at first"
  (destructuring-bind (idx expected) (list 0 "h")
    (expect (cl-cc/javascript::%js-string-at "hello" idx) :to-equal expected)))

(it-sequential "js-rt-string-at last"
  (destructuring-bind (idx expected) (list -1 "o")
    (expect (cl-cc/javascript::%js-string-at "hello" idx) :to-equal expected)))

(it-sequential "js-rt-string-at mid"
  (destructuring-bind (idx expected) (list 2 "l")
    (expect (cl-cc/javascript::%js-string-at "hello" idx) :to-equal expected)))

(it-sequential "js-rt-string-char-at-code-at"
  (expect (cl-cc/javascript::%js-string-char-at      "hello" 1) :to-equal "e")
  (expect (= 101 (cl-cc/javascript::%js-string-char-code-at "hello" 1)) :to-be-truthy))

(it-sequential "js-rt-string-last-index-of found-mid"
  (destructuring-bind (s sub expected) (list "abcabc" "b" 4)
    (expect (= expected (cl-cc/javascript::%js-string-last-index-of s sub)) :to-be-truthy)))

(it-sequential "js-rt-string-last-index-of found-end"
  (destructuring-bind (s sub expected) (list "aabb" "b" 3)
    (expect (= expected (cl-cc/javascript::%js-string-last-index-of s sub)) :to-be-truthy)))

(it-sequential "js-rt-string-last-index-of not-found"
  (destructuring-bind (s sub expected) (list "abc" "z" -1)
    (expect (= expected (cl-cc/javascript::%js-string-last-index-of s sub)) :to-be-truthy)))

(it-sequential "js-rt-string-last-index-of-position-bounds"
  (expect (= 4 (cl-cc/javascript::%js-string-last-index-of "ababa" "a" cl-cc/javascript::+js-undefined+)) :to-be-truthy)
  (expect (= 2 (cl-cc/javascript::%js-string-last-index-of "ababa" "a" "2")) :to-be-truthy)
  (expect (= 0 (cl-cc/javascript::%js-string-last-index-of "abcabc" "a" 2.8d0)) :to-be-truthy)
  (expect (= 0 (cl-cc/javascript::%js-string-last-index-of "ababa" "a" -99)) :to-be-truthy)
  (expect (= -1 (cl-cc/javascript::%js-string-last-index-of "ababa" "b" -99)) :to-be-truthy)
  (expect (= 5 (cl-cc/javascript::%js-string-last-index-of "ababa" "" 99)) :to-be-truthy))

(it-sequential "js-rt-string-concat"
  (expect (cl-cc/javascript::%js-string-concat "hello" " " "world") :to-equal "hello world"))

(it-sequential "js-rt-string-from-char-code-and-code-point"
  (expect (cl-cc/javascript::%js-string-from-char-code  65) :to-equal "A")
  (expect (cl-cc/javascript::%js-string-from-code-point 65) :to-equal "A"))

(it-sequential "js-rt-string-code-point-at"
  (expect (= 72 (cl-cc/javascript::%js-string-code-point-at "Hello" 0)) :to-be-truthy)
  (expect (= 101 (cl-cc/javascript::%js-string-code-point-at "Hello" 1)) :to-be-truthy))

(it-sequential "js-rt-string-is-well-formed"
  (let ((wf "hello"))
    (expect (cl-cc/javascript::%js-string-is-well-formed wf) :to-be-truthy)
    (expect (cl-cc/javascript::%js-string-to-well-formed wf) :to-equal wf)))

(it-sequential "js-rt-string-replace first"
  (destructuring-bind (fn s pat rep expected) (list #'cl-cc/javascript::%js-string-replace "aaa" "a" "b" "baa")
    (expect (funcall fn s pat rep) :to-equal expected)))

(it-sequential "js-rt-string-replace all"
  (destructuring-bind (fn s pat rep expected) (list #'cl-cc/javascript::%js-string-replace-all "aaa" "a" "b" "bbb")
    (expect (funcall fn s pat rep) :to-equal expected)))

(it-sequential "js-rt-method-resolution-string"
  (let* ((s "hello")
         (upper-fn (cl-cc/javascript::%js-get-prop s "toUpperCase")))
    (expect (functionp upper-fn) :to-be-truthy)
    (expect (funcall upper-fn) :to-equal "HELLO")))

;;; ─── Math ────────────────────────────────────────────────────────────────────

(it-sequential "js-rt-math-unary abs-neg"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-abs '(-5) 5)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-unary floor"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-floor '(3.7d0) 3)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-unary ceil"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-ceil '(3.2d0) 4)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-unary sign-neg"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-sign '(-7) -1)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-unary sign-zero"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-sign '(0) 0)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-unary sign-pos"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-sign '(7) 1)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-binary max"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-max '(3 9 1) 9)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-binary min"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-min '(3 9 1) 1)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-binary pow"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-pow '(2 3) 8)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-binary sqrt"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-sqrt '(9) 3.0d0)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-max-min-empty"
  (expect (cl-cc/javascript::%js-float-infinity-p
                (cl-cc/javascript::%js-math-min)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-infinity-p
                (cl-cc/javascript::%js-math-max)) :to-be-truthy)
  (expect (plusp (cl-cc/javascript::%js-math-min)) :to-be-truthy)
  (expect (minusp (cl-cc/javascript::%js-math-max)) :to-be-truthy))

(it-sequential "js-rt-math-negative-and-special-inputs"
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-sqrt -1.0d0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-log -1.0d0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-log2 -1.0d0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-log10 -1.0d0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-f16round cl-cc/javascript::*js-nan-float*)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-infinity-p
                (cl-cc/javascript::%js-math-f16round cl-cc/javascript::*js-inf-float*)) :to-be-truthy)
  (expect (= 0.0d0 (cl-cc/javascript::%js-math-f16round 0.0d0)) :to-be-truthy)
  (expect (= 0.0d0 (cl-cc/javascript::%js-math-f16round 1.0d-8)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-infinity-p
                (cl-cc/javascript::%js-math-f16round 65520.0d0)) :to-be-truthy))

(it-sequential "js-rt-math-special-value-branches"
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-abs cl-cc/javascript::*js-nan-float*)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-sign cl-cc/javascript::*js-nan-float*)) :to-be-truthy)
  (dolist (fn (list #'cl-cc/javascript::%js-math-floor
                    #'cl-cc/javascript::%js-math-ceil
                    #'cl-cc/javascript::%js-math-trunc
                    #'cl-cc/javascript::%js-math-round))
    (expect (cl-cc/javascript::%js-float-nan-p
                  (funcall fn cl-cc/javascript::*js-nan-float*)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-float-infinity-p
                  (funcall fn cl-cc/javascript::*js-inf-float*)) :to-be-truthy))
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-max 1 cl-cc/javascript::*js-nan-float* 3)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-nan-p
                (cl-cc/javascript::%js-math-min 1 cl-cc/javascript::*js-nan-float* 3)) :to-be-truthy))

(it-sequential "js-rt-math-f16round"
  (expect (= 1.3369140625d0 (cl-cc/javascript::%js-math-f16round 1.337d0)) :to-be-truthy)
  (expect (= 5.05078125d0 (cl-cc/javascript::%js-math-f16round 5.05d0)) :to-be-truthy)
  (expect (= 0.0d0 (cl-cc/javascript::%js-math-f16round -1.0d-8)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-infinity-p
    (cl-cc/javascript::%js-math-f16round -65520.0d0)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-float-infinity-p
    (cl-cc/javascript::%js-math-f16round 65520.0d0)) :to-be-truthy))

(it-sequential "js-rt-math-round-half-even"
  (expect (= 2 (cl-cc/javascript::%js-round-half-even 2.5d0)) :to-be-truthy)
  (expect (= 4 (cl-cc/javascript::%js-round-half-even 3.5d0)) :to-be-truthy)
  (expect (= -2 (cl-cc/javascript::%js-round-half-even -2.5d0)) :to-be-truthy)
  (expect (= -4 (cl-cc/javascript::%js-round-half-even -3.5d0)) :to-be-truthy))

(it-sequential "js-rt-math-extended-unary sinh-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-sinh 0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-extended-unary cosh-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-cosh 0 1.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-extended-unary tanh-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-tanh 0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-extended-unary cbrt-8"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-cbrt 8 2.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-extended-unary expm1-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-expm1 0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-extended-unary log1p-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-log1p 0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-round-trunc round-half-up"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-round 1.5d0 2.0d0)
    (expect (= expected (funcall fn x)) :to-be-truthy)))

(it-sequential "js-rt-math-round-trunc trunc-fraction"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-trunc 3.7d0 3.0d0)
    (expect (= expected (funcall fn x)) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics sin-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-sin 0.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics cos-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-cos 0.0d0 1.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics tan-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-tan 0.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics asin-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-asin 0.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics acos-1"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-acos 1.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-trig-basics atan-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-atan 0.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-atan2"
  (let ((result (cl-cc/javascript::%js-math-atan2 1.0d0 1.0d0)))
    (expect (< (abs (- result (atan 1.0d0))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-exp-log exp-0"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-exp 0.0d0 1.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-exp-log log-1"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-log 1.0d0 0.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-exp-log log2-8"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-log2 8.0d0 3.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-exp-log log10-1k"
  (destructuring-bind (fn x expected) (list #'cl-cc/javascript::%js-math-log10 1000.0d0 3.0d0)
    (expect (< (abs (- expected (funcall fn x))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-clz32-zero"
  (expect (= 32 (cl-cc/javascript::%js-math-clz32 0)) :to-be-truthy))

(it-sequential "js-rt-math-imul-sign-extension"
  (expect (= -2 (cl-cc/javascript::%js-math-imul -1 2)) :to-be-truthy))

(it-sequential "js-rt-math-hypot-clz32-fround-imul hypot-3-4"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-hypot '(3.0d0 4.0d0) 5.0d0)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-hypot-clz32-fround-imul clz32-1"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-clz32 '(1.0d0) 31.0d0)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-hypot-clz32-fround-imul imul-3-4"
  (destructuring-bind (fn args expected) (list #'cl-cc/javascript::%js-math-imul '(3.0d0 4.0d0) 12.0d0)
    (expect (= expected (apply fn args)) :to-be-truthy)))

(it-sequential "js-rt-math-fround"
  (let* ((input 1.337d0)
         (expected (coerce (coerce input 'single-float) 'double-float)))
    (expect (< (abs (- expected (cl-cc/javascript::%js-math-fround input))) 1.0d-10) :to-be-truthy)))

(it-sequential "js-rt-math-random-range"
  (let ((value (cl-cc/javascript::%js-math-random)))
    (expect (<= 0.0d0 value) :to-be-truthy)
    (expect (< value 1.0d0) :to-be-truthy)))

;;; ─── Number.prototype methods ────────────────────────────────────────────────

(it-sequential "js-rt-number-to-fixed zero-digits"
  (destructuring-bind (n digits expected) (list 3.14159d0 0 "3")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toFixed"))
         (result (funcall method digits)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-fixed two-digits"
  (destructuring-bind (n digits expected) (list 3.14159d0 2 "3.14")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toFixed"))
         (result (funcall method digits)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-fixed integer"
  (destructuring-bind (n digits expected) (list 42.0d0 0 "42")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toFixed"))
         (result (funcall method digits)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-string-radix base-10"
  (destructuring-bind (n radix expected) (list 255 10 "255")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toString"))
         (result (funcall method radix)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-string-radix base-16"
  (destructuring-bind (n radix expected) (list 255 16 "ff")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toString"))
         (result (funcall method radix)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-string-radix base-2"
  (destructuring-bind (n radix expected) (list 8 2 "1000")
    (let* ((method (cl-cc/javascript::%js-resolve-number-method n "toString"))
         (result (funcall method radix)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-number-to-precision one-sig"
  (destructuring-bind (n prec expected) (list 123.456d0 1 "1e+2")
    (expect (cl-cc/javascript::%js-number-to-precision n prec) :to-equal expected)))

(it-sequential "js-rt-number-to-precision three-sig"
  (destructuring-bind (n prec expected) (list 123.456d0 3 "123")
    (expect (cl-cc/javascript::%js-number-to-precision n prec) :to-equal expected)))

(it-sequential "js-rt-number-to-precision six-sig"
  (destructuring-bind (n prec expected) (list 3.14159d0 6 "3.14159")
    (expect (cl-cc/javascript::%js-number-to-precision n prec) :to-equal expected)))

(it-sequential "js-rt-number-to-precision zero"
  (destructuring-bind (n prec expected) (list 0.0d0 3 "0.00")
    (expect (cl-cc/javascript::%js-number-to-precision n prec) :to-equal expected)))

;;; ─── Number format helpers ───────────────────────────────────────────────────

(it-sequential "js-rt-strip-trailing-dot with-dot"
  (destructuring-bind (input expected) (list "8." "8")
    (expect (cl-cc/javascript::%js-strip-trailing-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-trailing-dot no-dot"
  (destructuring-bind (input expected) (list "3.14" "3.14")
    (expect (cl-cc/javascript::%js-strip-trailing-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-trailing-dot empty"
  (destructuring-bind (input expected) (list "" "")
    (expect (cl-cc/javascript::%js-strip-trailing-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-trailing-dot only-dot"
  (destructuring-bind (input expected) (list "." "")
    (expect (cl-cc/javascript::%js-strip-trailing-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-pre-exp-dot dot-e"
  (destructuring-bind (input expected) (list "1.e+2" "1e+2")
    (expect (cl-cc/javascript::%js-strip-pre-exp-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-pre-exp-dot dot-E"
  (destructuring-bind (input expected) (list "2.E+3" "2E+3")
    (expect (cl-cc/javascript::%js-strip-pre-exp-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-pre-exp-dot no-dot"
  (destructuring-bind (input expected) (list "1.2e+3" "1.2e+3")
    (expect (cl-cc/javascript::%js-strip-pre-exp-dot input) :to-equal expected)))

(it-sequential "js-rt-strip-pre-exp-dot no-exp"
  (destructuring-bind (input expected) (list "3.14" "3.14")
    (expect (cl-cc/javascript::%js-strip-pre-exp-dot input) :to-equal expected)))

;;; ─── Number strict predicates ────────────────────────────────────────────────

(it-sequential "js-rt-number-is-nan nan"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-nan-float* t)
    (expect (cl-cc/javascript::%js-number-is-nan val) :to-equal expected)))

(it-sequential "js-rt-number-is-nan string"
  (destructuring-bind (val expected) (list "NaN" nil)
    (expect (cl-cc/javascript::%js-number-is-nan val) :to-equal expected)))

(it-sequential "js-rt-number-is-nan number"
  (destructuring-bind (val expected) (list 42 nil)
    (expect (cl-cc/javascript::%js-number-is-nan val) :to-equal expected)))

(it-sequential "js-rt-number-is-finite int"
  (destructuring-bind (val expected) (list 42 t)
    (expect (cl-cc/javascript::%js-number-is-finite val) :to-equal expected)))

(it-sequential "js-rt-number-is-finite float"
  (destructuring-bind (val expected) (list 3.14d0 t)
    (expect (cl-cc/javascript::%js-number-is-finite val) :to-equal expected)))

(it-sequential "js-rt-number-is-finite nan"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-nan-float* nil)
    (expect (cl-cc/javascript::%js-number-is-finite val) :to-equal expected)))

(it-sequential "js-rt-number-is-finite inf"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-inf-float* nil)
    (expect (cl-cc/javascript::%js-number-is-finite val) :to-equal expected)))

(it-sequential "js-rt-number-is-finite string"
  (destructuring-bind (val expected) (list "42" nil)
    (expect (cl-cc/javascript::%js-number-is-finite val) :to-equal expected)))

(it-sequential "js-rt-number-is-integer int"
  (destructuring-bind (val expected) (list 42 t)
    (expect (cl-cc/javascript::%js-number-is-integer val) :to-equal expected)))

(it-sequential "js-rt-number-is-integer float"
  (destructuring-bind (val expected) (list 3.0d0 t)
    (expect (cl-cc/javascript::%js-number-is-integer val) :to-equal expected)))

(it-sequential "js-rt-number-is-integer frac"
  (destructuring-bind (val expected) (list 3.5d0 nil)
    (expect (cl-cc/javascript::%js-number-is-integer val) :to-equal expected)))

(it-sequential "js-rt-number-is-integer string"
  (destructuring-bind (val expected) (list "3" nil)
    (expect (cl-cc/javascript::%js-number-is-integer val) :to-equal expected)))

;;; ─── parseInt / parseFloat ───────────────────────────────────────────────────

(it-sequential "js-rt-parse-int decimal"
  (destructuring-bind (str radix expected) (list "42" 10 42)
    (let ((result (cl-cc/javascript::%js-parse-int str radix)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-parse-int hex"
  (destructuring-bind (str radix expected) (list "ff" 16 255)
    (let ((result (cl-cc/javascript::%js-parse-int str radix)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-parse-int binary"
  (destructuring-bind (str radix expected) (list "101" 2 5)
    (let ((result (cl-cc/javascript::%js-parse-int str radix)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-parse-int junk"
  (destructuring-bind (str radix expected) (list "abc" 10 :nan)
    (let ((result (cl-cc/javascript::%js-parse-int str radix)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-parse-float simple"
  (destructuring-bind (str expected) (list "3.14" 3.14d0)
    (let ((result (cl-cc/javascript::%js-parse-float str)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (< (abs (- expected result)) 1.0d-10) :to-be-truthy)))))

(it-sequential "js-rt-parse-float prefix"
  (destructuring-bind (str expected) (list "3.14abc" 3.14d0)
    (let ((result (cl-cc/javascript::%js-parse-float str)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (< (abs (- expected result)) 1.0d-10) :to-be-truthy)))))

(it-sequential "js-rt-parse-float int"
  (destructuring-bind (str expected) (list "42" 42.0d0)
    (let ((result (cl-cc/javascript::%js-parse-float str)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (< (abs (- expected result)) 1.0d-10) :to-be-truthy)))))

(it-sequential "js-rt-parse-float junk"
  (destructuring-bind (str expected) (list "abc" :nan)
    (let ((result (cl-cc/javascript::%js-parse-float str)))
    (if (eq expected :nan)
        (expect (cl-cc/javascript::%js-nan-p result) :to-be-truthy)
        (expect (< (abs (- expected result)) 1.0d-10) :to-be-truthy)))))

;;; ─── String coverage — uncovered functions ───────────────────────────────────

(it-sequential "js-rt-string-length"
  (expect (= 0 (cl-cc/javascript::%js-string-length "")) :to-be-truthy)
  (expect (= 5 (cl-cc/javascript::%js-string-length "hello")) :to-be-truthy))

(it-sequential "js-rt-string-split-separator csv"
  (destructuring-bind (s sep expected) (list "a,b,c" "," '("a" "b" "c"))
    (expect (%jr-list (cl-cc/javascript::%js-string-split s sep)) :to-equal expected)))

(it-sequential "js-rt-string-split-separator empty"
  (destructuring-bind (s sep expected) (list "a,,b" "," '("a" "" "b"))
    (expect (%jr-list (cl-cc/javascript::%js-string-split s sep)) :to-equal expected)))

(it-sequential "js-rt-string-split-separator no-sep"
  (destructuring-bind (s sep expected) (list "abc" "," '("abc"))
    (expect (%jr-list (cl-cc/javascript::%js-string-split s sep)) :to-equal expected)))

(it-sequential "js-rt-string-split-limit"
  (let ((parts (%jr-list (cl-cc/javascript::%js-string-split "a,b,c,d" "," 2))))
    (expect (= 2 (length parts)) :to-be-truthy)
    (expect (first parts) :to-equal "a")
    (expect (second parts) :to-equal "b")))

(it-sequential "js-rt-string-match-cases found"
  (destructuring-bind (s pat expected) (list "hello world" "world" "world")
    (let ((result (cl-cc/javascript::%js-string-match s pat)))
    (if (eq expected :null)
        (expect result :to-be cl-cc/javascript::+js-null+)
        (expect (aref result 0) :to-equal expected)))))

(it-sequential "js-rt-string-match-cases not-found"
  (destructuring-bind (s pat expected) (list "hello" "xyz" :null)
    (let ((result (cl-cc/javascript::%js-string-match s pat)))
    (if (eq expected :null)
        (expect result :to-be cl-cc/javascript::+js-null+)
        (expect (aref result 0) :to-equal expected)))))

(it-sequential "js-rt-string-match-all-multiple"
  (let ((all (cl-cc/javascript::%js-string-match-all "abab" "ab")))
    (expect (= 2 (length all)) :to-be-truthy)
    (expect (aref (aref all 0) 0) :to-equal "ab")
    (expect (aref (aref all 1) 0) :to-equal "ab")))

(it-sequential "js-rt-string-search-index found"
  (destructuring-bind (s pat expected) (list "hello" "ll" 2)
    (expect (= expected (cl-cc/javascript::%js-string-search s pat)) :to-be-truthy)))

(it-sequential "js-rt-string-search-index not-found"
  (destructuring-bind (s pat expected) (list "hello" "xyz" -1)
    (expect (= expected (cl-cc/javascript::%js-string-search s pat)) :to-be-truthy)))

(it-sequential "js-rt-string-normalize"
  (let* ((acute (string (code-char #x0301)))
         (composed-e (string (code-char #x00E9)))
         (decomposed-e (concatenate 'string "e" acute))
         (angstrom-sign (string (code-char #x212B)))
         (angstrom-letter (string (code-char #x00C5)))
         (fi-ligature (string (code-char #xFB01))))
    (expect (cl-cc/javascript::%js-string-normalize decomposed-e) :to-equal composed-e)
    (expect (cl-cc/javascript::%js-string-normalize composed-e "NFD") :to-equal decomposed-e)
    (expect (cl-cc/javascript::%js-string-normalize angstrom-sign "NFKC") :to-equal angstrom-letter)
    (expect (cl-cc/javascript::%js-string-normalize fi-ligature "NFKD") :to-equal "fi")
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/javascript::%js-string-normalize "cafe" "BAD")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))

(it-sequential "js-rt-string-locale-compare-order lt"
  (destructuring-bind (a b expected) (list "a" "b" -1.0d0)
    (expect (= expected (cl-cc/javascript::%js-string-locale-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-string-locale-compare-order eq"
  (destructuring-bind (a b expected) (list "x" "x" 0.0d0)
    (expect (= expected (cl-cc/javascript::%js-string-locale-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-string-locale-compare-order gt"
  (destructuring-bind (a b expected) (list "b" "a" 1.0d0)
    (expect (= expected (cl-cc/javascript::%js-string-locale-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-string-raw-tag"
  (let* ((raw    (%jr-arr "hello " " world"))
         (result (cl-cc/javascript::%js-string-raw raw "JS")))
    (expect result :to-equal "hello JS world")))

(it-sequential "js-rt-string-substring-cases basic"
  (destructuring-bind (s a b expected) (list "hello" 1 3 "el")
    (expect (cl-cc/javascript::%js-string-substring s a b) :to-equal expected)))

(it-sequential "js-rt-string-substring-cases clamp"
  (destructuring-bind (s a b expected) (list "hello" 0 100 "hello")
    (expect (cl-cc/javascript::%js-string-substring s a b) :to-equal expected)))

(it-sequential "js-rt-string-substring-cases reversed"
  (destructuring-bind (s a b expected) (list "hello" 3 1 "el")
    (expect (cl-cc/javascript::%js-string-substring s a b) :to-equal expected)))

(it-sequential "js-rt-string-locale-case upper"
  (destructuring-bind (fn input expected) (list #'cl-cc/javascript::%js-string-to-locale-upper-case "hello" "HELLO")
    (expect (funcall fn input) :to-equal expected)))

(it-sequential "js-rt-string-locale-case lower"
  (destructuring-bind (fn input expected) (list #'cl-cc/javascript::%js-string-to-locale-lower-case "HELLO" "hello")
    (expect (funcall fn input) :to-equal expected)))
