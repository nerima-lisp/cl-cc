;;;; tests/unit/expand/expander-numeric-tests.lisp — Numeric expander tests

(in-package :cl-cc/test)



;;; ─── + and * (variadic fold) ─────────────────────────────────────────────

(it-sequential "expander-plus-times-identity plus"
  (destructuring-bind (form expected) (list '(+) 0)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-identity times"
  (destructuring-bind (form expected) (list '(*) 1)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-unary-passthrough plus"
  (destructuring-bind (form expected) (list '(+ x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-unary-passthrough times"
  (destructuring-bind (form expected) (list '(* x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-binary-builtin plus"
  (destructuring-bind (form expected) (list '(+ a b) '(+ a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-binary-builtin times"
  (destructuring-bind (form expected) (list '(* a b) '(* a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-nary-left-fold plus"
  (destructuring-bind (form expected) (list '(+ a b c) '(+ (+ a b) c))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-plus-times-nary-left-fold times"
  (destructuring-bind (form expected) (list '(* a b c) '(* (* a b) c))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

;;; ─── - (subtraction / unary negation) ───────────────────────────────────

(it-sequential "expander-minus-zero-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(-))))

(it-sequential "expander-minus-unary-is-negation"
  (expect (cl-cc/expand:compiler-macroexpand-all '(- 7)) :to-equal '(- 0 7)))

(it-sequential "expander-minus-binary-is-passthrough"
  (expect (cl-cc/expand:compiler-macroexpand-all '(- a b)) :to-equal '(- a b)))

(it-sequential "expander-minus-nary-is-left-fold"
  (expect (cl-cc/expand:compiler-macroexpand-all '(- a b c)) :to-equal '(- (- a b) c)))

;;; ─── / (division / reciprocal) ───────────────────────────────────────────

(it-sequential "expander-slash-zero-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(/))))

(it-sequential "expander-slash-unary-is-reciprocal"
  (expect (cl-cc/expand:compiler-macroexpand-all '(/ x)) :to-equal '(/ 1 x)))

(it-sequential "expander-slash-binary-is-passthrough"
  (expect (cl-cc/expand:compiler-macroexpand-all '(/ a b)) :to-equal '(/ a b)))

(it-sequential "expander-slash-nary-is-left-fold"
  (expect (cl-cc/expand:compiler-macroexpand-all '(/ a b c)) :to-equal '(/ (/ a b) c)))

;;; ─── log ─────────────────────────────────────────────────────────────────

(it-sequential "expander-log-unary-is-passthrough"
  (expect (cl-cc/expand:compiler-macroexpand-all '(log x)) :to-equal '(log x)))

(it-sequential "expander-log-binary-is-change-of-base"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(log x y))))
    (expect (first result) :to-be '/)
    (expect (second result) :to-equal '(log x))
    (expect (third result) :to-equal '(log y))))

(it-sequential "expander-log-three-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(log x y z))))

;;; ─── min / max ────────────────────────────────────────────────────────────

(it-sequential "expander-min-max-zero-arg-signals-error min"
  (destructuring-bind (form) (list '(min))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-min-max-zero-arg-signals-error max"
  (destructuring-bind (form) (list '(max))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-min-max-unary-identity min"
  (destructuring-bind (form expected) (list '(min x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-min-max-unary-identity max"
  (destructuring-bind (form expected) (list '(max x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-min-max-binary-builtin min"
  (destructuring-bind (form expected) (list '(min a b) '(min a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-min-max-binary-builtin max"
  (destructuring-bind (form expected) (list '(max a b) '(max a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-min-max-nary-left-fold min"
  (destructuring-bind (form expected-op) (list '(min a b c) 'min)
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be expected-op))))

(it-sequential "expander-min-max-nary-left-fold max"
  (destructuring-bind (form expected-op) (list '(max a b c) 'max)
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be expected-op))))

;;; ─── gcd / lcm ────────────────────────────────────────────────────────────

(it-sequential "expander-gcd-lcm-zero-arg-identity gcd"
  (destructuring-bind (form expected) (list '(gcd) 0)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-gcd-lcm-zero-arg-identity lcm"
  (destructuring-bind (form expected) (list '(lcm) 1)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-gcd-lcm-unary-wraps-abs gcd"
  (destructuring-bind (form expected) (list '(gcd x) '(abs x))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-gcd-lcm-unary-wraps-abs lcm"
  (destructuring-bind (form expected) (list '(lcm x) '(abs x))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-gcd-lcm-binary-builtin gcd"
  (destructuring-bind (form expected) (list '(gcd a b) '(gcd a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-gcd-lcm-binary-builtin lcm"
  (destructuring-bind (form expected) (list '(lcm a b) '(lcm a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

;;; ─── float-sign ──────────────────────────────────────────────────────────

(it-sequential "expander-float-sign-unary-is-passthrough"
  (expect (cl-cc/expand:compiler-macroexpand-all '(float-sign x)) :to-equal '(float-sign x)))

(it-sequential "expander-float-sign-binary-scales-abs"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(float-sign x y))))
    (expect (first result) :to-be '*)
    (expect (second result) :to-equal '(float-sign x))
    (expect (third result) :to-equal '(abs y))))

(it-sequential "expander-float-sign-three-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(float-sign x y z))))

;;; ─── float ───────────────────────────────────────────────────────────────

(it-sequential "expander-float-unary-is-passthrough"
  (expect (cl-cc/expand:compiler-macroexpand-all '(float x)) :to-equal '(float x)))

(it-sequential "expander-float-binary-drops-prototype"
  (expect (cl-cc/expand:compiler-macroexpand-all '(float x 1.0)) :to-equal '(float x)))

(it-sequential "expander-float-three-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(float x 1.0 extra))))

;;; ─── logand / logior / logxor / logeqv ──────────────────────────────────

(it-sequential "expander-logical-bitwise-zero-arg-identity logand"
  (destructuring-bind (form expected) (list '(logand) -1)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-zero-arg-identity logior"
  (destructuring-bind (form expected) (list '(logior) 0)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-zero-arg-identity logxor"
  (destructuring-bind (form expected) (list '(logxor) 0)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-zero-arg-identity logeqv"
  (destructuring-bind (form expected) (list '(logeqv) -1)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-unary-passthrough logand"
  (destructuring-bind (form expected) (list '(logand x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-unary-passthrough logior"
  (destructuring-bind (form expected) (list '(logior x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-unary-passthrough logxor"
  (destructuring-bind (form expected) (list '(logxor x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-unary-passthrough logeqv"
  (destructuring-bind (form expected) (list '(logeqv x) 'x)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-binary-builtin logand"
  (destructuring-bind (form expected) (list '(logand a b) '(logand a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-binary-builtin logior"
  (destructuring-bind (form expected) (list '(logior a b) '(logior a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-binary-builtin logxor"
  (destructuring-bind (form expected) (list '(logxor a b) '(logxor a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-binary-builtin logeqv"
  (destructuring-bind (form expected) (list '(logeqv a b) '(logeqv a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-logical-bitwise-nary-left-fold logand"
  (destructuring-bind (form expected-op) (list '(logand a b c) 'logand)
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be expected-op))))

(it-sequential "expander-logical-bitwise-nary-left-fold logior"
  (destructuring-bind (form expected-op) (list '(logior a b c) 'logior)
    (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (expect (car result) :to-be expected-op)
    (expect (consp (second result)) :to-be-truthy)
    (expect (car (second result)) :to-be expected-op))))

