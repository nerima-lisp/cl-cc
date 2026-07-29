;;;; tests/unit/expand/expander-comparison-tests.lisp — Numeric and character comparison expander tests

(in-package :cl-cc/test)



;;; ─── = / < / > / <= / >= (numeric comparison chaining) ─────────────────

(it-sequential "expander-numeric-cmp-zero-arg-error eq"
  (destructuring-bind (form) (list '(=))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-numeric-cmp-zero-arg-error lt"
  (destructuring-bind (form) (list '(<))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-numeric-cmp-zero-arg-error gt"
  (destructuring-bind (form) (list '(>))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-numeric-cmp-zero-arg-error lte"
  (destructuring-bind (form) (list '(<=))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-numeric-cmp-zero-arg-error gte"
  (destructuring-bind (form) (list '(>=))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-numeric-cmp-unary-returns-t eq"
  (destructuring-bind (form expected) (list '(= x) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-unary-returns-t lt"
  (destructuring-bind (form expected) (list '(< x) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-unary-returns-t gt"
  (destructuring-bind (form expected) (list '(> x) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-unary-returns-t lte"
  (destructuring-bind (form expected) (list '(<= x) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-unary-returns-t gte"
  (destructuring-bind (form expected) (list '(>= x) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-binary-builtin eq"
  (destructuring-bind (form expected) (list '(= a b) '(= a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-binary-builtin lt"
  (destructuring-bind (form expected) (list '(< a b) '(< a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-binary-builtin lte"
  (destructuring-bind (form expected) (list '(<= a b) '(<= a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-cmp-nary-chain"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(= a b c))))
    ;; let-binding wrapper with AND/IF chain
    (expect (member (car result) '(let if and)) :to-be-truthy)))

;;; ─── /= (not-equal, all-distinct) ────────────────────────────────────────

(it-sequential "expander-neq-zero-arg-signals-error"
  (signals error (cl-cc/expand:compiler-macroexpand-all '(/=))))

(it-sequential "expander-neq-unary-returns-t"
  (expect (cl-cc/expand:compiler-macroexpand-all '(/= x)) :to-equal t))

(it-sequential "expander-neq-binary-expands-to-not-eq"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(/= a b))))
    (expect (first result) :to-be 'not)
    (expect (car (second result)) :to-be '=)))

(it-sequential "expander-neq-nary-generates-let-with-pairs"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(/= a b c))))
    ;; Top level must be (let ...)
    (expect (first result) :to-be 'let)
    ;; Body should start with nested IF checks over pairwise (/=) expansion.
    (let ((body (third result)))
      (expect (first body) :to-be 'if))))

;;; ─── char comparison chaining ────────────────────────────────────────────

(it-sequential "expander-char-cmp-zero-arg-error char="
  (destructuring-bind (form) (list '(char=))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-char-cmp-zero-arg-error char<"
  (destructuring-bind (form) (list '(char<))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-char-cmp-zero-arg-error char<="
  (destructuring-bind (form) (list '(char<=))
    (signals error (cl-cc/expand:compiler-macroexpand-all form))))

(it-sequential "expander-char-cmp-unary-returns-t char="
  (destructuring-bind (form expected) (list '(char= c) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-char-cmp-unary-returns-t char-equal"
  (destructuring-bind (form expected) (list '(char-equal c) t)
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-char-cmp-binary-builtin char="
  (destructuring-bind (form expected) (list '(char= a b) '(char= a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-char-cmp-binary-builtin char<"
  (destructuring-bind (form expected) (list '(char< a b) '(char< a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-char-cmp-binary-builtin char-lessp"
  (destructuring-bind (form expected) (list '(char-lessp a b) '(char-lessp a b))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

;;; ─── expander-numeric-identity-and-unary (original tests kept) ──────────

(it-sequential "expander-numeric-comparison-chain-uses-temporaries"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(= a b c))))
    (expect (car result) :to-be 'let)
    (let ((body (caddr result)))
      (expect (car body) :to-be 'if)
      (expect (car (second body)) :to-be '=)
      (expect (car (third body)) :to-be '=))))

(it-sequential "expander-numeric-arity-normalization log-1"
  (destructuring-bind (form expected) (list '(log x) '(log x))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-arity-normalization log-2"
  (destructuring-bind (form expected) (list '(log x y) '(/ (log x) (log y)))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-arity-normalization float-sign-2"
  (destructuring-bind (form expected) (list '(float-sign x y) '(* (float-sign x) (abs y)))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))

(it-sequential "expander-numeric-arity-normalization float-2"
  (destructuring-bind (form expected) (list '(float x y) '(float x))
    (expect (cl-cc/expand:compiler-macroexpand-all form) :to-equal expected)))
