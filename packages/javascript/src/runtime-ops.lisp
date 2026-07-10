;;;; packages/javascript/src/runtime-ops.lisp — JS operator runtime helpers

(in-package :cl-cc/javascript)

;;; ─── 32-bit integer coercion helpers ─────────────────────────────────────────

(defun %js-to-int32 (x)
  "Coerce X to an unsigned 32-bit integer (JS ToInt32 step 1)."
  (logand (truncate (%js-to-number x)) #xFFFFFFFF))

(defun %js-sign-extend32 (n)
  "Sign-extend a 32-bit value: if bit 31 is set, fold into the negative range."
  (if (logbitp 31 n) (- n #x100000000) n))

;;; ─── Bitwise binary operators ─────────────────────────────────────────────────

(defmacro define-js-bitwise-binop (name cl-op docstring)
  "Define a JS bitwise binary operator that coerces operands to 32-bit integers."
  `(defun ,name (a b)
     ,docstring
     (%js-sign-extend32 (,cl-op (%js-to-int32 a) (%js-to-int32 b)))))

(defun %js-bitwise-not (x)
  "JS ~x: flip all 32 bits then sign-extend."
  (%js-sign-extend32 (logxor (%js-to-int32 x) #xFFFFFFFF)))

(define-js-bitwise-binop %js-bitwise-or  logior "JS a | b.")
(define-js-bitwise-binop %js-bitwise-and logand "JS a & b.")
(define-js-bitwise-binop %js-bitwise-xor logxor "JS a ^ b.")

;;; ─── Shift operators ──────────────────────────────────────────────────────────

(defun %js-shift-left (a b)
  "JS a << b (b taken mod 32)."
  (let ((ia    (%js-to-int32 a))
        (shift (mod (logand (%js-to-int32 b) #x1F) 32)))
    (%js-sign-extend32 (logand (ash ia shift) #xFFFFFFFF))))

(defun %js-shift-right (a b)
  "JS a >> b, arithmetic (signed). b taken mod 32."
  (let ((ia    (%js-sign-extend32 (%js-to-int32 a)))
        (shift (mod (logand (%js-to-int32 b) #x1F) 32)))
    (ash ia (- shift))))

(defun %js-unsigned-shift-right (a b)
  "JS a >>> b, logical (unsigned, always non-negative). b taken mod 32."
  (ash (%js-to-int32 a)
       (- (mod (logand (%js-to-int32 b) #x1F) 32))))

;;; ─── Unary operators ──────────────────────────────────────────────────────────

(defun %js-unary-plus (x)
  "JS unary +: numeric coercion."
  (%js-to-number x))

;;; ─── Increment / decrement on non-variable targets ───────────────────────────
;;; These are fallbacks for ++/-- on non-assignable expressions.

(defun %js-postfix-inc (val) val)
(defun %js-postfix-dec (val) val)
(defun %js-prefix-inc  (val) (+ (%js-to-number val) 1))
(defun %js-prefix-dec  (val) (- (%js-to-number val) 1))

;;; ─── Regex literals ───────────────────────────────────────────────────────────
;;; Full RegExp implementation is in runtime-regex.lisp (loaded before this file).
;;; %js-make-regex is defined there as a native NFA-based engine.
;;; This file previously had a stub — removed to avoid shadowing the real impl.

;;; ─── Getter / setter accessor marker ─────────────────────────────────────────

(defun %js-accessor (kind fn)
  "Tag FN as a get/set accessor descriptor. KIND is \"get\" or \"set\"."
  (let ((ht (%js-make-ht)))
    (setf (gethash "__accessor__" ht) t
          (gethash "kind"         ht) kind
          (gethash "fn"           ht) fn)
    ht))

;;; Async wrappers and yield-from live in runtime-async.lisp (separated by concern).

;;; ─── Pattern destructuring assignment (fallback) ──────────────────────────────

(defun %js-assign-pattern (lhs rhs)
  "Fallback for destructuring assignment to a non-simple LHS: returns RHS."
  (declare (ignore lhs))
  rhs)

;;; ─── ES2025 explicit resource management ─────────────────────────────────────

(defun %js-using-register (resource)
  "Register RESOURCE for disposal at scope exit (simplified: identity)."
  resource)

;;; ─── new.target ──────────────────────────────────────────────────────────────

(defun %js-new-target ()
  "Return new.target — undefined outside a constructor."
  +js-undefined+)

;;; ─── BigInt (ES2020) ─────────────────────────────────────────────────────────
;;; js-bigint struct is declared in runtime.lisp (loads before this file).

(defun %js-bigint (x)
  "JS BigInt() constructor: coerce X to a BigInt."
  (cond ((js-bigint-p x) x)
        ((integerp x) (%make-js-bigint x))
        ((floatp x) (%make-js-bigint (truncate x)))
        ((stringp x) (handler-case (%make-js-bigint (parse-integer x :junk-allowed t))
                       (error () (error "Cannot convert ~A to BigInt" x))))
        (t (%make-js-bigint 0))))

(defun %js-bigint-to-string (bi &optional (radix 10))
  "Display a BigInt as string (without the n suffix at runtime).
JS always returns lowercase hex digits, so we normalise via string-downcase."
  (string-downcase (format nil "~vR" radix (js-bigint-value bi))))

;;; Extract integer value from a BigInt or plain number.
(defun %js-bigint-val (x)
  (if (js-bigint-p x) (js-bigint-value x) (truncate x)))

(defmacro define-js-bigint-binop (name cl-op)
  "Define a BigInt binary op (handles mixed BigInt/number operands)."
  `(defun ,name (a b)
     (%make-js-bigint (,cl-op (%js-bigint-val a) (%js-bigint-val b)))))

(define-js-bigint-binop %js-bigint-add +)
(define-js-bigint-binop %js-bigint-sub -)
(define-js-bigint-binop %js-bigint-mul *)
(define-js-bigint-binop %js-bigint-bitwise-and logand)
(define-js-bigint-binop %js-bigint-bitwise-or  logior)
(define-js-bigint-binop %js-bigint-bitwise-xor logxor)

(defmacro define-js-bigint-dividing-op (name cl-op)
  "Define a BigInt division/modulo op that throws on zero divisor."
  `(defun ,name (a b)
     (let ((av (%js-bigint-val a)) (bv (%js-bigint-val b)))
       (when (zerop bv) (error "Division by zero"))
       (%make-js-bigint (,cl-op av bv)))))

(define-js-bigint-dividing-op %js-bigint-div truncate)
(define-js-bigint-dividing-op %js-bigint-mod rem)

(defun %js-bigint-pow (a b)
  (%make-js-bigint (expt (%js-bigint-val a) (%js-bigint-val b))))

(defun %js-bigint-compare (a b)
  "Return -1/0/1 for BigInt comparison (also works if one side is a regular number)."
  (let ((av (%js-bigint-val a)) (bv (%js-bigint-val b)))
    (cond ((< av bv) -1) ((> av bv) 1) (t 0))))

(defun %js-bigint-lshift (a n)
  (%make-js-bigint (ash (%js-bigint-val a) (%js-bigint-val n))))

(defun %js-bigint-rshift (a n)
  (%make-js-bigint (ash (%js-bigint-val a) (- (%js-bigint-val n)))))

(defun %js-bigint-negate (a)
  (%make-js-bigint (- (%js-bigint-val a))))
;;; URI, base64, and TextEncoder/TextDecoder helpers live in
;;; runtime-ops-encoding.lisp.
