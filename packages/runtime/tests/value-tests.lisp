;;;; tests/unit/runtime/value-tests.lisp - NaN-Boxing Value Representation Tests
;;;
;;; Tests for cl-cc/runtime NaN-boxing: type predicates, encode/decode
;;; round-trips, singleton constants, and edge-case bit patterns.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Singleton constants
;;; ------------------------------------------------------------

(it-sequential "value-singleton-constants nil"
  (destructuring-bind (const expected-bits) (list cl-cc/runtime:+val-nil+ #x7FFF000000000000)
    (expect (= expected-bits const) :to-be-truthy)))

(it-sequential "value-singleton-constants t"
  (destructuring-bind (const expected-bits) (list cl-cc/runtime:+val-t+ #x7FFF000000000001)
    (expect (= expected-bits const) :to-be-truthy)))

(it-sequential "value-singleton-constants unbound"
  (destructuring-bind (const expected-bits) (list cl-cc/runtime:+val-unbound+ #x7FFF000000000002)
    (expect (= expected-bits const) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; val-nil-p / val-t-p / val-unbound-p
;;; ------------------------------------------------------------

(it-sequential "value-sentinel-predicates nil-recognizes-nil"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-nil-p cl-cc/runtime:+val-nil+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-truthy)))
    (funcall verify pred-fn val)))

(it-sequential "value-sentinel-predicates nil-rejects-t"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-nil-p cl-cc/runtime:+val-t+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-falsy)))
    (funcall verify pred-fn val)))

(it-sequential "value-sentinel-predicates t-recognizes-t"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-t-p cl-cc/runtime:+val-t+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-truthy)))
    (funcall verify pred-fn val)))

(it-sequential "value-sentinel-predicates t-rejects-nil"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-t-p cl-cc/runtime:+val-nil+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-falsy)))
    (funcall verify pred-fn val)))

(it-sequential "value-sentinel-predicates unbound-recognizes"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-unbound-p cl-cc/runtime:+val-unbound+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-truthy)))
    (funcall verify pred-fn val)))

(it-sequential "value-sentinel-predicates unbound-rejects-nil"
  (destructuring-bind (pred-fn val verify) (list #'cl-cc/runtime:val-unbound-p cl-cc/runtime:+val-nil+ (lambda (pred-fn val)
             (expect (funcall pred-fn val) :to-be-falsy)))
    (funcall verify pred-fn val)))

;;; ------------------------------------------------------------
;;; Fixnum encode/decode round-trip
;;; ------------------------------------------------------------

(it-sequential "value-fixnum-round-trip zero"
  (destructuring-bind (n) (list 0)
    (expect (= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))) :to-be-truthy)))

(it-sequential "value-fixnum-round-trip positive"
  (destructuring-bind (n) (list 42)
    (expect (= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))) :to-be-truthy)))

(it-sequential "value-fixnum-round-trip negative"
  (destructuring-bind (n) (list -1)
    (expect (= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))) :to-be-truthy)))

(it-sequential "value-fixnum-round-trip large-positive"
  (destructuring-bind (n) (list (1- (expt 2 50)))
    (expect (= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))) :to-be-truthy)))

(it-sequential "value-fixnum-round-trip large-negative"
  (destructuring-bind (n) (list (- (expt 2 50)))
    (expect (= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))) :to-be-truthy)))


(it-sequential "native-bignum-checked-helpers-promote-and-coerce"
  (let* ((max-fix (1- (ash 1 50)))
         (min-fix (- (ash 1 50)))
         (one (cl-cc/runtime:encode-fixnum 1))
         (big-add (cl-cc/runtime:rt-native-bignum-add
                   (cl-cc/runtime:encode-fixnum max-fix) one))
         (big-sub (cl-cc/runtime:rt-native-bignum-sub
                   (cl-cc/runtime:encode-fixnum min-fix) one))
         (big-mul (cl-cc/runtime:rt-native-bignum-mul
                   (cl-cc/runtime:encode-fixnum (ash 1 30))
                   (cl-cc/runtime:encode-fixnum (ash 1 30)))))
    (expect (= (ash 1 50) (cl-cc/runtime:rt-native-bignum-to-integer big-add)) :to-be-truthy)
    (expect (= (1- min-fix) (cl-cc/runtime:rt-native-bignum-to-integer big-sub)) :to-be-truthy)
    (expect (= (ash 1 60) (cl-cc/runtime:rt-native-bignum-to-integer big-mul)) :to-be-truthy)
    (expect (= 42 (cl-cc/runtime:decode-fixnum
                  (cl-cc/runtime:rt-native-bignum-sub
                   (cl-cc/runtime:rt-native-integer->value (ash 1 50))
                   (cl-cc/runtime:rt-native-integer->value (- (ash 1 50) 42))))) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; val-fixnum-p
;;; ------------------------------------------------------------

(it-sequential "value-fixnum-p-recognises zero"
  (destructuring-bind (n) (list 0)
    (expect (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum n)) :to-be-truthy)))

(it-sequential "value-fixnum-p-recognises positive"
  (destructuring-bind (n) (list 100)
    (expect (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum n)) :to-be-truthy)))

(it-sequential "value-fixnum-p-recognises negative"
  (destructuring-bind (n) (list -100)
    (expect (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum n)) :to-be-truthy)))

(it-sequential "value-fixnum-p-rejects-sentinels nil"
  (destructuring-bind (val) (list cl-cc/runtime:+val-nil+)
    (expect (cl-cc/runtime:val-fixnum-p val) :to-be-falsy)))

(it-sequential "value-fixnum-p-rejects-sentinels t"
  (destructuring-bind (val) (list cl-cc/runtime:+val-t+)
    (expect (cl-cc/runtime:val-fixnum-p val) :to-be-falsy)))

;;; ------------------------------------------------------------
;;; Character encode/decode round-trip
;;; ------------------------------------------------------------

(it-sequential "value-char-round-trip ascii"
  (destructuring-bind (c) (list #\A)
    (expect (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char c)) :to-equal c)))

(it-sequential "value-char-round-trip nul"
  (destructuring-bind (c) (list (code-char 0))
    (expect (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char c)) :to-equal c)))

(it-sequential "value-char-round-trip unicode"
  (destructuring-bind (c) (list (code-char #x1F600))
    (expect (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char c)) :to-equal c)))

;;; ------------------------------------------------------------
;;; val-char-p
;;; ------------------------------------------------------------

(it-sequential "value-char-p-cases encoded-char"
  (destructuring-bind (val verify) (list (cl-cc/runtime:encode-char #\x) (lambda (val)
             (expect (cl-cc/runtime:val-char-p val) :to-be-truthy)))
    (funcall verify val)))

(it-sequential "value-char-p-cases nil-sentinel"
  (destructuring-bind (val verify) (list cl-cc/runtime:+val-nil+ (lambda (val)
             (expect (cl-cc/runtime:val-char-p val) :to-be-falsy)))
    (funcall verify val)))

(it-sequential "value-char-p-cases encoded-fixnum"
  (destructuring-bind (val verify) (list (cl-cc/runtime:encode-fixnum 65) (lambda (val)
             (expect (cl-cc/runtime:val-char-p val) :to-be-falsy)))
    (funcall verify val)))

;;; ------------------------------------------------------------
;;; Double encode/decode round-trip
;;; ------------------------------------------------------------

(it-sequential "value-double-round-trip zero"
  (destructuring-bind (d) (list 0.0d0)
    (expect (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double d)) :to-equal d)))

(it-sequential "value-double-round-trip positive"
  (destructuring-bind (d) (list 3.14d0)
    (expect (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double d)) :to-equal d)))

(it-sequential "value-double-round-trip negative"
  (destructuring-bind (d) (list -0.1d0)
    (expect (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double d)) :to-equal d)))

;;; ------------------------------------------------------------
;;; val-double-p
;;; ------------------------------------------------------------

(it-sequential "value-double-p-true"
  (expect (cl-cc/runtime:val-double-p (cl-cc/runtime:encode-double 0.1d0)) :to-be-truthy))

(it-sequential "value-double-p-false-for-non-doubles fixnum"
  (destructuring-bind (val) (list (cl-cc/runtime:encode-fixnum 42))
    (expect (cl-cc/runtime:val-double-p val) :to-be-falsy)))

(it-sequential "value-double-p-false-for-non-doubles nil"
  (destructuring-bind (val) (list cl-cc/runtime:+val-nil+)
    (expect (cl-cc/runtime:val-double-p val) :to-be-falsy)))

;;; ------------------------------------------------------------
;;; Pointer encode/decode
;;; ------------------------------------------------------------

(it-sequential "value-pointer-round-trip object"
  (destructuring-bind (addr tag) (list #x0000DEADBEEF cl-cc/runtime:+tag-object+)
    (let ((v (cl-cc/runtime:encode-pointer addr tag)))
    (expect (= addr (cl-cc/runtime:decode-pointer v)) :to-be-truthy))))

(it-sequential "value-pointer-round-trip cons"
  (destructuring-bind (addr tag) (list #x0000CAFE1234 cl-cc/runtime:+tag-cons+)
    (let ((v (cl-cc/runtime:encode-pointer addr tag)))
    (expect (= addr (cl-cc/runtime:decode-pointer v)) :to-be-truthy))))

(it-sequential "value-pointer-round-trip function"
  (destructuring-bind (addr tag) (list #x0000000100FF cl-cc/runtime:+tag-function+)
    (let ((v (cl-cc/runtime:encode-pointer addr tag)))
    (expect (= addr (cl-cc/runtime:decode-pointer v)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; val-pointer-p / sub-tag predicates
;;; ------------------------------------------------------------

(it-sequential "value-pointer-p-recognises object"
  (destructuring-bind (tag) (list cl-cc/runtime:+tag-object+)
    (expect (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-pointer #x1000 tag)) :to-be-truthy)))

(it-sequential "value-pointer-p-recognises cons"
  (destructuring-bind (tag) (list cl-cc/runtime:+tag-cons+)
    (expect (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-pointer #x1000 tag)) :to-be-truthy)))

(it-sequential "value-pointer-p-recognises string"
  (destructuring-bind (tag) (list cl-cc/runtime:+tag-string+)
    (expect (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-pointer #x1000 tag)) :to-be-truthy)))

(it-sequential "value-pointer-p-rejects-non-pointers nil"
  (destructuring-bind (val) (list cl-cc/runtime:+val-nil+)
    (expect (cl-cc/runtime:val-pointer-p val) :to-be-falsy)))

(it-sequential "value-pointer-p-rejects-non-pointers char"
  (destructuring-bind (val) (list (cl-cc/runtime:encode-char #\A))
    (expect (cl-cc/runtime:val-pointer-p val) :to-be-falsy)))

(it-sequential "value-sub-tag-predicates object"
  (destructuring-bind (tag pred) (list cl-cc/runtime:+tag-object+ #'cl-cc/runtime:val-object-p)
    (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (expect (funcall pred v) :to-be-truthy))))

(it-sequential "value-sub-tag-predicates cons"
  (destructuring-bind (tag pred) (list cl-cc/runtime:+tag-cons+ #'cl-cc/runtime:val-cons-p)
    (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (expect (funcall pred v) :to-be-truthy))))

(it-sequential "value-sub-tag-predicates symbol"
  (destructuring-bind (tag pred) (list cl-cc/runtime:+tag-symbol+ #'cl-cc/runtime:val-symbol-p)
    (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (expect (funcall pred v) :to-be-truthy))))

(it-sequential "value-sub-tag-predicates function"
  (destructuring-bind (tag pred) (list cl-cc/runtime:+tag-function+ #'cl-cc/runtime:val-function-p)
    (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (expect (funcall pred v) :to-be-truthy))))

(it-sequential "value-sub-tag-predicates string"
  (destructuring-bind (tag pred) (list cl-cc/runtime:+tag-string+ #'cl-cc/runtime:val-string-p)
    (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (expect (funcall pred v) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; No collisions between types
;;; ------------------------------------------------------------

(it-sequential "value-no-collision-nil-vs-char"
  (expect (/= cl-cc/runtime:+val-nil+ cl-cc/runtime:+tag-char+) :to-be-truthy))

(it-sequential "value-pointer-tag-vs-char-no-collision cons"
  (destructuring-bind (tag) (list cl-cc/runtime:+tag-cons+)
    (let ((ptr-v  (cl-cc/runtime:encode-pointer 0 tag))
        (char-v (cl-cc/runtime:encode-char (code-char 0))))
    (expect (/= (ash ptr-v -48) (ash char-v -48)) :to-be-truthy))))

(it-sequential "value-pointer-tag-vs-char-no-collision function"
  (destructuring-bind (tag) (list cl-cc/runtime:+tag-function+)
    (let ((ptr-v  (cl-cc/runtime:encode-pointer 0 tag))
        (char-v (cl-cc/runtime:encode-char (code-char 0))))
    (expect (/= (ash ptr-v -48) (ash char-v -48)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; encode-bool
;;; ------------------------------------------------------------

(it-sequential "value-encode-bool t"
  (destructuring-bind (cl-val expected-tag) (list t cl-cc/runtime:+val-t+)
    (expect (= expected-tag (cl-cc/runtime:encode-bool cl-val)) :to-be-truthy)))

(it-sequential "value-encode-bool nil"
  (destructuring-bind (cl-val expected-tag) (list nil cl-cc/runtime:+val-nil+)
    (expect (= expected-tag (cl-cc/runtime:encode-bool cl-val)) :to-be-truthy)))

(it-sequential "value-encode-bool truthy"
  (destructuring-bind (cl-val expected-tag) (list 42 cl-cc/runtime:+val-t+)
    (expect (= expected-tag (cl-cc/runtime:encode-bool cl-val)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; cl-value->val / val->cl-value round-trips
;;; ------------------------------------------------------------

(it-sequential "value-interop-round-trip fixnum"
  (destructuring-bind (cl-val) (list 99)
    (expect (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val cl-val)) :to-equal cl-val)))

(it-sequential "value-interop-round-trip char"
  (destructuring-bind (cl-val) (list #\Z)
    (expect (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val cl-val)) :to-equal cl-val)))

(it-sequential "value-interop-nil"
  (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:cl-value->val nil)) :to-be-truthy)
  (expect (cl-cc/runtime:val->cl-value cl-cc/runtime:+val-nil+) :to-equal nil))

(it-sequential "value-interop-t"
  (expect (= cl-cc/runtime:+val-t+ (cl-cc/runtime:cl-value->val t)) :to-be-truthy)
  (expect (cl-cc/runtime:val->cl-value cl-cc/runtime:+val-t+) :to-equal t))

(it-sequential "value-interop-double"
  (expect (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val 2.71828d0)) :to-equal 2.71828d0))
