;;;; packages/javascript/src/runtime-builtins-globals.lisp — JS global function implementations
;;;;
;;;; Global built-in functions: parseInt, parseFloat, isNaN, isFinite,
;;;; extended Math (sinh/cosh/tanh/cbrt/expm1/log1p),
;;;; Number strict predicates (Number.isNaN/isFinite/isInteger),
;;;; structuredClone / queueMicrotask globals,
;;;; BigInt helpers, Iterator.from, Map.groupBy, and String.raw.
;;;;
;;;; Load order: after runtime-builtins.lisp (needs %js-to-number, %js-funcall,
;;;; %js-nan-p, %js-float-nan-p, %js-float-infinity-p, %js-to-string, etc.)

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Global function helpers (referenced in *js-builtin-specs* and js-program-forms)
;;; -----------------------------------------------------------------------

(defun %js-parse-int (s &optional (radix 10))
  "Parse integer from string S in the given RADIX (default 10)."
  (handler-case
    (let* ((str (string-trim '(#\Space #\Tab #\Newline) (%js-to-string s)))
           (r (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix)))))
      (or (parse-integer str :radix r :junk-allowed t) *js-nan-float*))
    (error () *js-nan-float*)))

(defun %js-parse-float (s)
  "JS parseFloat: parse the LONGEST leading numeric prefix of S (so \"3.14abc\"
-> 3.14), or NaN when there is no leading number."
  (let* ((str (string-trim '(#\Space #\Tab #\Newline #\Return) (%js-to-string s)))
         (len (length str)) (i 0) (saw-digit nil))
    (labels ((eat-digits ()
               (loop while (and (< i len) (digit-char-p (char str i)))
                     do (setf saw-digit t) (incf i))))
      (when (and (< i len) (member (char str i) '(#\+ #\-))) (incf i))
      (eat-digits)
      (when (and (< i len) (char= (char str i) #\.)) (incf i) (eat-digits))
      (unless saw-digit (return-from %js-parse-float *js-nan-float*))
      ;; optional exponent: e[+/-]digits — only consumed if well-formed
      (when (and (< i len) (member (char str i) '(#\e #\E)))
        (let ((save i))
          (incf i)
          (when (and (< i len) (member (char str i) '(#\+ #\-))) (incf i))
          (if (and (< i len) (digit-char-p (char str i)))
              (loop while (and (< i len) (digit-char-p (char str i))) do (incf i))
              (setf i save))))
      (handler-case
          (let ((*read-eval* nil)
                (*read-default-float-format* 'double-float))
            (let ((v (read-from-string (subseq str 0 i) nil *js-nan-float*)))
              (if (realp v) (coerce v 'double-float) *js-nan-float*)))
        (error () *js-nan-float*)))))

(defun %js-is-nan (x)
  "Return true if X converts to NaN."
  (%js-nan-p (%js-to-number x)))

(defun %js-is-finite (x)
  "Return true if X is finite (not NaN, not Infinity)."
  (let ((n (%js-to-number x)))
    (and (not (%js-float-nan-p n)) (not (%js-float-infinity-p n)))))

;;; -----------------------------------------------------------------------
;;;  Math coerce-unary helpers
;;; -----------------------------------------------------------------------

(defmacro define-js-math-coerce-unary (name cl-fn)
  "Define a unary JS Math helper that coerces its argument to double-float."
  `(defun ,name (x) (,cl-fn (coerce (%js-to-number x) 'double-float))))

(define-js-math-coerce-unary %js-math-sinh  sinh)
(define-js-math-coerce-unary %js-math-cosh  cosh)
(define-js-math-coerce-unary %js-math-tanh  tanh)
(define-js-math-coerce-unary %js-math-asinh asinh)
(define-js-math-coerce-unary %js-math-acosh acosh)
(define-js-math-coerce-unary %js-math-atanh atanh)

(defun %js-math-cbrt (x)
  (let ((v (coerce (%js-to-number x) 'double-float)))
    (if (minusp v)
        (- (expt (- v) (/ 1.0d0 3.0d0)))
        (expt v (/ 1.0d0 3.0d0)))))

(defun %js-math-expm1 (x) (- (exp (coerce (%js-to-number x) 'double-float)) 1.0d0))
(defun %js-math-log1p  (x) (log (+ 1.0d0 (coerce (%js-to-number x) 'double-float))))

;;; -----------------------------------------------------------------------
;;;  Number.isNaN / isFinite / isInteger strict predicates
;;; -----------------------------------------------------------------------

(defun %js-number-is-nan (x)     (%js-float-nan-p x))
(defun %js-number-is-finite (x)
  (and (numberp x) (not (%js-float-nan-p x)) (not (%js-float-infinity-p x))))
(defun %js-number-is-integer (x)
  (and (numberp x) (not (%js-float-nan-p x)) (= x (truncate x))))

;;; -----------------------------------------------------------------------
;;;  Standalone global-builtin helpers (referenced in *js-builtin-specs*)
;;; -----------------------------------------------------------------------

(defun %js-structured-clone (val &rest _opts)
  "structuredClone(value[, options]): deep clone VAL (options ignored)."
  (declare (ignore _opts))
  (%js-deep-clone val))

(defun %js-queue-microtask (fn &rest _)
  "queueMicrotask(fn): synchronous in our single-threaded model."
  (declare (ignore _))
  (%js-funcall fn)
  +js-undefined+)

;;; -----------------------------------------------------------------------
;;;  Complex built-in helpers
;;; -----------------------------------------------------------------------

(defun %js-bigint-as-int-n (width bigint)
  "BigInt.asIntN(width, bigint): mask BIGINT to WIDTH-bit signed integer."
  (let* ((w       (truncate (%js-to-number width)))
         (modulus (expt 2 w))
         (masked  (mod (%js-bigint-val bigint) modulus)))
    (%make-js-bigint (if (>= masked (expt 2 (1- w))) (- masked modulus) masked))))

(defun %js-bigint-as-uint-n (width bigint)
  "BigInt.asUintN(width, bigint): mask BIGINT to WIDTH-bit unsigned integer."
  (%make-js-bigint (mod (%js-bigint-val bigint) (expt 2 (truncate (%js-to-number width))))))

(defun %js-iterator-from-iterable (iterable)
  "Iterator.from(iterable): wrap any iterable/iterator in the Iterator protocol."
  (%js-add-iterator-helpers!
   (cond
     ((typep iterable 'js-map)
      (%js-map-entries iterable))
     ((typep iterable 'js-set)
      (%js-vec-to-iter (%js-set-keys iterable)))
     ((and (%js-ht-p iterable) (gethash "next" iterable))
      iterable)
     ((and (%js-ht-p iterable) (gethash "@@iterator" iterable))
      (%js-funcall (gethash "@@iterator" iterable)))
     ((%js-vec-p iterable)
      (%js-make-generator
       (lambda ()
         (loop for i below (length iterable)
               do (%js-yield (aref iterable i))))))
     ((stringp iterable)
      (%js-make-generator
       (lambda ()
         (loop for ch across iterable
               do (%js-yield (string ch))))))
     (t
      (%js-make-object "next"
                       (lambda ()
                         (%js-make-object "value" +js-undefined+ "done" t)))))))

(defun %js-map-group-by (iterable key-fn)
  "Map.groupBy(iterable, keyFn): group ITERABLE elements by KEY-FN result."
  (let ((result (%js-make-map)))
    (%js-for-of iterable
                (lambda (item)
                  (let* ((key      (%js-funcall key-fn item))
                         (existing (%js-map-get result key)))
                    (if (eq existing +js-undefined+)
                        (let ((arr (%js-make-vec 0)))
                          (vector-push-extend item arr)
                          (%js-map-set result key arr))
                        (vector-push-extend item existing)))))
    result))

;;; -----------------------------------------------------------------------
;;;  String.raw helper
;;; -----------------------------------------------------------------------

(defun %js-string-raw (strings &rest subs)
  "String.raw`...` tag: join raw literal portions with substitution values.
STRINGS is a vector of literal string parts; SUBS are the interpolated values.
Escape sequences in the literal parts are not processed."
  (with-output-to-string (out)
    (loop for i below (length strings)
          do (write-string (%js-to-string (aref strings i)) out)
             (when (< i (length subs))
                (write-string (%js-to-string (nth i subs)) out)))))
