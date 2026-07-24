(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; %fail-test compatibility shim
;;; ------------------------------------------------------------

(defun %fail-test (message &key expected actual form at)
  "Compatibility shim preserving the original %FAIL-TEST keyword signature
for the handful of test-support files (framework-compiler-run-string.lisp)
that build their own failure messages, backed by CL-WEAVE:FAIL."
  (declare (ignore at))
  (cl-weave:fail "~A~@[ (expected ~S)~]~@[ (got ~S)~]~@[ [form: ~S]~]"
                 message expected actual form))

;;; ------------------------------------------------------------
;;; Assertion macros, backed by CL-WEAVE:FAIL
;;; ------------------------------------------------------------
;;;
;;; Preserves every original assert-* macro's name and call signature (in
;;; particular the EXPECTED-then-ACTUAL argument order the original
;;; %assert-binary/%define-binary-assertion used, which is the reverse of
;;; cl-weave's own EXPECT/:to-equal ACTUAL-then-EXPECTED convention) so none
;;; of the ~14,000 existing call sites across the test suite need to change.
;;; CL-WEAVE:FAIL is a thin wrapper around CL-WEAVE::SIGNAL-ASSERTION-FAILURE
;;; and is exported public API, unlike the registration functions in
;;; framework-definitions.lisp.

(defmacro assert-faster-than (max-ns &body body)
  "Assert BODY completes in no more than MAX-NS nanoseconds.

This assertion intentionally performs a single deterministic measurement and is
intended for focused smoke tests, not statistical performance claims."
  (let ((limit (gensym "LIMIT"))
        (start (gensym "START"))
        (duration (gensym "DURATION")))
    `(let ((,limit ,max-ns))
       (check-type ,limit integer)
       (let ((,start (get-internal-real-time)))
         ,@body
         (let ((,duration (round (* (- (get-internal-real-time) ,start)
                                    (/ 1000000000 internal-time-units-per-second)))))
           (when (> ,duration ,limit)
             (cl-weave:fail "assert-faster-than failed (expected <= ~Dns, got ~Dns)" ,limit ,duration))
           t)))))

(defmacro assert-no-consing (&body body)
  "Assert BODY does not increase SBCL's bytes-consed counter."
  (let ((before (gensym "BEFORE"))
        (after (gensym "AFTER"))
        (delta (gensym "DELTA")))
    `(let ((,before (sb-ext:get-bytes-consed)))
       ,@body
       (let* ((,after (sb-ext:get-bytes-consed))
              (,delta (- ,after ,before)))
         (when (plusp ,delta)
           (cl-weave:fail "assert-no-consing failed (consed ~D bytes)" ,delta))
         t))))

(defmacro assert-no-allocation (&body body)
  "Alias for ASSERT-NO-CONSING."
  `(assert-no-consing ,@body))

(defmacro assert-type-equal (expected actual)
  "Assert that two type-nodes are structurally equal via type-equal-p."
  (let ((e (gensym "E")) (a (gensym "A")))
    `(let ((,e ,expected) (,a ,actual))
       (unless (type-equal-p ,e ,a)
         (cl-weave:fail "assert-type-equal: types not equal (expected ~A, got ~A)"
                        (type-to-string ,e) (type-to-string ,a)))
       t)))

(defmacro assert-unifies (t1 t2)
  "Assert that types T1 and T2 unify successfully."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (unless ,ok
         (cl-weave:fail "assert-unifies: types failed to unify"))
       t)))

(defmacro assert-not-unifies (t1 t2)
  "Assert that types T1 and T2 fail to unify."
  (let ((s (gensym "S")) (ok (gensym "OK")))
    `(multiple-value-bind (,s ,ok) (type-unify ,t1 ,t2)
       (declare (ignore ,s))
       (when ,ok
         (cl-weave:fail "assert-not-unifies: types unexpectedly unified"))
       t)))

;;; ------------------------------------------------------------
;;; Composite assertion macros — reduce boilerplate
;;; ------------------------------------------------------------

(defmacro assert-bool (expected form)
  "Assert FORM is truthy when EXPECTED is truthy, falsy when EXPECTED is falsy.
Eliminates the (if pred (assert-true ...) (assert-false ...)) pattern."
  (let ((v (gensym "V")) (e (gensym "E")))
    `(let ((,v ,form) (,e ,expected))
       (if ,e
           (unless ,v (cl-weave:fail "assert-bool: expected truthy value, got ~S" ,v))
           (when ,v (cl-weave:fail "assert-bool: expected falsy value, got ~S" ,v)))
       t)))

(defmacro assert-list-contains (list-form members-form &key length)
  "Assert LIST-FORM contains every element of MEMBERS-FORM (evaluated at runtime, equal test).
MEMBERS-FORM must evaluate to a list. Optionally assert LIST-FORM has exactly LENGTH elements."
  (let ((ls (gensym "LIST"))
        (ms (gensym "MEMBERS")))
    `(let ((,ls ,list-form)
           (,ms ,members-form))
       (dolist (m ,ms)
         (unless (member m ,ls :test #'equal)
           (cl-weave:fail "assert-list-contains: missing member ~S in ~S" m ,ls)))
       ,@(when length
           `((let ((actual-len (length ,ls)))
               (unless (= ,length actual-len)
                 (cl-weave:fail "assert-list-contains: wrong length (expected ~D, got ~D)"
                                ,length actual-len)))))
       t)))

(defmacro assert-bitfield (word-form &rest field-specs)
  "Assert each byte field of WORD-FORM equals its expected value.
Each field-spec is (byte-position byte-width expected-value)."
  (let ((w (gensym "WORD")))
    `(let ((,w ,word-form))
       ,@(mapcar (lambda (spec)
                   (destructuring-bind (pos width expected) spec
                     `(let ((actual (ldb (byte ,width ,pos) ,w)))
                        (unless (= ,expected actual)
                          (cl-weave:fail "assert-bitfield: field mismatch at byte(~A,~A) (expected ~S, got ~S)"
                                         ,width ,pos ,expected actual)))))
                 field-specs)
       t)))

;;; ------------------------------------------------------------
;;; assert-no-crash / assert-terminates (from framework-fuzz.lisp)
;;; ------------------------------------------------------------

(defmacro assert-no-crash (&body forms)
  "Assert that FORMS complete without signaling any serious condition."
  `(handler-case
       (progn ,@forms)
     (serious-condition (c)
       (cl-weave:fail "assert-no-crash: unexpected condition: ~A" c))))

(defmacro assert-terminates (form &key (timeout 5))
  "Assert that FORM completes within TIMEOUT seconds."
  `(handler-case
       (sb-ext:with-timeout ,timeout
         ,form)
     (sb-ext:timeout ()
       (cl-weave:fail "assert-terminates: form did not terminate within ~A seconds" ,timeout))))
