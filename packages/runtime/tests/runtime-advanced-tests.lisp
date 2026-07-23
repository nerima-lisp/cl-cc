;;;; tests/unit/runtime/runtime-advanced-tests.lisp — Runtime Advanced Unit Tests
;;;;
;;;; Tests for runtime.lisp (bitwise), runtime-math-io.lisp (symbols/hash/conditions/misc),
;;;; and runtime-io.lisp (I/O wrappers).
;;;;
;;;; String/char ops → runtime-strings-chars-tests.lisp
;;;; CLOS/generic ops → runtime-clos-tests.lisp

(in-package :cl-cc/test)


;;; ─── Bitwise ───────────────────────────────────────────────────────────────

(it-sequential "rt-bitwise-ops and"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-logand #b1110 #b1011 #b1010)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-ops or"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-logior #b1010 #b0101 #b1111)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-ops xor"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-logxor #b1010 #b0101 #b1111)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-ops ash-l"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-ash 2 2 8)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-ops ash-r"
  (destructuring-bind (fn a b expected) (list #'cl-cc/runtime:rt-ash 8 -2 2)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-predicate-conventions logtest-overlap"
  (destructuring-bind (pred-fn a b expected) (list #'cl-cc/runtime:rt-logtest #b1010 #b1000 1)
    (expect (= expected (funcall pred-fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-predicate-conventions logtest-disjoint"
  (destructuring-bind (pred-fn a b expected) (list #'cl-cc/runtime:rt-logtest #b1010 #b0101 0)
    (expect (= expected (funcall pred-fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-predicate-conventions logbitp-set"
  (destructuring-bind (pred-fn a b expected) (list #'cl-cc/runtime:rt-logbitp 0 1 1)
    (expect (= expected (funcall pred-fn a b)) :to-be-truthy)))

(it-sequential "rt-bitwise-predicate-conventions logbitp-clear"
  (destructuring-bind (pred-fn a b expected) (list #'cl-cc/runtime:rt-logbitp 1 1 0)
    (expect (= expected (funcall pred-fn a b)) :to-be-truthy)))

;;; ─── Symbol Operations ─────────────────────────────────────────────────────

(it-sequential "rt-symbol-name-returns-upcased-string"
  (expect (cl-cc/runtime:rt-symbol-name 'foo) :to-equal "FOO"))

(it-sequential "rt-make-symbol-creates-uninterned"
  (let ((s (cl-cc/runtime:rt-make-symbol "TEST")))
    (expect (symbol-name s) :to-equal "TEST")
    (expect (symbol-package s) :to-be-falsy)))

(it-sequential "rt-gensym-returns-unique-symbols"
  (expect (eq (cl-cc/runtime:rt-gensym) (cl-cc/runtime:rt-gensym)) :to-be-falsy))

(it-sequential "rt-symbol-plist-roundtrip"
  (let ((sym (cl-cc/runtime:rt-make-symbol "PLIST-TEST")))
    (cl-cc/runtime:rt-put-prop sym :color 'red)
    (expect (cl-cc/runtime:rt-get-prop sym :color) :to-be 'red)
    (cl-cc/runtime:rt-remprop sym :color)
    (expect (cl-cc/runtime:rt-get-prop sym :color) :to-be-falsy)))

(it-sequential "rt-intern-in-package"
  (let* ((pkg (find-package :cl-cc/test))
         (sym (cl-cc/runtime:rt-intern "RT-INTERN-TEST-SYM" pkg))
         (sym2 (cl-cc/runtime:rt-intern "RT-INTERN-TEST-SYM" pkg)))
    (expect (symbolp sym) :to-be-truthy)
    (expect (symbol-name sym) :to-equal "RT-INTERN-TEST-SYM")
    (expect sym2 :to-be sym)))

;;; ─── Hash Table Operations ─────────────────────────────────────────────────

(it-sequential "rt-hash-table-set-get-rem-count"
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :a ht 1)
    (cl-cc/runtime:rt-sethash :b ht 2)
    (expect (= 1 (cl-cc/runtime:rt-gethash :a ht)) :to-be-truthy)
    (expect (= 2 (cl-cc/runtime:rt-hash-count ht)) :to-be-truthy)
    (cl-cc/runtime:rt-remhash :a ht)
    (expect (= 1 (cl-cc/runtime:rt-hash-count ht)) :to-be-truthy)))

(it-sequential "rt-hash-table-keys-values-clrhash"
  (let ((ht (cl-cc/runtime:rt-make-hash-table)))
    (cl-cc/runtime:rt-sethash :x ht 10)
    (cl-cc/runtime:rt-sethash :y ht 20)
    (expect (= 2 (length (cl-cc/runtime:rt-hash-keys ht))) :to-be-truthy)
    (expect (= 2 (length (cl-cc/runtime:rt-hash-values ht))) :to-be-truthy)
    (expect (member :x (cl-cc/runtime:rt-hash-keys ht)) :to-be-truthy)
    (expect (member 10 (cl-cc/runtime:rt-hash-values ht)) :to-be-truthy)
    (cl-cc/runtime:rt-clrhash ht)
    (expect (= 0 (cl-cc/runtime:rt-hash-count ht)) :to-be-truthy)))

(it-sequential "rt-maphash-and-hash-test"
  (let ((ht (cl-cc/runtime:rt-make-hash-table :test #'equal)))
    (cl-cc/runtime:rt-sethash "a" ht 1)
    (cl-cc/runtime:rt-sethash "b" ht 2)
    (let ((collected nil))
      (cl-cc/runtime:rt-maphash (lambda (k v) (push (cons k v) collected)) ht)
      (expect (= 2 (length collected)) :to-be-truthy))
    (expect (cl-cc/runtime:rt-hash-test ht) :to-equal 'equal)))

(it-sequential "rt-hash-table-resizing-accessors"
  (let ((ht (cl-cc/runtime:rt-make-hash-table :test #'equal
                                              :size 4
                                              :rehash-size 2.0
                                              :rehash-threshold 0.75))
        (weak (cl-cc/runtime:rt-make-hash-table :test #'equal
                                                :size 4
                                                :rehash-size 3.0
                                                :rehash-threshold 0.6
                                                :weakness :key)))
    (expect (cl-cc/runtime:rt-hash-test ht) :to-equal 'equal)
    (expect (>= (cl-cc/runtime:rt-hash-size ht) 4) :to-be-truthy)
    (expect (= 2.0 (cl-cc/runtime:rt-hash-rehash-size ht)) :to-be-truthy)
    (expect (= 0.75 (cl-cc/runtime:rt-hash-rehash-threshold ht)) :to-be-truthy)
    (expect (cl-cc/runtime:rt-hash-table-weakness weak) :to-equal :key)
    (expect (>= (cl-cc/runtime:rt-hash-size weak) 4) :to-be-truthy)
    (expect (= 3.0 (cl-cc/runtime:rt-hash-rehash-size weak)) :to-be-truthy)
    (expect (= 0.6 (cl-cc/runtime:rt-hash-rehash-threshold weak)) :to-be-truthy)))

;;; ─── Conditions ────────────────────────────────────────────────────────────

(it-sequential "rt-signal-error-signals"
  (signals error (cl-cc/runtime:rt-signal-error "test error")))

(it-sequential "rt-signal-conditions"
  (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/runtime:rt-signal (make-condition 'simple-condition :format-control "test"))) (simple-condition () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))
  (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/runtime:rt-warn-fn "a warning")) (warning () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy)))

;;; ─── Misc ──────────────────────────────────────────────────────────────────

(it-sequential "rt-fboundp-uses-runtime-function-registry"
  (let ((sym (gensym "RT-FBOUNDP-TEST-")))
    (setf (gethash sym cl-cc/runtime::*rt-function-registry*) t)
    (expect (= 1 (cl-cc/runtime:rt-fboundp sym)) :to-be-truthy)
    (remhash sym cl-cc/runtime::*rt-function-registry*)
    (expect (= 0 (cl-cc/runtime:rt-fboundp sym)) :to-be-truthy)))

(it-sequential "rt-bootstrap-function-registry-uses-explicit-seed-list"
  (let ((cl-cc/runtime::*rt-function-registry* (make-hash-table :test #'eq)))
    (cl-cc/runtime::%rt-bootstrap-function-registry)
    (dolist (sym cl-cc/runtime::*rt-bootstrap-function-symbols*)
      (expect (gethash sym cl-cc/runtime::*rt-function-registry*) :to-be-truthy))))

(it-sequential "rt-package-registry-is-seeded-conservatively"
  (expect (find :cl-cc/runtime cl-cc/runtime::*rt-bootstrap-package-names*) :to-be-truthy)
  (expect (find :cl-cc/bootstrap cl-cc/runtime::*rt-bootstrap-package-names*) :to-be-truthy)
  (expect (find :cl cl-cc/runtime::*rt-bootstrap-package-names*) :to-be-truthy)
  (expect (gethash "CL-CC/RUNTIME" cl-cc/runtime:*rt-package-registry*) :to-be-truthy))

(it-sequential "rt-intern-preserves-seeded-host-package-symbols"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal)))
    (cl-cc/runtime::%rt-bootstrap-package-registry)
    (let* ((pkg (cl-cc/runtime:rt-find-package :cl-cc/bootstrap))
           (sym (cl-cc/runtime:rt-intern "*VM-PARSE-FORMS-HOOK-INSTALLER*" pkg)))
      (expect sym :to-be 'cl-cc/bootstrap:*vm-parse-forms-hook-installer*))))

(it-sequential "rt-runtime-callable-registration-publishes-bootstrap-hook"
  (expect cl-cc/bootstrap::*runtime-vm-callable-register-hook* :to-be-truthy))

(it-sequential "rt-package-layer-publishes-bootstrap-function-hooks"
  (expect cl-cc/bootstrap::*runtime-find-package-fn* :to-be-truthy)
  (expect cl-cc/bootstrap::*runtime-intern-fn* :to-be-truthy)
  (expect cl-cc/bootstrap::*runtime-set-symbol-value-fn* :to-be-truthy))

(it-sequential "rt-boundp-and-makunbound"
  (let ((sym (gensym "RT-BOUND-TEST-")))
    (expect (= 0 (cl-cc/runtime:rt-boundp sym)) :to-be-truthy)
    (cl-cc/runtime:rt-set-symbol-value sym 42)
    (expect (= 1 (cl-cc/runtime:rt-boundp sym)) :to-be-truthy)
    (cl-cc/runtime:rt-makunbound sym)
    (expect (= 0 (cl-cc/runtime:rt-boundp sym)) :to-be-truthy)))

(it-sequential "rt-coerce-works"
  (expect (cl-cc/runtime:rt-coerce #(1 2 3) 'list) :to-equal '(1 2 3)))

(it-sequential "rt-read-write-to-string"
  (expect (= 42 (cl-cc/runtime:rt-read-from-string "42")) :to-be-truthy)
  (expect (cl-cc/runtime:rt-write-to-string '(1 2 3)) :to-equal "(1 2 3)"))

(it-sequential "rt-random-and-time"
  (let ((r (cl-cc/runtime:rt-random 100)))
    (expect (integerp r) :to-be-truthy)
    (expect (and (>= r 0) (< r 100)) :to-be-truthy))
  (expect (integerp (cl-cc/runtime:rt-get-universal-time)) :to-be-truthy))

;;; ─── I/O Wrappers ──────────────────────────────────────────────────────────

(it-sequential "rt-string-stream-creation-and-io"
  (let ((s (cl-cc/runtime:rt-make-string-stream "hello")))
    (expect (cl-cc/runtime:rt-read-char s) :to-equal #\h))
  (let ((s (cl-cc/runtime:rt-make-string-stream "" :direction :output)))
    (cl-cc/runtime:rt-write-string "world" s)
    (expect (cl-cc/runtime:rt-get-output-stream-string s) :to-equal "world"))
  (let ((s (cl-cc/runtime:rt-make-string-output-stream)))
    (cl-cc/runtime:rt-stream-write-string s "test")
    (expect (cl-cc/runtime:rt-get-output-stream-string s) :to-equal "test")))

(it-sequential "rt-stream-predicates input-true"
  (destructuring-bind (pred-fn direction verify) (list #'cl-cc/runtime:rt-input-stream-p :input (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream))))
    (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream))))

(it-sequential "rt-stream-predicates input-false"
  (destructuring-bind (pred-fn direction verify) (list #'cl-cc/runtime:rt-input-stream-p :output (lambda (pred-fn stream) (assert-= 0 (funcall pred-fn stream))))
    (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream))))

(it-sequential "rt-stream-predicates output-true"
  (destructuring-bind (pred-fn direction verify) (list #'cl-cc/runtime:rt-output-stream-p :output (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream))))
    (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream))))

(it-sequential "rt-stream-predicates output-false"
  (destructuring-bind (pred-fn direction verify) (list #'cl-cc/runtime:rt-output-stream-p :input (lambda (pred-fn stream) (assert-= 0 (funcall pred-fn stream))))
    (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream))))

(it-sequential "rt-stream-predicates open-true"
  (destructuring-bind (pred-fn direction verify) (list #'cl-cc/runtime:rt-open-stream-p :input (lambda (pred-fn stream) (assert-= 1 (funcall pred-fn stream))))
    (let ((stream (ecase direction
                  (:input  (make-string-input-stream "x"))
                  (:output (make-string-output-stream)))))
    (funcall verify pred-fn stream))))

(it-sequential "rt-read-write-char-roundtrip"
  (let ((out (make-string-output-stream)))
    (cl-cc/runtime:rt-write-char #\Z out)
    (let ((in (make-string-input-stream (get-output-stream-string out))))
      (expect (cl-cc/runtime:rt-read-char in) :to-equal #\Z))))
