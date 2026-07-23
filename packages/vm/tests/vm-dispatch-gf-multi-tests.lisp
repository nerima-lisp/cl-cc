;;;; tests/unit/vm/vm-dispatch-gf-multi-tests.lisp
;;;; Coverage for src/vm/vm-dispatch-gf-multi.lisp:
;;;;   %vm-gf-uses-composite-keys-p, %vm-resolve-single-dispatch,
;;;;   %vm-resolve-composite-dispatch, vm-resolve-gf-method,
;;;;   vm-resolve-multi-dispatch, vm-try-dispatch-combinations,
;;;;   vm-try-dispatch-sub.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-single-dispatch-gf-ht (methods-plist)
  "Build a minimal generic-function hash-table for single dispatch.
METHODS-PLIST is a list of (class-name . method-fn) pairs."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

(defun make-indexed-single-dispatch-gf-ht (methods-plist eql-plist)
  "Build a single-dispatch GF with an authoritative EQL dispatch index.
EQL-PLIST is a list of (eql-value . method-fn) pairs."
  (let ((gf-ht (make-single-dispatch-gf-ht methods-plist))
        (eql-index (make-hash-table :test #'equal)))
    (dolist (pair eql-plist)
      (setf (gethash (car pair) eql-index) (list (cdr pair))))
    (setf (gethash :__eql-index__ gf-ht) eql-index)
    gf-ht))

(defun make-composite-dispatch-gf-ht (methods-plist)
  "Build a generic-function hash-table with list keys for composite dispatch."
  (let ((gf-ht (make-hash-table :test #'equal))
        (methods-ht (make-hash-table :test #'equal)))
    (dolist (pair methods-plist)
      (setf (gethash (car pair) methods-ht) (cdr pair)))
    (setf (gethash :__methods__ gf-ht) methods-ht)
    (setf (gethash :__name__ gf-ht) 'test-multi-gf)
    (setf (gethash :__eql-methods__ gf-ht) nil)
    gf-ht))

;;; ─── %vm-gf-uses-composite-keys-p ────────────────────────────────────────

(it-sequential "gf-multi-composite-keys-empty-table-returns-false"
  (expect (cl-cc/vm::%vm-gf-uses-composite-keys-p (make-hash-table :test #'equal)) :to-be-falsy))

(it-sequential "gf-multi-composite-keys-symbol-only-keys-return-false"
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer ht) #'identity)
    (setf (gethash 'string  ht) #'identity)
    (expect (cl-cc/vm::%vm-gf-uses-composite-keys-p ht) :to-be-falsy)))

(it-sequential "gf-multi-composite-keys-list-key-returns-true"
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(integer string) ht) #'identity)
    (expect (cl-cc/vm::%vm-gf-uses-composite-keys-p ht) :to-be-truthy)))

(it-sequential "gf-multi-composite-keys-mixed-keys-return-true"
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash 'integer          ht) #'identity)
    (setf (gethash '(integer string) ht) #'identity)
    (expect (cl-cc/vm::%vm-gf-uses-composite-keys-p ht) :to-be-truthy)))

;;; ─── %vm-resolve-single-dispatch ──────────────────────────────────────────

(it-sequential "gf-multi-single-dispatch-cases exact-integer-hit"
  (destructuring-bind (dispatch-type method-fn arg expect-match-p) (list 'integer #'identity 42 t)
    (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons dispatch-type method-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s arg)))
    (if expect-match-p
        (expect result :to-be method-fn)
        (expect result :to-be-null)))))

(it-sequential "gf-multi-single-dispatch-cases exact-string-hit"
  (destructuring-bind (dispatch-type method-fn arg expect-match-p) (list 'string #'string-upcase "hello" t)
    (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons dispatch-type method-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s arg)))
    (if expect-match-p
        (expect result :to-be method-fn)
        (expect result :to-be-null)))))

(it-sequential "gf-multi-single-dispatch-cases t-fallback"
  (destructuring-bind (dispatch-type method-fn arg expect-match-p) (list t #'not 99 t)
    (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons dispatch-type method-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s arg)))
    (if expect-match-p
        (expect result :to-be method-fn)
        (expect result :to-be-null)))))

(it-sequential "gf-multi-single-dispatch-cases no-match-returns-nil"
  (destructuring-bind (dispatch-type method-fn arg expect-match-p) (list 'string #'identity 42 nil)
    (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons dispatch-type method-fn))))
         (methods-ht (gethash :__methods__ gf-ht))
         (result (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s arg)))
    (if expect-match-p
        (expect result :to-be method-fn)
        (expect result :to-be-null)))))

(it-sequential "gf-multi-single-dispatch-eql-index-hit-precedes-class"
  (let* ((s (make-test-vm))
         (class-fn #'identity)
         (eql-fn #'not)
         (gf-ht (make-indexed-single-dispatch-gf-ht
                 (list (cons 'symbol class-fn))
                 (list (cons :read eql-fn))))
         (methods-ht (gethash :__methods__ gf-ht)))
    (expect (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s :read) :to-be eql-fn)))

(it-sequential "gf-multi-single-dispatch-eql-index-avoids-linear-scan"
  (let* ((s (make-test-vm))
         (fallback-fn #'identity)
         (stale-eql-fn #'not)
         (gf-ht (make-indexed-single-dispatch-gf-ht
                 (list (cons 'symbol fallback-fn))
                 nil))
         (methods-ht (gethash :__methods__ gf-ht)))
    (setf (gethash '(eql :read) methods-ht) stale-eql-fn)
    (expect (cl-cc/vm::%vm-resolve-single-dispatch gf-ht methods-ht s :read) :to-be fallback-fn)))

;;; ─── vm-try-dispatch-combinations ────────────────────────────────────────

(it-sequential "gf-multi-try-dispatch-zero-arity-returns-nil"
  (expect (cl-cc/vm::vm-try-dispatch-combinations (make-hash-table :test #'equal) '() 0) :to-be-null))

(it-sequential "gf-multi-try-dispatch-exact-class-match"
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer) ht) my-fn)
    (expect (cl-cc/vm::vm-try-dispatch-combinations ht '((integer string)) 1) :to-be my-fn)))

(it-sequential "gf-multi-try-dispatch-t-fallback"
  (let ((ht (make-hash-table :test #'equal))
        (fallback #'not))
    (setf (gethash '(t) ht) fallback)
    (expect (cl-cc/vm::vm-try-dispatch-combinations ht '((symbol integer t)) 1) :to-be fallback)))

(it-sequential "gf-multi-try-dispatch-no-match-returns-nil"
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(string) ht) #'identity)
    (expect (cl-cc/vm::vm-try-dispatch-combinations ht '((integer)) 1) :to-be-null)))

;;; ─── vm-try-dispatch-sub ─────────────────────────────────────────────────

(it-sequential "gf-multi-try-dispatch-sub-null-cpls-matches-prefix-directly"
  (let ((ht (make-hash-table :test #'equal))
        (my-fn #'identity))
    (setf (gethash '(integer string) ht) my-fn)
    (expect (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string)) :to-be my-fn)))

(it-sequential "gf-multi-try-dispatch-sub-missing-key-returns-nil"
  (let ((ht (make-hash-table :test #'equal)))
    (expect (cl-cc/vm::vm-try-dispatch-sub ht nil '(integer string)) :to-be-null)))

;;; ─── vm-resolve-gf-method (integration) ──────────────────────────────────

(it-sequential "gf-multi-resolve-gf-exact-integer-dispatch"
  (let* ((s (make-test-vm))
         (int-fn #'1+)
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'integer int-fn)))))
    (expect (cl-cc/vm::vm-resolve-gf-method gf-ht s 42) :to-be int-fn)))

(it-sequential "gf-multi-resolve-gf-no-match-signals-error"
  (let* ((s (make-test-vm))
         (gf-ht (make-single-dispatch-gf-ht (list (cons 'string #'identity)))))
    (signals error (cl-cc/vm::vm-resolve-gf-method gf-ht s 42))))

(it-sequential "gf-multi-resolve-gf-composite-list-key-dispatch"
  (let* ((s (make-test-vm))
         (multi-fn #'cons)
         (gf-ht (make-composite-dispatch-gf-ht
                 (list (cons '(integer integer) multi-fn)))))
    (expect (cl-cc/vm::vm-resolve-gf-method gf-ht s 1 '(1 2)) :to-be multi-fn)))
