;;;; tests/unit/vm/symbols-tests.lisp — VM Symbol Instruction Tests

(in-package :cl-cc/test)

;;; ─── Symbol Operations ───────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for symbol tests."
  (make-instance 'cl-cc/vm:vm-io-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm:execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── Package-Local Nicknames (FR-275) ─────────────────────────────────────

(defun str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists."
  (let ((package (find-package designator)))
    (when package
      (delete-package package))))

(defun str-vm-constructor (name)
  "Return exported VM constructor NAME from CL-CC, or skip until FR-275 exists."
  (multiple-value-bind (symbol status)
      (find-symbol name :cl-cc)
    (unless (and symbol (eq status :external) (fboundp symbol))
      (skip (format nil "FR-275 constructor ~A is not available yet" name)))
    (symbol-function symbol)))

(defvar *str-package-lock*
  (sb-thread:make-mutex :name "cl-cc symbol test package lock"))

(defun str-with-local-nickname-packages (target-name user-name thunk)
  "Run THUNK with fresh TARGET-NAME and USER-NAME packages, then clean up."
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists user-name)
    (str-delete-package-if-exists target-name)
    (let ((target (make-package target-name :use nil))
          (user (make-package user-name :use nil)))
      (unwind-protect
           (funcall thunk target user)
        (str-delete-package-if-exists user-name)
        (str-delete-package-if-exists target-name)))))

;;; ─── FR-895: Symbol Table Compaction ─────────────────────────────────────────

(it-sequential "sym-symbol-table-freeze-thaw"
  (let ((test-sym (gensym "FR895-TEST-")))
    (setf (cl-cc/vm::lookup-symbol (symbol-name test-sym)) test-sym)
    (expect (cl-cc/vm::lookup-symbol (symbol-name test-sym)) :to-be test-sym)
    ;; Freeze — table becomes read-only
    (cl-cc/vm::freeze-symbol-table)
    (expect cl-cc/vm::*symbol-table-frozen* :to-be-truthy)
    (expect (vectorp cl-cc/vm::*symbol-table-compact*) :to-be-truthy)
    (expect (cl-cc/vm::lookup-symbol (symbol-name test-sym)) :to-be test-sym)
    ;; Frozen — adding new symbol should error
    (let ((new-sym (gensym "FR895-FROZEN-")))
      (signals error (setf (cl-cc/vm::lookup-symbol (symbol-name new-sym)) new-sym)))
    ;; Thaw — back to dynamic
    (cl-cc/vm::thaw-symbol-table)
    (expect cl-cc/vm::*symbol-table-frozen* :to-be-null)
    (expect cl-cc/vm::*symbol-table-compact* :to-be-null)
    (let ((new-sym (gensym "FR895-THAWED-")))
      (setf (cl-cc/vm::lookup-symbol (symbol-name new-sym)) new-sym)
      (expect (cl-cc/vm::lookup-symbol (symbol-name new-sym)) :to-be new-sym))))

(it-sequential "sym-symbol-index"
  (let ((a (gensym "FR895-IDX-A-"))
        (b (gensym "FR895-IDX-B-"))
        (c (gensym "FR895-IDX-C-")))
    (let ((ia (cl-cc/vm::symbol-index a))
          (ib (cl-cc/vm::symbol-index b))
          (ic (cl-cc/vm::symbol-index c)))
      (expect (integerp ia) :to-be-truthy)
      (expect (integerp ib) :to-be-truthy)
      (expect (integerp ic) :to-be-truthy)
      ;; Each gets a unique index
      (expect (= ia ib) :to-be-falsy)
      (expect (= ib ic) :to-be-falsy)
      ;; Same symbol returns same index
      (expect (cl-cc/vm::symbol-index a) :to-equal ia))))

(it-sequential "sym-register-weak-symbol"
  (let ((test-sym (gensym "FR895-WEAK-")))
    (cl-cc/vm::register-weak-symbol test-sym)
    (expect (gethash (symbol-name test-sym) cl-cc/vm::*symbol-table-weak*) :to-be test-sym)))

;;; ─── FR-896: Package Lock / Sealed ──────────────────────────────────────────

(defun %str-delete-package-if-exists (designator)
  "Delete DESIGNATOR's package when it exists, unlocking first if necessary."
  (let ((pkg (find-package designator)))
    (when pkg
      (cl-cc/vm::unlock-package pkg)
      (delete-package pkg))))

(it-sequential "sym-with-unlocked-packages"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-c)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-c :use nil)))
           (cl-cc/vm::lock-package pkg)
           ;; Without unlock, intern should error
           (signals cl-cc/vm::package-locked-error (intern "SHOULD-FAIL" pkg))
           ;; With unlock, intern should succeed
           (let ((result
                   (cl-cc/vm::with-unlocked-packages (:fr896-lock-test-c)
                     (intern "SHOULD-SUCCEED" pkg))))
             (expect (symbolp result) :to-be-truthy)
             (expect (symbol-name result) :to-equal "SHOULD-SUCCEED"))
           ;; After unlock block, package should be re-locked
           (expect (cl-cc/vm::package-locked-p pkg) :to-be-truthy)
           (signals cl-cc/vm::package-locked-error (intern "SHOULD-FAIL-AGAIN" pkg)))
      (%str-delete-package-if-exists :fr896-lock-test-c))))

;;; ─── Self-Host Mode (FR-626) ──────────────────────────────────────────────

