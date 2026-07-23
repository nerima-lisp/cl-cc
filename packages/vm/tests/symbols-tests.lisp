;;;; tests/unit/vm/symbols-tests.lisp — VM Symbol Instruction Tests

(in-package :cl-cc/test)

;;; ─── Symbol Operations ───────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for symbol tests."
  (make-instance 'cl-cc/vm::vm-io-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

(it-sequential "sym-symbol-name"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 'hello)
    (str-exec (cl-cc:make-vm-symbol-name :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "HELLO")))

(it-sequential "sym-make-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "FOO")
    (str-exec (cl-cc:make-vm-make-symbol :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "FOO"))))

(it-sequential "sym-intern-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "INTERN-TEST-SYM-12345")
    (str-exec (cl-cc:make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "INTERN-TEST-SYM-12345"))))

(it-sequential "sym-find-package"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 :cl-user)
    (str-exec (cl-cc:make-vm-find-package :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy)))

(it-sequential "sym-find-symbol"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "CAR")
    (cl-cc/vm::vm-reg-set s :R2 :cl)
    (str-exec (cl-cc:make-vm-find-symbol :dst :R0 :src :R1 :pkg :R2) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "CAR")
      (expect (second (cl-cc/vm::vm-values-list s)) :to-equal :external))))

(it-sequential "sym-gensym"
  (let ((s (str-vm)))
    (str-exec (cl-cc:make-vm-gensym-inst :dst :R0) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-package result) :to-equal nil))))

(it-sequential "sym-keywordp keyword"
  (destructuring-bind (value expected) (list :test 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-keywordp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "sym-keywordp symbol"
  (destructuring-bind (value expected) (list 'hello 0)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-keywordp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

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

(it-sequential "sym-defpackage-local-nicknames-expands"
  (sb-thread:with-mutex (*str-package-lock*)
    (str-delete-package-if-exists :fr275-defpackage-local-user)
    (str-delete-package-if-exists :fr275-defpackage-local-target)
    (unwind-protect
         (progn
           (expect (macroexpand-1
             '(defpackage #:fr275-defpackage-local-user
                (:use #:cl)
                (:local-nicknames (#:ln #:fr275-defpackage-local-target)))) :to-be-truthy)
           (eval '(defpackage #:fr275-defpackage-local-target (:use #:cl)))
           (eval '(defpackage #:fr275-defpackage-local-user
                   (:use #:cl)
                   (:local-nicknames (#:ln #:fr275-defpackage-local-target))))
           (let ((*package* (find-package :fr275-defpackage-local-user)))
             (expect (find-package :ln) :to-be (find-package :fr275-defpackage-local-target))))
      (str-delete-package-if-exists :fr275-defpackage-local-user)
      (str-delete-package-if-exists :fr275-defpackage-local-target))))

(it-sequential "sym-vm-add-package-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-ADD-TARGET"
   "FR275-VM-ADD-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (let ((*package* user))
         (expect (find-package "LN") :to-be target))))))

(it-sequential "sym-vm-remove-package-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-REMOVE-TARGET"
   "FR275-VM-REMOVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME"))
           (remove-ctor (str-vm-constructor "MAKE-VM-REMOVE-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (str-exec (funcall remove-ctor :dst :R0 :pkg :R3 :nick :R1 :target nil) s)
       (let ((*package* user))
         (expect (find-package "LN") :to-be-null))))))

(it-sequential "sym-vm-find-package-uses-local-nickname"
  (str-with-local-nickname-packages
   "FR275-VM-RESOLVE-TARGET"
   "FR275-VM-RESOLVE-USER"
   (lambda (target user)
     (let ((s (str-vm))
           (add-ctor (str-vm-constructor "MAKE-VM-ADD-PACKAGE-LOCAL-NICKNAME")))
       (cl-cc/vm::vm-reg-set s :R1 "LN")
       (cl-cc/vm::vm-reg-set s :R2 target)
       (cl-cc/vm::vm-reg-set s :R3 user)
       (str-exec (funcall add-ctor :dst :R0 :pkg :R3 :nick :R1 :target :R2) s)
       (setf (gethash '*package* (cl-cc/vm::vm-global-vars s)) user)
       (let ((*package* user))
         (str-exec (cl-cc:make-vm-find-package :dst :R0 :src :R1) s))
        (expect (cl-cc/vm::vm-reg-get s :R0) :to-be target)))))

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

(it-sequential "sym-lock-package"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-a)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-a :use nil)))
           (expect (cl-cc/vm::package-locked-p pkg) :to-be-null)
           (cl-cc/vm::lock-package pkg)
           (expect (cl-cc/vm::package-locked-p pkg) :to-be-truthy)
           (cl-cc/vm::unlock-package pkg)
           (expect (cl-cc/vm::package-locked-p pkg) :to-be-null))
      (%str-delete-package-if-exists :fr896-lock-test-a))))

(it-sequential "sym-package-locked-error-on-intern"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-b)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-b :use nil)))
           (cl-cc/vm::lock-package pkg)
           (signals cl-cc/vm::package-locked-error (intern "LOCKED-SYMBOL" pkg))
           ;; Package should remain locked
           (expect (cl-cc/vm::package-locked-p pkg) :to-be-truthy))
      (%str-delete-package-if-exists :fr896-lock-test-b))))

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

(it-sequential "sym-default-locked-packages"
  (expect (find-package :cl) :to-be-truthy)
  (expect (cl-cc/vm::package-locked-p (find-package :cl)) :to-be-truthy))

(it-sequential "sym-check-package-lock-signals"
  (sb-thread:with-mutex (*str-package-lock*)
    (%str-delete-package-if-exists :fr896-lock-test-d)
    (unwind-protect
         (let ((pkg (make-package :fr896-lock-test-d :use nil)))
           (cl-cc/vm::lock-package pkg)
           (signals cl-cc/vm::package-locked-error (cl-cc/vm::check-package-lock pkg :intern))
            (cl-cc/vm::unlock-package pkg)
            ;; Unlocked should not signal
            (expect (null (cl-cc/vm::check-package-lock pkg :intern)) :to-be-truthy))
       (%str-delete-package-if-exists :fr896-lock-test-d))))

;;; ─── Self-Host Mode (FR-626) ──────────────────────────────────────────────

(it-sequential "sym-self-host-intern-works-with-runtime-registry"
  (let ((cl-cc/vm::*vm-self-host-mode* t)
        (s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "SELF-HOST-INTERN-SYM")
    (str-exec (cl-cc:make-vm-intern-symbol :dst :R0 :src :R1 :pkg nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (symbolp result) :to-be-truthy)
      (expect (symbol-name result) :to-equal "SELF-HOST-INTERN-SYM"))))

(it-sequential "sym-self-host-find-package-works-with-runtime-registry"
  (let ((cl-cc/vm::*vm-self-host-mode* t)
        (s (str-vm)))
    ;; Register a package in the runtime registry
    (cl-cc/runtime:rt-make-package :self-host-test-pkg)
    (cl-cc/vm::vm-reg-set s :R1 :self-host-test-pkg)
    (str-exec (cl-cc:make-vm-find-package :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy)))
