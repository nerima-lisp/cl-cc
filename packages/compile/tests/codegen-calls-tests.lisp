;;;; tests/unit/compile/codegen-calls-tests.lisp
;;;; Unit tests for src/compile/codegen-calls.lisp
;;;;
;;;; Covers: %try-compile-funcall, %try-compile-apply,
;;;;   %try-compile-noescape-cons, %try-compile-noescape-array (direct),
;;;;   %compile-normal-call (GF path + normal path),
;;;;   and compile-ast(ast-call) dispatch — tail call and GF paths.

(in-package :cl-cc/test)

;;; ─── helper functions extracted for readability ────────────────────────────

(it-sequential "compile-call-arg-registers-preserves-argument-order"
  (let* ((ctx (make-codegen-ctx))
         (regs (cl-cc/compile::%compile-call-arg-registers
                (list (make-ast-int :value 10)
                      (make-ast-int :value 20))
                ctx))
         (instructions (codegen-instructions ctx)))
    (expect (= 2 (length regs)) :to-be-truthy)
    (expect (= 2 (length instructions)) :to-be-truthy)
    (let ((first (first instructions))
          (second (second instructions)))
      (expect (typep first 'cl-cc/vm::vm-const) :to-be-truthy)
      (expect (typep second 'cl-cc/vm::vm-const) :to-be-truthy)
      (expect (= 10 (cl-cc::vm-const-value first)) :to-be-truthy)
      (expect (= 20 (cl-cc::vm-const-value second)) :to-be-truthy)
      (expect (first regs) :to-be (cl-cc/vm::vm-dst first))
      (expect (second regs) :to-be (cl-cc/vm::vm-dst second)))))

(it-sequential "emit-call-like-instruction-selects-call-variant"
  (let* ((ctx (make-codegen-ctx))
         (func-reg (cl-cc/compile:make-register ctx))
         (arg-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (expect (cl-cc/compile::%emit-call-like-instruction nil result-reg func-reg (list arg-reg) ctx) :to-be result-reg)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-null))
  (let* ((ctx (make-codegen-ctx))
         (func-reg (cl-cc/compile:make-register ctx))
         (arg-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (expect (cl-cc/compile::%emit-call-like-instruction t result-reg func-reg (list arg-reg) ctx) :to-be result-reg)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))

;;; ─── %try-compile-funcall ─────────────────────────────────────────────────

(it-sequential "try-compile-funcall-returns-nil-for-non-funcall-sym apply"
  (destructuring-bind (sym) (list 'apply)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-funcall-returns-nil-for-non-funcall-sym car"
  (destructuring-bind (sym) (list 'car)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-funcall-returns-nil-for-non-funcall-sym nil"
  (destructuring-bind (sym) (list nil)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-funcall-nil-args-returns-nil"
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall 'funcall nil result-reg nil ctx)))
    (expect ret :to-be-null)))

(it-sequential "try-compile-funcall-success-emits-vm-call-and-returns-result-reg"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-funcall
                'funcall
                (list (make-ast-var :name 'fn) (make-ast-int :value 1))
                result-reg nil ctx)))
      (expect ret :to-be result-reg)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy))))

(it-sequential "try-compile-funcall-tail-position-emits-tail-call"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (cl-cc/compile::%try-compile-funcall
     'funcall
     (list (make-ast-var :name 'fn) (make-ast-int :value 99))
     result-reg t ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))

;;; ─── %try-compile-apply ──────────────────────────────────────────────────

(it-sequential "try-compile-apply-returns-nil-for-non-apply-sym funcall"
  (destructuring-bind (sym) (list 'funcall)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-apply sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-apply-returns-nil-for-non-apply-sym list"
  (destructuring-bind (sym) (list 'list)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-apply sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-apply-returns-nil-for-non-apply-sym nil"
  (destructuring-bind (sym) (list nil)
    (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-apply sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (expect ret :to-be-null))))

(it-sequential "try-compile-apply-emits-vm-apply-and-returns-result-reg"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'f fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                 'apply
                 (list (make-ast-var :name 'f) (make-ast-int :value 1))
                 result-reg nil ctx)))
      (expect ret :to-be result-reg)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy))))

(it-sequential "try-compile-apply-quoted-function-symbol-uses-function-designator-helper"
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                'apply
                (list (make-ast-quote :value 'f) (make-ast-int :value 1))
                result-reg nil ctx)))
      (expect ret :to-be result-reg)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy))))

(it-sequential "try-compile-apply-list-call-spread-emits-vm-call"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'f fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                'apply
                (list (make-ast-var :name 'f)
                      (make-ast-int :value 1)
                      (make-ast-call :func (make-ast-var :name 'list)
                                     :args (list (make-ast-int :value 2)
                                                 (make-ast-int :value 3))))
                result-reg nil ctx)))
      (expect ret :to-be result-reg)
      (let ((call-inst (codegen-find-inst ctx 'cl-cc/vm::vm-call)))
        (expect call-inst :to-be-truthy)
        (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-null)
        (expect (= 3 (length (cl-cc/vm::vm-args call-inst))) :to-be-truthy)))))

(it-sequential "try-compile-apply-list-call-spread-tail-emits-vm-tail-call"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'f fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                'apply
                (list (make-ast-var :name 'f)
                      (make-ast-int :value 1)
                      (make-ast-call :func (make-ast-var :name 'list)
                                     :args (list (make-ast-int :value 2)
                                                 (make-ast-int :value 3))))
                result-reg t ctx)))
      (expect ret :to-be result-reg)
      (let ((tail-call-inst (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call)))
        (expect tail-call-inst :to-be-truthy)
        (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)
        (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-null)
        (expect (= 3 (length (cl-cc/vm::vm-args tail-call-inst))) :to-be-truthy)))))

(it-sequential "try-compile-apply-local-list-call-spread-keeps-vm-apply"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx))
         (list-reg (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'f fn-reg) (cons 'list list-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                'apply
                (list (make-ast-var :name 'f)
                      (make-ast-call :func (make-ast-var :name 'list)
                                     :args (list (make-ast-int :value 2)
                                                 (make-ast-int :value 3))))
                result-reg nil ctx)))
      (expect ret :to-be result-reg)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-apply) :to-be-truthy))))

(it-sequential "try-compile-apply-tail-position-marks-vm-apply-tail-p"
  (let* ((ctx (make-codegen-ctx))
          (fn-reg (cl-cc/compile:make-register ctx))
          (xs-reg (cl-cc/compile:make-register ctx))
          (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'f fn-reg) (cons 'xs xs-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                 'apply
                 (list (make-ast-var :name 'f) (make-ast-var :name 'xs))
                result-reg t ctx)))
      (expect ret :to-be result-reg)
      (let ((apply-inst (codegen-find-inst ctx 'cl-cc/vm::vm-apply)))
        (expect apply-inst :to-be-truthy)
        (expect (cl-cc/vm::vm-tail-p apply-inst) :to-be-truthy)))))

;;; ─── %try-compile-noescape-cons ──────────────────────────────────────────

(it-sequential "try-compile-noescape-cons-car-emits-vm-move-from-car-reg"
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile:make-register ctx))
         (cdr-reg    (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-cons
                'car
                (list (make-ast-var :name 'p))
                result-reg ctx)))
      (expect ret :to-be result-reg)
      (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
        (expect move :to-be-truthy)
        (expect (cl-cc/vm::vm-src move) :to-be car-reg)))))

(it-sequential "try-compile-noescape-cons-cdr-emits-vm-move-from-cdr-reg"
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile:make-register ctx))
         (cdr-reg    (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (cl-cc/compile::%try-compile-noescape-cons
     'cdr (list (make-ast-var :name 'p)) result-reg ctx)
    (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
      (expect move :to-be-truthy)
      (expect (cl-cc/vm::vm-src move) :to-be cdr-reg))))

(it-sequential "try-compile-noescape-cons-unregistered-binding-returns-nil"
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx))
         (ret (cl-cc/compile::%try-compile-noescape-cons
               'car (list (make-ast-var :name 'unregistered)) result-reg ctx)))
    (expect ret :to-be-null)))

(it-sequential "try-compile-noescape-cons-non-car-cdr-sym-returns-nil"
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile:make-register ctx))
         (cdr-reg    (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-cons
                'first (list (make-ast-var :name 'p)) result-reg ctx)))
      (expect ret :to-be-null))))

;;; ─── %try-compile-noescape-array ─────────────────────────────────────────

(it-sequential "try-compile-noescape-array-length-emits-vm-const-with-size"
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc/compile:make-register ctx))
         (r1 (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 2 nil r0 r1))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-array
                'array-length
                (list (make-ast-var :name 'arr))
                result-reg ctx)))
      (expect ret :to-be result-reg)
      (let ((const (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
        (expect const :to-be-truthy)
        (expect (= 2 (cl-cc::vm-const-value const)) :to-be-truthy)))))

(it-sequential "try-compile-noescape-array-aref-emits-vm-move-to-element-reg"
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc/compile:make-register ctx))
         (r1 (cl-cc/compile:make-register ctx))
         (r2 (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 3 nil r0 r1 r2))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-array
                'aref
                (list (make-ast-var :name 'arr) (make-ast-int :value 1))
                result-reg ctx)))
      (expect ret :to-be result-reg)
      (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
        (expect move :to-be-truthy)
        (expect (cl-cc/vm::vm-src move) :to-be r1)))))

;;; ─── %compile-normal-call ─────────────────────────────────────────────────

(it-sequential "compile-normal-call-gf-emits-generic-call-not-call"
  (let* ((ctx (make-codegen-ctx))
         (gf-reg     (cl-cc/compile:make-register ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (gethash 'my-gf (cl-cc/compile:ctx-global-generics ctx)) gf-reg)
    (cl-cc/compile::%compile-normal-call
     'my-gf 'my-gf (list (make-ast-int :value 1)) result-reg nil ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))

(it-sequential "compile-normal-call-ordinary-fn-emits-vm-call"
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-normal-call
     'ordinary-fn 'ordinary-fn (list (make-ast-int :value 1)) result-reg nil ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)))

;;; ─── compile-ast(ast-call) — dispatch integration ────────────────────────

(it-sequential "codegen-call-dispatch-funcall-tail-emits-tail-call"
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (setf (cl-cc/compile:ctx-tail-position ctx) t)
    (compile-ast (make-ast-call :func 'funcall
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))


(it-sequential "codegen-call-dispatch-gf-emits-generic-call"
  (let* ((ctx (make-codegen-ctx))
         (gf-reg (cl-cc/compile:make-register ctx)))
    (setf (gethash 'my-speak (cl-cc/compile:ctx-global-generics ctx)) gf-reg)
    (compile-ast (make-ast-call :func 'my-speak
                                :args (list (make-ast-int :value 1)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call) :to-be-truthy)))

(it-sequential "codegen-call-ast-function-float-add-emits-vm-float-add"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func (make-ast-function :name '+)
                                :args (list (make-ast-quote :value 1.0)
                                            (make-ast-quote :value 2.0)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-float-add) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))
(it-sequential "codegen-call-quoted-function-designator-float-add-emits-vm-float-add"
  (let* ((ctx (make-codegen-ctx))
         (floats (list (make-ast-quote :value 1.0)
                       (make-ast-quote :value 2.0))))
    (compile-ast (make-ast-call :func (make-ast-quote :value '+)
                                :args floats)
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-float-add) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-null)))
