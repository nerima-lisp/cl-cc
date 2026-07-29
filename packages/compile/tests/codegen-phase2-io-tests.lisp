;;;; tests/unit/compile/codegen-phase2-io-tests.lisp — Phase-2 I/O Handler Tests
;;;
;;; Continuation of codegen-phase2-tests.lisp.
;;; Tests for make-string-input-stream, open, peek-char, and write-string handlers.
;;;
;;; Helpers make-codegen-ctx / codegen-instructions / codegen-find-inst are
;;; defined in codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)

;;; ─── Section 9: MAKE-STRING-INPUT-STREAM ────────────────────────────────────

(it-sequential "codegen-phase2-make-string-input-stream-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'make-string-input-stream
                                :args (list (make-ast-quote :value "hello")))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string-stream)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-string-stream-direction inst) :to-be :input))))

;;; ─── Section 10: OPEN ───────────────────────────────────────────────────────

(it-sequential "codegen-phase2-open-direction default-input"
  (destructuring-bind (args expected-dir) (list (list (make-ast-quote :value "/tmp/in.txt")) :input)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-open-file-direction inst) :to-be expected-dir)))))

(it-sequential "codegen-phase2-open-direction explicit-output"
  (destructuring-bind (args expected-dir) (list (list (make-ast-quote :value "/tmp/out.txt")
                 (make-ast-var :name :direction)
                 (make-ast-var :name :output)) :output)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open :args args) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-open-file-direction inst) :to-be expected-dir)))))

(it-sequential "codegen-phase2-open-external-format"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'open
                                :args (list (make-ast-quote :value "/tmp/utf16.txt")
                                            (make-ast-var :name :external-format)
                                            (make-ast-var :name :utf-16)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-open-file-external-format inst) :to-be :utf-16)
      (expect (cl-cc/vm::vm-open-file-external-format-reg inst) :to-be-null))))

;;; ─── Section 11: PEEK-CHAR ──────────────────────────────────────────────────

(it-sequential "codegen-phase2-peek-char-emits-vm-peek-char one-arg"
  (destructuring-bind (reg args) (list :R10 (list (make-ast-var :name 'handle)))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'handle reg)))
    (compile-ast (make-ast-call :func 'peek-char :args args) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char) :to-be-truthy))))

(it-sequential "codegen-phase2-peek-char-emits-vm-peek-char two-args"
  (destructuring-bind (reg args) (list :R11 (list (make-ast-var :name nil)
                                  (make-ast-var :name 'handle)))
    (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'handle reg)))
    (compile-ast (make-ast-call :func 'peek-char :args args) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char) :to-be-truthy))))

;;; ─── Section 12: WRITE-STRING ───────────────────────────────────────────────

(it-sequential "codegen-phase2-write-string-arg-dispatch one-arg"
  (destructuring-bind (env args inst-type) (list nil (list (make-ast-quote :value "hello")) 'cl-cc/vm::vm-princ)
    (let ((ctx (make-codegen-ctx)))
    (when env
      (setf (cl-cc/compile:ctx-env ctx) env))
    (compile-ast (make-ast-call :func 'write-string :args args) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-phase2-write-string-arg-dispatch two-args"
  (destructuring-bind (env args inst-type) (list (list (cons 'out :R20)) (list (make-ast-quote :value "hello")
                 (make-ast-var :name 'out)) 'cl-cc/vm::vm-stream-write-string-inst)
    (let ((ctx (make-codegen-ctx)))
    (when env
      (setf (cl-cc/compile:ctx-env ctx) env))
    (compile-ast (make-ast-call :func 'write-string :args args) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))
