;;;; tests/unit/compile/codegen-io-tests.lisp — Codegen I/O output tests
;;;; Read/print tests → codegen-io-read-tests.lisp.

(in-package :cl-cc/test)

;;; ── WRITE-STRING ─────────────────────────────────────────────────────────

(it-sequential "phase2-write-string-arg-dispatch one-arg"
  (destructuring-bind (ctx form inst-type) (list (make-codegen-ctx) (make-call 'write-string (make-quoted "hello")) 'cl-cc/vm::vm-princ)
    (compile-ast form ctx) (expect (codegen-find-inst ctx inst-type) :to-be-truthy)))

(it-sequential "phase2-write-string-arg-dispatch two-args"
  (destructuring-bind (ctx form inst-type) (list (make-ctx-with-vars 'stream) (make-call 'write-string (make-quoted "hello") (make-var 'stream)) 'cl-cc/vm::vm-stream-write-string-inst)
    (compile-ast form ctx) (expect (codegen-find-inst ctx inst-type) :to-be-truthy)))

;;; ── FORMAT ────────────────────────────────────────────────────────────────

(it-sequential "phase2-format-destinations"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 42))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ-to-string-inst) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-princ)) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 't)
                            (make-quoted "~A")
                            (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ) :to-be-truthy))
  (let ((ctx (make-ctx-with-vars 'out-stream)))
    (compile-ast (make-call 'format
                            (make-var 'out-stream)
                            (make-quoted "hello")
                            (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst) :to-be-truthy)))

(it-sequential "phase2-format-static-string-lowering"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A ~S~~~%")
                            (make-quoted "x")
                            (make-quoted "y"))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-princ-to-string-inst) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-write-to-string-inst) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate) :to-be-truthy)))

(it-sequential "phase2-format-static-repeat-literal-lowering"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "a~2%~3~~2|z"))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate) :to-be-truthy)))

(it-sequential "phase2-format-static-empty-string-lowering"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~0%"))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-null)))

(it-sequential "phase2-format-static-fallbacks"
  (let ((ctx (make-ctx-with-vars 'fmt)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-var 'fmt)
                            (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~{~A~}")
                            (make-quoted '(1 2)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~A ~A")
                            (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format
                            (make-var 'nil)
                            (make-quoted "~2A")
                            (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst) :to-be-truthy)))

(it-sequential "phase2-format-requires-two-args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'format (make-var 'nil)) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-format-inst)) :to-be-truthy)))

;;; ── OPEN ──────────────────────────────────────────────────────────────────

(it-sequential "phase2-open-variants"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-open-file) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open (make-quoted "/tmp/f")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (expect (cl-cc::vm-open-file-direction inst) :to-be :input)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'open
                            (make-quoted "/tmp/f")
                            (make-var :direction)
                            (make-var :output))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-open-file)))
      (expect (cl-cc::vm-open-file-direction inst) :to-be :output))))

;;; ── PEEK-CHAR ────────────────────────────────────────────────────────────

(it-sequential "phase2-peek-char-arities"
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char) :to-be-truthy))
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'peek-char (make-var 'nil) (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-peek-char) :to-be-truthy)))

;;; ── MAKE-STRING-INPUT-STREAM ─────────────────────────────────────────────

(it-sequential "phase2-make-string-input-stream-compilation"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string-input-stream (make-quoted "hello")) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string-stream)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-string-stream-direction inst) :to-be :input))))

;;; ── CONCATENATE ──────────────────────────────────────────────────────────

(it-sequential "phase2-concatenate-variants"
  (let ((ctx (make-ctx-with-vars 'suffix)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'string)
                            (make-quoted "hello ")
                            (make-var 'suffix))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'concatenate
                            (make-quoted 'list)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)) :to-be-truthy))
  (let ((ctx (make-ctx-with-vars 'string)))
    (compile-ast (make-call 'concatenate
                            (make-var 'string)
                            (make-quoted "a")
                            (make-quoted "b"))
                 ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate)) :to-be-truthy)))
