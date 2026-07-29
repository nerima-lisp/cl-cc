;;;; tests/unit/compile/codegen-io-read-tests.lisp — Codegen I/O Read/Print tests
;;;;
;;;; Continuation of codegen-io-tests.lisp.
;;;; Tests for read-char, read-line, read, read-byte, terpri, fresh-line,
;;;; print, prin1, princ, write-to-string, close, unread-char, listen.

(in-package :cl-cc/test)

;;; ── READ-CHAR / READ-LINE / READ ─────────────────────────────────────────

(it-sequential "phase2-stream-reader-simple read-char"
  (destructuring-bind (fn inst-type) (list 'read-char 'cl-cc/vm::vm-read-char)
    (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call fn (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-stream-reader-simple read-line"
  (destructuring-bind (fn inst-type) (list 'read-line 'cl-cc/vm::vm-read-line)
    (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call fn (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-stream-reader-simple read"
  (destructuring-bind (fn inst-type) (list 'read 'cl-cc/vm::vm-read-sexp-inst)
    (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call fn (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-stream-reader-eof-value read-char"
  (destructuring-bind (fn inst-type) (list 'read-char 'cl-cc/vm::vm-read-char)
    (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call fn (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-eq) :to-be-truthy))))

(it-sequential "phase2-stream-reader-eof-value read-line"
  (destructuring-bind (fn inst-type) (list 'read-line 'cl-cc/vm::vm-read-line)
    (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call fn (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-eq) :to-be-truthy))))

(it-sequential "phase2-stream-reader-eof-value read"
  (destructuring-bind (fn inst-type) (list 'read 'cl-cc/vm::vm-read-sexp-inst)
    (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call fn (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-eq) :to-be-truthy))))

(it-sequential "phase2-stream-reader-default-handle read-char"
  (destructuring-bind (fn inst-type) (list 'read-char 'cl-cc/vm::vm-read-char)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (let ((const-0 (find-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 0 (cl-cc/vm::vm-value i))))
                            (codegen-instructions ctx))))
      (expect const-0 :to-be-truthy)))))

(it-sequential "phase2-stream-reader-default-handle read-line"
  (destructuring-bind (fn inst-type) (list 'read-line 'cl-cc/vm::vm-read-line)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (let ((const-0 (find-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 0 (cl-cc/vm::vm-value i))))
                            (codegen-instructions ctx))))
      (expect const-0 :to-be-truthy)))))

(it-sequential "phase2-stream-reader-default-handle read"
  (destructuring-bind (fn inst-type) (list 'read 'cl-cc/vm::vm-read-sexp-inst)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy)
    (let ((const-0 (find-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 0 (cl-cc/vm::vm-value i))))
                            (codegen-instructions ctx))))
      (expect const-0 :to-be-truthy)))))

;;; ── READ-BYTE ─────────────────────────────────────────────────────────────

(it-sequential "phase2-read-byte-eof-value"
  (let ((ctx (make-ctx-with-vars 'handle 'eof-err 'eof-val)))
    (compile-ast (make-call 'read-byte (make-var 'handle) (make-var 'eof-err) (make-var 'eof-val)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-read-byte) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-eq) :to-be-truthy)))

;;; ── TERPRI / FRESH-LINE ──────────────────────────────────────────────────

(it-sequential "phase2-newline-handler-zero-arg terpri"
  (destructuring-bind (fn inst-type) (list 'terpri 'cl-cc/vm::vm-terpri-inst)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-newline-handler-zero-arg fresh-line"
  (destructuring-bind (fn inst-type) (list 'fresh-line 'cl-cc/vm::vm-fresh-line-inst)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-newline-handler-stream-arg terpri"
  (destructuring-bind (fn) (list 'terpri)
    (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-var 'stream)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-write-char) :to-be-truthy)
    (let ((nl-const (find-if (lambda (i)
                               (and (cl-cc::vm-const-p i)
                                    (eql #\Newline (cl-cc/vm::vm-value i))))
                             (codegen-instructions ctx))))
      (expect nl-const :to-be-truthy)))))

(it-sequential "phase2-newline-handler-stream-arg fresh-line"
  (destructuring-bind (fn) (list 'fresh-line)
    (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-var 'stream)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-write-char) :to-be-truthy)
    (let ((nl-const (find-if (lambda (i)
                               (and (cl-cc::vm-const-p i)
                                    (eql #\Newline (cl-cc/vm::vm-value i))))
                             (codegen-instructions ctx))))
      (expect nl-const :to-be-truthy)))))

;;; ── PRINT / PRIN1 / PRINC ────────────────────────────────────────────────

(it-sequential "phase2-print-single-arg print"
  (destructuring-bind (fn inst-type) (list 'print 'cl-cc/vm::vm-print-inst)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn (make-int 42)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-print-single-arg prin1"
  (destructuring-bind (fn inst-type) (list 'prin1 'cl-cc/vm::vm-prin1)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn (make-int 42)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-print-single-arg princ"
  (destructuring-bind (fn inst-type) (list 'princ 'cl-cc/vm::vm-princ)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn (make-int 42)) ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "phase2-print-with-stream print"
  (destructuring-bind (fn) (list 'print)
    (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-int 42) (make-var 'stream)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst) :to-be-truthy))))

(it-sequential "phase2-print-with-stream prin1"
  (destructuring-bind (fn) (list 'prin1)
    (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-int 42) (make-var 'stream)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst) :to-be-truthy))))

(it-sequential "phase2-print-with-stream princ"
  (destructuring-bind (fn) (list 'princ)
    (let ((ctx (make-ctx-with-vars 'stream)))
    (compile-ast (make-call fn (make-int 42) (make-var 'stream)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-stream-write-string-inst) :to-be-truthy))))

;;; ── WRITE-TO-STRING ──────────────────────────────────────────────────────

(it-sequential "phase2-write-to-string-with-keywords"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'write-to-string
                            (make-int 42)
                            (make-var :pretty)
                            (make-var 't))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-write-to-string-inst) :to-be-truthy)))

;;; ── CLOSE ────────────────────────────────────────────────────────────────

(it-sequential "phase2-close-emits-close-file"
  (let ((ctx (make-ctx-with-vars 'handle)))
    (compile-ast (make-call 'close (make-var 'handle)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-close-file) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-const) :to-be-truthy)))

;;; ── UNREAD-CHAR ──────────────────────────────────────────────────────────

(it-sequential "phase2-unread-char-one-arg"
  (let ((ctx (make-ctx-with-vars 'ch)))
    (compile-ast (make-call 'unread-char (make-var 'ch)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-unread-char) :to-be-truthy)))

;;; ── LISTEN ───────────────────────────────────────────────────────────────

(it-sequential "phase2-listen-zero-arg"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'listen) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-listen-inst) :to-be-truthy)))
