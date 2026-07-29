;;;; tests/unit/compile/phase2-handler-tests.lisp
;;;;
;;;; Unit tests for the *phase2-builtin-handlers* dispatch table in codegen.lisp.
;;;;
;;;; Each of the 17 registered Phase 2 handlers gets dedicated coverage:
;;;;   - the correct VM instruction is emitted
;;;;   - structural properties are verified (optional args, keyword parsing)
;;;;   - guard conditions that trigger fallthrough return nil
;;;;
;;;; Helpers: reuses make-codegen-ctx / codegen-instructions / codegen-find-inst
;;;; from codegen-tests.lisp (same suite, loaded before this file).

(in-package :cl-cc/test)



;;; ── MAKE-HASH-TABLE ───────────────────────────────────────────────────────

(it-sequential "phase2-make-hash-table-variants no-test"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-hash-table) ctx)
               (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table) :to-be-truthy))))
    (funcall scenario)))

(it-sequential "phase2-make-hash-table-variants quoted-test"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted 'equal))
                            ctx)
               (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
                 (expect inst :to-be-truthy)
                 ;; test-reg should be non-nil — a register was allocated for the test sym
                 (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))
    (funcall scenario)))

(it-sequential "phase2-make-hash-table-variants var-test"
  (destructuring-bind (scenario) (list (lambda ()
              (let ((ctx (make-codegen-ctx)))
                (compile-ast (make-ast-let
                              :bindings (list (cons 'equal (make-quoted 'equal)))
                              :body (list (make-call 'make-hash-table
                                                     (make-var :test)
                                                     (make-var 'equal))))
                             ctx)
                (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
                  (expect inst :to-be-truthy)
                  (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))
    (funcall scenario)))

(it-sequential "phase2-make-hash-table-variants function-test"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-fn 'equal))
                            ctx)
               (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table) :to-be-truthy))))
    (funcall scenario)))

;;; ── GETHASH ───────────────────────────────────────────────────────────────

(it-sequential "phase2-gethash-arities"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash (make-quoted 'k) (make-quoted 'ht)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-gethash)))
      (expect inst :to-be-truthy)
      (expect (null (cl-cc::vm-gethash-default inst)) :to-be-truthy)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'k) (make-quoted 'ht) (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-gethash)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-gethash-default inst) :to-be-truthy))))

;;; ── MAPHASH ───────────────────────────────────────────────────────────────

(it-sequential "phase2-maphash-codegen"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash (make-quoted 'fn) (make-quoted 'ht)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-hash-table-keys) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    ;; Last emitted const before halt should be nil (the return value)
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                  (codegen-instructions ctx))))
      (expect (some (lambda (i) (null (cl-cc::vm-const-value i))) consts) :to-be-truthy))))

;;; ── MAKE-ARRAY / MAKE-ADJUSTABLE-VECTOR ──────────────────────────────────

(it-sequential "phase2-make-array-variants fixed-array-emits-inst"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-array (make-int 10)) ctx)
               (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy))))
    (funcall scenario)))

(it-sequential "phase2-make-array-variants fixed-array-not-adjustable"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-array (make-int 10)) ctx)
               (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
                 (expect (null (cl-cc::vm-make-array-fill-pointer inst)) :to-be-truthy)
                 (expect (null (cl-cc::vm-make-array-adjustable inst)) :to-be-truthy)))))
    (funcall scenario)))

(it-sequential "phase2-make-array-variants adjustable-vector-emits-inst"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
               (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-array) :to-be-truthy))))
    (funcall scenario)))

(it-sequential "phase2-make-array-variants adjustable-vector-is-adjustable"
  (destructuring-bind (scenario) (list (lambda ()
             (let ((ctx (make-codegen-ctx)))
               (compile-ast (make-call 'make-adjustable-vector (make-int 10)) ctx)
               (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-array)))
                 (expect (cl-cc::vm-make-array-fill-pointer inst) :to-be-truthy)
                 (expect (cl-cc::vm-make-array-adjustable inst) :to-be-truthy)))))
    (funcall scenario)))

;;; ── ARRAY-ROW-MAJOR-INDEX ────────────────────────────────────────────────

(it-sequential "phase2-array-row-major-index"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index (make-int 10) (make-int 0)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-array-row-major-index) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index
                            (make-int 10) (make-int 0) (make-int 1))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-array-row-major-index) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'array-row-major-index
                            (make-int 10) (make-int 2) (make-int 3))
                 ctx)
    ;; Subscripts are accumulated via vm-cons cells
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)))

;;; ── ENCODE-UNIVERSAL-TIME ────────────────────────────────────────────────

(it-sequential "phase2-encode-universal-time-six-args"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'encode-universal-time
                            (make-int 0) (make-int 0) (make-int 12)
                            (make-int 1) (make-int 1) (make-int 2024))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-encode-universal-time) :to-be-truthy)))

(it-sequential "phase2-encode-universal-time-wrong-arity-falls-through"
  (let ((ctx (make-codegen-ctx)))
    ;; Only 3 args — handler guard fails, falls through to normal call
    (compile-ast (make-call 'encode-universal-time
                            (make-int 0) (make-int 0) (make-int 12))
                 ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-encode-universal-time)) :to-be-truthy)))

;;; ── MAKE-STRING ──────────────────────────────────────────────────────────

(it-sequential "phase2-make-string-variants"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-string) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string (make-int 5)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string)))
      (expect (null (cl-cc::vm-make-string-char inst)) :to-be-truthy)))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-string
                            (make-int 5)
                            (make-var :initial-element)
                            (make-quoted #\x))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-string)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-string-char inst) :to-be-truthy))))

;;; ── TYPEP ─────────────────────────────────────────────────────────────────

(it-sequential "phase2-typep-variants"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'integer)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'typep (make-int 42) (make-quoted 'string)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-typep)))
      (expect (cl-cc::vm-typep-type-name inst) :to-be 'string)))
  (let ((ctx (make-ctx-with-vars 'integer)))
    ;; Pass unquoted ast-var — handler guard fails
    (compile-ast (make-call 'typep (make-int 42) (make-var 'integer)) ctx)
    (expect (null (codegen-find-inst ctx 'cl-cc/vm::vm-typep)) :to-be-truthy)))

;;; ── CALL-NEXT-METHOD ─────────────────────────────────────────────────────

;;; ── I/O AND STRING HANDLERS MOVED TO codegen-io-tests ───────────────────────

;;; ── Handler table completeness ────────────────────────────────────────────

(it-sequential "phase2-all-handlers-registered"
  (let ((expected '("MAKE-HASH-TABLE" "GETHASH" "MAPHASH"
                    "MAKE-ARRAY" "MAKE-ADJUSTABLE-VECTOR"
                    "ARRAY-ROW-MAJOR-INDEX" "ENCODE-UNIVERSAL-TIME"
                    "MAKE-STRING" "TYPEP"
                    "SLOT-BOUNDP" "SLOT-EXISTS-P" "SLOT-MAKUNBOUND"
                    "CALL-NEXT-METHOD" "WRITE-STRING"
                    "FORMAT" "OPEN" "PEEK-CHAR"
                    "MAKE-STRING-INPUT-STREAM" "CONCATENATE")))
    (dolist (name expected)
      (expect (gethash name cl-cc/compile::*phase2-builtin-handlers*) :to-be-truthy))))
