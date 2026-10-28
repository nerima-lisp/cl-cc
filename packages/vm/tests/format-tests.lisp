;;;; tests/unit/vm/format-tests.lisp — VM formatted output and reader tests

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun fmt-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (make-instance 'cl-cc/vm:vm-io-state :output-stream out))

(defun fmt-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm:execute-instruction inst state 0 (make-hash-table :test #'equal)))

(defun fmt-capture (state)
  "Get captured output from the vm-state's output stream."
  (get-output-stream-string (cl-cc/vm:vm-output-stream state)))

;;; ─── write-to-string / princ-to-string ────────────────────────────────────

;;; ─── princ / prin1 / print / terpri / fresh-line ─────────────────────────

;;; ─── format ───────────────────────────────────────────────────────────────

;;; ─── String Output Stream ────────────────────────────────────────────────

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ──────────────────

;;; ─── Wave 2: ~_ directive test (partial ANSI, conditional newline) ────────

;;; ─── ~F / ~E / ~$ floating-point directive parameters ──────────────────────

;;; ─── ~( ~) case conversion (ANSI) ─────────────────────────────────────────

;;; ─── ~T tabulation after rendered directives ──────────────────────────────

;;; ─── ~{ ~} iteration with ~^ separator (ANSI) ──────────────────────────────
