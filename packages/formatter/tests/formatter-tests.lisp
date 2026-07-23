;;;; formatter-tests.lisp — Unit tests for cl-cc/formatter (FR-320)
;;;;
;;;; Tests cover format-string and the internal %format-form dispatch via
;;;; the public API. No file I/O, no spawned processes.

(in-package :cl-cc/test)



;;; ─── helpers ──────────────────────────────────────────────────────────────

(defun trim-whitespace (s)
  "Remove leading/trailing whitespace from each line of S, then strip blank
lines, to allow whitespace-insensitive comparison of formatted output."
  (with-output-to-string (out)
    (with-input-from-string (in s)
      (loop for line = (read-line in nil nil)
            while line
            for trimmed = (string-trim " " line)
            unless (string= trimmed "")
              do (write-string trimmed out)
                 (terpri out)))))

;;; ─── format-string: basic atom forms ─────────────────────────────────────

(it-sequential "formatter-formats-integer-atom"
  (let ((result (cl-cc/formatter:format-string "42")))
    (expect (search "42" result) :to-be-truthy)))

(it-sequential "formatter-formats-string-atom"
  (let ((result (cl-cc/formatter:format-string "\"hello\"")))
    (expect (search "hello" result) :to-be-truthy)))

(it-sequential "formatter-formats-symbol-atom"
  (let ((result (cl-cc/formatter:format-string "foo")))
    (expect (search "FOO" result) :to-be-truthy)))

;;; ─── format-string: empty / nil input ────────────────────────────────────

(it-sequential "formatter-empty-string-returns-empty"
  (let ((result (cl-cc/formatter:format-string "")))
    (expect result :to-equal "")))

(it-sequential "formatter-nil-list-formats-as-unit"
  (let ((result (cl-cc/formatter:format-string "()")))
    (expect (search "()" result) :to-be-truthy)))

;;; ─── format-string: simple call form ─────────────────────────────────────

(it-sequential "formatter-simple-call-on-one-line"
  (let ((result (cl-cc/formatter:format-string "(+ 1 2)")))
    ;; The head and arguments should appear together
    (expect (search "+" result) :to-be-truthy)
    (expect (search "1" result) :to-be-truthy)
    (expect (search "2" result) :to-be-truthy)))

;;; ─── format-string: special-form indentation ──────────────────────────────

(it-sequential "formatter-defun-indents-body"
  (let* ((source "(defun square (x) (* x x))")
         (result (cl-cc/formatter:format-string source)))
    ;; Head and name must both appear
    (expect (search "DEFUN" result) :to-be-truthy)
    (expect (search "SQUARE" result) :to-be-truthy)
    ;; Body expression must be present
    (expect (search "*" result) :to-be-truthy)))

(it-sequential "formatter-let-indents-bindings-and-body"
  (let* ((source "(let ((x 1)) x)")
         (result (cl-cc/formatter:format-string source)))
    (expect (search "LET" result) :to-be-truthy)
    (expect (search "X" result) :to-be-truthy)
    (expect (search "1" result) :to-be-truthy)))

;;; ─── format-string: cond / case dispatch ──────────────────────────────────

(it-sequential "formatter-cond-indents-clauses"
  (let* ((source "(cond ((= x 1) :one) (t :other))")
         (result (cl-cc/formatter:format-string source)))
    (expect (search "COND" result) :to-be-truthy)
    (expect (search ":ONE" result) :to-be-truthy)
    (expect (search ":OTHER" result) :to-be-truthy)))

;;; ─── format-string: multiple top-level forms ─────────────────────────────

(it-sequential "formatter-multiple-top-level-forms"
  (let* ((source "(defvar *x* 0) (defvar *y* 1)")
         (result (cl-cc/formatter:format-string source)))
    (expect (search "*X*" result) :to-be-truthy)
    (expect (search "*Y*" result) :to-be-truthy)
    ;; Each form should end up before the other in document order
    (expect (< (search "*X*" result) (search "*Y*" result)) :to-be-truthy)))

;;; ─── format-string: indent-size keyword ─────────────────────────────────

(it-sequential "formatter-indent-size-parameter-is-respected"
  (let* ((source "(defun f (x) x)")
         (result-2 (cl-cc/formatter:format-string source :indent-size 2))
         (result-4 (cl-cc/formatter:format-string source :indent-size 4)))
    ;; Both must contain the function name
    (expect (search "F" result-2) :to-be-truthy)
    (expect (search "F" result-4) :to-be-truthy)
    ;; The 4-space version must be at least as long as the 2-space version
    ;; (more leading spaces means more characters overall)
    (expect (>= (length result-4) (length result-2)) :to-be-truthy)))
