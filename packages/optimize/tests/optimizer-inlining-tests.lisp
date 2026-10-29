;;;; optimizer-inlining-tests.lisp — Inlining pass unit tests
(in-package :cl-cc/test)


;;; ── Inlining Pass: Unit Tests ──────────────────────────────────────────────

(defun inline-has-call-p (instructions)
  "T if INSTRUCTIONS contains at least one vm-call."
  (some #'cl-cc:vm-call-p instructions))

