;;;; optimizer-pipeline-roadmap-backend.lisp — Backend roadmap evidence profile and lookup
;;;; Extracted from optimizer-pipeline-roadmap.lisp.
;;;; Load order: after optimizer-pipeline-roadmap data slices.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Entries checked in order; NIL hi means open-ended (>= lo).
  (append +opt-backend-roadmap-foundation-evidence-profile-ranges+
          +opt-backend-roadmap-baseline-evidence-profile-ranges+
          +opt-backend-roadmap-memory-numeric-evidence-profile-ranges+
          +opt-backend-roadmap-lowering-evidence-profile-ranges+
          +opt-backend-roadmap-codegen-evidence-profile-ranges+
          +opt-backend-roadmap-speculative-evidence-profile-ranges+)
  "Alist of (lo hi modules api-symbols test-anchors) range entries for
`%opt-backend-roadmap-evidence-profile'.")
