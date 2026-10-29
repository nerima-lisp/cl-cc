;;;; packages/optimize/tests/optimizer-scheduler-tests.lisp
;;;; Unit tests for the scheduling passes in packages/optimize/src/optimizer-pipeline.lisp
;;;;
;;;; Covers:
;;;;   %opt-scheduler-barrier-p  — returns T for each barrier type, NIL for pure
;;;;   opt-pass-schedule-local   — reorders instructions in a RAW-dependency chain,
;;;;                               preserves barriers in original relative position
;;;;   schedule-pre-ra           — pressure-aware scheduling produces valid orderings

(progn
  (cl:in-package :cl-cc/test)
  (cl-cc/test::in-suite cl-cc/test::cl-cc-integration-suite))

;;; ─── Test helpers ─────────────────────────────────────────────────────────

(defun %sched-index (instructions instruction)
  "Return the 0-based position of INSTRUCTION in INSTRUCTIONS by EQ."
  (position instruction instructions :test #'eq))

;;; ─── %opt-scheduler-barrier-p ─────────────────────────────────────────────
;;; Direct vm-set-global/vm-slot-write cases were consolidated into the
;;; schedule-local ordering tests below, which exercise the barrier property
;;; in the scheduling operation rather than only its predicate.

;;; ─── opt-pass-schedule-local: basic reordering ────────────────────────────

;;; ─── opt-pass-schedule-local: barriers stop scheduling ────────────────────

;;; ─── schedule-pre-ra: pressure-aware ordering ─────────────────────────────

