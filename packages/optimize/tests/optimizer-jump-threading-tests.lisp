;;;; packages/optimize/tests/optimizer-jump-threading-tests.lisp
;;;; Unit tests for optimizer-jump-threading.lisp
;;;;
;;;; Covers: %opt-jump-branch-comparison-fact (nil and non-nil paths),
;;;;   %opt-jump-fact-killed-p, %opt-jump-same-comparison-p,
;;;;   %opt-jump-known-constant / %opt-jump-put-constant / %opt-jump-kill-constant,
;;;;   %opt-jump-rewrite-block-with-fact (redundant-comparison rewrite,
;;;;     move propagation, kill-on-redef),
;;;;   %opt-pass-jump-propagate-edge-values (integration smoke),
;;;;   opt-pass-jump-threading-with-propagation (end-to-end chain+propagation).

(in-package :cl-cc/test)

;;; ─── %opt-jump-fact-killed-p ─────────────────────────────────────────────

;;; ─── %opt-jump-same-comparison-p ─────────────────────────────────────────

;;; ─── constant alist helpers ───────────────────────────────────────────────

;;; ─── %opt-jump-branch-comparison-fact ───────────────────────────────────

;;; ─── %opt-jump-rewrite-block-with-fact ───────────────────────────────────

;;; ─── %opt-pass-jump-propagate-edge-values integration smoke ──────────────

;;; ─── opt-pass-jump-threading-with-propagation end-to-end ─────────────────

