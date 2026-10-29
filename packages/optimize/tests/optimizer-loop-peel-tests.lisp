;;;; Unit tests for FR-682 loop peeling.

(in-package :cl-cc/test)

;;;; ─── FR-514 loop fusion / fission (optimizer-loop-fusion.lisp) ──────────────
;;;;
;;;; Fusion/fission operate on the optimizer's linear "canonical counted loop"
;;;; shape: LABEL / (vm-lt iv limit → cond) / (vm-jump-zero cond → exit) / body… /
;;;; (vm-add iv iv step) / (vm-jump → head) / exit-LABEL. These tests exercise the
;;;; dependence-analysis helpers directly plus the two pass entry points.

;;; ─── memory-dependence arithmetic helpers ───────────────────────────────────

;;; ─── instruction classification helpers ─────────────────────────────────────

;;; ─── label + register helpers ───────────────────────────────────────────────

;;; ─── loop fusion pass ───────────────────────────────────────────────────────

(defun %fr-514-counted-loop (head exit iv limit step core)
  "Build a canonical counted loop: label / lt / jz / CORE… / add-step / jump / exit."
  (append (list (make-vm-label :name head)
                (make-vm-lt :dst (intern (format nil "COND-~A" head) :keyword)
                            :lhs iv :rhs limit)
                (make-vm-jump-zero :reg (intern (format nil "COND-~A" head) :keyword)
                                   :label exit))
          core
          (list (make-vm-add :dst iv :lhs iv :rhs step)
                (make-vm-jump :label head)
                (make-vm-label :name exit))))

;;; ─── loop fission pass ──────────────────────────────────────────────────────

