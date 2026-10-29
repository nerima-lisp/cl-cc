;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators, cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────
(defun make-test-cfg-linear ()
  "Build a CFG from a simple linear instruction sequence: CONST → ADD → RET."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const :dst :r0 :value 1)
         (make-vm-const :dst :r1 :value 2)
         (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
         (make-vm-ret   :reg :r2))))
(defun make-test-cfg-branch ()
  "Build a CFG with a conditional branch (entry JUMP-ZERO→then, else JUMP→exit, then, exit RET)."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "then")
         (make-vm-const    :dst :r1 :value 99)
         (make-vm-jump     :label "exit")
         (make-vm-label    :name "then")
         (make-vm-const    :dst :r1 :value 42)
         (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r1))))
(defun make-test-cfg-loop ()
  "Build a CFG with a simple natural loop and a separate exit block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-label    :name "head")
          (make-vm-const    :dst :r0 :value 1)
          (make-vm-jump-zero :reg :r0 :label "exit")
          (make-vm-jump     :label "head")
          (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-hot-cold ()
  "Build a CFG with one loop-heavy hot block and one cold exit block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 1)
         (make-vm-jump-zero :reg :r0 :label "cold")
         (make-vm-label    :name "hot")
         (make-vm-const    :dst :r1 :value 2)
         (make-vm-jump     :label "hot")
          (make-vm-label    :name "cold")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-cold-signal ()
  "Build a CFG with a normal block and an explicit cold error block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const     :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "hot")
         (make-vm-label    :name "cold")
          (cl-cc:make-vm-signal-error :error-reg :r0)
          (make-vm-ret      :reg :r0)
          (make-vm-label    :name "hot")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-critical-edge ()
  "Build a CFG with one critical edge into the THEN block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
          (make-vm-jump-zero :reg :r0 :label "then")
          (make-vm-const    :dst :r1 :value 99)
          (make-vm-jump     :label "then")
          (make-vm-label    :name "then")
          (make-vm-const    :dst :r1 :value 42)
          (make-vm-label    :name "merge")
          (make-vm-ret      :reg :r1))))

;;; ─── Basic CFG Construction ──────────────────────────────────────────────
;;; ─── Predecessor / Successor Edges ──────────────────────────────────────
;;; ─── RPO ─────────────────────────────────────────────────────────────────
;;; ─── Dominator Tree ──────────────────────────────────────────────────────
;;; ─── Dominance Frontiers ─────────────────────────────────────────────────
;;; ─── Post-Dominator Tree ────────────────────────────────────────────────
;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────
;;; ─── cfg-idf (Iterated Dominance Frontier) ──────────────────────────────────
;;; ─── %cfg-fallthrough-edge / %cfg-jump-target-edge ───────────────────────
;;; ─── %cfg-replace-successor / %cfg-replace-predecessor / %cfg-replace-terminator
;;; ─── %cfg-ensure-label ────────────────────────────────────────────────────
