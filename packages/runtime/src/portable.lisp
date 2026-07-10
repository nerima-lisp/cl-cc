;;;; packages/runtime/src/portable.lisp — Self-Host Portability Facades
;;;;
;;;; Thin wrappers over cl-cc/runtime synchronization primitives and
;;;; SBCL-specific APIs. Provides consistent names (rt-make-lock,
;;;; rt-with-lock) that map to sync.lisp's API. Under self-host,
;;;; provides deterministic single-thread/no-op/stub behavior.
;;;;
;;;; This file must be loaded AFTER sync.lisp in the ASDF component order.

(in-package :cl-cc/runtime)

;; ── Lock wrappers (map to sync.lisp API) ─────────────────────────────────

(defun rt-make-lock (&optional name)
  "Create a lock. Maps to rt-make-mutex from sync.lisp."
  (rt-make-mutex :name name))

(defmacro rt-with-lock ((lock) &body body)
  "Execute BODY with LOCK held. Maps to rt-with-mutex from sync.lisp."
  `(rt-with-mutex (,lock) ,@body))

(defun rt-lock (lock &optional (wait-p t))
  "Acquire LOCK. Maps to rt-mutex-lock from sync.lisp."
  (rt-mutex-lock lock :timeout (unless wait-p 0)))

(defun rt-unlock (lock)
  "Release LOCK. Maps to rt-mutex-unlock from sync.lisp."
  (rt-mutex-unlock lock))

(defun rt-try-lock (lock)
  "Try to acquire LOCK without blocking."
  (rt-mutex-lock lock :timeout 0))
