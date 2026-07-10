;;;; packages/javascript/src/runtime-builtins-platform-atomics.lisp — Atomics helpers

(in-package :cl-cc/javascript)

(defun %js-make-atomics ()
  "Atomics global object (pause)."
  (%js-make-object
   "pause" (lambda (&rest _) (declare (ignore _)) +js-undefined+)))
