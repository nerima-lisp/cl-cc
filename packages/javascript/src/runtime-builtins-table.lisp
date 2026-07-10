;;;; packages/javascript/src/runtime-builtins-table.lisp -- JS built-in map builder
;;;;
;;;; *js-builtin-specs* now lives in runtime-builtins-table-specs.lisp.
;;;; This file only builds *js-builtin-map* from the shared dispatch table.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Built-in dispatch table
;;; -----------------------------------------------------------------------

(defun %build-js-builtin-map ()
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (spec *js-builtin-specs*)
      (setf (gethash (car spec) ht) (cdr spec)))
    ht))

(defvar *js-builtin-map* (%build-js-builtin-map)
  "Dispatch table from JS built-in name to CL function.")
