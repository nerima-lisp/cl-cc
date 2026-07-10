;;;; packages/javascript/src/runtime-builtins-intl-core.lisp -- Intl shared helpers

(in-package :cl-cc/javascript)

(defun %js-intl-option (options name default)
  (if (hash-table-p options)
      (multiple-value-bind (value present-p) (gethash name options)
        (if present-p value default))
      default))
