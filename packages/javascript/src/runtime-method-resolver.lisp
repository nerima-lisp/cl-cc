;;;; packages/javascript/src/runtime-method-resolver.lisp — JS prototype method dispatch
;;;;
;;;; Loaded last so every %js-array-*/%js-string-*/etc. helper is already defined.
;;;; The actual tables and dispatch code live in the split files loaded from the ASD.

(in-package :cl-cc/javascript)
