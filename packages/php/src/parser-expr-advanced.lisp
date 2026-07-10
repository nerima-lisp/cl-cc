;;;; frontend/php/parser-expr-advanced.lisp -- PHP Extended Expression Handler anchor
;;;;
;;;; Extended expression handlers are split by bounded context: core
;;;; AST/reference helpers, string/exception helpers, function-call lowering,
;;;; short-circuit lowering, closures, yield, array helpers, match, compound assignment,
;;;; skip helpers, and keyword dispatch.
;;;;
;;;; Depends on parser-expr.lisp for expression parsing primitives loaded before
;;;; this anchor. The split files are loaded immediately after this file and
;;;; before parser-stmt-lowering.lisp.

(in-package :cl-cc/php)
