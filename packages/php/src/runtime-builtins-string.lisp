;;;; String PHP builtin helpers.
;;;;
;;;; The runtime string builtins are split by bounded context:
;;;; core search/join helpers, formatting, multibyte handling, encoding,
;;;; transforms/comparison, analysis/output, ctype predicates, and extras.

(in-package :cl-cc/php)
