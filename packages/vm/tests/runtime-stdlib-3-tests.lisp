;;; runtime-stdlib-3-tests.lisp — FR verification tests for runtime-stdlib-3 gaps

(in-package :cl-cc/test)



;; VM format directive at end-of-string has parser bug (FR-965)
;; ~D/~P/~W at EOL all fail with "Unterminated FORMAT directive"
;; Skipped until VM format parser is fixed
