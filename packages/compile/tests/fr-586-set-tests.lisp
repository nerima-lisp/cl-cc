(in-package :cl-cc/test)

(it-sequential "fr-586-set-is-core-builtin"
  (expect (cdr (assoc 'set cl-cc/compile::*builtin-binary-entries*)) :to-equal 'cl-cc::make-vm-set-symbol-value))

(it-sequential "fr-586-set-not-in-binary-custom-table"
  (expect (assoc 'set cl-cc/compile::*builtin-binary-custom-entries*) :to-be-falsy))
