;;;; tests/unit/vm/vm-bridge-tests.lisp
;;;; Unit tests for src/vm/vm-bridge.lisp — host bridge + CLOS slot metadata.
;;;;
;;;; Covers: hash-table-values, vm-register-host-bridge,
;;;;   slot-definition-name, slot-definition-initform,
;;;;   slot-definition-initargs, slot-definition-allocation,
;;;;   %class-slot-initargs-for-slot, %class-slot-metadata,
;;;;   %class-slot-definitions, rt-plist-put,
;;;;   generic-function-methods, generic-function-method-combination.

(in-package :cl-cc/test)

;;; ─── hash-table-values ───────────────────────────────────────────────────

;;; ─── vm-register-host-bridge ─────────────────────────────────────────────

(it-sequential "vm-bridge-registers-php-current-closure"
  (expect (functionp (cl-cc/vm::vm-bridge-callable 'cl-cc/php::%php-current-closure)) :to-be-truthy)
  (expect (functionp (gethash "PHP-CURRENT-CLOSURE" cl-cc/vm::*vm-runtime-callables*)) :to-be-truthy))

;;; ─── slot-definition-name ────────────────────────────────────────────────

;;; ─── slot-definition-initform ────────────────────────────────────────────

;;; ─── slot-definition-initargs ────────────────────────────────────────────

;;; ─── slot-definition-allocation ──────────────────────────────────────────

;;; ─── %class-slot-initargs-for-slot ──────────────────────────────────────

;;; ─── %class-slot-metadata ────────────────────────────────────────────────

;;; ─── %class-slot-definitions ─────────────────────────────────────────────

;;; ─── rt-plist-put ────────────────────────────────────────────────────────

;;; ─── generic-function-methods ────────────────────────────────────────────

;;; ─── generic-function-method-combination ─────────────────────────────────

