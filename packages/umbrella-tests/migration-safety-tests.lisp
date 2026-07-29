;;;; tests/unit/migration-safety-tests.lisp — Symbol identity safety net
;;;;
;;;; PURPOSE: Guard against symbol identity breakage during the package-by-
;;;; feature migration (Phase 2: source-level `in-package` changes).
;;;;
;;;; The self-hosting compiler relies on EQ identity of symbols across
;;;; packages for CLOS dispatch, Prolog unification, and the macro
;;;; environment.  If a migration step accidentally creates a second,
;;;; shadowed copy of a symbol, defmethod specializers silently stop
;;;; matching and tests pass while runtime behavior is wrong.
;;;;
;;;; These tests catch that class of bug early.

(in-package :cl-cc/test)

;;; ----------------------------------------------------------------
;;; Suite
;;; ----------------------------------------------------------------



;;; ----------------------------------------------------------------
;;; A. Bootstrap symbol identity
;;; ----------------------------------------------------------------
;;; The 12 pre-interned symbols in package-exports-1.lisp must be the
;;; SAME object in :cl-cc and :cl-cc/bootstrap.  If they diverge, the
;;; Prolog engine and compiler see different symbols and silently fail
;;; to unify terms.

(it-sequential "bootstrap-symbol-identity"
  (dolist (name '("BINOP" "CONST" "VAR" "CMP"
                  "INTEGER-TYPE" "BOOLEAN-TYPE"
                  "ENV-LOOKUP" "MAKE-CST-TOKEN"
                  "LEXER-TOKEN-P" "LEXER-TOKEN-TYPE"
                  "LEXER-TOKEN-VALUE" "OUR-EVAL"))
    (let ((cl-cc-sym   (find-symbol name :cl-cc))
          (boot-sym    (find-symbol name :cl-cc/bootstrap)))
      ;; Both packages must have the symbol at all.
      (expect cl-cc-sym :to-be-truthy)
      (expect boot-sym :to-be-truthy)
      ;; And it must be the exact same object.
      (expect cl-cc-sym :to-be boot-sym))))

;;; ----------------------------------------------------------------
;;; B. AST type export verification
;;; ----------------------------------------------------------------
;;; Every AST class used as a defmethod specializer in compile/ must
;;; be :external in :cl-cc/ast.  If a symbol is merely :internal or
;;; :inherited, a future `in-package :cl-cc/compile` would shadow it,
;;; breaking method dispatch on that specializer.

(it-sequential "ast-types-exported"
  (dolist (name '("AST-INT" "AST-VAR" "AST-BINOP" "AST-IF" "AST-LET"
                  "AST-LAMBDA" "AST-DEFUN" "AST-DEFCLASS" "AST-DEFGENERIC"
                  "AST-DEFMETHOD" "AST-MAKE-INSTANCE" "AST-SLOT-VALUE"
                  "AST-CALL" "AST-APPLY" "AST-VALUES"
                  "AST-MULTIPLE-VALUE-BIND" "AST-CATCH" "AST-THROW"
                  "AST-BLOCK" "AST-RETURN-FROM" "AST-TAGBODY" "AST-GO"
                  "AST-SETQ" "AST-QUOTE" "AST-THE" "AST-PROGN"
                  "AST-PRINT" "AST-HOLE" "AST-FUNCTION"
                  "AST-FLET" "AST-LABELS" "AST-UNWIND-PROTECT"
                  "AST-HANDLER-CASE" "AST-DEFVAR" "AST-DEFMACRO"
                  "AST-SET-SLOT-VALUE" "AST-SET-GETHASH"
                  "AST-MULTIPLE-VALUE-CALL"))
    (multiple-value-bind (sym status)
        (find-symbol name :cl-cc/ast)
      (expect sym :to-be-truthy)
      (expect status :to-be :external))))

;;; ----------------------------------------------------------------
;;; C. VM instruction type export verification
;;; ----------------------------------------------------------------
;;; Same rationale as B but for VM instruction types used as defmethod
;;; specializers in emit/ (regalloc, x86-64, aarch64, wasm backends).

(it-sequential "vm-types-exported"
  (dolist (name '("VM-INSTRUCTION" "VM-CONST" "VM-FUNC-REF"
                  "VM-GET-GLOBAL" "VM-SET-GLOBAL"
                  "VM-MOVE" "VM-BINOP" "VM-SELECT"
                  "VM-JUMP-ZERO" "VM-PRINT" "VM-HALT" "VM-RET"
                  "VM-CLOSURE" "VM-MAKE-CLOSURE" "VM-CLOSURE-REF-IDX"
                  "VM-CALL" "VM-TAIL-CALL"
                  "VM-CONS" "VM-CAR" "VM-CDR" "VM-RPLACA" "VM-RPLACD"
                  "VM-PUSH" "VM-POP"
                  "VM-CLASS-DEF" "VM-MAKE-OBJ"
                  "VM-SLOT-READ" "VM-SLOT-WRITE"
                  "VM-REGISTER-METHOD" "VM-GENERIC-CALL"
                  "VM-VALUES" "VM-MV-BIND" "VM-APPLY"
                  "VM-ESTABLISH-HANDLER" "VM-SIGNAL-ERROR"
                  "VM-ESTABLISH-CATCH" "VM-THROW"))
    (multiple-value-bind (sym status)
        (find-symbol name :cl-cc/vm)
      (expect sym :to-be-truthy)
      (expect status :to-be :external))))

;;; ----------------------------------------------------------------
;;; D. Cross-package symbol identity for CLOS dispatch
;;; ----------------------------------------------------------------
;;; The umbrella :cl-cc package re-exports symbols from child packages.
;;; Existing defmethod forms are written in :cl-cc, so they resolve
;;; symbols via :cl-cc.  The defining packages (:cl-cc/ast, :cl-cc/vm)
;;; own the class definitions.  If the symbol in :cl-cc is NOT eq to
;;; the one in the child package, defmethod specializers will silently
;;; dispatch against the wrong (or no) class.

(it-sequential "clos-dispatch-ast-symbol-identity"
  (dolist (name '("AST-INT" "AST-VAR" "AST-BINOP" "AST-IF" "AST-LET"
                  "AST-LAMBDA" "AST-DEFUN" "AST-DEFCLASS" "AST-DEFGENERIC"
                  "AST-DEFMETHOD" "AST-MAKE-INSTANCE" "AST-SLOT-VALUE"
                  "AST-CALL" "AST-APPLY" "AST-VALUES"
                  "AST-MULTIPLE-VALUE-BIND" "AST-CATCH" "AST-THROW"
                  "AST-BLOCK" "AST-RETURN-FROM" "AST-TAGBODY" "AST-GO"
                  "AST-SETQ" "AST-QUOTE" "AST-THE" "AST-PROGN"
                  "AST-PRINT" "AST-HOLE" "AST-FUNCTION"
                  "AST-FLET" "AST-LABELS" "AST-UNWIND-PROTECT"
                  "AST-HANDLER-CASE" "AST-DEFVAR" "AST-DEFMACRO"
                  "AST-SET-SLOT-VALUE" "AST-SET-GETHASH"
                  "AST-MULTIPLE-VALUE-CALL"))
    (let ((umbrella (find-symbol name :cl-cc))
          (child    (find-symbol name :cl-cc/ast)))
      (expect umbrella :to-be-truthy)
      (expect child :to-be-truthy)
      (expect umbrella :to-be child))))

(it-sequential "clos-dispatch-vm-symbol-identity"
  (dolist (name '("VM-INSTRUCTION" "VM-CONST" "VM-FUNC-REF"
                  "VM-GET-GLOBAL" "VM-SET-GLOBAL"
                  "VM-MOVE" "VM-BINOP" "VM-SELECT"
                  "VM-JUMP-ZERO" "VM-PRINT" "VM-HALT" "VM-RET"
                  "VM-CLOSURE" "VM-MAKE-CLOSURE" "VM-CLOSURE-REF-IDX"
                  "VM-CALL" "VM-TAIL-CALL"
                  "VM-CONS" "VM-CAR" "VM-CDR" "VM-RPLACA" "VM-RPLACD"
                  "VM-PUSH" "VM-POP"
                  "VM-CLASS-DEF" "VM-MAKE-OBJ"
                  "VM-SLOT-READ" "VM-SLOT-WRITE"
                  "VM-REGISTER-METHOD" "VM-GENERIC-CALL"
                  "VM-VALUES" "VM-MV-BIND" "VM-APPLY"
                  "VM-ESTABLISH-HANDLER" "VM-SIGNAL-ERROR"
                  "VM-ESTABLISH-CATCH" "VM-THROW"))
    (let ((umbrella (find-symbol name :cl-cc))
          (child    (find-symbol name :cl-cc/vm)))
      (expect umbrella :to-be-truthy)
      (expect child :to-be-truthy)
      (expect umbrella :to-be child))))
