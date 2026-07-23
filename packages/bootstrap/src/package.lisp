(cl:in-package :cl-user)

;;;; packages/bootstrap/src/package.lisp — :cl-cc/bootstrap
;;;;
;;;; Phase 2 prerequisite: the 12 symbols that must be interned before
;;;; cl-cc/optimize (egraph rules) and cl-cc/compile load.
;;;;
;;;; Why a separate package?
;;;;   cl-cc/optimize's egraph rewrite rules (egraph-rules.lisp) use binop/const/var/cmp/... as Prolog pattern atoms
;;;;   matched via cl-prolog:UNIFY.
;;;;   cl-cc/compile defines our-eval, called back by the compiler pipeline at runtime.
;;;;   Without a common bootstrap these subsystems would need to import from
;;;;   :cl-cc, which loads *after* them — creating a circular dependency.
;;;;
;;;; Consumers:
;;;;   cl-cc/optimize — (:use :cl :cl-cc/bootstrap :cl-cc/vm) [egraph rule pattern atoms]
;;;;   cl-cc/compile  — (:use :cl ... :cl-cc/bootstrap)  [defines our-eval, our-load here]
;;;;   cl-cc/parse    — (:use :cl ... :cl-cc/bootstrap)  [defines lexer-token-* here]
;;;;   cl-cc/expand   — (:use :cl :cl-cc/bootstrap)       [references our-eval, our-load, run-string-repl]
;;;;   cl-cc          — (:use ... :cl-cc/bootstrap)       [re-exports all]

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Selfhost code can consult the umbrella package name before the full
  ;; umbrella definition is loaded. Create a minimal placeholder early so
  ;; package lookups succeed during bootstrap and Prolog loading.
  (unless (find-package :cl-cc)
    (defpackage :cl-cc
      (:use :cl))))

(defpackage :cl-cc/bootstrap
  (:use :cl)
  (:export
   ;; Compiler re-entry point — defined in cl-cc/compile, called by the pipeline
   #:our-eval
   ;; REPL entry points — defined in cl-cc/compile; referenced in cl-cc/expand macro templates
   ;; Must live in bootstrap so expand can reference them before compile loads.
   #:our-load
   #:run-string-repl
    ;; VM bootstrap installers — defined in cl-cc/vm, consumed by runtime/parse/expand/selfhost
    #:*vm-runtime-callable-installer*
    #:*runtime-vm-callable-register-hook*
    #:*runtime-package-registry-provider*
    #:*runtime-find-package-fn*
    #:*runtime-intern-fn*
    #:*runtime-set-symbol-value-fn*
    #:*vm-eval-hook-installer*
    #:*vm-macroexpand-hook-installer*
    #:*vm-parse-forms-hook-installer*
   ;; Prolog type/relation predicate atoms (keys in *prolog-rules* fact DB)
   #:binop #:const #:var #:cmp
   #:integer-type #:boolean-type #:env-lookup
   ;; CST token bridge — defined in cl-cc/parse, referenced in DCG rules
   #:make-cst-token
   #:lexer-token-p #:lexer-token-type #:lexer-token-value
   ;; Quasiquote reader symbols — produced by cst.lisp, consumed by macro.lisp
   ;; Both cl-cc/parse and cl-cc use bootstrap, so they share the same symbol objects.
   #:backquote #:unquote #:unquote-splicing
    ;; Runtime helpers — used by early parser/expander code before runtime is loaded.
    ;; Must live in bootstrap so packages share the same symbols without conflict.
    #:rt-plist-put
    #:rt-slot-set))

(in-package :cl-cc/bootstrap)

(defvar *vm-runtime-callable-installer* nil)
(defvar *runtime-vm-callable-register-hook* nil)
(defvar *runtime-package-registry-provider* nil)
(defvar *runtime-find-package-fn* nil)
(defvar *runtime-intern-fn* nil)
(defvar *runtime-set-symbol-value-fn* nil)
(defvar *vm-eval-hook-installer* nil)
(defvar *vm-macroexpand-hook-installer* nil)
(defvar *vm-parse-forms-hook-installer* nil)

(defun rt-plist-put (plist indicator value)
  "Return a new plist with INDICATOR set to VALUE. Non-destructive."
  (let ((result nil) (found nil) (p plist))
    (loop while p do
          (let ((k (car p)))
            (if (eq k indicator)
                (progn (push indicator result) (push value result) (setf found t))
                (progn (push k result) (push (cadr p) result)))
            (setf p (cddr p))))
    (unless found
      (push indicator result) (push value result))
    (nreverse result)))

(defun rt-slot-set (obj slot-name value)
  "Set SLOT-NAME of OBJ to VALUE and return VALUE."
  (setf (slot-value obj slot-name) value))
