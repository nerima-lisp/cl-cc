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
;;;;   cl-cc/javascript, cl-cc/php — register themselves as compiled-language
;;;;   backends via the register-backend-* functions below, instead of the
;;;;   pipeline hand-maintaining per-language knowledge of frontend internals.

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
    #:rt-slot-set
    ;; Backend self-registration — a compiled-language frontend (cl-cc/javascript,
    ;; cl-cc/php, ...) calls these at load time instead of the pipeline hand-coding
    ;; per-language knowledge of frontend internals. See the register-* docstrings.
    #:register-backend-bridge-provider
    #:backend-bridge-providers
    #:register-backend-vm-integration-installer
    #:backend-vm-integration-installers
    #:register-backend-global-seeder
    #:backend-global-seeders
    #:register-backend-parser
    #:find-backend-parser))

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

;;; Backend self-registration.
;;;
;;; A compiled-language frontend (cl-cc/javascript, cl-cc/php, ...) owns the
;;; knowledge of its own runtime helpers, VM/closure integration, and global
;;; seeding. Rather than the pipeline hand-maintaining a per-language alist of
;;; that knowledge (coupling it to every frontend's internal symbols), each
;;; frontend registers itself here at load time; the pipeline iterates the
;;; registries below without naming any backend.

(defvar *backend-bridge-providers* nil
  "List of zero-argument thunks. Each thunk returns an alist of
(SYMBOL . FUNCTION) VM host-bridge entries contributed by one backend.")

(defun register-backend-bridge-provider (thunk)
  "Register THUNK as a source of VM host-bridge entries for a backend."
  (pushnew thunk *backend-bridge-providers*)
  thunk)

(defun backend-bridge-providers ()
  "Return every host-bridge entry contributed by a registered backend."
  (mapcan (lambda (thunk) (copy-list (funcall thunk))) *backend-bridge-providers*))

(defvar *backend-vm-integration-installers* nil
  "List of zero-argument thunks, each installing one backend's JS/PHP-callback
<->VM-closure integration into that backend's runtime specials.")

(defun register-backend-vm-integration-installer (thunk)
  "Register THUNK, which installs one backend's VM/closure integration."
  (pushnew thunk *backend-vm-integration-installers*)
  thunk)

(defun backend-vm-integration-installers ()
  "Return every registered VM-integration installer thunk."
  (copy-list *backend-vm-integration-installers*))

(defvar *backend-global-seeders* nil
  "List of one-argument functions, each seeding a backend's prelude globals
into a VM STATE's global table.")

(defun register-backend-global-seeder (fn)
  "Register FN, a function of one argument (a VM state), as a backend's
prelude-global seeder."
  (pushnew fn *backend-global-seeders*)
  fn)

(defun backend-global-seeders ()
  "Return every registered backend global-seeder function."
  (copy-list *backend-global-seeders*))

(defvar *backend-parsers* nil
  "Alist of (LANGUAGE-KEYWORD . PARSE-FUNCTION) registered by backends.")

(defun register-backend-parser (language fn)
  "Register FN as the source parser for LANGUAGE (a keyword such as
:javascript or :php), replacing any earlier registration for that language."
  (setf *backend-parsers*
        (acons language fn (remove language *backend-parsers* :key #'car)))
  fn)

(defun find-backend-parser (language)
  "Return the parse function registered for LANGUAGE, or NIL."
  (cdr (assoc language *backend-parsers*)))
