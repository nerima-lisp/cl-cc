;;;; packages/expand/src/package.lisp --- cl-cc/expand package definition
;;;;
;;;; Macro expansion subsystem: macro environment, defmacro machinery,
;;;; macroexpansion (our-macroexpand-1/-all), lambda-list parsing and
;;;; destructuring, and the LOOP/DO/CASE/TYPECASE control-flow macros.
;;;;
;;;; Uses cl-cc/bootstrap so source files can reference our-eval unqualified.
;;;; cl-cc/type is accessed qualified (cl-cc/type:...) so not in :use.

(defpackage :cl-cc/expand
  (:use :cl :cl-cc/bootstrap)
  (:shadow #:compiler-macro-function #:diagnostic)
  (:export
   ;; --- expander-data.lisp --- constant table + accessor/struct maps -----
   #:*constant-table*
    #:*accessor-slot-map*
     #:*defstruct-read-only-accessor-map*
     #:*defstruct-slot-registry*
     #:*defstruct-type-registry*
     #:*setf-compound-place-handlers*
      #:*declaim-inline-registry*
     #:*declaim-optimize-registry*
     #:*global-proclamations*
     #:with-fresh-defstruct-registries
     #:declaration-optimize-quality

   ;; --- expander-core.lisp --- macro expander construction -------------
   #:make-macro-expander

   ;; --- macro.lisp --- macro environment + expansion engine -----------
   #:macro-env
   #:macro-env-table
   #:our-defmacro
   #:our-macroexpand-1
   #:our-macroexpand
   #:our-macroexpand-all
   #:register-macro
   #:lookup-macro
   #:*macro-environment*
   #:*macro-eval-fn*
   #:*symbol-macro-table*
    #:*compiler-macro-table*
    #:*deftransform-table*
    #:register-deftransform
    #:lookup-deftransform
    #:deftransform
    #:define-deftransform
    #:*macroexpand-step-cache*
    #:*macroexpand-all-cache*

    ;; --- macro-lambda-list.lisp --- lambda-list helpers ----------------
   #:parse-lambda-list
   #:destructure-lambda-list
   #:generate-lambda-bindings

   ;; --- expander.lisp --- compiler macro expansion --------------------
    #:compiler-macroexpand-all

   ;; --- control-flow macros ------------------------------------------
    #:dolist
    #:dotimes
    #:do
   #:do*
   #:case
    #:typecase
    #:loop
    #:likely
    #:unlikely

   ;; --- expander-typed-params.lisp --- typed lambda-list helpers ----------
   #:*function-type-registry*
   #:register-function-type
   #:lambda-list-has-typed-p
   #:strip-typed-params
   #:lambda-list-info-environment

   ;; --- macros used via run-string (need umbrella re-export) ---------
   #:defun/c
   #:copy-hash-table
   #:copy-structure
   #:make-iterator
   #:iterator-next
   #:doiterator
   #:with-iterator
   ;; --- hygienic macros (FR-804/805) ---------------------------------
   #:define-syntax
   #:syntax-rules
   #:with-gensyms
   #:once-only
   ;; --- lazy evaluation (FR-856/857) ---------------------------------
   #:promise
   #:promisep
   #:%make-promise
   #:force
   #:delay
   #:memoize
   #:memoize-stats
   #:memoize-clear
   #:*memoize-registry*
   ;; --- FR-130: perfect hash tables (exported for testing) -------------
    #:*compiler-special-forms-table*
    #:*all-builtin-names-table*
    #:*binary-builtins-table*
    #:*unary-builtins-table*
    #:*variadic-fold-builtins-table*
    #:*variadic-fold-or-list-table*

   ;; --- macros-package-system.lisp --- package-system runtime bridge ----
   ;; Reached (as cl-cc/expand::rt-use-package /
   ;; cl-cc/expand::add-package-local-nickname) from
   ;; packages/selfhost/src/pipeline-selfhost.lisp's host-bridge
   ;; registration table.
   #:rt-use-package
   #:add-package-local-nickname

   ;; --- macros-runtime-support.lisp / runtime-stdlib-3-expander.lisp ---
   ;; declaim-clause recording, reached (as cl-cc/expand::%record-declaim-*)
   ;; from packages/stdlib/src/stdlib-source.lisp's self-hosted PROCLAIM
   ;; definition. Prefix kept as-is; these are still implementation
   ;; details, just visible ones.
   #:%record-declaim-inline-clause
   #:%record-declaim-optimize-clause
   #:%record-declaim-type-clause
   #:%record-declaim-ftype-clause
   #:%record-declaim-special-clause))
