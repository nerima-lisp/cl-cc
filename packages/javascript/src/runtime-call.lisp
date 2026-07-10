;;;; packages/javascript/src/runtime-call.lisp — JS callback invocation and method resolution
;;;;
;;;; %js-funcall / %js-call-with-this / %js-method-ref / %js-proto-method-lookup,
;;;; plus the receiver binding and prototype-chain lookup helpers used by
;;;; runtime-property and late-bound method tables.
;;;;
;;;; Load order: after runtime.lisp (needs type predicates and constants),
;;;;             before runtime-property.lisp (used by %js-get-prop / %js-set-prop).

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Callback invocation
;;; -----------------------------------------------------------------------

(defvar %js-this +js-undefined+
  "Dynamically bound to the receiver when a JS method is called.
JS source `this.x' compiles to (%js-get-prop %js-this \"x\").
Methods are called via %js-funcall-with-this which establishes this binding.")

(defvar *js-apply-fn*
  (lambda (fn args)
    ;; A callable JS object (e.g. `super', Intl/Symbol stubs) carries its
    ;; implementation under __call__; otherwise host APPLY.
    (if (and (hash-table-p fn) (gethash "__call__" fn))
        (apply (gethash "__call__" fn) args)
        (apply fn args)))
  "Invoker used to call a JS callback value (e.g. the FN passed to Array.map /
filter / reduce / sort). Defaults to host APPLY (plus __call__ objects) so plain
host lambdas work in unit tests; the pipeline rebinds it to a VM-closure-aware
invoker so a callback that is a compiled-JS closure is dispatched through the VM.")

(defun %js-funcall (fn &rest args)
  "Call JS callback FN with ARGS through the installed *js-apply-fn* invoker.
Array higher-order methods use this instead of CL:FUNCALL so the same code path
works whether FN is a host function (tests) or a compiled-JS closure (runtime)."
  (funcall *js-apply-fn* fn args))

(defvar *js-apply-with-this-fn*
  (lambda (this fn args)
    (let ((%js-this this))
      (funcall *js-apply-fn* fn args)))
  "Invoke a method/constructor FN with `this' = THIS. The default binds only the
host special %js-this (enough for host-function methods in unit tests). The
pipeline installs a version that ALSO sets the VM-global %js-this, because a
compiled-JS method body reads `this' via vm-get-global and cannot see the host
dynamic binding.")

(defun %js-call-with-this (this fn args)
  "Call method/constructor FN with `this' bound to THIS for both host and
compiled-VM method bodies."
  (funcall *js-apply-with-this-fn* this fn args))

;;; -----------------------------------------------------------------------
;;;  Method resolution hooks
;;; -----------------------------------------------------------------------

(defvar *js-method-resolver* nil
  "When set, a function (receiver method-name-string) -> a bound method closure,
or +js-undefined+ when the name is not a method. Installed by a late runtime
file once every %js-array-*/%js-string-* method is defined, so %js-get-prop can
resolve obj.method without a load-order cycle. This is what lets `arr.push(x)',
`nums.map(f)' and `s.toUpperCase()' resolve to a callable value the VM can
invoke — mirroring how console.log resolves to a function value.")

(defun %js-method-ref (obj key)
  "Resolve OBJ.KEY to a bound method via *js-method-resolver*, else +js-undefined+."
  (if *js-method-resolver*
      (funcall *js-method-resolver* obj key)
      +js-undefined+))

(defvar *js-callable-p* #'functionp
  "Predicate: is X a callable JS value? Defaults to host FUNCTIONP; the pipeline
extends it to also recognize a compiled-JS closure (vm-closure-object), so
prototype-chain method lookup can tell a method from an inherited data value.")

(defun %js-proto-accessor-lookup (obj accessor-key)
  "Walk OBJ's __proto__ chain for ACCESSOR-KEY (e.g. \"__get_x\" / \"__set_x\")
and return the accessor function, or NIL.  Used for class getters/setters, which
live on the prototype."
  (loop with proto = (gethash "__proto__" obj)
        while (%js-ht-p proto)
        do (multiple-value-bind (val found) (gethash accessor-key proto)
             (when found (return val)))
           (setf proto (gethash "__proto__" proto))
        finally (return nil)))

(defun %js-proto-method-lookup (obj k)
  "Walk OBJ's __proto__ chain for key K (JS prototype method resolution). A
getter (__get_K on the chain) is invoked with `this' = OBJ and its result
returned. A callable found under K is a METHOD: return a closure that binds
%js-this to OBJ and then calls the method. A non-callable inherited value is
returned as-is; a miss yields undefined.
The dynamic binding of %js-this is what makes `this.x' in method bodies work."
  (let ((getter-key (concatenate 'string "__get_" k)))
    (loop with proto = (gethash "__proto__" obj)
          while (%js-ht-p proto)
          do ;; an inherited getter takes precedence and is invoked immediately
             (multiple-value-bind (gfn gfound) (gethash getter-key proto)
               (when gfound
                 (return-from %js-proto-method-lookup (%js-call-with-this obj gfn nil))))
             (multiple-value-bind (val found) (gethash k proto)
               (when found
                 (return-from %js-proto-method-lookup
                   (if (funcall *js-callable-p* val)
                       ;; Bind %js-this dynamically so `this' in the method body
                       ;; resolves to the receiver, then dispatch through %js-funcall
                       ;; for VM-closure compatibility.  For a `super' object
                       ;; (carrying __super_this__) bind to the REAL instance, not the
                       ;; super object, so super.method() sees the correct `this'.
                       (let ((method val)
                             (receiver (multiple-value-bind (st found)
                                           (gethash "__super_this__" obj)
                                         (if found st obj))))
                         (lambda (&rest args)
                           (%js-call-with-this receiver method args)))
                       val))))
             (setf proto (gethash "__proto__" proto))
          finally (return +js-undefined+))))
