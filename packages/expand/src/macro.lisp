(in-package :cl-cc/expand)

;; Defined in expander-data.lisp, but declare it here too so compile-file sees
;; the intended dynamic binding even when this file is compiled first.
(defvar *macro-eval-fn*)

(defun %bootstrap-macro-eval (form)
  "Bootstrap macro evaluator.
Prefer `our-eval`; signal an explicit error if the selfhosted evaluator is not yet available."
  (if (fboundp 'our-eval)
      (our-eval form)
      (error "OUR-EVAL is unavailable during macro bootstrap for ~S" form)))

;;; CL-CC Macro System
;;; A complete macro system implementation with:
;;; - Full destructuring-bind for lambda lists
;;; - Environment classes for lexical scoping
;;; - Macro expansion (single and full)
;;; - Built-in macros for bootstrap

(defclass macro-env ()
  ((macros :initform (make-hash-table :test 'eq) :reader macro-env-table))
  (:documentation "Global environment for macro definitions."))

;;; Macro Environment and Registration - also available at compile-time

(defvar *macro-environment* (make-instance 'macro-env)
  "Global macro environment for macro definitions.")

(defvar *macroexpand-step-cache*
  (make-hash-table :test #'eq :weakness :key)
  "Weak cache mapping ENV → (equal-hash-table FORM → (EXPANDED-P . VALUE)).")

(defvar *macroexpand-all-cache*
  (make-hash-table :test #'eq :weakness :key)
  "Weak cache mapping ENV → (equal-hash-table FORM → recursively EXPANDED-FORM).")

(defvar *compiler-macro-table*)

;; No global lock needed: %with-isolated-macro-environment gives each test worker
;; its own fresh cache instances, so no two threads ever share the same hash table.
;; A mutex here would cause GC-safepoint deadlocks: make-hash-table can trigger GC
;; inside the lock while other threads are stuck in futex-wait (without-gcing).
(defmacro %with-macroexpansion-cache-lock (&body body)
  `(progn ,@body))

(defun %macroexpansion-cache-table (root env)
  (or (gethash env root)
      (setf (gethash env root)
            (make-hash-table :test #'equal))))

(defun %macroexpansion-cache-lookup (form env &optional (root *macroexpand-step-cache*))
  (%with-macroexpansion-cache-lock
    (gethash form (%macroexpansion-cache-table root env))))

(defun %macroexpansion-cache-store (form env expanded &optional (root *macroexpand-step-cache*))
  (%with-macroexpansion-cache-lock
    (setf (gethash form (%macroexpansion-cache-table root env)) expanded)))

(defun %reset-macroexpansion-caches ()
  "Drop cached macroexpansions after macro environment changes."
  (%with-macroexpansion-cache-lock
    (setf *macroexpand-step-cache* (make-hash-table :test #'eq :weakness :key)
          *macroexpand-all-cache* (make-hash-table :test #'eq :weakness :key)))
  ;; FR-153: also clear the memoization cache when macros are redefined
  (when (fboundp 'clear-macro-expansion-cache)
    (clear-macro-expansion-cache)))

(defun %contains-uninterned-symbol-p (node)
  "Return T when NODE or any subtree contains an uninterned symbol.
Gensym-hygienic expansions are never cached."
  (typecase node
    (symbol (null (symbol-package node)))
    (cons   (or (%contains-uninterned-symbol-p (car node))
                (%contains-uninterned-symbol-p (cdr node))))
    (t nil)))

(defun %side-effecting-macro-form-p (form)
  "Return T when expanding FORM mutates macroexpansion-visible global state."
  (and (consp form)
       (symbolp (car form))
       (member (symbol-name (car form))
               '("DECLAIM" "DEFINE-COMPILER-MACRO" "DEFINE-DEFTRANSFORM"
                 "OUR-DEFMACRO" "AWAIT" "ASYNC-HANDLER")
               :test #'string=)))

(defun %cacheable-macroexpansion-p (form)
  "Return T when FORM is safe to reuse from the macroexpansion cache."
  (and (not (%contains-uninterned-symbol-p form))
       (not (%side-effecting-macro-form-p form))))

(defun %register-expander (table name expander)
  "Register NAME in TABLE and invalidate macroexpansion caches."
  (setf (gethash name table) expander)
  (%reset-macroexpansion-caches)
  name)

(defun register-macro (name expander)
  "Register NAME as a macro with EXPANDER in the global environment.
EXPANDER may be either a host function or a descriptor consumed by
`invoke-registered-expander'."
  (%register-expander (macro-env-table *macro-environment*) name expander))

(defun register-compiler-macro (name expander)
  "Register NAME as a compiler macro expander in the global environment.
EXPANDER may be either a host function or a descriptor consumed by
`invoke-registered-expander'."
  (%register-expander *compiler-macro-table* name expander))

(defparameter *expander-descriptor-kinds*
  '(:macro-expander :compiler-macro-expander :register-macro-expander)
  "Valid :kind values for data-backed macro expander descriptors.")

(defun %expander-descriptor-p (object)
  "Return T when OBJECT is a data-backed macro expander descriptor."
  (and (listp object)
       (not (null (member (getf object :kind)
                          *expander-descriptor-kinds*
                          :test #'eq)))))

(defun %maybe-postprocess-expansion (result descriptor env)
  "Apply post-expansion processing to RESULT if the descriptor requests it."
  (case (getf descriptor :post-expand)
    (:our-macroexpand-all (our-macroexpand-all result env))
    (otherwise result)))

(defun %descriptor-declaration-form-p (form)
  "Return T when FORM is a leading declaration in a descriptor body."
  (and (consp form) (eq (car form) 'declare)))

(defun %descriptor-runtime-body (body)
  "Drop leading declarations before descriptor-backed expander evaluation."
  (loop for forms = body then (cdr forms)
        while (and (consp forms)
                   (%descriptor-declaration-form-p (car forms)))
        finally (return forms)))

(defun %nest-let-bindings (bindings body)
  "Build nested LET forms from BINDINGS ending in BODY forms."
  (reduce (lambda (binding inner) (list 'let (list binding) inner))
          (%normalize-let-bindings bindings)
          :from-end t
          :initial-value (cons 'locally body)))

(defun %invoke-expander-descriptor (descriptor form env)
  "Evaluate DESCRIPTOR against FORM and ENV through `*macro-eval-fn*'."
  (case (getf descriptor :kind)
    (:macro-expander
      (let* ((lambda-list (getf descriptor :lambda-list))
             (body        (%descriptor-runtime-body (getf descriptor :body)))
             (form-var    (gensym "FORM"))
             (eval-form   `(let ((,form-var ',form))
                             ,(%nest-let-bindings
                               (generate-lambda-bindings lambda-list form-var)
                               body))))
        (%maybe-postprocess-expansion (funcall *macro-eval-fn* eval-form) descriptor env)))
    (:compiler-macro-expander
      (multiple-value-bind (effective-lambda-list whole-var environment-var)
          (%compiler-macro-lambda-list-parts (getf descriptor :lambda-list))
        (let* ((body     (%descriptor-runtime-body (getf descriptor :body)))
               (form-var (gensym "FORM"))
               (bindings (append
                          (when whole-var (list (list whole-var form-var)))
                          (when environment-var (list (list environment-var `',env)))
                          (destructure-lambda-list
                           effective-lambda-list
                           `(%compiler-macro-argument-tail ,form-var))))
               (eval-form `(let ((,form-var ',form))
                             ,(%nest-let-bindings bindings body))))
          (funcall *macro-eval-fn* eval-form))))
    (:register-macro-expander
     (let* ((parameters (getf descriptor :parameters))
            (body       (%descriptor-runtime-body (getf descriptor :body)))
            (form-var   (first parameters))
            (env-var    (second parameters))
            (bindings   (append (when form-var `((,form-var ',form)))
                                (when env-var  `((,env-var  ',env)))))
            (eval-form  `(let ,bindings ,@body)))
        (funcall *macro-eval-fn* eval-form)))
    (otherwise
     (error "Unknown expander descriptor kind: ~S" descriptor))))

(defun invoke-registered-expander (expander form env)
  "Invoke EXPANDER on FORM and ENV.
Supports both host functions and descriptor-backed expanders."
  (cond
    ((functionp expander) (funcall expander form env))
    ((%expander-descriptor-p expander) (%invoke-expander-descriptor expander form env))
    (t (error "Unsupported expander representation: ~S" expander))))

(defun lookup-macro (name)
  "Look up macro NAME in the global macro environment."
  (or (gethash name (macro-env-table *macro-environment*))
      (and (symbolp name)
           (let ((local (intern (symbol-name name) :cl-cc/expand)))
             (gethash local (macro-env-table *macro-environment*))))))

(defun %compiler-macro-argument-tail (form)
  "Return the effective argument tail for a compiler macro FORM."
  (if (and (consp form) (eq (car form) 'funcall))
      (cddr form)
      (cdr form)))

(defun %compiler-macro-lambda-list-parts (lambda-list)
  "Return LAMBDA-LIST without &whole/&environment plus their binding vars."
  (loop with result = nil
        with whole = nil
        with environment = nil
        with tail = lambda-list
        while tail
        for item = (car tail)
        if (eq item '&whole)
          do (unless (and (cdr tail) (symbolp (cadr tail)))
               (error "Invalid &WHOLE in compiler macro lambda list: ~S" lambda-list))
             (setf whole (cadr tail)  tail (cddr tail))
        else if (eq item '&environment)
          do (unless (and (cdr tail) (symbolp (cadr tail)))
               (error "Invalid &ENVIRONMENT in compiler macro lambda list: ~S" lambda-list))
             (setf environment (cadr tail)  tail (cddr tail))
        else
          do (push item result)
             (setf tail (cdr tail))
        finally (return (values (nreverse result) whole environment))))

(defun lookup-compiler-macro (name)
  "Look up compiler macro NAME in the global compiler-macro environment."
  (gethash name *compiler-macro-table*))

(defun compiler-macro-function (name &optional environment)
  "Return the compiler macro function registered for NAME, or NIL."
  (declare (ignore environment))
  (lookup-compiler-macro name))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  "Set NAME's compiler macro function in the global compiler macro table."
  (declare (ignore environment))
  (if new-function
      (setf (gethash name *compiler-macro-table*) new-function)
      (remhash name *compiler-macro-table*))
  new-function)
