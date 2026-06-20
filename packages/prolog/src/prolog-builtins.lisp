(in-package :cl-cc/prolog)

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

(defvar *builtin-predicates* (make-hash-table :test 'eq))

(declaim (ftype function subst-for-eval eval-lisp-condition
                      solve-conjunction solve-goal our-eval
                      register-builtin-specs))

(defun register-builtin-specs (table specs)
  "Populate TABLE from SPECS entries of the form (NAME HANDLER)."
  (dolist (spec specs)
    (destructuring-bind (name handler) spec
      (setf (gethash name table)
            (symbol-function handler)))))

(defun prolog-cut-handler (args env k)
  (declare (ignore args))
  (funcall k env)
  (signal 'prolog-cut))

(defun prolog-or-handler (args env k)
  (dolist (alt args)
    (solve-goal alt env k)))

(defun prolog-unify-handler (args env k)
  (destructuring-bind (left right) args
    (when-unify-succeeds (new-env left right env)
      (funcall k new-env))))

(defun prolog-not-unify-handler (args env k)
  (destructuring-bind (left right) args
    (when (unify-failed-p (unify left right env))
      (funcall k env))))

(defun prolog-when-handler (args env k)
  (when (eval-lisp-condition (first args) env)
    (funcall k env)))

(defun subst-for-eval (form env)
  "Like logic-substitute, but wraps substituted non-self-evaluating symbols in
   (quote ...) so they survive CL eval, and skips (quote ...) forms."
  (cond
    ((logic-var-p form)
      (let ((val (logic-substitute form env)))
        (if (logic-var-p val)
            val
            (if (and (symbolp val) val (not (keywordp val)))
                `(quote ,val)
                val))))
    ((and (consp form) (eq (car form) 'quote))
     form)
    ((consp form)
     (cons (subst-for-eval (car form) env)
           (subst-for-eval (cdr form) env)))
    (t form)))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules."
  (handler-case
      (let ((substituted (subst-for-eval condition env)))
        (typecase substituted
          (cons (our-eval substituted))
          (t substituted)))
    (error () nil)))
