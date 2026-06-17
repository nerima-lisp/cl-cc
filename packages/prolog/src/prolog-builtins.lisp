(in-package :cl-cc/prolog)

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

(defvar *builtin-predicates*)

(declaim (ftype function subst-for-eval eval-lisp-condition
                      solve-conjunction solve-goal our-eval))

(defmacro def-binary-prolog-handler (name (left right) &body body)
  "Define a binary builtin handler with ARGS destructured once at the top."
  `(defun ,name (args env k)
     (destructuring-bind (,left ,right) args
       ,@body)))

(defun prolog-cut-handler (args env k)
  (declare (ignore args))
  (funcall k env)
  (signal 'prolog-cut))

(defun prolog-and-handler (args env k)
  (solve-conjunction args env k))

(defun prolog-or-handler (args env k)
  (dolist (alt args)
    (solve-goal alt env k)))

(def-binary-prolog-handler prolog-unify-handler (left right)
  (when-unify-succeeds (new-env left right env)
    (funcall k new-env)))

(def-binary-prolog-handler prolog-not-unify-handler (left right)
  (let ((v1 (logic-substitute left env))
        (v2 (logic-substitute right env)))
    (when (not (equal v1 v2))
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

(defun %invoke-builtin-goal (predicate args env k)
  "Invoke the built-in predicate handler for PREDICATE, if one exists."
  (let ((builtin (gethash predicate *builtin-predicates*)))
    (when builtin
      (funcall builtin args env k)
      t)))

(eval-when (:load-toplevel :execute)
  (setf *builtin-predicates*
        (make-builtin-predicate-table *builtin-predicate-specs*)))
