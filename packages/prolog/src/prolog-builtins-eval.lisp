(in-package :cl-cc/prolog)

;;; Prolog builtins: Lisp condition preparation and evaluation.
;;;
;;; This file is intentionally separate from predicate dispatch so the data
;;; table and handler wiring in prolog-builtins.lisp stay focused on CPS flow.

(declaim (ftype function subst-for-eval eval-lisp-condition))

(defun subst-for-eval (form env)
  "Like logic-substitute, but wraps substituted non-self-evaluating symbols in
   (quote ...) so they survive CL eval, and skips (quote ...) forms."
  (labels ((walk (current)
             (cond
               ((logic-var-p current)
                (let ((value (logic-substitute current env)))
                  (cond
                    ((logic-var-p value) value)
                    ((and (symbolp value)
                          (not (null value))
                          (not (keywordp value)))
                     `(quote ,value))
                    (t value))))
               ((and (consp current)
                     (eq (car current) 'quote)
                     (consp (cdr current))
                     (null (cddr current)))
                current)
               ((consp current)
                (cons (walk (car current))
                      (walk (cdr current))))
               (t current))))
    (walk form)))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules via our-eval."
  (handler-case
      (let ((prepared (subst-for-eval condition env)))
        (if (consp prepared)
            (our-eval prepared)
            prepared))
    (error () nil)))
