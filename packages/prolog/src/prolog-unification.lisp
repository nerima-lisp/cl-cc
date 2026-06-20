;;; packages/prolog/src/prolog-unification.lisp
;;;
;;; Logic variable representation and unification/substitution helpers.

(in-package :cl-cc/prolog)

;;; Logic Variables

(defun logic-var-p (x)
  "Check if X is a logic variable (symbols starting with ?)."
  (and (symbolp x)
       (let ((name (symbol-name x)))
         (and (> (length name) 0)
              (char= (char name 0) #\?)))))

(defun %walk-prolog-term (term on-variable on-atom on-cons)
  "Walk TERM and dispatch logic variables, atoms, and cons cells through callbacks."
  (cond ((logic-var-p term)
         (funcall on-variable term))
        ((consp term)
         (funcall on-cons term))
        (t
         (funcall on-atom term))))

(defun %resolve-logic-binding (var env)
  "Return VAR's bound value from ENV, or VAR itself when unbound."
  (let ((binding (assoc var env)))
    (if binding
        (cdr binding)
        var)))

;;; Enhanced Unification with Occurs Check

(declaim (ftype (function (t t &optional t) t) unify))

(defun %occurs-check-term (var term env)
  "Walk TERM and report whether VAR appears after resolving bindings."
  (%walk-prolog-term term
                     (lambda (logic-var)
                       (let ((resolved (%resolve-logic-binding logic-var env)))
                         (if (eq resolved logic-var)
                             (eq var logic-var)
                             (%occurs-check-term var resolved env))))
                     (lambda (_atom)
                       (declare (ignore _atom))
                       nil)
                     (lambda (cons-node)
                       (or (%occurs-check-term var (car cons-node) env)
                           (%occurs-check-term var (cdr cons-node) env)))))

(defun occurs-check (var term env)
  "Check if VAR occurs in TERM (prevents infinite structures like ?X = f(?X))."
  (%occurs-check-term var term env))

(defun unify-failed-p (result)
  "Return T if RESULT represents a unification failure (distinguished from empty env)."
  (eq result :unify-fail))

(defmacro when-unify-succeeds ((result left right env) &body body)
  "Bind RESULT to the environment produced by unifying LEFT and RIGHT."
  `(let ((,result (unify ,left ,right ,env)))
     (unless (unify-failed-p ,result)
       ,@body)))

(defun %unify-logic-vars (term1 term2 env)
  "Unify TERM1 and TERM2 after resolving any existing bindings."
  (let ((binding1 (assoc term1 env))
        (binding2 (assoc term2 env)))
    (cond ((and binding1 binding2)
           (unify (cdr binding1) (cdr binding2) env))
          (binding1
           (unify (cdr binding1) term2 env))
          (binding2
           (unify term1 (cdr binding2) env))
          (t
           (acons term1 term2 env)))))

(defun %unify-logic-var (var term env)
  "Unify VAR with TERM, applying the occurs check before extending ENV."
  (let ((binding (assoc var env)))
    (if binding
        (unify (cdr binding) term env)
        (if (occurs-check var term env)
            :unify-fail
            (acons var term env)))))

(defun %unify-conses (term1 term2 env)
  "Unify two cons cells by unifying the car before recursing on the cdr."
  (let ((head-env (unify (car term1) (car term2) env)))
    (if (unify-failed-p head-env)
        :unify-fail
        (unify (cdr term1) (cdr term2) head-env))))

(defun unify (term1 term2 &optional (env nil))
  "Unify two terms, returning updated environment or :UNIFY-FAIL on failure.
   TERM1 and TERM2 can be atoms, logic variables (?x), or cons cells.
   NOTE: Returns NIL for a successful empty environment (not failure — use
   unify-failed-p to distinguish failure from an empty environment)."
  (cond
    ((and (logic-var-p term1) (logic-var-p term2))
     (%unify-logic-vars term1 term2 env))
    ((logic-var-p term1)
     (%unify-logic-var term1 term2 env))
    ((logic-var-p term2)
     (unify term2 term1 env))
    ((and (consp term1) (consp term2))
     (%unify-conses term1 term2 env))
    ((equal term1 term2) env)
    (t :unify-fail)))

;;; Variable Substitution

(defun logic-substitute (template env)
  "Substitute logic variables in TEMPLATE using bindings from ENV."
  (%walk-prolog-term template
                     (lambda (var)
                       (let ((resolved (%resolve-logic-binding var env)))
                         (if (eq resolved var)
                             var
                             (logic-substitute resolved env))))
                     #'identity
                     (lambda (cons-node)
                       (cons (logic-substitute (car cons-node) env)
                             (logic-substitute (cdr cons-node) env)))))
