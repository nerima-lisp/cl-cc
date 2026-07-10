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

(defun %resolve-logic-var-binding (logic-var env)
  "Return the bound value for LOGIC-VAR, or LOGIC-VAR when unbound."
  (let ((binding (assoc logic-var env)))
    (if binding
        (cdr binding)
        logic-var)))

;;; Enhanced Unification with Occurs Check

(declaim (ftype (function (t t &optional t) t) unify))

(defun %occurs-check-term (var term env)
  "Walk TERM and report whether VAR appears after resolving bindings."
  (cond ((logic-var-p term)
         (let ((resolved (%resolve-logic-var-binding term env)))
           (if (eq resolved term)
               (eq var term)
               (%occurs-check-term var resolved env))))
        ((consp term)
         (or (%occurs-check-term var (car term) env)
             (%occurs-check-term var (cdr term) env)))
        (t
         nil)))

(defun occurs-check (var term env)
  "Check if VAR occurs in TERM (prevents infinite structures like ?X = f(?X))."
  (%occurs-check-term var term env))

(defmacro when-unify-succeeds ((result left right env) &body body)
  "Bind RESULT to the environment produced by unifying LEFT and RIGHT."
  `(let ((,result (unify ,left ,right ,env)))
     (unless (eq ,result :unify-fail)
       ,@body)))

(defmacro when-unify-fails ((left right env) &body body)
  "Run BODY only when unifying LEFT and RIGHT fails."
  `(when (eq (unify ,left ,right ,env) :unify-fail)
     ,@body))

(defun %unify-logic-var (var term env)
  "Unify VAR with TERM, applying the occurs check before extending ENV."
  (let ((binding (assoc var env)))
    (if binding
        (unify (cdr binding) term env)
        (if (occurs-check var term env)
            :unify-fail
            (acons var term env)))))

(defun %unify-conses (term1 term2 env)
  "Unify two cons cells by chaining the head environment into the tail."
  (let ((tail-env :unify-fail))
    (when-unify-succeeds (head-env (car term1) (car term2) env)
      (setf tail-env (unify (cdr term1) (cdr term2) head-env)))
    tail-env))

(defun unify (term1 term2 &optional (env nil))
  "Unify two terms, returning updated environment or :UNIFY-FAIL on failure.
   TERM1 and TERM2 can be atoms, logic variables (?x), or cons cells.
   NOTE: Returns NIL for a successful empty environment; failure returns the
   internal :UNIFY-FAIL sentinel."
  (cond
    ((and (logic-var-p term1) (logic-var-p term2))
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
  (cond ((logic-var-p template)
         (let ((resolved (%resolve-logic-var-binding template env)))
           (if (eq resolved template)
               template
               (logic-substitute resolved env))))
        ((consp template)
         (cons (logic-substitute (car template) env)
               (logic-substitute (cdr template) env)))
        (t
         template)))
