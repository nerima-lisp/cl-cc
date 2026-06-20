(in-package :cl-cc/prolog)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Solver and Query Interface
;;;
;;; Contains: solve-goal, solve-conjunction,
;;; %collect-projected-solutions, query-all.
;;;
;;; Core terms/unification live in prolog-unification.lisp. Built-in predicate
;;; dispatch and Lisp-condition evaluation live in prolog-builtins.lisp. Rule
;;; application helpers live here.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(declaim (ftype function solve-goal solve-conjunction solve-prolog-rule
                solve-prolog-rules solve-atomic-goal query-all))

(defmacro with-prolog-cut-handling (&body body)
  `(handler-case
       (progn ,@body)
     (prolog-cut ()
       :cut)))

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL should be a list (predicate arg1 arg2 ...) or a bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  (cond
    ((null goal)
     (funcall k env))
    ((eq goal '!)
     (funcall k env)
     (signal 'prolog-cut))
    (t
     (solve-atomic-goal goal env k))))

(defun solve-atomic-goal (goal env k)
  "Dispatch an atomic GOAL to a built-in predicate or a registered rule."
  (let ((predicate (car goal))
        (args (cdr goal)))
    (or (let ((builtin (gethash predicate *builtin-predicates*)))
          (when builtin
            (funcall builtin args env k)
            t))
        (solve-prolog-rules predicate args env k))))

(defun solve-prolog-rule (rule args env k)
  "Try RULE against ARGS, returning :CUT when the rule body cuts."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule)))
    (when-unify-succeeds (new-env args (cdr head) env)
      (when (eq (with-prolog-cut-handling
                  (if body
                      (solve-conjunction body new-env k)
                      (funcall k new-env)))
                :cut)
        :cut))))

(defun solve-prolog-rules (predicate args env k)
  "Try every rule registered under PREDICATE and stop early on cut."
  (dolist (rule (gethash predicate *prolog-rules*))
    (when (eq (solve-prolog-rule rule args env k) :cut)
      (return-from solve-prolog-rules :cut))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (with-prolog-cut-handling
        (solve-goal (car goals) env
                    (lambda (new-env)
                      (solve-conjunction (cdr goals) new-env k))))))

(defun %collect-projected-solutions (goal &optional projector limit)
  "Collect solutions for GOAL, optionally projecting each environment and
   stopping after LIMIT."
  (let ((solutions nil)
        (count 0))
    (labels ((emit-solution (env)
               (when (or (null limit) (< count limit))
                 (push (if projector
                           (funcall projector env)
                           (logic-substitute goal env))
                       solutions)
                 (incf count)
                 (when (and limit (>= count limit))
                   (signal 'prolog-cut)))))
      (with-prolog-cut-handling
        (solve-goal goal nil #'emit-solution)))
    (nreverse solutions)))

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (%collect-projected-solutions goal))

(eval-when (:load-toplevel :execute)
  (register-builtin-specs *builtin-predicates* *builtin-predicate-specs*))
