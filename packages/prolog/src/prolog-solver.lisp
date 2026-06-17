(in-package :cl-cc/prolog)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Solver and Query Interface
;;;
;;; Contains: solve-goal, solve-conjunction,
;;; %collect-projected-solutions,
;;; query-all/one/first-n.
;;;
;;; Core terms/unification live in prolog-unification.lisp. Built-in predicate
;;; dispatch and Lisp-condition evaluation live in prolog-builtins.lisp. Rule
;;; application helpers live here.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(declaim (ftype function cut-goal-p
                solve-goal solve-conjunction solve-prolog-rules
                %solve-prolog-rule
                query-all
                query-one
                query-first-n))

(defun cut-goal-p (goal)
  (and (symbolp goal)
       (string= (symbol-name goal) "!")))

(defmacro with-prolog-cut-handling (&body body)
  `(handler-case
       (progn ,@body)
     (prolog-cut ()
       :cut)))

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL should be a list (predicate arg1 arg2 ...) or a bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  (if (cut-goal-p goal)
      (progn
        (funcall k env)
        (signal 'prolog-cut))
      (let ((predicate (car goal))
            (args (cdr goal)))
        (when (%invoke-builtin-goal predicate args env k)
          (return-from solve-goal))
        (solve-prolog-rules predicate args env k))))

(defun solve-prolog-rules (predicate args env k)
  "Try every rule registered under PREDICATE and stop early on cut."
  (dolist (rule (gethash predicate *prolog-rules*))
    (when (eq (%solve-prolog-rule rule args env k) :cut)
      (return-from solve-prolog-rules :cut))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (with-prolog-cut-handling
        (solve-goal (car goals) env
                    (lambda (new-env)
                      (solve-conjunction (cdr goals) new-env k))))))

(defun %solve-prolog-rule (rule args env k)
  "Attempt to solve RULE against ARGS and call K for each matching environment."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule)))
    (when-unify-succeeds (new-env args (cdr head) env)
      (with-prolog-cut-handling
        (if body
            (solve-conjunction body new-env k)
            (funcall k new-env))))))

(defun %collect-projected-solutions (goal &optional projector limit)
  "Collect solutions for GOAL, optionally projecting each environment and
   stopping after LIMIT."
  (let ((solutions nil)
        (count 0))
    (with-prolog-cut-handling
      (solve-goal goal nil
                  (lambda (env)
                    (when (or (null limit) (< count limit))
                      (push (if projector
                                (funcall projector env)
                                (logic-substitute goal env))
                            solutions)
                      (incf count)
                      (when (and limit (>= count limit))
                        (signal 'prolog-cut))))))
    (nreverse solutions)))

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (%collect-projected-solutions goal))

(defun query-one (goal)
  "Return first solution for GOAL, or NIL if no solution exists."
  (first (%collect-projected-solutions goal nil 1)))

(defun query-first-n (goal n)
  "Return the first N solutions for GOAL."
  (%collect-projected-solutions goal nil n))
