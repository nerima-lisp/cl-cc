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

(defmacro with-prolog-solution-collection ((emit results &key limit) &body body)
  "Collect values emitted by EMIT into RESULTS, preserving Prolog cut semantics."
  (let ((count (gensym "COUNT-")))
    `(let ((,results nil)
           (,count 0))
       (flet ((,emit (value)
                (when (or (null ,limit) (< ,count ,limit))
                  (push value ,results)
                  (incf ,count)
                  (when (and ,limit (>= ,count ,limit))
                    (signal 'prolog-cut)))))
         ,@body
         (nreverse ,results)))))

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL should be a list (predicate arg1 arg2 ...) or a bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  (cond
    ((null goal)
     (funcall k env))
    ((prolog-cut-goal-p goal)
     (funcall k env)
     (signal 'prolog-cut))
    (t
     (solve-atomic-goal goal env k))))

(defun solve-atomic-goal (goal env k)
  "Dispatch an atomic GOAL to a built-in predicate or a registered rule."
  (multiple-value-bind (predicate args) (prolog-compound-form-parts goal)
    (let ((builtin (gethash predicate *builtin-predicates*)))
      (if builtin
          (progn
            (funcall builtin args env k)
            t)
          (solve-prolog-rules predicate args env k)))))

(defun solve-prolog-rule (rule args env k)
  "Try RULE against ARGS, returning :CUT when the rule body cuts."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule)))
    (multiple-value-bind (_predicate head-args) (prolog-compound-form-parts head)
      (declare (ignore _predicate))
      (when-unify-succeeds (new-env args head-args env)
        (when (eq (with-prolog-cut-handling
                    (if body
                        (solve-conjunction body new-env k)
                        (funcall k new-env)))
                  :cut)
          :cut)))))

(defun solve-prolog-rules (predicate args env k)
  "Try every rule registered under PREDICATE and stop early on cut."
  (dolist (rule (gethash predicate *prolog-rules*))
    (when (eq (solve-prolog-rule rule args env k) :cut)
      (return-from solve-prolog-rules :cut))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (destructuring-bind (goal . rest-goals) goals
        (with-prolog-cut-handling
          (solve-goal goal env
                      (lambda (new-env)
                        (solve-conjunction rest-goals new-env k)))))))

(defun %collect-projected-solutions (goal &optional projector limit)
  "Collect solutions for GOAL, optionally projecting each environment and
   stopping after LIMIT."
  (with-prolog-solution-collection (emit results :limit limit)
    (with-prolog-cut-handling
      (solve-goal goal nil
                  (lambda (env)
                    (emit (if projector
                              (funcall projector env)
                              (logic-substitute goal env))))))))

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (%collect-projected-solutions goal))

(eval-when (:load-toplevel :execute)
  (register-builtin-specs *builtin-predicates* *builtin-predicate-specs*))
