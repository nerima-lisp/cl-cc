;;;; dcg-builtins.lisp — DCG combinator builtins and token matching

(in-package :cl-cc/prolog)

(defun %dcg-alt (args env k)
  "Try each alternative rule; Args: (alt1 alt2 ... s-in s-out)."
  (let ((stream-args (last args 2)))
    (unless (and stream-args (ldiff args stream-args))
      (error "dcg-alt requires at least one alternative and two stream arguments: ~S" args))
    (dolist (alt (ldiff args stream-args))
      (solve-goal (list alt (first stream-args) (second stream-args)) env k))))

(defun %dcg-continue-with-stream (s-out env k rest)
  "Advance the output stream and resume K with the unified environment."
  (when-unify-succeeds (new-env s-out rest env)
    (funcall k new-env)))

(define-prolog-builtin %dcg-opt (rule s-in s-out)
  "Optional match (0 or 1); Args: (rule s-in s-out)."
  (solve-goal (list rule s-in s-out) env k)
  (%dcg-continue-with-stream s-out env k s-in))

(defun %dcg-star-loop (rule s-out k current-in current-env)
  "Recursive helper for dcg-star: consume input with RULE until fixed point."
  (when-unify-succeeds (eps-env current-in s-out current-env)
    (funcall k eps-env))
  (let ((mid (gensym "?MID")))
    (solve-goal (list rule current-in mid) current-env
                (lambda (new-env)
                  (let ((resolved-mid (logic-substitute mid new-env))
                        (resolved-in  (logic-substitute current-in new-env)))
                    (unless (equal resolved-mid resolved-in)
                      (%dcg-star-loop rule s-out k mid new-env)))))))

(define-prolog-builtin %dcg-star (rule s-in s-out)
  "Kleene star (zero or more); Args: (rule s-in s-out)."
  (%dcg-star-loop rule s-out k s-in env))

(define-prolog-builtin %dcg-plus (rule s-in s-out)
  "One or more matches; Args: (rule s-in s-out)."
  (let ((mid (gensym "?MID")))
    (solve-goal (list rule s-in mid) env
                (lambda (env1)
                  (%dcg-star-loop rule s-out k mid env1)))))

(defun %dcg-skip-loop (s-out env k remaining)
  "Recursive helper for dcg-error-recovery: skip until a sync token."
  (cond
    ((null remaining)
     (%dcg-continue-with-stream s-out env k nil))
    (t
     (destructuring-bind (token . rest) remaining
       (if (and (consp token)
                (member (car token) *dcg-sync-tokens*))
           (%dcg-continue-with-stream s-out env k remaining)
           (%dcg-skip-loop s-out env k rest))))))

(define-prolog-builtin %dcg-error-recovery (s-in s-out)
  "Skip tokens until a sync point; Args: (s-in s-out)."
  (%dcg-skip-loop s-out env k (logic-substitute s-in env)))

(defmacro when-dcg-token-stream-matches ((token rest expected-type env s-in) &body body)
  "Run BODY with TOKEN and REST when S-IN starts with EXPECTED-TYPE."
  (let ((expected-type-value (gensym "EXPECTED-TYPE-"))
        (input (gensym "INPUT-")))
    `(let ((,expected-type-value ,expected-type)
           (,input (logic-substitute ,s-in ,env)))
       (when (consp ,input)
         (destructuring-bind (,token . ,rest) ,input
           (declare (ignorable ,token ,rest))
           (when (and (consp ,token)
                      (eq (car ,token) ,expected-type-value))
             (locally
               ,@body)))))))

(define-prolog-builtin %dcg-token-match (expected-type s-in s-out)
  "Match a token by type; Args: (expected-type s-in s-out)."
  (when-dcg-token-stream-matches (token rest expected-type env s-in)
    (%dcg-continue-with-stream s-out env k rest)))

(define-prolog-builtin %dcg-token-match-value (expected-type value-var s-in s-out)
  "Match a token by type and bind its value; Args: (expected-type value-var s-in s-out)."
  (when-dcg-token-stream-matches (token rest expected-type env s-in)
    (when-unify-succeeds (val-env value-var (cdr token) env)
      (%dcg-continue-with-stream s-out val-env k rest))))

(eval-when (:load-toplevel :execute)
  (register-builtin-specs *builtin-predicates* *dcg-builtin-specs*))
