;;;; dcg-builtins.lisp — DCG combinator builtins and token matching

(in-package :cl-cc/prolog)

(defun %dcg-alt (args env k)
  "Try each alternative rule; Args: (alt1 alt2 ... s-in s-out)."
  (let ((s-in (car (last args 2)))
        (s-out (car (last args 1)))
        (alts (butlast args 2)))
    (dolist (alt alts)
      (solve-goal (list alt s-in s-out) env k))))

(defun %dcg-continue-with-stream (s-out env k rest)
  "Advance the output stream and resume K with the unified environment."
  (when-unify-succeeds (new-env s-out rest env)
    (funcall k new-env)))

(defun %dcg-opt (args env k)
  "Optional match (0 or 1); Args: (rule s-in s-out)."
  (destructuring-bind (rule s-in s-out) args
    (solve-goal (list rule s-in s-out) env k)
    (%dcg-continue-with-stream s-out env k s-in)))

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

(defun %dcg-star (args env k)
  "Kleene star (zero or more); Args: (rule s-in s-out)."
  (destructuring-bind (rule s-in s-out) args
    (%dcg-star-loop rule s-out k s-in env)))

(defun %dcg-plus (args env k)
  "One or more matches; Args: (rule s-in s-out)."
  (destructuring-bind (rule s-in s-out) args
    (let ((mid (gensym "?MID")))
      (solve-goal (list rule s-in mid) env
                  (lambda (env1)
                    (solve-goal (list 'dcg-star rule mid s-out) env1 k))))))

(defun %dcg-skip-loop (s-out env k remaining)
  "Recursive helper for dcg-error-recovery: skip until a sync token."
  (cond
    ((null remaining)
     (%dcg-continue-with-stream s-out env k nil))
    ((and (consp (car remaining))
          (member (caar remaining) *dcg-sync-tokens*))
     (%dcg-continue-with-stream s-out env k remaining))
    (t (%dcg-skip-loop s-out env k (cdr remaining)))))

(defun %dcg-error-recovery (args env k)
  "Skip tokens until a sync point; Args: (s-in s-out)."
  (destructuring-bind (s-in s-out) args
    (%dcg-skip-loop s-out env k (logic-substitute s-in env))))

(defun %dcg-token-stream-match (expected-type env s-in)
  "Return the matched token cell and the remaining stream when S-IN starts with EXPECTED-TYPE."
  (let ((input (logic-substitute s-in env)))
    (when (and (consp input)
               (consp (car input))
               (eq (caar input) expected-type))
      (values (car input) (cdr input)))))

(defun %dcg-token-match (args env k)
  "Match a token by type; Args: (expected-type s-in s-out)."
  (destructuring-bind (expected-type s-in s-out) args
    (multiple-value-bind (_token rest)
        (%dcg-token-stream-match expected-type env s-in)
      (declare (ignore _token))
      (%dcg-continue-with-stream s-out env k rest))))

(defun %dcg-token-match-value (args env k)
  "Match a token by type and bind its value; Args: (expected-type value-var s-in s-out)."
  (destructuring-bind (expected-type value-var s-in s-out) args
    (multiple-value-bind (token rest)
        (%dcg-token-stream-match expected-type env s-in)
      (when token
        (when-unify-succeeds (val-env value-var (cdr token) env)
          (%dcg-continue-with-stream s-out val-env k rest))))))

(eval-when (:load-toplevel :execute)
  (register-builtin-specs *builtin-predicates* *dcg-builtin-specs*))
