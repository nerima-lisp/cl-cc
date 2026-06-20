;;;; dcg-query.lisp — DCG public query API

(in-package :cl-cc/prolog)

(defun %phrase-solutions (rule-name input &optional limit)
  (let ((goal (list rule-name input '?dcg-rest))
        (projection (lambda (env)
                      (logic-substitute '?dcg-rest env))))
    (if limit
        (%collect-projected-solutions goal projection limit)
        (%collect-projected-solutions goal projection))))

(defun phrase (rule-name input)
  "Parse INPUT (a list of tokens) with DCG RULE-NAME.
   Returns the first solution's remaining input, or NIL on failure."
  (first (%phrase-solutions rule-name input 1)))

(defun phrase-all (rule-name input)
  "Return all parse results for DCG RULE-NAME applied to INPUT."
  (%phrase-solutions rule-name input))
