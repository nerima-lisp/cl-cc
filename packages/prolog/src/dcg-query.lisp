;;;; dcg-query.lisp — DCG public query API

(in-package :cl-cc/prolog)

(defun %phrase-solutions (rule-name input &key limit)
  "Collect projected DCG results for RULE-NAME applied to INPUT."
  (%collect-projected-solutions
   (list rule-name input '?dcg-rest)
   (lambda (env)
     (logic-substitute '?dcg-rest env))
   limit))

(defun phrase (rule-name input)
  "Parse INPUT (a list of tokens) with DCG RULE-NAME.
   Returns the first solution's remaining input, or NIL on failure."
  (first (%phrase-solutions rule-name input :limit 1)))

(defun phrase-all (rule-name input)
  "Return all parse results for DCG RULE-NAME applied to INPUT."
  (%phrase-solutions rule-name input))
