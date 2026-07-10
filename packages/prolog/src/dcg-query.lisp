;;;; dcg-query.lisp — DCG public query API

(in-package :cl-cc/prolog)

(defun phrase (rule-name input)
  "Parse INPUT (a list of tokens) with DCG RULE-NAME.
   Returns the first solution's remaining input, or NIL on failure."
  (first (%collect-projected-solutions (list rule-name input '?dcg-rest)
                                       (lambda (env)
                                         (logic-substitute '?dcg-rest env))
                                       1)))

(defun phrase-all (rule-name input)
  "Return all parse results for DCG RULE-NAME applied to INPUT."
  (%collect-projected-solutions (list rule-name input '?dcg-rest)
                                (lambda (env)
                                  (logic-substitute '?dcg-rest env))))
