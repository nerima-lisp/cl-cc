(in-package :cl-cc/test)

(defun %prolog-fact-forms (facts)
  (mapcar (lambda (fact)
            `(cl-cc/prolog:def-rule ,fact))
          facts))

(defun %prolog-rule-forms (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (head &rest rule-body) rule
              `(cl-cc:def-rule ,head ,@rule-body)))
          rules))

(defun %dcg-token-rule-forms (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (name token-type) rule
              `(cl-cc/prolog:def-rule
                   (,name ((,token-type . ?v) . ?rest) ?rest))))
          rules))

(defun %dcg-rule-forms (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (name &rest rule-body) rule
              `(cl-cc:def-dcg-rule ,name ,@rule-body)))
          rules))

(defmacro with-prolog-facts (facts &body body)
  "Run BODY in a fresh Prolog DB after installing FACTS."
  `(%with-prolog-fixture
     ,@(%prolog-fact-forms facts)
     ,@body))

(defmacro with-prolog-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after installing RULES."
  `(%with-prolog-fixture
     ,@(%prolog-rule-forms rules)
     ,@body))

(defmacro with-dcg-token-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after installing token-consuming DCG rules.

RULES is a list of (NAME TOKEN-TYPE) pairs; each pair becomes a fact of the
form (NAME ((TOKEN-TYPE . ?V) . ?REST) ?REST)."
  `(%with-prolog-fixture
     ,@(%dcg-token-rule-forms rules)
     ,@body))

(defmacro with-dcg-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after defining DCG rules from RULES.

RULES is a list of (NAME &rest BODY-FORMS) entries that expand to
cl-cc:def-dcg-rule forms."
  `(%with-prolog-fixture
     ,@(%dcg-rule-forms rules)
     ,@body))
