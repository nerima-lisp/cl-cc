(in-package :cl-cc/test)

(defmacro with-prolog-facts (facts &body body)
  "Run BODY in a Prolog fixture after installing FACTS."
  `(%with-prolog-fixture
     ,@(mapcar (lambda (fact)
                 `(cl-cc/prolog:def-rule ,fact))
               facts)
     ,@body))

(defmacro with-prolog-rules (rules &body body)
  "Run BODY in a Prolog fixture after installing RULES."
  `(%with-prolog-fixture
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (head &rest rule-body) rule
                   `(cl-cc/prolog:def-rule ,head ,@rule-body)))
               rules)
     ,@body))

(defmacro with-dcg-rules (rules &body body)
  "Run BODY in a Prolog fixture after defining DCG rules from RULES.

RULES is a list of (NAME &rest BODY-FORMS) entries that expand to
cl-cc/prolog:def-dcg-rule forms."
  `(%with-prolog-fixture
     ,@(mapcar (lambda (rule)
                 (destructuring-bind (name &rest rule-body) rule
                   `(cl-cc/prolog:def-dcg-rule ,name ,@rule-body)))
               rules)
     ,@body))
