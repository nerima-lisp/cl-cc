(in-package :cl-cc/test)

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs."
  (loop for (type val) on type-value-pairs by #'cddr
        collect (cons type val)))

(defmacro dcg-goal (name &rest args)
  "Build a raw DCG builtin goal form for NAME and ARGS."
  `(list ',name ,@args))

(defmacro with-prolog-results ((results thunk &key projector) &body body)
  "Bind RESULTS to every value emitted by THUNK."
  `(let ((,results nil))
     (handler-case
         (funcall ,thunk
                  (lambda (result)
                    (push ,(if projector
                               `(funcall ,projector result)
                               'result)
                          ,results)))
       (cl-cc/prolog:prolog-cut ()))
     (let ((,results (nreverse ,results)))
       (declare (ignorable ,results))
       ,@body)))

(defmacro with-prolog-goal-results ((results goal &key projector) &body body)
  "Bind RESULTS to every value emitted while solving GOAL."
  `(with-prolog-results (,results
                         (lambda (emit)
                           (cl-cc:solve-goal ,goal nil emit))
                         :projector ,projector)
      ,@body))

(defun %expand-prolog-projection-form (form env)
  (cond
    ((and (symbolp form)
          (let ((name (symbol-name form)))
            (and (> (length name) 0)
                 (char= (char name 0) #\?))))
     `(cl-cc:logic-substitute ',form ,env))
    ((consp form)
     `(,(%expand-prolog-projection-form (car form) env)
       ,@(mapcar (lambda (subform)
                   (%expand-prolog-projection-form subform env))
                 (cdr form))))
    (t form)))

(defmacro with-prolog-goal-projections ((results goal projection-form) &body body)
  "Bind RESULTS to GOAL solutions projected through PROJECTION-FORM."
  `(with-prolog-goal-results (,results ,goal
                               :projector ,(when projection-form
                                             `(lambda (env)
                                                ,(%expand-prolog-projection-form projection-form 'env))))
       ,@body))

(defmacro assert-prolog-query-count= (goal expected-count)
  "Assert that GOAL yields EXPECTED-COUNT solutions."
  `(with-prolog-goal-results (results ,goal)
     (assert-= ,expected-count (length results))))

(defmacro with-prolog-unify-result ((env left right &optional initial-env) &body body)
  "Bind ENV to the result of unifying LEFT and RIGHT."
  `(let ((,env (cl-cc:unify ,left ,right ,initial-env)))
     (declare (ignorable ,env))
     ,@body))

(defmacro %with-prolog-fixture (setup-forms &body body)
  `(with-fresh-prolog
     ,@setup-forms
     ,@body))

(defmacro with-prolog-facts (facts &body body)
  "Run BODY in a fresh Prolog DB after installing FACTS."
  `(%with-prolog-fixture
       ,(mapcar (lambda (fact)
                  `(cl-cc/prolog:def-fact ,fact))
                facts)
     ,@body))

(defmacro with-prolog-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after installing RULES."
  `(%with-prolog-fixture
       ,(mapcar (lambda (rule)
                  (destructuring-bind (head &rest rule-body) rule
                    `(cl-cc:def-rule ,head ,@rule-body)))
                rules)
     ,@body))

(defmacro with-dcg-token-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after installing token-consuming DCG rules.

RULES is a list of (NAME TOKEN-TYPE) pairs; each pair becomes a fact of the
form (NAME ((TOKEN-TYPE . ?V) . ?REST) ?REST)."
  `(%with-prolog-fixture
       ,(mapcar (lambda (rule)
                  (destructuring-bind (name token-type) rule
                    `(cl-cc/prolog:def-fact
                      (,name ((,token-type . ?v) . ?rest) ?rest))))
                rules)
     ,@body))

(defmacro with-dcg-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after defining DCG rules from RULES.

RULES is a list of (NAME &rest BODY-FORMS) entries that expand to
cl-cc:def-dcg-rule forms."
  `(%with-prolog-fixture
       ,(mapcar (lambda (rule)
                  (destructuring-bind (name &rest rule-body) rule
                    `(cl-cc:def-dcg-rule ,name ,@rule-body)))
                rules)
     ,@body))

(defmacro assert-dcg-solves (goal &optional projection-var)
  "Assert that GOAL has at least one substitution."
  `(with-prolog-goal-projections (results ,goal ,projection-var)
     (assert-true results)))

(defmacro assert-dcg-first-result= (goal expected &optional projection-var)
  "Assert that GOAL returns EXPECTED as its first remaining stream."
  `(with-prolog-goal-projections (results ,goal ,projection-var)
     (assert-true results)
     (assert-equal ,expected (first results))))

(defmacro assert-dcg-result-contains (goal expected &optional projection-var)
  "Assert that GOAL returns EXPECTED in its remaining streams."
  `(with-prolog-goal-projections (results ,goal ,projection-var)
     (assert-true (member ,expected results :test #'equal))))

(defmacro assert-dcg-no-results (goal &optional projection-var)
  "Assert that GOAL returns no substitutions."
  `(with-prolog-goal-projections (results ,goal ,projection-var)
     (assert-null results)))
