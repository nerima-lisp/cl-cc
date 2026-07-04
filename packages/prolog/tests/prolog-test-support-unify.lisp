(in-package :cl-cc/test)

(defmacro with-prolog-unify-result ((env left right &optional initial-env) &body body)
  "Bind ENV to the result of unifying LEFT and RIGHT."
  `(let ((,env (cl-cc:unify ,left ,right ,initial-env)))
     (declare (ignorable ,env))
     ,@body))

(defmacro assert-prolog-unify-result= (expected left right &optional initial-env)
  "Assert that unifying LEFT and RIGHT yields EXPECTED."
  `(assert-equal ,expected (cl-cc:unify ,left ,right ,initial-env)))

(defmacro assert-prolog-unify-fails (left right &optional initial-env)
  "Assert that unifying LEFT and RIGHT fails."
  `(assert-prolog-unify-result= :unify-fail ,left ,right ,initial-env))

(defun collect-prolog-unify-handler-results (args &optional initial-env)
  "Collect environments emitted by the CPS prolog-unify-handler."
  (let ((results nil))
    (funcall #'cl-cc/prolog::prolog-unify-handler
             args
             initial-env
             (lambda (env)
               (push env results)))
    (nreverse results)))

(defmacro assert-prolog-unify-handler-results= (expected args &optional initial-env)
  "Assert that prolog-unify-handler emits EXPECTED environments."
  `(assert-equal ,expected
                 (collect-prolog-unify-handler-results ,args ,initial-env)))

(defmacro assert-prolog-logic-substitute= (var expected env)
  "Assert that VAR resolves to EXPECTED in ENV."
  `(assert-= ,expected (cl-cc:logic-substitute ,var ,env)))
