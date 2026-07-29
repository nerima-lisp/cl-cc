;;;; tests/unit/vm/package-tests.lisp — package export smoke tests

(in-package :cl-cc/test)



(it-sequential "cl-cc-package-exists"
  (expect (find-package :cl-cc) :to-be-truthy))

(it-sequential "cl-cc-package-exports-representatives"
  (dolist (name '("CST-NODE" "RUN-STRING" "QUERY-GRAMMAR" "OUR-MACROEXPAND-ALL" "AST-INT"))
    (multiple-value-bind (sym status)
        (find-symbol name :cl-cc)
      (expect sym :to-be-truthy)
      (expect status :to-be :external))))
