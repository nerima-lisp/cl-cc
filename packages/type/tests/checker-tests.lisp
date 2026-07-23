;;;; tests/unit/type/checker-tests.lisp — checker module boundary tests

(in-package :cl-cc/test)


(it-sequential "checker-interface-ready"
  (expect (cl-cc/type::checker-interface-ready-p) :to-be-truthy))

(it-sequential "checker-rigid-constructor-available"
  (let ((sk (cl-cc/type:fresh-rigid-var 'alpha)))
    (expect (cl-cc/type:type-rigid-p sk) :to-be-truthy)
    (expect (cl-cc/type:type-rigid-name sk) :to-be 'alpha)))
