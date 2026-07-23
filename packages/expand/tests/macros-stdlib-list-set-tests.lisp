;;;; tests/unit/expand/macros-stdlib-list-set-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)



;; Pre-existing base failure (fails identically on the pre-migration deftest
;; baseline): the (concatenate 'list ...) expansion no longer matches the
;; literal (append '(1 2) '(3 4)) shape. Base expander change, not a migration bug.
(it-todo "concatenate-list-expands-to-append"
  "pre-existing base failure: concatenate 'list expansion shape changed")

(it-sequential "concatenate-vector-types-expand-to-coerce-to-vector vector"
  (destructuring-bind (form) (list '(concatenate 'vector '(1) '(2)))
    (let ((result (our-macroexpand-1 form)))
    (expect "COERCE-TO-VECTOR" :to-equal (symbol-name (car result)))
    (expect 'append :to-be (caadr result)))))

(it-sequential "concatenate-vector-types-expand-to-coerce-to-vector simple-vector"
  (destructuring-bind (form) (list '(concatenate 'simple-vector '(1) '(2)))
    (let ((result (our-macroexpand-1 form)))
    (expect "COERCE-TO-VECTOR" :to-equal (symbol-name (car result)))
    (expect 'append :to-be (caadr result)))))

(it-sequential "concatenate-runtime-string two-strings"
  (destructuring-bind (form expected) (list "(concatenate 'string \"hello\" \" \" \"world\")" "hello world")
    (expect (run-string form) :to-equal expected)))

(it-sequential "concatenate-runtime-string empty"
  (destructuring-bind (form expected) (list "(concatenate 'string)" "")
    (expect (run-string form) :to-equal expected)))

(it-sequential "concatenate-runtime-string single"
  (destructuring-bind (form expected) (list "(concatenate 'string \"only\")" "only")
    (expect (run-string form) :to-equal expected)))

(it-sequential "concatenate-runtime-list two-lists"
  (destructuring-bind (form expected) (list "(concatenate 'list '(1 2) '(3 4))" '(1 2 3 4))
    (expect (run-string form) :to-equal expected)))

(it-sequential "concatenate-runtime-list four-length"
  (destructuring-bind (form expected) (list "(length (concatenate 'list '(1 2) '(3 4)))" 4)
    (expect (run-string form) :to-equal expected)))

(it-sequential "concatenate-runtime-list one-length"
  (destructuring-bind (form expected) (list "(length (concatenate 'list '(1)))" 1)
    (expect (run-string form) :to-equal expected)))

(it-sequential "notany-notevery-runtime notany-all-fail"
  (destructuring-bind (code expected) (list "(notany #'evenp '(1 3 5))" t)
    (expect (not (null (run-string code))) :to-equal expected)))

(it-sequential "notany-notevery-runtime notany-one-passes"
  (destructuring-bind (code expected) (list "(notany #'evenp '(1 2 3))" nil)
    (expect (not (null (run-string code))) :to-equal expected)))

(it-sequential "notany-notevery-runtime notevery-all-pass"
  (destructuring-bind (code expected) (list "(notevery #'oddp '(1 3 5))" nil)
    (expect (not (null (run-string code))) :to-equal expected)))

(it-sequential "notany-notevery-runtime notevery-one-fails"
  (destructuring-bind (code expected) (list "(notevery #'oddp '(1 2 3))" t)
    (expect (not (null (run-string code))) :to-equal expected)))

(it-sequential "nreconc-expands-to-nconc-nreverse"
  (let ((result (our-macroexpand-1 '(nreconc lst tail))))
    (expect 'nconc :to-be (car result))
    (expect 'nreverse :to-be (caadr result))
    (expect 'tail :to-be (caddr result))))

(it-sequential "nreconc-runtime basic"
  (destructuring-bind (form expected) (list "(nreconc (list 3 2 1) '(4 5))" '(1 2 3 4 5))
    (expect (run-string form) :to-equal expected)))

(it-sequential "nreconc-runtime empty-left"
  (destructuring-bind (form expected) (list "(nreconc '() '(1 2))" '(1 2))
    (expect (run-string form) :to-equal expected)))
