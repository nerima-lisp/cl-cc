;;;; tests/unit/expand/macros-stdlib-sequence-map-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)



(it-sequential "coerce-quoted-type-expansions to-string"
  (destructuring-bind (form expected-name) (list '(coerce v 'string) "COERCE-TO-STRING")
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect 'v :to-equal (cadr result)))))

(it-sequential "coerce-quoted-type-expansions to-simple-string"
  (destructuring-bind (form expected-name) (list '(coerce v 'simple-string) "COERCE-TO-STRING")
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect 'v :to-equal (cadr result)))))

(it-sequential "coerce-quoted-type-expansions to-list"
  (destructuring-bind (form expected-name) (list '(coerce v 'list) "COERCE-TO-LIST")
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect 'v :to-equal (cadr result)))))

(it-sequential "coerce-quoted-type-expansions to-vector"
  (destructuring-bind (form expected-name) (list '(coerce v 'vector) "COERCE-TO-VECTOR")
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect 'v :to-equal (cadr result)))))

(it-sequential "coerce-quoted-type-expansions to-character"
  (destructuring-bind (form expected-name) (list '(coerce v 'character) "CHARACTER")
    (let ((result (our-macroexpand-1 form)))
    (expect expected-name :to-equal (symbol-name (car result)))
    (expect 'v :to-equal (cadr result)))))

(it-sequential "coerce-unquoted-type-fallback"
  (let ((result (our-macroexpand-1 '(coerce v type-var))))
    (expect "%COERCE-RUNTIME" :to-equal (symbol-name (car result)))
    (expect (symbol-package 'type-var) :to-be (symbol-package (car result)))
    (expect 'v :to-equal (cadr result))))

(it-sequential "map-delegates-to-mapcar-coerce"
  (expect '(coerce (mapcar fn (coerce seq 'list)) 'list) :to-equal (our-macroexpand-1 '(map 'list fn seq))))

(it-sequential "dest-returning-sequence-expanders map-into"
  (destructuring-bind (form) (list '(map-into dest fn src))
    (let* ((result    (our-macroexpand-1 form))
         (dest-var  (first (first (second result))))
         (last-form (car (last (cddr result)))))
    (expect (car result) :to-be 'let)
    (expect dest-var :to-be last-form))))

(it-sequential "replace-expansion-vector-path"
  (let* ((result (our-macroexpand-1 '(replace dest src))))
    (expect (car result) :to-be 'let*)))

(it-sequential "merge-expansion"
  (let* ((result   (our-macroexpand-1 '(merge 'list l1 l2 pred)))
         (bindings (second result))
         (body     (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (= 3 (length bindings)) :to-be-truthy)
    (expect (car body) :to-be 'labels)))
