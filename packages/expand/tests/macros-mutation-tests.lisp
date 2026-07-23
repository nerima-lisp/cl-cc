;;;; tests/unit/expand/macros-mutation-tests.lisp
;;;; Mutation macro coverage for src/expand/macros-mutation.lisp

(in-package :cl-cc/test)



(it-sequential "push-expands-to-setf-cons"
  (expect '(setf lst (cons v lst)) :to-equal (our-macroexpand-1 '(push v lst))))

(it-sequential "pop-expansion"
  (let* ((result      (our-macroexpand-1 '(pop lst)))
         (bindings    (cadr result))       ; ((#:TMP lst))
         (setf-form   (caddr result))      ; (setf lst (cdr #:TMP))
         (tmp-sym     (caar bindings)))    ; the gensym bound to lst
    (expect 'let :to-be (car result))
    ;; Binding binds tmp gensym to lst
    (expect 'lst :to-equal (cadar bindings))
    ;; Setf form structure: (setf lst (cdr tmp))
    (expect 'setf :to-be (car setf-form))
    (expect 'lst :to-be (cadr setf-form))
    ;; Value arg to setf is (cdr tmp)
    (expect 'cdr :to-be (car (caddr setf-form)))
    (expect tmp-sym :to-be (cadr (caddr setf-form)))))

(it-sequential "incf-decf-expansion incf-default"
  (destructuring-bind (form expected) (list '(incf x) '(setq x (+ x 1)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "incf-decf-expansion incf-custom"
  (destructuring-bind (form expected) (list '(incf x 5) '(setq x (+ x 5)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "incf-decf-expansion decf-default"
  (destructuring-bind (form expected) (list '(decf x) '(setq x (- x 1)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "incf-decf-expansion decf-custom"
  (destructuring-bind (form expected) (list '(decf x 3) '(setq x (- x 3)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "pushnew-default-expansion"
  (let* ((result    (our-macroexpand-1 '(pushnew item place)))
         (unless-form (caddr result))
         (member-call (second unless-form)))
    (expect 'let :to-be (car result))
    (expect 'unless :to-be (car unless-form))
    (expect 'member :to-be (car member-call))))

(it-sequential "pushnew-with-test-passes-test-to-member"
  (let* ((result      (our-macroexpand-1 '(pushnew item place :test #'equal)))
         (unless-form (caddr result))
         (member-call (second unless-form))
         (last-arg    (car (last member-call))))
    (expect (= (length member-call) 5) :to-be-truthy)
    (expect '#'equal :to-equal last-arg)))

(it-sequential "pushnew-runtime-behavior adds-missing"
  (destructuring-bind (expected code) (list 4 "(let ((lst (list 1 2 3))) (pushnew 4 lst) (length lst))")
    (expect (= expected (run-string code)) :to-be-truthy)))

(it-sequential "pushnew-runtime-behavior no-duplicate"
  (destructuring-bind (expected code) (list 3 "(let ((lst (list 1 2 3))) (pushnew 2 lst) (length lst))")
    (expect (= expected (run-string code)) :to-be-truthy)))

;;; ─── compound place: %compound-place-binding ──────────────────────────────

(it-sequential "compound-place-expansion-uses-let* push-aref"
  (destructuring-bind (form) (list '(push v (aref arr i)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let*)))

(it-sequential "compound-place-expansion-uses-let* pop-aref"
  (destructuring-bind (form) (list '(pop (aref arr i)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let*)))

(it-sequential "compound-place-expansion-uses-let* incf-aref"
  (destructuring-bind (form) (list '(incf (aref arr i)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let*)))

(it-sequential "compound-place-expansion-uses-let* decf-aref"
  (destructuring-bind (form) (list '(decf (aref arr i)))
    (expect (car (our-macroexpand-1 form)) :to-be 'let*)))

(it-sequential "compound-place-subform-evaluated-once"
  (let* ((result   (our-macroexpand-1 '(push v (aref arr i))))
         (bindings (second result))
         (names    (mapcar #'first bindings)))
    (expect (car result) :to-be 'let*)
    (expect (> (length bindings) 1) :to-be-truthy)
    (expect (member 'i names) :to-be-falsy)))
