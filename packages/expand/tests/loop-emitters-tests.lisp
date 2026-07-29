;;;; tests/unit/expand/loop-emitters-tests.lisp — LOOP emitter layer unit tests

(in-package :cl-cc/test)



(it-sequential "loop-emitters-register-canonical-dispatch-functions iter-from"
  (destructuring-bind (table-type key) (list :iter :from)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-emitters-register-canonical-dispatch-functions iter-hash-values"
  (destructuring-bind (table-type key) (list :iter :hash-values)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-emitters-register-canonical-dispatch-functions acc-sum"
  (destructuring-bind (table-type key) (list :acc :sum)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-emitters-register-canonical-dispatch-functions acc-collect"
  (destructuring-bind (table-type key) (list :acc :collect)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-emitters-register-canonical-dispatch-functions condition-while"
  (destructuring-bind (table-type key) (list :condition :while)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-emitters-register-canonical-dispatch-functions condition-thereis"
  (destructuring-bind (table-type key) (list :condition :thereis)
    (expect (functionp
    (gethash key
             (ecase table-type
               (:iter cl-cc/expand::*loop-iter-emitters*)
               (:acc cl-cc/expand::*loop-acc-emitters*)
               (:condition cl-cc/expand::*loop-condition-emitters*)))) :to-be-truthy)))

(it-sequential "loop-iter-emitter-from-produces-boundary-tests"
  (multiple-value-bind (bindings end-tests pre-body step-forms)
      (funcall (gethash :from cl-cc/expand::*loop-iter-emitters*)
               'i
               '(:from 1 :to 5 :by 2))
    (expect bindings :to-equal '((i 1)))
    (expect end-tests :to-equal '((> i 5)))
    (expect pre-body :to-be-null)
    (expect step-forms :to-equal '((setq i (+ i 2))))))

(it-sequential "loop-acc-emitter-sum-accumulates-numerically"
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :sum cl-cc/expand::*loop-acc-emitters*)
               'total
               'item
               nil
               nil
               nil)
    (expect body :to-equal '(setq total (the number (+ total item))))
    (expect (mapcar (lambda (b) (list (car b) (cadr b))) bindings) :to-equal (list (list 'total '(the number 0))))
    (expect result-form :to-be-null)))

(it-sequential "loop-acc-emitter-collect-adds-implicit-nreverse-only-without-into"
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :collect cl-cc/expand::*loop-acc-emitters*)
               'items
               'item
               nil
               nil
               nil)
    (expect body :to-equal '(setq items (cons item items)))
    (expect bindings :to-equal '((items nil)))
    (expect result-form :to-equal '((nreverse items))))
  (multiple-value-bind (body bindings result-form)
      (funcall (gethash :collect cl-cc/expand::*loop-acc-emitters*)
               'items
               'item
               nil
               nil
               'external-items)
    (expect body :to-equal '(setq items (cons item items)))
    (expect bindings :to-equal '((items nil)))
    (expect result-form :to-be-null)))

(it-sequential "loop-iter-emitter-in-with-destructuring-and-by-function"
  (multiple-value-bind (bindings end-tests pre-body step-forms)
      (funcall (gethash :in cl-cc/expand::*loop-iter-emitters*)
               '(a . rest)
               '(:in xs :by next-cell))
    (expect (= 4 (length bindings)) :to-be-truthy)
    (expect (member '(a nil) bindings :test #'equal) :to-be-truthy)
    (expect (member '(rest nil) bindings :test #'equal) :to-be-truthy)
    (let* ((list-binding (find-if (lambda (binding)
                                    (equal 'xs (second binding)))
                                  bindings))
           (real-binding (find-if (lambda (binding)
                                    (and (consp (second binding))
                                         (eq 'car (first (second binding)))))
                                  bindings))
           (list-var (first list-binding))
           (real-var (first real-binding)))
      (expect list-binding :to-be-truthy)
      (expect real-binding :to-be-truthy)
      (expect real-binding :to-equal `(,real-var (car ,list-var)))
      (expect end-tests :to-equal `((null ,list-var)))
      (expect (= 2 (length pre-body)) :to-be-truthy)
      (expect (member `(setq a (car ,real-var)) pre-body :test #'equal) :to-be-truthy)
      (expect (member `(setq rest (cdr ,real-var)) pre-body :test #'equal) :to-be-truthy)
      (expect (= 4 (length step-forms)) :to-be-truthy)
      (expect (member `(setq ,list-var (funcall next-cell ,list-var))
                           step-forms :test #'equal) :to-be-truthy)
      (expect (member `(setq ,real-var (car ,list-var))
                           step-forms :test #'equal) :to-be-truthy)
      (expect (member `(setq a (car ,real-var))
                           step-forms :test #'equal) :to-be-truthy)
      (expect (member `(setq rest (cdr ,real-var))
                           step-forms :test #'equal) :to-be-truthy))))

(it-sequential "loop-condition-emitters-produce-expected-forms until"
  (destructuring-bind (type form end-tag expected) (list :until 'stop-now 'done '(when stop-now (go done)))
    (expect (funcall (gethash type cl-cc/expand::*loop-condition-emitters*)
                         form
                         end-tag) :to-equal expected)))

(it-sequential "loop-condition-emitters-produce-expected-forms always"
  (destructuring-bind (type form end-tag expected) (list :always 'ok 'done '(unless ok (return nil)))
    (expect (funcall (gethash type cl-cc/expand::*loop-condition-emitters*)
                         form
                         end-tag) :to-equal expected)))

(it-sequential "loop-condition-emitters-produce-expected-forms never"
  (destructuring-bind (type form end-tag expected) (list :never 'bad 'done '(when bad (return nil)))
    (expect (funcall (gethash type cl-cc/expand::*loop-condition-emitters*)
                         form
                         end-tag) :to-equal expected)))

(it-sequential "loop-condition-emitter-thereis-wraps-result-in-a-single-binding"
  (let ((form (funcall (gethash :thereis cl-cc/expand::*loop-condition-emitters*)
                       'probe
                       'done)))
    (expect (car form) :to-be 'let)
    (expect (= 1 (length (second form))) :to-be-truthy)
    (expect (mapcar #'second (second form)) :to-equal '(probe))
    (let ((temp-var (caar (second form))))
      (expect (symbolp temp-var) :to-be-truthy)
      (expect (third form) :to-equal `(when ,temp-var (return ,temp-var))))))

(it-sequential "loop-condition-emitter-while-emits-goto"
  (expect (funcall (gethash :while cl-cc/expand::*loop-condition-emitters*)
                         'keep-going
                         'done) :to-equal '(unless keep-going (go done))))
;; force rebuild
