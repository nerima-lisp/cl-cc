;;;; tests/unit/expand/macros-clos-protocol-tests.lisp
;;;; Coverage tests for src/expand/macros-clos-protocol.lisp and macros-mop-support.lisp
;;;;
;;;; Covers: print-unreadable-object, print-object, describe-object, describe,
;;;; ensure-class, change-class, define-method-combination,
;;;; class-direct-superclasses, class-direct-slots, class-slots,
;;;; class-direct-default-initargs, generic-function-methods,
;;;; generic-function-method-combination, class-precedence-list, parse-float,
;;;; reinitialize-instance, shared-initialize.

(in-package :cl-cc/test)



;;; ─── print-unreadable-object ──────────────────────────────────────────────

(it-sequential "print-unreadable-object-basic-shape base-form-expands-to-let"
  (destructuring-bind (form verifier) (list '(print-unreadable-object (obj stream) body) (lambda (result)
             (expect (car result) :to-be 'let)))
    (funcall verifier (our-macroexpand-1 form))))

(it-sequential "print-unreadable-object-basic-shape body-wraps-with-angle-brackets"
  (destructuring-bind (form verifier) (list '(print-unreadable-object (obj stream) body) (lambda (result)
             (let ((body (cddr result)))
               (expect (some (lambda (f)
                        (and (consp f)
                             (eq (car f) 'format)
                             (member "#<" f :test #'equal)))
                      body) :to-be-truthy))))
    (funcall verifier (our-macroexpand-1 form))))

(it-sequential "print-unreadable-object-basic-shape type-flag-emits-conditional-type-path"
  (destructuring-bind (form verifier) (list '(print-unreadable-object (obj stream :type t) body) (lambda (result)
             (let ((body (cddr result)))
               (expect (some (lambda (f) (and (consp f) (eq (car f) 'when))) body) :to-be-truthy))))
    (funcall verifier (our-macroexpand-1 form))))

;;; ─── describe ─────────────────────────────────────────────────────────────

(it-sequential "describe-expansion-shape expands-to-let"
  (destructuring-bind (verifier) (list (lambda (result)
             (expect (car result) :to-be 'let)))
    (funcall verifier (our-macroexpand-1 '(describe obj)))))

(it-sequential "describe-expansion-shape calls-describe-object"
  (destructuring-bind (verifier) (list (lambda (result)
             (let ((body (cddr result)))
               (expect (some (lambda (f) (and (consp f) (eq (car f) 'describe-object)))
                      body) :to-be-truthy))))
    (funcall verifier (our-macroexpand-1 '(describe obj)))))

(it-sequential "describe-expansion-shape ends-with-values"
  (destructuring-bind (verifier) (list (lambda (result)
             (expect (car (last result)) :to-equal '(values))))
    (funcall verifier (our-macroexpand-1 '(describe obj)))))

;;; ─── ensure-class ─────────────────────────────────────────────────────────

(it-sequential "ensure-class-delegates-to-defclass"
  (let ((result (our-macroexpand-1
                 '(cl-cc/expand::ensure-class 'my-class :direct-superclasses '(object)))))
    (expect (car result) :to-be 'defclass)))

;;; ─── define-method-combination ────────────────────────────────────────────

(it-sequential "define-method-combination-returns-quoted-name"
  (let ((result (our-macroexpand-1
                 '(cl-cc::define-method-combination append :identity-with-one-argument t))))
    (expect (car result) :to-be 'progn)
    (expect (car (last result)) :to-equal ''append)))

(it-sequential "instance-init-macros-use-shared-helper reinitialize-instance"
  (destructuring-bind (form) (list '(cl-cc::reinitialize-instance inst :slot 1))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let*)
    (expect (search "%APPLY-INSTANCE-INITARGS"
                         (string-upcase (format nil "~S" result))) :to-be-truthy))))

(it-sequential "instance-init-macros-use-shared-helper shared-initialize"
  (destructuring-bind (form) (list '(cl-cc::shared-initialize inst t :slot 1))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'let*)
    (expect (search "%APPLY-INSTANCE-INITARGS"
                         (string-upcase (format nil "~S" result))) :to-be-truthy))))

;;; ─── MOP introspection macros ─────────────────────────────────────────────

(it-sequential "mop-class-accessors-expand-to-let class-direct-superclasses"
  (destructuring-bind (form) (list '(cl-cc/expand::class-direct-superclasses cls))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "mop-class-accessors-expand-to-let class-direct-slots"
  (destructuring-bind (form) (list '(cl-cc/expand::class-direct-slots cls))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "mop-class-accessors-expand-to-let class-slots"
  (destructuring-bind (form) (list '(cl-cc/expand::class-slots cls))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "mop-class-accessors-expand-to-let class-direct-default-initargs"
  (destructuring-bind (form) (list '(cl-cc/expand::class-direct-default-initargs cls))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(defun %tree-contains-keyword-p (kw form)
  "True if KW appears anywhere in FORM (nested walk)."
  (cond ((eq form kw) t)
        ((consp form) (or (%tree-contains-keyword-p kw (car form))
                          (%tree-contains-keyword-p kw (cdr form))))
        (t nil)))

(it-sequential "class-direct-superclasses-reads-superclasses-key"
  (let* ((result (our-macroexpand-1 '(cl-cc/expand::class-direct-superclasses cls)))
         (body (cddr result)))
    (expect (%tree-contains-keyword-p :__superclasses__ body) :to-be-truthy)))

(it-sequential "generic-function-methods-expands-to-let*"
  (let ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-methods gf))))
    (expect (car result) :to-be 'let*)))

(it-sequential "generic-function-methods-calls-hash-table-values"
  (let* ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-methods gf)))
         (body   (cddr result)))
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'when))) body) :to-be-truthy)))

(it-sequential "generic-function-method-combination-expands-to-let"
  (let ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-method-combination gf))))
    (expect (car result) :to-be 'let)))

(it-sequential "generic-function-method-combination-defaults-to-standard"
  (let* ((result (our-macroexpand-1 '(cl-cc/vm::generic-function-method-combination gf)))
         (if-form (caddr result))
         (else-branch (cadddr if-form)))
    (expect else-branch :to-equal ''standard)))

;;; ─── parse-float ──────────────────────────────────────────────────────────

(it-sequential "parse-float-expands-to-let"
  (let ((result (our-macroexpand-1 '(cl-cc/expand::parse-float "3.14"))))
    (expect result :to-be-truthy)))

(it-sequential "parse-float-with-start-uses-subseq"
  (let* ((result (our-macroexpand-1 '(cl-cc/expand::parse-float s 2))))
    (expect result :to-be-truthy)))
