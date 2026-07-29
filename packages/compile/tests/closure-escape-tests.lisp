;;;; tests/unit/compile/closure-escape-tests.lisp — Escape analysis and closure-sharing tests
;;;;
;;;; Tests the escape analysis helpers and closure sharing/normalization utilities
;;;; from the compile package. Depends on closure-tests.lisp being loaded first
;;;; (via ASDF :serial t) for shared helpers.

(in-package :cl-cc/test)


;;; ─── Conservative escape analysis helper ─────────────────────────────────

(it-sequential "binding-escapes-direct-return-returns-true"
  (expect (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast:make-ast-var :name 'p))
    'p) :to-be-truthy))

(it-sequential "binding-escapes-safe-consumer-returns-nil"
  (expect (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast:make-ast-call
           :func 'car
           :args (list (cl-cc/ast:make-ast-var :name 'p))))
    'p
    :safe-consumers '("CAR")) :to-be-null))

(it-sequential "binding-escapes-inner-lambda-capture-returns-true"
  (expect (cl-cc/compile::binding-escapes-in-body-p
    (list (cl-cc/ast:make-ast-lambda
           :params '()
           :body (list (cl-cc/ast:make-ast-var :name 'p))))
    'p) :to-be-truthy))

(it-sequential "binding-escape-kinds-reports direct-return"
  (destructuring-bind (expected-kind forms binding) (list :return (list (cl-cc/ast:make-ast-var :name 'p)) 'p)
    (expect (member expected-kind (cl-cc/compile::binding-escape-kinds-in-body forms binding)) :to-be-truthy)))

(it-sequential "binding-escape-kinds-reports external-call"
  (destructuring-bind (expected-kind forms binding) (list :external-call (list (cl-cc/ast:make-ast-call :func 'list
                                       :args (list (cl-cc/ast:make-ast-var :name 'p)))) 'p)
    (expect (member expected-kind (cl-cc/compile::binding-escape-kinds-in-body forms binding)) :to-be-truthy)))

(it-sequential "binding-escape-kinds-reports inner-capture"
  (destructuring-bind (expected-kind forms binding) (list :capture (list (cl-cc/ast:make-ast-lambda :params '()
                                         :body (list (cl-cc/ast:make-ast-var :name 'p)))) 'p)
    (expect (member expected-kind (cl-cc/compile::binding-escape-kinds-in-body forms binding)) :to-be-truthy)))

(it-sequential "closure-key-normalization capture-key"
  (destructuring-bind (expected actual) (list '(x y) (cl-cc/compile::closure-capture-key '((y . :r2) (x . :r1) (x . :r9))))
    (expect actual :to-equal expected)))

(it-sequential "closure-key-normalization sharing-key"
  (destructuring-bind (expected actual) (list '("L0" (x y)) (cl-cc/compile::closure-sharing-key "L0" '((y . :r2) (x . :r1))))
    (expect actual :to-equal expected)))

(it-sequential "binding-direct-call-count-ignores-non-call-refs"
  (expect (cl-cc/compile::binding-direct-call-count-in-body
                 (list (cl-cc/ast:make-ast-call :func 'f :args nil)
                       (cl-cc/ast:make-ast-var :name 'f))
                 'f) :to-equal 1))

(it-sequential "binding-one-shot-single-call-returns-true"
  (expect (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast:make-ast-call :func 'f :args (list (cl-cc/ast:make-ast-int :value 1))))
    'f) :to-be-truthy))

(it-sequential "binding-one-shot-multi-call-returns-false"
  (expect (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast:make-ast-call :func 'f :args nil)
          (cl-cc/ast:make-ast-call :func 'f :args nil))
    'f) :to-be-falsy))

(it-sequential "binding-one-shot-captured-in-lambda-returns-false"
  (expect (cl-cc/compile::binding-one-shot-p
    (list (cl-cc/ast:make-ast-lambda :params '() :body (list (cl-cc/ast:make-ast-var :name 'f))))
    'f) :to-be-falsy))

(it-sequential "group-shared-sibling-captures-groups-by-capture-set"
  (let ((groups (cl-cc/compile::group-shared-sibling-captures
                 '(((x . :r1) (y . :r2))
                   ((y . :r8) (x . :r7))
                   ((z . :r3))))))
    (expect (hash-table-count groups) :to-equal 1)
    (expect (length (gethash '(x y) groups)) :to-equal 2)
    (expect (gethash '(z) groups) :to-be-falsy)))

(it-sequential "group-shareable-closures-groups-by-label-and-captures"
  (let ((groups (cl-cc/compile::group-shareable-closures
                 '((:entry-label "L0" :captured-vars ((x . :r1) (y . :r2)))
                   (:entry-label "L0" :captured-vars ((y . :r8) (x . :r7)))
                   (:entry-label "L1" :captured-vars ((x . :r1) (y . :r2)))))))
    (expect (hash-table-count groups) :to-equal 1)
    (expect (length (gethash '("L0" (x y)) groups)) :to-equal 2)))

(it-sequential "binding-escape-defun-body-yields-capture"
  (expect (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast:make-ast-defun
                   :name 'inner
                   :params '()
                   :body (list (cl-cc/ast:make-ast-var :name 'p))))
            'p)) :to-be-truthy))

(it-sequential "binding-escape-apply-arg-yields-external-call"
  (expect (member :external-call
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast:make-ast-apply
                   :func (cl-cc/ast:make-ast-var :name 'f)
                   :args (list (cl-cc/ast:make-ast-var :name 'p))))
            'p)) :to-be-truthy))

(it-sequential "binding-escape-flet-body-yields-capture"
  (expect (member :capture
           (cl-cc/compile::binding-escape-kinds-in-body
            (list (cl-cc/ast:make-ast-flet
                   :bindings nil
                   :body (list (cl-cc/ast:make-ast-var :name 'p))))
            'p)) :to-be-truthy))

(it-sequential "binding-no-escape-empty-body-returns-nil"
  (expect (cl-cc/compile::binding-escape-kinds-in-body nil 'p) :to-be-null)
  (expect (cl-cc/compile::binding-escape-kinds-in-body '() 'p) :to-be-null))

(it-sequential "binding-no-escape-absent-binding-returns-nil"
  (expect (cl-cc/compile::binding-escape-kinds-in-body
    (list (cl-cc/ast:make-ast-var :name 'q))
    'p) :to-be-null))
