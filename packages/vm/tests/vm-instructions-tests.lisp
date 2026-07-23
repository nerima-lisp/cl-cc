;;;; tests/unit/vm/vm-instructions-tests.lisp — VM instruction definition tests

(in-package :cl-cc/test)



(it-sequential "vm-instruction-sexp-tags const"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-const :dst :r0 :value 42) :const)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags move"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-move :dst :r0 :src :r1) :move)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags add"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-add :dst :r0 :lhs :r1 :rhs :r2) :add)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags slot-boundp"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-slot-boundp :dst :r0 :obj-reg :r1 :slot-name-sym 'field) :slot-boundp)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags closure"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-closure :dst :r0 :label 'L :params '(x)
                                                   :optional-params nil :rest-param nil
                                                   :key-params nil :captured '(:r1)) :closure)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags func-ref"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-func-ref :dst :r0 :label 'L :params '(x)
                                                    :optional-params nil :rest-param nil
                                                    :key-params nil) :func-ref)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-sexp-tags make-closure"
  (destructuring-bind (inst expected-tag) (list (cl-cc:make-vm-make-closure :dst :r0 :label 'L :params '(x)
                                                       :env-regs '(:r1)) :make-closure)
    (expect (first (cl-cc:instruction->sexp inst)) :to-be expected-tag)))

(it-sequential "vm-instruction-readers"
  (let ((inst (cl-cc:make-vm-closure :dst :r7 :label 'LBL :params '(a b)
                                     :optional-params '(c) :rest-param 'rest
                                     :key-params '((:k k)) :captured '(:r1 :r2))))
    (expect (cl-cc/vm::vm-dst inst) :to-be :r7)
    (expect (cl-cc/vm::vm-label-name inst) :to-be 'LBL)
    (expect (cl-cc/vm::vm-closure-params inst) :to-equal '(a b))
    (expect (cl-cc/vm::vm-closure-optional-params inst) :to-equal '(c))
    (expect (cl-cc/vm::vm-closure-rest-param inst) :to-be 'rest)
    (expect (cl-cc/vm::vm-closure-key-params inst) :to-equal '((:k k)))
    (expect (cl-cc/vm::vm-captured-vars inst) :to-equal '(:r1 :r2))))
