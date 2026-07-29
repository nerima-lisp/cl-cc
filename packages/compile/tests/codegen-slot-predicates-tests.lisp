;;;; tests/unit/compile/codegen-slot-predicates-tests.lisp — Slot-predicate codegen tests

(in-package :cl-cc/test)

(it-sequential "codegen-slot-predicate-emits-instruction boundp"
  (destructuring-bind (form inst-type) (list 'slot-boundp 'cl-cc/vm::vm-slot-boundp)
    (let ((ctx (make-ctx-with-vars 'obj)))
    (compile-ast (make-call form (make-var 'obj) (make-quoted 'field)) ctx)
    (let ((inst (codegen-find-inst ctx inst-type)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'field)))))

(it-sequential "codegen-slot-predicate-emits-instruction exists-p"
  (destructuring-bind (form inst-type) (list 'slot-exists-p 'cl-cc/vm::vm-slot-exists-p)
    (let ((ctx (make-ctx-with-vars 'obj)))
    (compile-ast (make-call form (make-var 'obj) (make-quoted 'field)) ctx)
    (let ((inst (codegen-find-inst ctx inst-type)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'field)))))

(it-sequential "codegen-slot-predicate-emits-instruction makunbound"
  (destructuring-bind (form inst-type) (list 'slot-makunbound 'cl-cc/vm::vm-slot-makunbound)
    (let ((ctx (make-ctx-with-vars 'obj)))
    (compile-ast (make-call form (make-var 'obj) (make-quoted 'field)) ctx)
    (let ((inst (codegen-find-inst ctx inst-type)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'field)))))
