;;;; tests/unit/compile/codegen-string-kwargs-tests.lisp — String keyword-argument codegen tests

(in-package :cl-cc/test)

(it-sequential "codegen-string-comparison-keywords-use-subseq"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'string=
                            (make-quoted "hello")
                            (make-quoted "yellow")
                            (make-ast-the :type 'keyword
                                          :value (make-var :start1))
                            (make-int 1)
                            (make-ast-the :type 'keyword
                                          :value (make-var :end1))
                            (make-int 4)
                            (make-ast-the :type 'keyword
                                          :value (make-var :start2))
                            (make-int 1)
                            (make-ast-the :type 'keyword
                                          :value (make-var :end2))
                            (make-int 4))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-subseq) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc::vm-string=) :to-be-truthy)))

(it-sequential "codegen-string-case-keywords-reconstruct-string"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'string-upcase
                            (make-quoted "hello")
                            (make-ast-the :type 'keyword
                                          :value (make-var :start))
                            (make-int 1)
                            (make-ast-the :type 'keyword
                                          :value (make-var :end))
                            (make-int 4))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-subseq) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-concatenate) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-string-upcase) :to-be-truthy)))
