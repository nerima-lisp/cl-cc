;;;; tests/unit/compile/codegen-hash-table-tests.lisp — Hash-table codegen tests

(in-package :cl-cc/test)

;;; Keep this file self-contained so targeted suite runs do not depend on
;;; loading the shared phase-2 helper file first.
(defun make-call (func &rest arg-forms)
  (make-ast-call :func func :args arg-forms))

(defun make-int (n)
  (make-ast-int :value n))

(defun make-var (s)
  (make-ast-var :name s))

(defun make-quoted (v)
  (make-ast-quote :value v))

(defun make-fn (name)
  (make-ast-function :name name))

(it-sequential "codegen-make-hash-table-test-designators quoted"
  (destructuring-bind (form) (list (make-call 'make-hash-table (make-var :test) (make-quoted 'equal)))
    (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))

(it-sequential "codegen-make-hash-table-test-designators function"
  (destructuring-bind (form) (list (make-call 'make-hash-table (make-var :test) (make-fn 'equalp)))
    (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))

(it-sequential "codegen-make-hash-table-test-designators the-function"
  (destructuring-bind (form) (list (make-call 'make-hash-table
                                     (make-var :test)
                                     (make-ast-the
                                      :type 'function
                                      :value (make-fn 'equal))))
    (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))

(it-sequential "codegen-make-hash-table-test-designators the-keyword"
  (destructuring-bind (form) (list (make-call 'make-hash-table
                                     (make-ast-the
                                      :type 'keyword
                                      :value (make-var :test))
                                     (make-quoted 'equal)))
    (let ((ctx (make-codegen-ctx)))
    (compile-ast form ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test inst) :to-be-truthy)))))

(it-sequential "codegen-make-hash-table-emits-size-option"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'make-hash-table
                            (make-var :test)
                            (make-quoted 'eql)
                            (make-var :size)
                            (make-int 100))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-hash-size inst) :to-be-truthy))))

(it-sequential "codegen-gethash-emits-default-register"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-quoted 'table)
                            (make-int 0))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-gethash)))
      (expect inst :to-be-truthy)
      (expect (cl-cc::vm-gethash-default inst) :to-be-truthy))))

(it-sequential "codegen-gethash-specializes-direct-make-hash-table-test eq"
  (destructuring-bind (test-sym inst-type) (list 'eq 'cl-cc/vm::vm-gethash-eq)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted test-sym)))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-gethash-specializes-direct-make-hash-table-test eql"
  (destructuring-bind (test-sym inst-type) (list 'eql 'cl-cc/vm::vm-gethash-eql)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted test-sym)))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-gethash-specializes-direct-make-hash-table-test equal"
  (destructuring-bind (test-sym inst-type) (list 'equal 'cl-cc/vm::vm-gethash-equal)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted 'key)
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted test-sym)))
                 ctx)
    (expect (codegen-find-inst ctx inst-type) :to-be-truthy))))

(it-sequential "codegen-gethash-specializes-let-bound-static-hash-table"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-call 'gethash
                                         (make-quoted "k")
                                         (make-var 'ht))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-truthy)))

(it-sequential "codegen-gethash-specializes-let-bound-static-hash-table-through-ast-the"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'ht
                                        (make-ast-the
                                         :type '(or hash-table null)
                                         :value (make-call 'make-hash-table
                                                           (make-var :test)
                                                           (make-quoted 'equal)))))
                  :body (list (make-call 'gethash
                                         (make-quoted "k")
                                         (make-var 'ht))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-truthy)))

(it-sequential "codegen-gethash-keeps-generic-path-for-equalp"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'gethash
                            (make-quoted "k")
                            (make-call 'make-hash-table
                                       (make-var :test)
                                       (make-quoted 'equalp)))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-falsy)))

(it-sequential "codegen-make-hash-table-dynamic-test-is-evaluated"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'test (make-quoted 'equal)))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'test))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (let ((make-inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect make-inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test make-inst) :to-be-truthy))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-falsy)))

(it-sequential "codegen-make-hash-table-test-symbol-variable-is-dynamic"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'eql (make-quoted 'equal)))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'eql))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (let ((make-inst (codegen-find-inst ctx 'cl-cc/vm::vm-make-hash-table)))
      (expect make-inst :to-be-truthy)
      (expect (cl-cc::vm-make-hash-table-test make-inst) :to-be-truthy))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-falsy)))

(it-sequential "codegen-gethash-masks-shadowed-static-hash-binding"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'test (make-quoted 'equal))
                                  (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht
                                                     (make-call 'make-hash-table
                                                                (make-var :test)
                                                                (make-var 'test))))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eq) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-falsy)))

(it-sequential "codegen-gethash-masks-shadowed-non-hash-binding"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'ht
                                        (make-call 'make-hash-table
                                                   (make-var :test)
                                                   (make-quoted 'equal))))
                  :body (list (make-ast-let
                               :bindings (list (cons 'ht (make-quoted 'not-a-table)))
                               :body (list (make-call 'gethash
                                                      (make-quoted "k")
                                                      (make-var 'ht))))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eq) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-eql) :to-be-falsy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-gethash-equal) :to-be-falsy)))

(it-sequential "codegen-maphash-emits-loop-support"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'maphash
                            (make-quoted 'fn)
                            (make-quoted 'ht))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-hash-table-keys) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-call) :to-be-truthy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (null (cl-cc::vm-const-value i))))
                       (codegen-instructions ctx)) :to-be-truthy)))
