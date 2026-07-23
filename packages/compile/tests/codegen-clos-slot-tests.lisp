;;;; tests/unit/compile/codegen-clos-slot-tests.lisp — Codegen CLOS Slot & Phase2 Tests

(in-package :cl-cc/test)

;;; ─── compile-ast: ast-slot-value ─────────────────────────────────────────────

(it-sequential "codegen-slot-value"
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'obj :R42)))
    (let* ((reg  (compile-ast (cl-cc/ast:make-ast-slot-value
                                :object (cl-cc/ast:make-ast-var :name 'obj)
                                :slot 'radius)
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'radius)
      (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-noescape-make-instance-slot-value-bypasses-heap-object"
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex))))))
                 :body (list (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))))

(it-sequential "codegen-noescape-make-instance-slot-value-bypasses-heap-object-through-ast-the"
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex))))))
                 :body (list (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-the
                                       :type 't
                                       :value (cl-cc/ast:make-ast-var :name 'obj))
                              :slot 'name)))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))))

(it-sequential "codegen-escaped-make-instance-slot-value-falls-back"
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value 'rex))))))
                 :body (list (cl-cc/ast:make-ast-lambda :params '() :body (list (cl-cc/ast:make-ast-var :name 'obj)))
                             (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'name)))
                ctx)))
       (expect (keywordp reg) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-truthy)
       (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-truthy))))

(it-sequential "codegen-branch-local-make-instance-sinks-initarg-evaluation"
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc/ast:make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc/ast:make-ast-make-instance
                        :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc/ast:make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc/ast:make-ast-int :value 1)
                                                 (cl-cc/ast:make-ast-int :value 2))))))))
           :body
           (list (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'flag)
                   :then (cl-cc/ast:make-ast-slot-value
                          :object (cl-cc/ast:make-ast-var :name 'obj)
                          :slot 'name)
                   :else (cl-cc/ast:make-ast-int :value 0)))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (> cons-pos jump-pos) :to-be-truthy)))

(it-sequential "codegen-branch-local-make-instance-sinks-initarg-evaluation-through-ast-the"
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc/ast:make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc/ast:make-ast-the
                        :type 'my-dog
                        :value (cl-cc/ast:make-ast-make-instance
                                :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                :initargs
                                (list (cons :name
                                            (cl-cc/ast:make-ast-call
                                             :func 'cons
                                             :args (list (cl-cc/ast:make-ast-int :value 1)
                                                         (cl-cc/ast:make-ast-int :value 2)))))))))
           :body
           (list (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'flag)
                  :then (cl-cc/ast:make-ast-slot-value
                         :object (cl-cc/ast:make-ast-var :name 'obj)
                         :slot 'name)
                  :else (cl-cc/ast:make-ast-int :value 0)))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (> cons-pos jump-pos) :to-be-truthy)))

(it-sequential "codegen-multibinding-branch-local-make-instance-sinks-initarg-evaluation"
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/ast:make-ast-let
               :bindings (list (cons 'obj
                                     (cl-cc/ast:make-ast-make-instance
                                      :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                      :initargs (list (cons :name
                                                            (cl-cc/ast:make-ast-call
                                                             :func 'cons
                                                             :args (list (cl-cc/ast:make-ast-int :value 1)
                                                                         (cl-cc/ast:make-ast-int :value 2)))))))
                               (cons 'flag (cl-cc/ast:make-ast-int :value 1)))
               :body (list (cl-cc/ast:make-ast-if
                            :cond (cl-cc/ast:make-ast-var :name 'flag)
                            :then (cl-cc/ast:make-ast-slot-value
                                   :object (cl-cc/ast:make-ast-var :name 'obj)
                                   :slot 'name)
                            :else (cl-cc/ast:make-ast-int :value 0)))))
         (reg (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (> cons-pos jump-pos) :to-be-truthy)))

(it-sequential "codegen-branch-local-make-instance-shadowed-binding-does-not-sink"
  (let* ((ctx (make-codegen-ctx))
         (ast nil)
         (reg nil)
         (insts nil)
         (jump-pos nil)
         (cons-pos nil))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'flag :R10)))
    (setf ast
          (cl-cc/ast:make-ast-let
           :bindings
           (list (cons 'obj
                       (cl-cc/ast:make-ast-make-instance
                        :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                        :initargs
                        (list (cons :name
                                    (cl-cc/ast:make-ast-call
                                     :func 'cons
                                     :args (list (cl-cc/ast:make-ast-int :value 1)
                                                 (cl-cc/ast:make-ast-int :value 2))))))))
           :body
           (list (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'flag)
                  :then (cl-cc/ast:make-ast-let
                         :bindings (list (cons 'obj
                                                (cl-cc/ast:make-ast-make-instance
                                                 :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                                 :initargs (list (cons :name (cl-cc/ast:make-ast-quote :value "shadow"))))))
                          :body (list (cl-cc/ast:make-ast-slot-value
                                       :object (cl-cc/ast:make-ast-var :name 'obj)
                                       :slot 'name)))
                   :else (cl-cc/ast:make-ast-int :value 0)))))

    (setf reg (compile-ast ast ctx))
    (setf insts (codegen-instructions ctx)
          jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)
          cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts))
    (expect (keywordp reg) :to-be-truthy)
    (expect jump-pos :to-be-truthy)
    (expect cons-pos :to-be-truthy)
    (expect (< cons-pos jump-pos) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)))

;;; ─── compile-ast: ast-set-slot-value ─────────────────────────────────────────

(it-sequential "codegen-set-slot-value"
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'obj :R60)))
    (let* ((reg  (compile-ast (cl-cc/ast:make-ast-set-slot-value
                                :object (cl-cc/ast:make-ast-var :name 'obj)
                                :slot 'weight
                                :value (cl-cc/ast:make-ast-int :value 42))
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write)))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'weight)
      (expect (keywordp reg) :to-be-truthy))))

(it-sequential "codegen-noescape-make-instance-set-slot-value-bypasses-slot-write"
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :weight (cl-cc/ast:make-ast-int :value 1))))))
                 :body (list (cl-cc/ast:make-ast-set-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'weight
                              :value (cl-cc/ast:make-ast-int :value 42))
                             (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'weight)))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))))

(it-sequential "codegen-noescape-make-instance-set-slot-value-bypasses-slot-write-through-ast-the"
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (cl-cc/ast:make-ast-let
                 :bindings (list (cons 'obj (cl-cc/ast:make-ast-make-instance
                                            :class (cl-cc/ast:make-ast-quote :value 'my-dog)
                                            :initargs (list (cons :weight (cl-cc/ast:make-ast-int :value 1))))))
                 :body (list (cl-cc/ast:make-ast-set-slot-value
                              :object (cl-cc/ast:make-ast-the
                                       :type 't
                                       :value (cl-cc/ast:make-ast-var :name 'obj))
                              :slot 'weight
                              :value (cl-cc/ast:make-ast-int :value 42))
                             (cl-cc/ast:make-ast-slot-value
                              :object (cl-cc/ast:make-ast-var :name 'obj)
                              :slot 'weight)))
                ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-make-obj) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-write) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-slot-read) :to-be-null)
      (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy))))

;;; ─── phase2 CLOS helpers ─────────────────────────────────────────────────────

(it-sequential "phase2-slot-ops-emit-instruction slot-boundp"
  (destructuring-bind (fn-name vm-type) (list 'slot-boundp 'cl-cc/vm::vm-slot-boundp)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn-name (make-int 0) (make-quoted 'name)) ctx)
    (expect (codegen-find-inst ctx vm-type) :to-be-truthy))))

(it-sequential "phase2-slot-ops-emit-instruction slot-exists-p"
  (destructuring-bind (fn-name vm-type) (list 'slot-exists-p 'cl-cc/vm::vm-slot-exists-p)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn-name (make-int 0) (make-quoted 'name)) ctx)
    (expect (codegen-find-inst ctx vm-type) :to-be-truthy))))

(it-sequential "phase2-slot-ops-emit-instruction slot-makunbound"
  (destructuring-bind (fn-name vm-type) (list 'slot-makunbound 'cl-cc/vm::vm-slot-makunbound)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call fn-name (make-int 0) (make-quoted 'name)) ctx)
    (expect (codegen-find-inst ctx vm-type) :to-be-truthy))))

(it-sequential "phase2-slot-boundp-stores-slot-name"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'slot-boundp (make-int 0) (make-quoted 'foo)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-slot-boundp)))
      (expect (cl-cc/vm::vm-slot-name-sym inst) :to-be 'foo))))

(it-sequential "phase2-call-next-method-args-reg no-args"
  (destructuring-bind (ast args-reg-truthy-p) (list (make-call 'call-next-method) nil)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-call-next-method)))
      (expect inst :to-be-truthy)
      (if args-reg-truthy-p
          (expect (cl-cc::vm-call-next-method-args-reg inst) :to-be-truthy)
          (expect (cl-cc::vm-call-next-method-args-reg inst) :to-be-falsy))))))

(it-sequential "phase2-call-next-method-args-reg with-args"
  (destructuring-bind (ast args-reg-truthy-p) (list (make-call 'call-next-method (make-int 42)) t)
    (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-call-next-method)))
      (expect inst :to-be-truthy)
      (if args-reg-truthy-p
          (expect (cl-cc::vm-call-next-method-args-reg inst) :to-be-truthy)
          (expect (cl-cc::vm-call-next-method-args-reg inst) :to-be-falsy))))))

(it-sequential "phase2-call-next-method-args-is-cons-list"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-call 'call-next-method (make-int 1) (make-int 2)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-cons) :to-be-truthy)))
