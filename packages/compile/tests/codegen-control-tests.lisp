;;;; tests/unit/compile/codegen-control-tests.lisp — Codegen control-flow tests

(in-package :cl-cc/test)

;;; ─── compile-ast: ast-block / ast-tagbody / ast-go ───────────────────────────

(it-sequential "codegen-block-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-block :name 'my-block
                                            :body (list (make-ast-int :value 42)))
                           ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-label) :to-be-truthy)))

(it-sequential "codegen-tagbody-compilation"
  (let* ((ctx  (make-codegen-ctx))
         (reg  (compile-ast (make-ast-tagbody
                              :tags (list (cons 'tag1 (list (make-ast-int :value 1)))
                                          (cons 'tag2 (list (make-ast-int :value 2)))))
                            ctx))
         (labels (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-label))
                                (codegen-instructions ctx))))
    (expect (>= (length labels) 2) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-catch-compilation"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-catch
                             :tag  (make-ast-quote :value 'my-tag)
                             :body (list (make-ast-int :value 42)))
                           ctx)))
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-establish-catch) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-label) :to-be-truthy)
    (expect (keywordp reg) :to-be-truthy)))

(it-sequential "codegen-throw-compiles-tag-and-value"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-throw
                   :tag (make-ast-quote :value 'my-tag)
                   :value (make-ast-int :value 42))
                  ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-throw) :to-be-truthy)
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const)) insts)))
      (expect (>= (length consts) 2) :to-be-truthy))))

;;; ─── lookup-block ────────────────────────────────────────────────────────────

(it-sequential "lookup-block-finds-block single-block"
  (destructuring-bind (env name expected-exit expected-reg) (list (list (cons 'my-block (cons "exit_0" :R3))) 'my-block "exit_0" :R3)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-block-env ctx) env)
    (let ((info (cl-cc/compile::lookup-block ctx name)))
      (expect (car info) :to-equal expected-exit)
      (expect (cdr info) :to-be expected-reg)))))

(it-sequential "lookup-block-finds-block multi-block"
  (destructuring-bind (env name expected-exit expected-reg) (list (list (cons 'outer (cons "outer_exit" :R0))
                 (cons 'inner (cons "inner_exit" :R1))) 'inner "inner_exit" :R1)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-block-env ctx) env)
    (let ((info (cl-cc/compile::lookup-block ctx name)))
      (expect (car info) :to-equal expected-exit)
      (expect (cdr info) :to-be expected-reg)))))

(it-sequential "lookup-block-signals-for-unknown-name"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (signals error (cl-cc/compile::lookup-block ctx 'nonexistent-block))))

;;; ─── lookup-tag ──────────────────────────────────────────────────────────────

(it-sequential "lookup-tag-returns-label first-tag"
  (destructuring-bind (env tag expected-label) (list (list (cons 'loop-start "tag_0")
                 (cons 'loop-end   "tag_1")) 'loop-start "tag_0")
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx) env)
    (expect (cl-cc/compile::lookup-tag ctx tag) :to-equal expected-label))))

(it-sequential "lookup-tag-returns-label second-tag"
  (destructuring-bind (env tag expected-label) (list (list (cons 'loop-start "tag_0")
                 (cons 'loop-end   "tag_1")) 'loop-end "tag_1")
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx) env)
    (expect (cl-cc/compile::lookup-tag ctx tag) :to-equal expected-label))))

(it-sequential "lookup-tag-returns-label shadowed-tag"
  (destructuring-bind (env tag expected-label) (list (list (cons 'retry "inner_tag_5")
                 (cons 'retry "outer_tag_1")) 'retry "inner_tag_5")
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx) env)
    (expect (cl-cc/compile::lookup-tag ctx tag) :to-equal expected-label))))

(it-sequential "lookup-tag-signals-for-unknown-tag"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (signals error (cl-cc/compile::lookup-tag ctx 'missing-tag))))

;;; ─── FR-920 / FR-902 utility hooks ──────────────────────────────────────────

(it-sequential "codegen-forward-reference-resolves-and-patches"
  (let ((cl-cc/compile:*forward-reference-patch-table* (make-hash-table :test #'equal))
        (patched nil)
        (defs (make-hash-table :test #'equal)))
    (setf (gethash 'later defs) :resolved)
    (cl-cc/compile:record-forward-reference
     'later 12
     :kind :call
     :patch-fn (lambda (name value fixup)
                 (setf patched (list name value (getf fixup :location)))))
    (multiple-value-bind (resolved unresolved)
        (cl-cc/compile:resolve-forward-references defs)
      (expect resolved :to-equal '(later))
      (expect unresolved :to-be-null)
      (expect patched :to-equal '(later :resolved 12)))))

(it-sequential "codegen-forward-reference-errors-when-unresolved"
  (let ((cl-cc/compile:*forward-reference-patch-table* (make-hash-table :test #'equal)))
    (cl-cc/compile:record-forward-reference 'missing 99)
    (signals cl-cc/compile:unresolved-forward-reference-error (cl-cc/compile:resolve-forward-references nil :errorp t))))

(it-sequential "vm-pgo-data-roundtrips-basic-hash-table"
  (let* ((path (merge-pathnames (format nil "clcc-pgo-~A.msgpack" (gensym))
                                (uiop:temporary-directory)))
         (data (make-hash-table :test #'equal)))
    (unwind-protect
         (progn
           (setf (gethash "calls" data) 7)
           (cl-cc/vm:save-pgo-data data path)
           (let ((loaded (cl-cc/vm:load-pgo-data path)))
             (expect (gethash "calls" loaded) :to-equal 7)))
      (ignore-errors (delete-file path)))))

;;; ─── type-error-message-from-mismatch ────────────────────────────────────────

(it-sequential "type-error-message-contains-expected-and-got"
  (let* ((expected-type (cl-cc/type:parse-type-specifier 'integer))
         (actual-type   (cl-cc/type:parse-type-specifier 'string))
         (err (make-condition 'cl-cc/type:type-mismatch-error
                              :expected expected-type
                              :actual   actual-type))
         (msg (cl-cc/compile::type-error-message-from-mismatch err)))
    (expect (stringp msg) :to-be-truthy)
    (expect (search "expected" msg) :to-be-truthy)
    (expect (search "got" msg) :to-be-truthy)))

;;; ─── compile-ast: ast-return-from ────────────────────────────────────────────

(it-sequential "codegen-return-from-emits-move-and-jump"
  (let* ((ctx (make-codegen-ctx))
         (exit-label "exit_test_0")
         (result-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-block-env ctx)
          (list (cons 'outer (cons exit-label result-reg))))
    (compile-ast (make-ast-return-from :name 'outer
                                       :value (make-ast-int :value 99))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-move) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump) :to-be-truthy)))

(it-sequential "codegen-return-from-inside-block-exits-at-correct-label"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-block
                :name 'exit-test
                :body (list (make-ast-return-from :name 'exit-test
                                                   :value (make-ast-int :value 42))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump))
                                (codegen-instructions ctx))))
      (expect (>= (length jumps) 1) :to-be-truthy))))

;;; ─── compile-ast: ast-go ─────────────────────────────────────────────────────

(it-sequential "codegen-go-emits-vm-jump-to-named-tag"
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile:ctx-tagbody-env ctx)
          (list (cons 'loop-start "tag_loop_start_0")))
    (compile-ast (make-ast-go :tag 'loop-start) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-jump) :to-be-truthy)))

(it-sequential "codegen-go-inside-tagbody-emits-multiple-jumps"
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-tagbody
                :tags (list (cons 'start (list (make-ast-go :tag 'end)))
                            (cons 'end   (list (make-ast-int :value 0)))))
               ctx)))
    (expect (keywordp reg) :to-be-truthy)
    (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump))
                                (codegen-instructions ctx))))
      (expect (>= (length jumps) 2) :to-be-truthy))))

(it-sequential "codegen-block-restores-block-env"
  (let ((ctx (make-codegen-ctx))
        (old-env (list (cons 'outer (cons "outer_exit" :R0)))))
    (setf (cl-cc/compile:ctx-block-env ctx) old-env)
    (compile-ast (make-ast-block
                  :name 'inner
                  :body (list (make-ast-int :value 1)))
                 ctx)
    (expect (cl-cc/compile:ctx-block-env ctx) :to-equal old-env)))

(it-sequential "codegen-tagbody-restores-tagbody-env"
  (let ((ctx (make-codegen-ctx))
        (old-env (list (cons 'outer "outer_tag"))))
    (setf (cl-cc/compile:ctx-tagbody-env ctx) old-env)
    (compile-ast (make-ast-tagbody
                  :tags (list (cons 'inner (list (make-ast-int :value 1)))))
                 ctx)
    (expect (cl-cc/compile:ctx-tagbody-env ctx) :to-equal old-env)))

;;; ─── %emit-the-runtime-assertion ─────────────────────────────────────────────

(it-sequential "emit-the-runtime-assertion-emits-vm-typep-for-non-trivial-type"
  (let* ((ctx (make-codegen-ctx))  ; ctx-safety defaults to 1
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'integer)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)))

(it-sequential "emit-the-runtime-assertion-skip-cases t-type"
  (destructuring-bind (ty safety-override) (list 't nil)
    (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (when safety-override (setf (cl-cc/compile:ctx-safety ctx) safety-override))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg ty)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-null))))

(it-sequential "emit-the-runtime-assertion-skip-cases nil-type"
  (destructuring-bind (ty safety-override) (list nil nil)
    (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (when safety-override (setf (cl-cc/compile:ctx-safety ctx) safety-override))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg ty)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-null))))

(it-sequential "emit-the-runtime-assertion-skip-cases safety-zero"
  (destructuring-bind (ty safety-override) (list 'integer 0)
    (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (when safety-override (setf (cl-cc/compile:ctx-safety ctx) safety-override))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg ty)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-null))))

(it-sequential "emit-the-runtime-assertion-no-failure-p-emits-typep-only"
  (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'string :emit-failure-p nil)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-type-error-condition) :to-be-null)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error) :to-be-null)))

(it-sequential "emit-the-runtime-assertion-failure-p-emits-type-error-condition-and-signal-error"
  (let* ((ctx (make-codegen-ctx))
         (value-reg (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%emit-the-runtime-assertion ctx value-reg 'integer :emit-failure-p t)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-type-error-condition) :to-be-truthy)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error) :to-be-truthy)))

;;; ─── compile-ast: ast-the ────────────────────────────────────────────────────

(it-sequential "codegen-the-with-declared-integer-type-emits-typep"
  (let* ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc:make-ast-the :type 'integer :value (make-ast-int :value 42)) ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-truthy)))

(it-sequential "codegen-the-nested-transparent-value-emits-no-extra-typep"
  (let* ((ctx (make-codegen-ctx))
         (reg :R1)
         (env (cl-cc/type:type-env-extend 'x
                                          (cl-cc/type:type-to-scheme
                                           (cl-cc/type:parse-type-specifier 'integer))
                                          (cl-cc/type:type-env-empty))))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x reg))
          (cl-cc/compile:ctx-type-env ctx) env)
    (compile-ast (make-ast-the
                  :type 'integer
                  :value (make-ast-the
                          :type 'integer
                          :value (make-ast-var :name 'x)))
                 ctx)
    (expect (= 0 (length (remove-if-not (lambda (inst)
                                       (typep inst 'cl-cc/vm::vm-typep))
                                     (codegen-instructions ctx)))) :to-be-truthy)))

(it-sequential "codegen-the-with-local-defun-safety-zero-skips-typep"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defun :name 'safe-zero-defun
                                 :params nil
                                 :declarations '((optimize (safety 0)))
                                 :body (list (make-ast-the :type 'integer
                                                           :value (make-ast-int :value 42))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-null)))

(it-sequential "codegen-the-with-local-let-safety-zero-skips-typep"
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let :bindings nil
                               :declarations '((optimize (safety 0)))
                               :body (list (make-ast-the :type 'integer
                                                         :value (make-ast-int :value 42))))
                 ctx)
    (expect (codegen-find-inst ctx 'cl-cc/vm::vm-typep) :to-be-null)))

(it-sequential "codegen-let-optimize-inline-policy-propagates-to-lambda-closure speed-three"
  (destructuring-bind (declarations expected-policy) (list '((optimize (speed 3))) :inline)
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda :params '(x)
                                                         :body (list (make-ast-var :name 'x)))))
               :declarations declarations
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy inst) :to-be expected-policy)))))

(it-sequential "codegen-let-optimize-inline-policy-propagates-to-lambda-closure debug-three"
  (destructuring-bind (declarations expected-policy) (list '((optimize (debug 3))) :notinline)
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda :params '(x)
                                                         :body (list (make-ast-var :name 'x)))))
               :declarations declarations
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy inst) :to-be expected-policy)))))

(it-sequential "codegen-let-optimize-inline-policy-propagates-to-lambda-closure space-two"
  (destructuring-bind (declarations expected-policy) (list '((optimize (space 2))) :notinline)
    (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda :params '(x)
                                                         :body (list (make-ast-var :name 'x)))))
               :declarations declarations
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (or (codegen-find-inst ctx 'cl-cc/vm::vm-closure)
                    (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))))
      (expect inst :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy inst) :to-be expected-policy)))))
