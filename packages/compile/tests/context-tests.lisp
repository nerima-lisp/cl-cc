;;;; tests/unit/compile/context-tests.lisp — Compiler Context Unit Tests
;;;;
;;;; Tests for compiler-context: register allocation, label generation,
;;;; emit, variable lookup, builtin special variables initialization.

(in-package :cl-cc/test)



;;; ─── make-register ──────────────────────────────────────────────────────────

(it-sequential "ctx-make-register-behavior"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    ;; first result is :R0, a keyword
    (let ((reg (cl-cc/compile:make-register ctx)))
      (expect (keywordp reg) :to-be-truthy)
      (expect reg :to-equal :R0))
    ;; subsequent calls yield :R1, :R2
    (expect (cl-cc/compile:make-register ctx) :to-equal :R1)
    (expect (cl-cc/compile:make-register ctx) :to-equal :R2))
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (expect (cl-cc/compile:ctx-next-register ctx) :to-equal 0)
    (cl-cc/compile:make-register ctx)
    (expect (cl-cc/compile:ctx-next-register ctx) :to-equal 1)))

;;; ─── make-label ─────────────────────────────────────────────────────────────

(it-sequential "ctx-make-label-behavior"
  (let ((cl-cc/compile:*repl-label-counter* nil))
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    ;; format: string with prefix and index
      (let ((lbl (cl-cc/compile:make-label ctx "IF")))
        (expect (stringp lbl) :to-be-truthy)
        (expect lbl :to-equal "IF_0"))
    ;; same prefix increments
      (expect (cl-cc/compile:make-label ctx "IF") :to-equal "IF_1")))
  (let ((cl-cc/compile:*repl-label-counter* nil))
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
      (expect (cl-cc/compile:make-label ctx "IF") :to-equal "IF_0")
      (expect (cl-cc/compile:make-label ctx "ELSE") :to-equal "ELSE_1")
      (expect (cl-cc/compile:make-label ctx "END") :to-equal "END_2"))))

;;; ─── emit ───────────────────────────────────────────────────────────────────

(it-sequential "ctx-emit-behavior"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    ;; push + return value
    (expect (cl-cc/compile:emit ctx :test) :to-be :test)
    (expect (length (cl-cc/compile:ctx-instructions ctx)) :to-equal 1)
    (expect (first (cl-cc/compile:ctx-instructions ctx)) :to-be :test)
    ;; LIFO: second emit becomes first in list
    (cl-cc/compile:emit ctx :second)
    (expect (first (cl-cc/compile:ctx-instructions ctx)) :to-be :second)
    (expect (second (cl-cc/compile:ctx-instructions ctx)) :to-be :test)))

;;; ─── lookup-var ─────────────────────────────────────────────────────────────

(it-sequential "ctx-lookup-var-behavior"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (push (cons 'x :R0) (cl-cc/compile:ctx-env ctx))
    (expect (cl-cc/compile:lookup-var ctx 'x) :to-be :R0))
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (signals error (cl-cc/compile:lookup-var ctx 'nonexistent)))
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (push (cons 'x :R0) (cl-cc/compile:ctx-env ctx))
    (push (cons 'x :R5) (cl-cc/compile:ctx-env ctx))
    (expect (cl-cc/compile:lookup-var ctx 'x) :to-be :R5)))

;;; ─── initialize-instance ────────────────────────────────────────────────────

(it-sequential "ctx-initialization"
  (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
    (expect (cl-cc/compile:ctx-top-level-p ctx) :to-be-truthy)
    (expect (cl-cc/compile:ctx-instructions ctx) :to-be-null)
    (expect (cl-cc/compile:ctx-env ctx) :to-be-null)
    (expect (typep (cl-cc/compile:ctx-type-env ctx) 'cl-cc/type:type-env) :to-be-truthy)
    (expect (cl-cc/type::type-env-bindings (cl-cc/compile:ctx-type-env ctx)) :to-be-null)
    (expect (gethash '*standard-output* (cl-cc/compile:ctx-global-variables ctx)) :to-be-truthy)
    (expect (gethash '*standard-input* (cl-cc/compile:ctx-global-variables ctx)) :to-be-truthy)
    ;; *features* is populated at warm-boot time, not at fresh context creation
    (expect (gethash '*package* (cl-cc/compile:ctx-global-variables ctx)) :to-be-truthy)))

;;; ─── REPL state ─────────────────────────────────────────────────────────────

(it-sequential "ctx-context-find-package-resolution cl"
  (destructuring-bind (name expect-p) (list :cl t)
    (let ((result (cl-cc/compile::%context-find-package name)))
    (if expect-p
        (expect (hash-table-p result) :to-be-truthy)
        (expect result :to-be-falsy)))))

(it-sequential "ctx-context-find-package-resolution cl-user"
  (destructuring-bind (name expect-p) (list :cl-user t)
    (let ((result (cl-cc/compile::%context-find-package name)))
    (if expect-p
        (expect (hash-table-p result) :to-be-truthy)
        (expect result :to-be-falsy)))))

(it-sequential "ctx-context-find-package-resolution keyword"
  (destructuring-bind (name expect-p) (list :keyword t)
    (let ((result (cl-cc/compile::%context-find-package name)))
    (if expect-p
        (expect (hash-table-p result) :to-be-truthy)
        (expect result :to-be-falsy)))))

(it-sequential "ctx-context-find-package-resolution unknown"
  (destructuring-bind (name expect-p) (list :totally-nonexistent-package-xyz nil)
    (let ((result (cl-cc/compile::%context-find-package name)))
    (if expect-p
        (expect (hash-table-p result) :to-be-truthy)
        (expect result :to-be-falsy)))))

(it-sequential "ctx-repl-state-persistence"
  (let ((cl-cc/compile:*repl-label-counter* 100))
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
      (expect (cl-cc/compile:ctx-next-label ctx) :to-equal 100)
      (expect (cl-cc/compile:make-label ctx "L") :to-equal "L_100")))
  (let ((cl-cc/compile:*repl-global-variables* (make-hash-table :test #'eq)))
    (setf (gethash 'my-var cl-cc/compile:*repl-global-variables*) t)
    (let ((ctx (make-instance 'cl-cc/compile:compiler-context)))
      (expect (gethash 'my-var (cl-cc/compile:ctx-global-variables ctx)) :to-be-truthy))))
