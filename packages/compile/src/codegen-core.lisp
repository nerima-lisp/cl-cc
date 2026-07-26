(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Core: Basic Forms + Control Flow
;;;
;;; Contains: defgeneric, *typed-binop-ctors*, and compile-ast methods for
;;; primitive/control-flow AST node types.
;;; The let-binding optimization subsystem is in codegen-core-let.lisp.
;;;
;;; Load order: before codegen-core-let, codegen-clos, codegen-functions, codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; NOTE: optimize-instructions (cl-cc/optimize) and emit-assembly (defined in
;; codegen-locals.lisp) are called via the umbrella use-package bridge.
;; No declaim needed — it would intern zombie symbols in this package.

(defgeneric compile-ast (node ctx))

(defvar *string-literal-pool* nil
  "Per-compilation-unit pool mapping literal constants to vm-const registers.")

(defun %copy-string-literal-pool (pool)
  "Return a shallow copy of literal constant POOL using EQUAL comparison."
  (when pool
    (let ((copy (make-hash-table :test #'equal
                                 :rehash-size (hash-table-rehash-size pool)
                                 :rehash-threshold (hash-table-rehash-threshold pool))))
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
                 pool)
      copy)))

(defun %call-with-no-tail-position (ctx thunk)
  "Call THUNK with CTX tail-position cleared, then restore the previous value."
  (let ((old-tail (ctx-tail-position ctx)))
    (unwind-protect
         (progn
           (setf (ctx-tail-position ctx) nil)
           (funcall thunk))
      (setf (ctx-tail-position ctx) old-tail))))

(defmacro %with-no-tail-position (ctx &body body)
  "Clear tail-position tracking while evaluating BODY."
  `(%call-with-no-tail-position ,ctx (lambda () ,@body)))

(defun %poolable-literal-p (value)
  "Return T when VALUE is a literal kind safe to deduplicate in the constant pool."
  (or (integerp value)
      (floatp value)
      (stringp value)
      (symbolp value)
      (characterp value)))

(defun %literal-pool-key (pool value)
  "Return VALUE's scoped hash-table key for the literal constant pool."
  (list pool
        (type-of value)
        (if (stringp value) (copy-seq value) value)))

(defun %literal-pool-call-barrier-p (inst)
  "Return T when INST ends the region a pooled literal's register is valid in.

Calls clobber caller-visible temporaries, and labels and closure headers end
straight-line control flow. The latter matter because the pool's validity token
is a *static* count of barriers emitted so far: without them, a literal pooled
inside a method body and one at a call site could carry the same count while
sitting in unrelated control-flow regions, so the call site would reuse a
register the body only ever writes when it runs. That is how

  (defmethod h ((x (eql 42))) 42) (h 42)

dispatched on a stale register and found no applicable method, while the same
method with any other body value dispatched correctly."
  (or (typep inst 'vm-call)
      (typep inst 'vm-tail-call)
      (typep inst 'vm-generic-call)
      (typep inst 'vm-apply)
      (typep inst 'vm-label)
      (typep inst 'vm-closure)
      (typep inst 'vm-func-ref)))

(defun %literal-pool-barrier-count (ctx)
  "Return the number of call-like barriers emitted in CTX so far."
  (count-if #'%literal-pool-call-barrier-p (ctx-instructions ctx)))

(defun %literal-pool-lookup (key barrier pool)
  "Return KEY's pooled register when it is valid for the current BARRIER."
  (multiple-value-bind (entry present-p)
      (gethash key pool)
    (cond
      ((not present-p) (values nil nil))
      ((and (consp entry) (eql (cdr entry) barrier))
       (values (car entry) t))
      (t (values nil nil)))))

(defun %literal-pool-invalidate-register (pool register)
  "Drop every POOL entry whose cached register is REGISTER.

A caller-supplied DST overwrites that register with a new value, so any literal
still recorded as living there is stale. Without this, emitting T into R5 on one
branch and NIL into R5 on the other left the pool claiming T was in R5; a later
request for T then compiled to a move from R5 — which by then held NIL. That is
how (not (null 4)) evaluated to NIL."
  (when (and pool register)
    (let ((dead nil))
      (maphash (lambda (key entry)
                 (when (and (consp entry) (eq (car entry) register))
                   (push key dead)))
               pool)
      (dolist (key dead) (remhash key pool)))))

(defun %emit-constant (ctx value &key dst)
  "Return a register containing VALUE.
Literals reuse a per-compilation-unit vm-const register via an EQUAL hash table."
  (if (and (%poolable-literal-p value) *string-literal-pool*)
      (let ((key (%literal-pool-key *string-literal-pool* value))
            (barrier (%literal-pool-barrier-count ctx)))
        (multiple-value-bind (existing present-p)
            (%literal-pool-lookup key barrier *string-literal-pool*)
        (if present-p
            (progn
              (when (and dst (not (eq dst existing)))
                (%literal-pool-invalidate-register *string-literal-pool* dst)
                (emit ctx (make-vm-move :dst dst :src existing)))
              (or dst existing))
            (let ((target-reg (or dst (make-register ctx))))
              (when dst
                (%literal-pool-invalidate-register *string-literal-pool* dst))
              (emit ctx (make-vm-const :dst target-reg :value value))
              (setf (gethash key *string-literal-pool*) (cons target-reg barrier))
              target-reg))))
      (let ((target-reg (or dst (make-register ctx))))
        (emit ctx (make-vm-const :dst target-reg :value value))
        target-reg)))

(defun %emit-vm-branch-boolean-as-cl-boolean (ctx predicate-reg result-reg label-prefix)
  "Convert a VM branch boolean in PREDICATE-REG into Common Lisp T/NIL."
  (let ((false-label (make-label ctx (format nil "~A_false" label-prefix)))
        (end-label (make-label ctx (format nil "~A_end" label-prefix))))
    (emit ctx (make-vm-jump-zero :reg predicate-reg :label false-label))
    (%emit-constant ctx t :dst result-reg)
    (emit ctx (make-vm-jump :label end-label))
    (emit ctx (make-vm-label :name false-label))
    (%emit-constant ctx nil :dst result-reg)
    (emit ctx (make-vm-label :name end-label))
    result-reg))

;;; ── Binary operator dispatch table (data layer) ──────────────────────────
;;;
;;; One table carries every constructor variant for a numeric/comparison operator.
;;; This keeps the policy declarative: adding a new operator means adding one
;;; entry, not updating 3 parallel tables plus dispatch code.

(defparameter *numeric-binop-ctor-specs*
  '((+  :generic make-vm-add    :fixnum make-vm-integer-add :float make-vm-float-add)
    (-  :generic make-vm-sub    :fixnum make-vm-integer-sub :float make-vm-float-sub)
    (*  :generic make-vm-mul    :fixnum make-vm-integer-mul :float make-vm-float-mul)
    (/  :generic make-vm-cl-div :float  make-vm-float-div)
    (=  :generic make-vm-num-eq :fixnum make-vm-num-eq)
    (<  :generic make-vm-lt     :fixnum make-vm-lt)
    (>  :generic make-vm-gt     :fixnum make-vm-gt)
    (<= :generic make-vm-le     :fixnum make-vm-le)
    (>= :generic make-vm-ge     :fixnum make-vm-ge))
  "(operator keyword constructor ...) specs for generic/fixnum/float binop codegen.")

(defparameter *numeric-comparison-binops* '(= < > <= >=)
  "Numeric binops whose VM result must be normalized to Common Lisp T/NIL in value position.")

(defun %numeric-comparison-binop-p (op)
  "Return T when OP produces a VM branch boolean rather than an arithmetic value."
  (member op *numeric-comparison-binops* :test #'eq))

(defun %lookup-numeric-binop-ctor-symbol (op kind)
  "Return the constructor symbol for OP/KIND, or NIL when no specialization exists."
  (let ((entry (assoc op *numeric-binop-ctor-specs* :test #'eq)))
    (when entry (getf (cdr entry) kind))))

(defun %numeric-binop-ctor-function (op kind)
  "Return the constructor function for OP/KIND or NIL when absent."
  (let ((symbol (%lookup-numeric-binop-ctor-symbol op kind)))
    (when symbol
      (symbol-function symbol))))

(defun binop-ctor (op)
  "Return the generic instruction constructor for OP."
  (or (%numeric-binop-ctor-function op :generic)
      (error "Unknown binary operator: ~S" op)))

(defparameter +codegen-fixnum-type+
  (parse-type-specifier 'fixnum))

(defparameter +codegen-float-type+
  (parse-type-specifier 'float))

(defparameter +codegen-symbol-type+
  (parse-type-specifier 'symbol))

(defun %ast-proven-type (ctx ast)
  "Return the currently proven type for AST, if any."
  (cond
    ((typep ast 'ast-int)
     (if (typep (ast-int-value ast) 'fixnum)
         +codegen-fixnum-type+
         nil))
    ((typep ast 'ast-the)
     (or (let ((declared (ast-the-type ast)))
           (if declared (parse-type-specifier declared) nil))
         (%ast-proven-type ctx (ast-the-value ast))))
    ((typep ast 'ast-var)
     (multiple-value-bind (scheme found-p)
         (type-env-lookup (ast-var-name ast) (ctx-type-env ctx))
       (if found-p (instantiate scheme) nil)))
    ((typep ast 'ast-quote)
     (let ((val (ast-quote-value ast)))
       (cond
         ((floatp val) +codegen-float-type+)
         (t nil))))
    (t nil)))

(defun %proven-fixnum-type-p (ty)
  "Return T if TY is a proven subtype of fixnum (or nil when type is unknown)."
  (and ty (is-subtype-p ty +codegen-fixnum-type+)))

(defun %proven-float-type-p (ty)
  "Return T if TY is a proven subtype of float."
  (and ty (is-subtype-p ty +codegen-float-type+)))

(defun %proven-symbol-type-p (ty)
  "Return T if TY is a proven subtype of symbol."
  (and ty (is-subtype-p ty +codegen-symbol-type+)))

(defun %float-literal-node-p (node)
  "Return T if NODE is a quoted float literal."
  (loop while (typep node 'ast-the)
        do (setf node (ast-the-value node)))
  (and (typep node 'ast-quote) (floatp (ast-quote-value node))))

(defun %numeric-binop-constructor (op lhs rhs ctx)
  "Select a numeric-specialized constructor for OP/LHS/RHS when possible.
Falls back to the generic binop-ctor when no specialization applies."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (cond
      ((and (%proven-fixnum-type-p lhs-type) (%proven-fixnum-type-p rhs-type))
       (or (%numeric-binop-ctor-function op :fixnum) (binop-ctor op)))
      ((or (and (%proven-float-type-p lhs-type) (%proven-float-type-p rhs-type))
           (and (%float-literal-node-p lhs) (%float-literal-node-p rhs)))
       (or (%numeric-binop-ctor-function op :float) (binop-ctor op)))
      (t (binop-ctor op)))))

(defun %numeric-binop-specialization-kind (lhs rhs ctx)
  "Return :FIXNUM, :FLOAT, or NIL for the statically proven binary operand kind."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (cond
      ((and (%proven-fixnum-type-p lhs-type) (%proven-fixnum-type-p rhs-type)) :fixnum)
      ((or (and (%proven-float-type-p lhs-type) (%proven-float-type-p rhs-type))
           (and (%float-literal-node-p lhs) (%float-literal-node-p rhs))) :float)
      (t nil))))

(defun %guard-type-for-numeric-kind (kind)
  (case kind
    (:fixnum 'fixnum)
    (:float 'float)
    (t nil)))

(defun %equality-predicate-constructor (func-sym lhs rhs ctx)
  "Select a specialized constructor for EQ/EQL/EQUAL when operand types are proven.
Fixnum pairs lower to direct numeric comparison. Symbol pairs lower to pointer-style
VM equality. Unknown types preserve each predicate's existing generic path."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (cond
      ((and (%proven-fixnum-type-p lhs-type) (%proven-fixnum-type-p rhs-type))
       #'make-vm-num-eq)
      ((and (%proven-symbol-type-p lhs-type) (%proven-symbol-type-p rhs-type))
       #'make-vm-eq)
      ((member func-sym '(eq eql) :test #'eq)
       #'make-vm-eq)
      ((eq func-sym 'equal)
       #'make-vm-equal)
      (t nil))))

;;; ── Primitive literal forms ──────────────────────────────────────────────

(defmethod compile-ast ((node ast-int) ctx)
  (%emit-constant ctx (ast-int-value node)))

(defmethod compile-ast ((node ast-hole) ctx)
  (declare (ignore ctx))
  (ast-error node "Typed hole '_' must be filled before compilation."))

(defun %earmuffed-special-name-p (name)
  "T when NAME is a symbol whose name is earmuffed (e.g. *FOO*) — the universal
CL convention for a special (dynamic) variable. Used to decide whether an
otherwise-unknown reference may be deferred to a runtime global read instead of
failing at compile time."
  (and (symbolp name)
       (let ((s (symbol-name name)))
         (and (>= (length s) 3)
              (char= (char s 0) #\*)
              (char= (char s (1- (length s))) #\*)))))

(defmethod compile-ast ((node ast-var) ctx)
  (let ((name (ast-var-name node)))
    (when (or (eq name t) (eq name nil) (keywordp name))
      (return-from compile-ast (%emit-constant ctx name)))
    (let ((local-entry (%assoc-eq name (ctx-env ctx))))
      (when (and local-entry (%member-eq-p name (ctx-boxed-vars ctx)))
        (let ((dst (make-register ctx)))
          (emit ctx (make-vm-car :dst dst :src (cdr local-entry)))
          (return-from compile-ast dst)))
      (when local-entry
        (return-from compile-ast (cdr local-entry))))
    (multiple-value-bind (constant-value constant-present-p)
        (gethash name *constant-table*)
      (when constant-present-p
        (return-from compile-ast (%emit-constant ctx constant-value))))
    (when (or (gethash name (ctx-global-variables ctx)) (boundp name))
      (let ((cache-reg (%global-cache-reg ctx name)))
        (when cache-reg
          (return-from compile-ast cache-reg))
        (let ((dst (make-register ctx)))
          (emit ctx (make-vm-get-global :dst dst :name name))
          (return-from compile-ast dst))))
    ;; Earmuffed (*foo*) names are special/dynamic by CL convention. A reference
    ;; to one that is not yet known at compile time is the classic deferred-binding
    ;; case — the value arrives at runtime from a (load ...) or an earlier-executed
    ;; top-level form in the same compilation unit. Emit a deferred global read:
    ;; vm-get-global signals a clean "Unbound global variable" at RUNTIME if it is
    ;; genuinely never set (correct CL semantics for an unbound special). Non-
    ;; earmuffed unknowns still fail fast here so lexical typos are caught.
    (if (%earmuffed-special-name-p name)
        (let ((dst (make-register ctx)))
          (emit ctx (make-vm-get-global :dst dst :name name))
          dst)
        (error "Unbound variable: ~S" name))))

(defmethod compile-ast ((node ast-binop) ctx)
  ;; binop is never in tail position itself; clear to prevent sub-expression leakage
  (%with-no-tail-position ctx
    (let* ((lhs-ast (ast-binop-lhs node))
           (rhs-ast (ast-binop-rhs node))
           (op (ast-binop-op node))
           (lhs-reg (compile-ast lhs-ast ctx))
           ;; Protect LHS-REG across RHS: if RHS contains a call (e.g.
           ;; `(+ (* n n) (f x))'), the call must save/restore the LHS temp instead
           ;; of clobbering it — incremental liveness can't see LHS's future use here.
           (rhs-reg (%compile-operand-protecting rhs-ast ctx (list lhs-reg)))
           (dst (make-register ctx))
           (comparison-p (%numeric-comparison-binop-p op))
           (result-reg (if comparison-p (make-register ctx) dst))
           (kind (%numeric-binop-specialization-kind lhs-ast rhs-ast ctx))
           (guard-type (%guard-type-for-numeric-kind kind))
           (ctor (%numeric-binop-constructor op lhs-ast rhs-ast ctx)))
      (if (and (eq (ctx-target ctx) :vm)
               kind guard-type (> (ctx-safety ctx) 0))
          (let ((deopt-label (make-label ctx "deopt_binop"))
                (slow-label (make-label ctx "deopt_binop_slow"))
                (done-label (make-label ctx "deopt_binop_done"))
                (deopt-id (make-label ctx "deopt_guard")))
            (emit ctx (cl-cc/vm::make-vm-type-check
                       :src lhs-reg :type-name guard-type :label deopt-label :id deopt-id))
            (emit ctx (cl-cc/vm::make-vm-type-check
                       :src rhs-reg :type-name guard-type :label deopt-label :id deopt-id))
            (emit ctx (funcall ctor :dst result-reg :lhs lhs-reg :rhs rhs-reg))
            (emit ctx (make-vm-jump :label done-label))
            (emit ctx (make-vm-label :name deopt-label))
            (emit ctx (cl-cc/vm::make-vm-deopt
                       :label slow-label :id deopt-id :reason (list :type-check guard-type)))
            (emit ctx (make-vm-label :name slow-label))
            (emit ctx (funcall (binop-ctor op) :dst result-reg :lhs lhs-reg :rhs rhs-reg))
            (emit ctx (make-vm-label :name done-label)))
          (emit ctx (funcall ctor :dst result-reg :lhs lhs-reg :rhs rhs-reg)))
      (when comparison-p
        (%emit-vm-branch-boolean-as-cl-boolean ctx result-reg dst "binop_bool"))
      dst)))

(defmethod compile-ast ((node ast-progn) ctx)
  (let ((forms (ast-progn-forms node)))
    (if forms
        (let ((tail (ctx-tail-position ctx))
              (last nil))
          (loop for rest on forms
                do (setf (ctx-tail-position ctx) (if (null (cdr rest)) tail nil))
                   (setf last (compile-ast (car rest) ctx)))
          last)
        (%emit-constant ctx nil))))

(defmethod compile-ast ((node ast-print) ctx)
  (%with-no-tail-position ctx
    (let ((reg (compile-ast (ast-print-expr node) ctx)))
      (emit ctx (make-vm-print :reg reg))
      reg)))

(defun %branch-type-env (ctx guard-var guard-type branch)
  "Return a branch-specialized type environment for GUARD-VAR/GUARD-TYPE."
  (let ((base-env (ctx-type-env ctx)))
    (if guard-var
        (if (eq branch :then)
            (type-env-extend guard-var (type-to-scheme guard-type) base-env)
            (if (eq branch :else)
                (multiple-value-bind (scheme found-p)
                    (type-env-lookup guard-var base-env)
                  (let ((current-type (if found-p (instantiate scheme) nil)))
                    (if (and current-type (typep current-type 'type-union))
                        (type-env-extend guard-var
                                         (type-to-scheme
                                          (narrow-union-type current-type guard-type))
                                         base-env)
                        base-env)))
                base-env))
        base-env)))

(defun %case-of-case-collapse-node (node outer-cond thenp)
  "Recursively skip NODE when it is an ast-if whose condition equals OUTER-COND.
Returns the matching branch (THEN or ELSE per THENP) until the condition differs."
  (if (and (typep node 'ast-if)
           (equal (ast-to-sexp (ast-if-cond node))
                  (ast-to-sexp outer-cond)))
      (%case-of-case-collapse-node (if thenp (ast-if-then node) (ast-if-else node))
                                   outer-cond thenp)
      node))

(defun %case-of-case-collapse-branch (outer-cond branch thenp)
  "Collapse nested IF branches that repeat the same outer condition.
Only removes the inner test when the condition matches exactly."
  (%case-of-case-collapse-node branch outer-cond thenp))

(defun %compile-if-branch (ast ctx dst tail guard-var guard-type branch &optional jump-label)
  "Compile one branch of an IF into CTX with type-env narrowing.
Emits a move from the branch result into DST; optionally emits a jump to JUMP-LABEL."
  (setf (ctx-tail-position ctx) tail)
  (let ((saved-type-env (ctx-type-env ctx))
        (saved-guard-vars (ctx-guard-narrowed-vars ctx)))
    (unwind-protect
         (let ((*string-literal-pool* (%copy-string-literal-pool *string-literal-pool*)))
           (setf (ctx-type-env ctx) (%branch-type-env ctx guard-var guard-type branch))
           ;; The :then branch proves GUARD-VAR at runtime via the guard test;
           ;; record it so redundant `the` assertions keep a lightweight check
           ;; but drop the failure path.
           (when (and guard-var (eq branch :then))
             (setf (ctx-guard-narrowed-vars ctx)
                   (cons guard-var (ctx-guard-narrowed-vars ctx))))
           (let ((result-reg (compile-ast ast ctx)))
             (%with-no-tail-position ctx
               (emit ctx (make-vm-move :dst dst :src result-reg))
               (when jump-label
                 (emit ctx (make-vm-jump :label jump-label))))))
      (setf (ctx-type-env ctx) saved-type-env)
      (setf (ctx-guard-narrowed-vars ctx) saved-guard-vars))))

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((tail       (ctx-tail-position ctx))
         (cond-ast   (ast-if-cond node))
         (then-ast   (%case-of-case-collapse-branch cond-ast (ast-if-then node) t))
         (else-ast   (%case-of-case-collapse-branch cond-ast (ast-if-else node) nil))
         (guard-info (multiple-value-list (extract-type-guard cond-ast)))
         (guard-var  (first guard-info))
         (guard-type (second guard-info))
         (cond-reg   (%with-no-tail-position ctx
                       (compile-ast cond-ast ctx)))
         (dst        (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label  (make-label ctx "ifend")))
    (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
    (%compile-if-branch then-ast ctx dst tail guard-var guard-type :then end-label)
    (emit ctx (make-vm-label :name else-label))
    (%compile-if-branch else-ast ctx dst tail guard-var guard-type :else nil)
    (emit ctx (make-vm-label :name end-label))
    dst))
