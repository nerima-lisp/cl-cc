(in-package :cl-cc/compile)

(defun %compile-body/k (forms ctx continuation)
  "Compile FORMS as a body, then call CONTINUATION with the last register."
  (let ((last nil))
    (dolist (form forms)
      (setf last (compile-ast form ctx)))
    (funcall continuation last)))

(defun %compile-body-with-tail (body tail ctx)
  "Compile BODY forms with tail-position tracking and return the last result register."
  (labels ((scan (forms last-reg)
             (if (consp forms)
                 (progn
                   (setf (ctx-tail-position ctx)
                         (if (null (cdr forms)) tail nil))
                   (scan (cdr forms) (compile-ast (car forms) ctx)))
                 last-reg)))
    (scan body nil)))

(defmacro %with-restored-tail-position (ctx &body body)
  "Run BODY with CTX tail-position restored afterward."
  `(let ((old-tail (ctx-tail-position ,ctx)))
     (unwind-protect
          (progn ,@body)
       (setf (ctx-tail-position ,ctx) old-tail))))

(defmacro %with-restored-ctx-env ((accessor ctx new-env) &body body)
  "Set CTX env ACCESSOR to NEW-ENV for BODY, restoring the original afterward."
  `(let ((old-env (,accessor ,ctx)))
     (unwind-protect
          (progn
            (setf (,accessor ,ctx) ,new-env)
            ,@body)
       (setf (,accessor ,ctx) old-env))))

(defun %compile-body-with-tail-ret (body tail ctx)
  "Compile BODY with tail tracking and emit a return from the last result."
  (let ((last-reg (%compile-body-with-tail body tail ctx)))
    (%call-with-no-tail-position ctx
      (lambda ()
        (emit ctx (make-vm-ret :reg last-reg))))))
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Control Flow, Assignment, and Type Assertions
;;;
;;; Contains: %compile-body/k, %compile-body-with-tail, %compile-body-with-tail-ret,
;;; %with-restored-tail-position,
;;; lookup-block, compile-ast for ast-block/ast-return-from,
;;; lookup-tag, compile-ast for ast-tagbody/ast-go,
;;; compile-ast for ast-setq/ast-quote/ast-the,
;;; type-error-message-from-mismatch, %emit-the-runtime-assertion.
;;;
;;; Primitive and if-form compilation (compile-ast for ast-int through ast-if)
;;; plus the binop dispatch table and helpers are in codegen-core.lisp (loads before).
;;;
;;; Load order: after codegen-core.lisp, before codegen-core-let.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Control flow: block / return-from ────────────────────────────────────
;;; (Let-binding optimization subsystem is in codegen-core-let.lisp.)

(defun %lookup-associated-entry (env name error-format)
  "Return the cdr of NAME in ENV or signal ERROR-FORMAT."
  (let ((entry (%assoc-eq name env)))
    (if entry
        (cdr entry)
        (error error-format name))))

(defun lookup-block (ctx name)
  "Look up a block by name, returning (exit-label . result-reg) or error."
  (%lookup-associated-entry (ctx-block-env ctx) name "Unknown block: ~S"))

(defmethod compile-ast ((node ast-block) ctx)
  (%with-no-tail-position ctx
    (let* ((block-name (ast-block-name node))
           (exit-label (make-label ctx "block_exit"))
           (result-reg (make-register ctx))
           (block-env (cons (cons block-name (cons exit-label result-reg))
                            (ctx-block-env ctx))))
      (%with-restored-ctx-env (ctx-block-env ctx block-env)
        (emit ctx (make-vm-move
                   :dst result-reg
                   :src (%compile-body/k (ast-block-body node) ctx #'identity))))
      (emit ctx (make-vm-label :name exit-label))
      result-reg)))

(defmethod compile-ast ((node ast-return-from) ctx)
  (%with-no-tail-position ctx
    (let* ((block-info (lookup-block ctx (ast-return-from-name node)))
           (exit-label (car block-info))
           (result-reg (cdr block-info))
           (value-reg (compile-ast (ast-return-from-value node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src value-reg))
      (emit ctx (make-vm-jump :label exit-label))
      result-reg)))

;;; ── Control flow: tagbody / go ───────────────────────────────────────────

(defun lookup-tag (ctx tag)
  "Look up a tag within the current tagbody, returning its label or error."
  (%lookup-associated-entry (ctx-tagbody-env ctx) tag "Unknown tag: ~S"))

(defmethod compile-ast ((node ast-tagbody) ctx)
  (%with-no-tail-position ctx
    (let* ((*string-literal-pool* nil)
           (tags (ast-tagbody-tags node))
           (end-label (make-label ctx "tagbody_end"))
           (result-reg (make-register ctx))
           (tag-labels (loop for tag-entry in tags
                             collect (cons (car tag-entry) (make-label ctx "tag"))))
           (tagbody-env (append tag-labels (ctx-tagbody-env ctx))))
      (%with-restored-ctx-env (ctx-tagbody-env ctx tagbody-env)
        (if tag-labels
            (emit ctx (make-vm-jump :label (cdr (car tag-labels))))
            (emit ctx (make-vm-const :dst result-reg :value nil)))
        (loop for tag-entry in tags
              do (let* ((tag   (car tag-entry))
                        (forms (cdr tag-entry))
                        (label (%lookup-associated-entry tag-labels tag "Unknown tag: ~S")))
                   (emit ctx (make-vm-label :name label))
                   (dolist (form forms) (compile-ast form ctx)))))
      (emit ctx (make-vm-label :name end-label))
      (emit ctx (make-vm-const :dst result-reg :value nil))
      result-reg)))

(defmethod compile-ast ((node ast-go) ctx)
  (let ((label (lookup-tag ctx (ast-go-tag node))))
    (emit ctx (make-vm-jump :label label)))
  (%emit-constant ctx nil))

;;; ── Assignment: setq / quote / the ──────────────────────────────────────

(defmethod compile-ast ((node ast-setq) ctx)
  (%with-no-tail-position ctx
    (let* ((var-name (ast-setq-var node))
           (value-reg (compile-ast (ast-setq-value node) ctx))
           (local-entry (%assoc-eq var-name (ctx-env ctx))))
      (cond
        ((and local-entry (%member-eq-p var-name (ctx-boxed-vars ctx)))
         ;; Boxed variable: write via (rplaca box new-val)
         (emit ctx (make-vm-rplaca :cons (cdr local-entry) :val value-reg))
         value-reg)
        (local-entry
         (emit ctx (make-vm-move :dst (cdr local-entry) :src value-reg))
         (cdr local-entry))
        ((gethash var-name (ctx-global-variables ctx))
         (let ((cache-reg (%global-cache-reg ctx var-name)))
           (when cache-reg
             (emit ctx (make-vm-move :dst cache-reg :src value-reg)))
           (emit ctx (make-vm-set-global :name var-name :src (or cache-reg value-reg))))
         value-reg)
        (t
         ;; Assigning to an as-yet-unknown variable creates it as a global. This
         ;; matches the dynamic-language semantics of the PHP/JS frontends (a fresh
         ;; `$x = …' / `x = …' defines the variable) and lets a programmatically
         ;; built setq node — e.g. the preg_match($s,$m) $matches out-param
         ;; lowering — target a variable the parser never declared.
         (setf (gethash var-name (ctx-global-variables ctx)) t)
         (emit ctx (make-vm-set-global :name var-name :src value-reg))
         value-reg)))))

(defmethod compile-ast ((node ast-quote) ctx)
  (%emit-constant ctx (ast-quote-value node)))

(defun type-error-message-from-mismatch (e)
  "Extract a human-readable message from a type-mismatch-error condition."
  (format nil "expected ~A but got ~A"
          (type-to-string (type-mismatch-error-expected e))
          (type-to-string (type-mismatch-error-actual e))))

(defun %values-type-specifier-p (declared-spec)
  (and (consp declared-spec)
       (eq (car declared-spec) 'values)))

(defun %emit-the-runtime-assertion (ctx value-reg declared-spec &key (emit-failure-p t))
  "Emit a runtime assertion for (the DECLARED-TYPE VALUE-REG).
When EMIT-FAILURE-P is NIL, keep the lightweight type check but omit failure handling."
  (unless (or (null declared-spec)
              (eq declared-spec 't)
              (type-unknown-p declared-spec))
    (when (> (ctx-safety ctx) 0)
      (let ((check-reg (make-register ctx))
            (values-type-p (%values-type-specifier-p declared-spec)))
        (if values-type-p
            (emit ctx (make-vm-values-typep :dst check-reg :src value-reg :type-name declared-spec))
            (emit ctx (make-vm-typep :dst check-reg :src value-reg :type-name declared-spec)))
        (when emit-failure-p
          (let ((fail-label (make-label ctx "the_fail"))
                (done-label (make-label ctx "the_done"))
                (error-reg (make-register ctx)))
            (emit ctx (make-vm-jump-zero :reg check-reg :label fail-label))
            (emit ctx (make-vm-jump :label done-label))
            (emit ctx (make-vm-label :name fail-label))
            (emit ctx (make-vm-type-error-condition
                       :dst error-reg
                       :expected-type declared-spec
                       :datum-reg value-reg
                       :values-p values-type-p))
            (emit ctx (make-vm-signal-error :error-reg error-reg))
            (emit ctx (make-vm-label :name done-label)))))))
  value-reg)

(defmethod compile-ast ((node ast-the) ctx)
  "Compile a type declaration. In typed-function mode, verifies the type at compile time."
  (let* ((value (ast-the-value node))
         (transparent-value (%ast-transparent-designator-node value))
         (reg (compile-ast value ctx))
         (declared (ast-the-type node))
         (declared-spec (and declared (parse-type-specifier declared)))
         (proven-type (and (typep transparent-value 'ast-var)
                           (%ast-proven-type ctx transparent-value)))
         (proven-type-matches-p (and proven-type
                                     declared-spec
                                     (type-equal-p proven-type declared-spec))))
    (when *compiling-typed-fn*
      (when (and declared-spec
                 (not proven-type-matches-p))
        (handler-case
            (check transparent-value declared-spec
                   (or (ctx-type-env ctx)
                       (type-env-empty)))
          (type-mismatch-error (e)
            (error 'ast-compilation-error
                   :location (format nil "~A:~A"
                                     (ast-source-file node)
                                     (ast-source-line node))
                   :format-control "Type error in ~A: ~A"
                   :format-arguments (list *compiling-typed-fn*
                                           (type-error-message-from-mismatch e))))
          (type-inference-error () nil))))
    (unless proven-type-matches-p
      (%emit-the-runtime-assertion ctx reg declared))
    reg))
