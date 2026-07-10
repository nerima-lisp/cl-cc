;;;; frontend/php/parser-expr-advanced-core.lisp -- PHP Extended Expression Core Helpers
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Tiny AST Builders ───────────────────────────────────────────────────────

(defun %php-null-quote ()
  "Return the AST representation of PHP null."
  (make-ast-quote :value +php-null+))

(defun %php-call (name &rest args)
  "Build an AST call to NAME with ARGS."
  (make-ast-call :func (make-ast-var :name name) :args args))

(defun %php-truthy-call (expr)
  "Build a PHP truthiness helper call for conditional contexts."
  (%php-call 'cl-cc/php::%php-truthy expr))

(defun %php-shadow-ref-vars (ref-vars bound-vars)
  "Remove BOUND-VARS from REF-VARS for recursive body rewriting."
  (set-difference ref-vars (mapcar #'%php-param-name bound-vars) :test #'eq))

(defun %php-rewrite-ref-vars (node-or-list ref-vars)
  "Rewrite reads/writes of REF-VARS so PHP by-reference locals use ref boxes."
  (labels ((walk-list (forms refs)
             (mapcar (lambda (form) (walk form refs)) forms))
           (walk-bindings (bindings refs)
             (mapcar (lambda (binding)
                       (cons (car binding) (walk (cdr binding) refs)))
                     bindings))
           (walk-initargs (initargs refs)
             (loop for (key . value) in initargs
                   collect (cons key (walk value refs))))
           (call (name &rest args)
             (apply #'%php-call name args))
           (ref-var-p (sym refs)
             (member sym refs :test #'eq))
           (walk (node refs)
             (cond
               ((null node) nil)
               ((listp node) (walk-list node refs))
               ((ast-var-p node)
                (let ((name (ast-var-name node)))
                  (if (ref-var-p name refs)
                      (call 'cl-cc/php::%php-deref node)
                      node)))
               ((ast-setq-p node)
                (let ((var (ast-setq-var node))
                      (value (walk (ast-setq-value node) refs)))
                  (if (ref-var-p var refs)
                      (call 'cl-cc/php::%php-ref-set! (make-ast-var :name var) value)
                      (make-ast-setq :var var :value value))))
               ((ast-binop-p node)
                (make-ast-binop :op (ast-binop-op node)
                                :lhs (walk (ast-binop-lhs node) refs)
                                :rhs (walk (ast-binop-rhs node) refs)))
               ((ast-if-p node)
                (make-ast-if :cond (walk (ast-if-cond node) refs)
                             :then (walk (ast-if-then node) refs)
                             :else (walk (ast-if-else node) refs)))
               ((ast-progn-p node)
                (make-ast-progn :forms (walk-list (ast-progn-forms node) refs)))
               ((ast-print-p node)
                (make-ast-print :expr (walk (ast-print-expr node) refs)))
               ((ast-let-p node)
                (let* ((bindings (ast-let-bindings node))
                       (ref-bindings (remove-if-not (lambda (binding)
                                                      (ref-var-p (car binding) refs))
                                                    bindings))
                       (plain-bindings (remove-if (lambda (binding)
                                                    (ref-var-p (car binding) refs))
                                                  bindings))
                       (body-refs (%php-shadow-ref-vars refs (mapcar #'car plain-bindings)))
                       (ref-sets (mapcar (lambda (binding)
                                           (call 'cl-cc/php::%php-ref-set!
                                                 (make-ast-var :name (car binding))
                                                 (walk (cdr binding) refs)))
                                         ref-bindings)))
                  (if ref-bindings
                      (make-ast-progn
                       :forms (append
                               ref-sets
                               (if plain-bindings
                                   (list (make-ast-let
                                          :bindings (walk-bindings plain-bindings refs)
                                          :declarations (ast-let-declarations node)
                                          :body (walk-list (ast-let-body node) body-refs)))
                                   (walk-list (ast-let-body node) refs))))
                      (make-ast-let :bindings (walk-bindings bindings refs)
                                    :declarations (ast-let-declarations node)
                                    :body (walk-list (ast-let-body node) body-refs)))))
               ((ast-lambda-p node)
                (let ((body-refs (%php-shadow-ref-vars
                                  refs
                                  (append (ast-lambda-params node)
                                          (ast-lambda-optional-params node)
                                          (ast-lambda-key-params node)
                                          (when (ast-lambda-rest-param node)
                                            (list (ast-lambda-rest-param node)))))))
                  (make-ast-lambda :params (ast-lambda-params node)
                                   :optional-params (ast-lambda-optional-params node)
                                   :rest-param (ast-lambda-rest-param node)
                                   :key-params (ast-lambda-key-params node)
                                   :declarations (ast-lambda-declarations node)
                                   :body (walk-list (ast-lambda-body node) body-refs)
                                   :env (ast-lambda-env node))))
               ((ast-defun-p node)
                (let ((body-refs (%php-shadow-ref-vars
                                  refs
                                  (append (ast-defun-params node)
                                          (ast-defun-optional-params node)
                                          (ast-defun-key-params node)
                                          (when (ast-defun-rest-param node)
                                            (list (ast-defun-rest-param node)))))))
                  (make-ast-defun :name (ast-defun-name node)
                                  :params (ast-defun-params node)
                                  :optional-params (ast-defun-optional-params node)
                                  :rest-param (ast-defun-rest-param node)
                                  :key-params (ast-defun-key-params node)
                                  :declarations (ast-defun-declarations node)
                                  :documentation (ast-defun-documentation node)
                                  :body (walk-list (ast-defun-body node) body-refs))))
               ((ast-call-p node)
                (make-ast-call :func (walk (ast-call-func node) refs)
                               :args (walk-list (ast-call-args node) refs)))
               ((ast-apply-p node)
                (make-ast-apply :func (walk (ast-apply-func node) refs)
                                :args (walk-list (ast-apply-args node) refs)))
               ((ast-block-p node)
                (make-ast-block :name (ast-block-name node)
                                :body (walk-list (ast-block-body node) refs)))
               ((ast-return-from-p node)
                (make-ast-return-from :name (ast-return-from-name node)
                                      :value (walk (ast-return-from-value node) refs)))
               ((ast-tagbody-p node)
                (make-ast-tagbody
                 :tags (mapcar (lambda (tag)
                                  (if (typep tag 'cl-cc/ast:ast-node)
                                      (walk tag refs)
                                      tag))
                                (ast-tagbody-tags node))))
               ((ast-values-p node)
                (make-ast-values :forms (walk-list (ast-values-forms node) refs)))
               ((ast-multiple-value-call-p node)
                (make-ast-multiple-value-call :func (walk (ast-mv-call-func node) refs)
                                              :args (walk-list (ast-mv-call-args node) refs)))
               ((ast-multiple-value-prog1-p node)
                (make-ast-multiple-value-prog1 :first (walk (ast-mv-prog1-first node) refs)
                                               :forms (walk-list (ast-mv-prog1-forms node) refs)))
               ((ast-multiple-value-bind-p node)
                (let ((body-refs (%php-shadow-ref-vars refs (ast-mvb-vars node))))
                  (make-ast-multiple-value-bind :vars (ast-mvb-vars node)
                                                :values-form (walk (ast-mvb-values-form node) refs)
                                                :body (walk-list (ast-mvb-body node) body-refs))))
               ((ast-catch-p node)
                (make-ast-catch :tag (walk (ast-catch-tag node) refs)
                                :body (walk-list (ast-catch-body node) refs)))
               ((ast-throw-p node)
                (make-ast-throw :tag (walk (ast-throw-tag node) refs)
                                :value (walk (ast-throw-value node) refs)))
               ((ast-unwind-protect-p node)
                (make-ast-unwind-protect :protected (walk (ast-unwind-protected node) refs)
                                         :cleanup (walk-list (ast-unwind-cleanup node) refs)))
               ((ast-handler-case-p node)
                (make-ast-handler-case :form (walk (ast-handler-case-form node) refs)
                                       :clauses (mapcar (lambda (clause)
                                                          (destructuring-bind (types var body) clause
                                                            (list types var
                                                                  (walk-list body
                                                                             (%php-shadow-ref-vars refs
                                                                                                    (when var (list var)))))))
                                                        (ast-handler-case-clauses node))))
               ((ast-list-p node)
                (make-ast-list :elements (walk-list (ast-list-elements node) refs)))
               ((ast-the-p node)
                (make-ast-the :type (ast-the-type node)
                              :value (walk (ast-the-value node) refs)))
               ((ast-make-instance-p node)
                (make-ast-make-instance :class (walk (ast-make-instance-class node) refs)
                                        :initargs (walk-initargs (ast-make-instance-initargs node) refs)))
               ((ast-slot-value-p node)
                (make-ast-slot-value :object (walk (ast-slot-value-object node) refs)
                                     :slot (ast-slot-value-slot node)))
               ((ast-set-slot-value-p node)
                (make-ast-set-slot-value :object (walk (ast-set-slot-value-object node) refs)
                                         :slot (ast-set-slot-value-slot node)
                                         :value (walk (ast-set-slot-value-value node) refs)))
               ((ast-set-gethash-p node)
                (make-ast-set-gethash :key (walk (ast-set-gethash-key node) refs)
                                      :table (walk (ast-set-gethash-table node) refs)
                                      :value (walk (ast-set-gethash-value node) refs)))
               (t node))))
    (walk node-or-list ref-vars)))

(defun %php-reference-marker (expr)
  "Mark a parsed &EXPR reference operand until assignment lowering sees it."
  (%php-call '%php-reference-marker expr))

(defun %php-reference-marker-p (node)
  "Return true when NODE is the internal &EXPR marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-reference-marker)))))

(defun %php-reference-marker-expr (node)
  "Return the referenced expression stored in a reference marker."
  (first (ast-call-args node)))

(defun %php-reference-assignment-marker (dest source)
  "Mark $DEST = &$SOURCE until statement-sequence lowering can create aliases."
  (%php-call '%php-reference-assignment-marker
             (make-ast-var :name dest)
             (make-ast-var :name source)))

(defun %php-reference-assignment-marker-p (node)
  "Return true when NODE is the internal reference-assignment marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-reference-assignment-marker)))))

(defun %php-reference-assignment-dest (node)
  "Return the destination symbol for a reference-assignment marker."
  (ast-var-name (first (ast-call-args node))))

(defun %php-reference-assignment-source (node)
  "Return the source symbol for a reference-assignment marker."
  (ast-var-name (second (ast-call-args node))))

(defun %php-variable-init-expression-marker (var value result)
  "Mark a PHP expression that first initializes VAR before yielding RESULT."
  (%php-call '%php-variable-init-expression-marker
             (make-ast-var :name var)
             value
             result))

(defun %php-variable-init-expression-marker-p (node)
  "Return true when NODE is the internal first-use variable init marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-variable-init-expression-marker)))))

(defun %php-variable-init-expression-var (node)
  "Return the variable symbol stored in a first-use init marker."
  (ast-var-name (first (ast-call-args node))))

(defun %php-variable-init-expression-value (node)
  "Return the initialization value stored in a first-use init marker."
  (second (ast-call-args node)))

(defun %php-variable-init-expression-result (node)
  "Return the expression result stored in a first-use init marker."
  (third (ast-call-args node)))

(defun %php-hoist-variable-init-expressions (node)
  "Rewrite first-use variable init markers and return hoisted VAR/VALUE pairs.

Markers can appear inside a larger expression, for example `echo (++$x).$x`.
The hoisted empty-body lets are emitted before the current statement so
php-finish-let-bindings scopes them over the rewritten statement and the rest of
the PHP block."
  (let ((bindings nil))
    (labels ((walk-list (forms)
               (mapcar #'walk forms))
             (walk-bindings (pairs)
               (mapcar (lambda (pair)
                         (cons (car pair) (walk (cdr pair))))
                       pairs))
             (walk-initargs (initargs)
               (loop for (key . value) in initargs
                     collect (cons key (walk value))))
             (walk (form)
               (cond
                 ((null form) nil)
                 ((listp form) (walk-list form))
                 ((%php-variable-init-expression-marker-p form)
                  (let ((value (walk (%php-variable-init-expression-value form))))
                    (push (cons (%php-variable-init-expression-var form) value)
                          bindings))
                  (walk (%php-variable-init-expression-result form)))
                 ((ast-binop-p form)
                  (make-ast-binop :op (ast-binop-op form)
                                  :lhs (walk (ast-binop-lhs form))
                                  :rhs (walk (ast-binop-rhs form))))
                 ((ast-if-p form)
                  (make-ast-if :cond (walk (ast-if-cond form))
                               :then (walk (ast-if-then form))
                               :else (walk (ast-if-else form))))
                 ((ast-progn-p form)
                  (make-ast-progn :forms (walk-list (ast-progn-forms form))))
                 ((ast-print-p form)
                  (make-ast-print :expr (walk (ast-print-expr form))))
                 ((ast-let-p form)
                  (make-ast-let :bindings (walk-bindings (ast-let-bindings form))
                                :declarations (ast-let-declarations form)
                                :body (walk-list (ast-let-body form))))
                 ((ast-setq-p form)
                  (make-ast-setq :var (ast-setq-var form)
                                 :value (walk (ast-setq-value form))))
                 ((ast-call-p form)
                  (make-ast-call :func (walk (ast-call-func form))
                                 :args (walk-list (ast-call-args form))))
                 ((ast-apply-p form)
                  (make-ast-apply :func (walk (ast-apply-func form))
                                  :args (walk-list (ast-apply-args form))))
                 ((ast-block-p form)
                  (make-ast-block :name (ast-block-name form)
                                  :body (walk-list (ast-block-body form))))
                 ((ast-return-from-p form)
                  (make-ast-return-from :name (ast-return-from-name form)
                                        :value (walk (ast-return-from-value form))))
                 ((ast-values-p form)
                  (make-ast-values :forms (walk-list (ast-values-forms form))))
                 ((ast-multiple-value-call-p form)
                  (make-ast-multiple-value-call :func (walk (ast-mv-call-func form))
                                                :args (walk-list (ast-mv-call-args form))))
                 ((ast-multiple-value-prog1-p form)
                  (make-ast-multiple-value-prog1 :first (walk (ast-mv-prog1-first form))
                                                 :forms (walk-list (ast-mv-prog1-forms form))))
                 ((ast-multiple-value-bind-p form)
                  (make-ast-multiple-value-bind :vars (ast-mvb-vars form)
                                                :values-form (walk (ast-mvb-values-form form))
                                                :body (walk-list (ast-mvb-body form))))
                 ((ast-catch-p form)
                  (make-ast-catch :tag (walk (ast-catch-tag form))
                                  :body (walk-list (ast-catch-body form))))
                 ((ast-throw-p form)
                  (make-ast-throw :tag (walk (ast-throw-tag form))
                                  :value (walk (ast-throw-value form))))
                 ((ast-unwind-protect-p form)
                  (make-ast-unwind-protect :protected (walk (ast-unwind-protected form))
                                           :cleanup (walk-list (ast-unwind-cleanup form))))
                 ((ast-handler-case-p form)
                  (make-ast-handler-case
                   :form (walk (ast-handler-case-form form))
                   :clauses (mapcar (lambda (clause)
                                      (destructuring-bind (types var body) clause
                                        (list types var (walk-list body))))
                                    (ast-handler-case-clauses form))))
                 ((ast-list-p form)
                  (make-ast-list :elements (walk-list (ast-list-elements form))))
                 ((ast-the-p form)
                  (make-ast-the :type (ast-the-type form)
                                :value (walk (ast-the-value form))))
                 ((ast-make-instance-p form)
                  (make-ast-make-instance :class (walk (ast-make-instance-class form))
                                          :initargs (walk-initargs
                                                     (ast-make-instance-initargs form))))
                 ((ast-slot-value-p form)
                  (make-ast-slot-value :object (walk (ast-slot-value-object form))
                                       :slot (ast-slot-value-slot form)))
                 ((ast-set-slot-value-p form)
                  (make-ast-set-slot-value :object (walk (ast-set-slot-value-object form))
                                           :slot (ast-set-slot-value-slot form)
                                           :value (walk (ast-set-slot-value-value form))))
                 ((ast-set-gethash-p form)
                  (make-ast-set-gethash :key (walk (ast-set-gethash-key form))
                                        :table (walk (ast-set-gethash-table form))
                                        :value (walk (ast-set-gethash-value form))))
                 (t form))))
      (values (walk node) (nreverse bindings)))))

(defun %php-bind-or-set (var value bound-vars)
  "Bind VAR to VALUE when new in this sequence, otherwise assign it."
  (if (member var bound-vars :test #'eq)
      (make-ast-setq :var var :value value)
      (make-ast-let :bindings (list (cons var value)) :body nil)))

(defun %php-form-bound-vars (form)
  "Return variables newly introduced by FORM at the current statement level."
  (if (and (ast-let-p form) (null (ast-let-body form)))
      (mapcar #'car (ast-let-bindings form))
      nil))

(defun %php-lower-reference-assignments (forms known-vars)
  "Lower $b = &$a aliases over a statement sequence using PHP ref boxes."
  (let ((out nil)
        (bound-vars (copy-list known-vars))
        (ref-vars nil))
    (dolist (form forms)
      (if (%php-reference-assignment-marker-p form)
          (let* ((dest (%php-reference-assignment-dest form))
                 (source (%php-reference-assignment-source form)))
            (unless (member source ref-vars :test #'eq)
              (push (%php-bind-or-set
                     source
                     (%php-call 'cl-cc/php::%php-make-ref
                                (if (member source bound-vars :test #'eq)
                                    (make-ast-var :name source)
                                    (%php-null-quote)))
                     bound-vars)
                    out)
              (pushnew source bound-vars :test #'eq)
              (pushnew source ref-vars :test #'eq))
            (push (%php-bind-or-set dest (make-ast-var :name source) bound-vars) out)
            (pushnew dest bound-vars :test #'eq)
            (pushnew dest ref-vars :test #'eq))
          (multiple-value-bind (hoisted-form init-bindings)
              (%php-hoist-variable-init-expressions form)
            (dolist (binding init-bindings)
              (push (%php-bind-or-set (car binding) (cdr binding) bound-vars) out)
              (pushnew (car binding) bound-vars :test #'eq))
            (let ((lowered (if ref-vars
                               (%php-rewrite-ref-vars hoisted-form ref-vars)
                               hoisted-form)))
            (dolist (var (%php-form-bound-vars hoisted-form))
              (pushnew var bound-vars :test #'eq))
              (push lowered out)))))
    (nreverse out)))
