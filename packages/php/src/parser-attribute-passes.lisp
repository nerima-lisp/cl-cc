;;;; parser-attribute-passes.lisp — PHP attribute validation and AST rewrites
(in-package :cl-cc/php)

(declaim (special *php-interface-registry*))

(defun %php-attribute-short-name (name)
  "Return the unqualified tail of a PHP attribute name."
  (let ((pos (position #\\ name :from-end t)))
    (if pos (subseq name (1+ pos)) name)))

(defun %php-metadata-plist-p (metadata)
  "Return true when METADATA is a property list."
  (and (listp metadata)
       (evenp (length metadata))
       (loop for rest on metadata by #'cddr
             always (keywordp (first rest)))))

(defun %php-metadata-get (metadata key)
  "Read KEY from METADATA only when it is stored as a property list."
  (when (%php-metadata-plist-p metadata)
    (getf metadata key)))

(defun %php-no-discard-attribute (node)
  "Return NODE's #[NoDiscard] attribute, if present."
  (let ((attrs (append (copy-list (%php-metadata-get (ast-imports node)
                                                      :php-attributes))
                       (when (ast-defun-p node)
                         (copy-list (%php-metadata-get (ast-defun-declarations node)
                                          :php-attributes))))))
    (find-if (lambda (attribute)
               (and (php-attribute-p attribute)
                    (string-equal "NoDiscard"
                                  (%php-attribute-short-name
                                   (php-attribute-name attribute)))))
             attrs)))

(defun %php-override-attribute-p (attribute)
  "Return true when ATTRIBUTE names #[Override]."
  (and (php-attribute-p attribute)
       (string-equal "Override"
                     (%php-attribute-short-name (php-attribute-name attribute)))))

(defun %php-member-attributes (slot)
  "Return all PHP attributes attached to SLOT or its method body."
  (append (copy-list (%php-metadata-get (ast-imports slot)
                                        :php-attributes))
          (when (and (ast-slot-def-p slot)
                     (ast-defun-p (ast-slot-initform slot)))
            (copy-list (%php-metadata-get (ast-defun-declarations (ast-slot-initform slot))
                                          :php-attributes)))))

(defun %php-member-override-attribute-p (slot)
  "Return SLOT's #[Override] attribute, if present."
  (find-if #'%php-override-attribute-p (%php-member-attributes slot)))

(defun %php-member-kind (slot)
  "Return SLOT's PHP member kind."
  (cond ((not (ast-slot-def-p slot)) nil)
        ((ast-defun-p (ast-slot-initform slot)) :method)
        ((getf (ast-imports slot) :php-class-constant) :constant)
        (t :property)))

(defun %php-member-private-p (slot)
  "Return true when SLOT is marked private."
  (member :private (getf (ast-imports slot) :php-modifiers) :test #'eq))

(defun %php-interface-member-present-p (interface-name member-name
                                        &optional (seen (make-hash-table :test #'equal)))
  "Return true when INTERFACE-NAME or one of its ancestors declares MEMBER-NAME."
  (unless (gethash interface-name seen)
    (setf (gethash interface-name seen) t)
    (let ((record (gethash interface-name *php-interface-registry*)))
      (when record
        (or (find member-name (getf record :methods)
                  :key (lambda (sig) (getf sig :name))
                  :test #'equal)
            (some (lambda (parent)
                    (%php-interface-member-present-p parent member-name seen))
                  (getf record :parents)))))))

(defun %php-class-member-present-p (class-registry class-name member-name kind
                                    &optional (seen (make-hash-table :test #'equal)))
  "Return true when CLASS-NAME or one of its ancestors declares MEMBER-NAME."
  (unless (gethash class-name seen)
    (setf (gethash class-name seen) t)
    (let ((class-node (gethash class-name class-registry)))
      (when class-node
        (or (find-if (lambda (slot)
                       (and (ast-slot-def-p slot)
                            (eq (ast-slot-name slot) member-name)
                            (not (%php-member-private-p slot))
                            (ecase kind
                              (:method (ast-defun-p (ast-slot-initform slot)))
                              (:property (and (not (ast-defun-p (ast-slot-initform slot)))
                                              (not (getf (ast-imports slot)
                                                         :php-class-constant))))
                              (:constant (getf (ast-imports slot) :php-class-constant)))))
                     (ast-defclass-slots class-node))
            (some (lambda (parent)
                    (cond ((gethash parent *php-interface-registry*)
                           (and (eq kind :method)
                                (%php-interface-member-present-p parent member-name seen)))
                          ((gethash parent class-registry)
                           (%php-class-member-present-p class-registry parent member-name kind seen))
                          (t nil)))
                  (ast-defclass-superclasses class-node)))))))

(defun %php-class-registry (stmts)
  "Build a compile-time registry for all class-like nodes in STMTS."
  (let ((classes (make-hash-table :test #'equal)))
    (labels ((visit (node)
               (when (ast-defclass-p node)
                 (setf (gethash (ast-defclass-name node) classes) node))
               (dolist (child (ast-children node))
                 (visit child))))
      (dolist (stmt stmts)
        (visit stmt)))
    classes))

(defun %php-validate-override-slot (class-registry class-node slot)
  "Signal an error when SLOT's #[Override] attribute has no inherited target."
  (when (%php-member-override-attribute-p slot)
    (let ((kind (%php-member-kind slot))
          (member-name (ast-slot-name slot)))
      (unless (some (lambda (parent)
                      (cond ((gethash parent *php-interface-registry*)
                             (and (eq kind :method)
                                  (%php-interface-member-present-p parent member-name)))
                            ((gethash parent class-registry)
                             (%php-class-member-present-p class-registry parent member-name kind))
                            (t nil)))
                    (ast-defclass-superclasses class-node))
        (ast-error slot "#[Override] on ~A does not match an inherited ~A."
                   (symbol-name member-name)
                   (ecase kind
                     (:method "method")
                     (:property "property")
                     (:constant "class constant")))))))

(defun %php-validate-override-tree (stmts)
  "Validate #[Override] attributes across the parsed AST tree."
  (let ((class-registry (%php-class-registry stmts)))
    (labels ((visit (node)
               (when (ast-defclass-p node)
                 (dolist (slot (ast-defclass-slots node))
                   (%php-validate-override-slot class-registry node slot)))
               (dolist (child (ast-children node))
                 (visit child))))
      (dolist (stmt stmts)
        (visit stmt))))
  stmts)

(defun %php-attribute-string-arg (arg)
  "Return ARG as a string value when it is a literal PHP attribute argument."
  (let ((value (if (and (consp arg)
                        (string-equal (getf arg :name) "message"))
                   (getf arg :value)
                   arg)))
    (when (and (ast-quote-p value) (stringp (ast-quote-value value)))
      (ast-quote-value value))))

(defun %php-no-discard-message-argument (attribute)
  "Return the optional #[NoDiscard] message argument."
  (let ((args (php-attribute-args attribute)))
    (or (some #'%php-attribute-string-arg
              (remove-if-not (lambda (arg)
                               (and (consp arg)
                                    (string-equal (getf arg :name) "message")))
                             args))
        (%php-attribute-string-arg (first args)))))

(defun %php-no-discard-function-message (function-key attribute)
  "Build the user warning message for a discarded #[NoDiscard] function result."
  (let* ((function-name (string-downcase function-key))
         (base (format nil "The return value of function ~A() should either be used or intentionally ignored by casting it as (void)"
                       function-name))
         (message (%php-no-discard-message-argument attribute)))
    (if (and message (plusp (length message)))
        (format nil "~A, ~A" base message)
        base)))

(defun %php-no-discard-method-message (method-key attribute)
  "Build the user warning message for a discarded #[NoDiscard] method result."
  (let* ((method-name (string-downcase method-key))
         (base (format nil "The return value of method ~A() should either be used or intentionally ignored by casting it as (void)"
                       method-name))
         (message (%php-no-discard-message-argument attribute)))
    (if (and message (plusp (length message)))
        (format nil "~A, ~A" base message)
        base)))

(defun %php-symbol-name-key (symbol)
  "Return SYMBOL's case-insensitive PHP lookup key."
  (string-upcase (symbol-name symbol)))

(defun %php-defun-name-key (node)
  "Return the case-insensitive PHP function lookup key for NODE."
  (%php-symbol-name-key (ast-defun-name node)))

(defun %php-method-message-key (class-name method-name)
  "Return the case-insensitive lookup key for CLASS-NAME::METHOD-NAME."
  (format nil "~A::~A"
          (%php-symbol-name-key class-name)
          (%php-symbol-name-key method-name)))

(defun %php-direct-call-name-key (node)
  "Return NODE's direct function-call key when NODE is a plain call statement."
  (when (and (ast-call-p node)
             (ast-var-p (ast-call-func node)))
    (%php-symbol-name-key (ast-var-name (ast-call-func node)))))

(defun %php-class-env-class (env var-name)
  "Return the inferred class symbol for VAR-NAME in ENV."
  (cdr (assoc var-name env :test #'eq)))

(defun %php-class-env-set (env var-name class-name)
  "Return ENV updated so VAR-NAME has CLASS-NAME, or no known class."
  (let ((without-var (remove var-name env :key #'car :test #'eq)))
    (if class-name
        (acons var-name class-name without-var)
        without-var)))

(defun %php-expression-class-symbol (expr env)
  "Infer EXPR's class symbol when it is a simple object construction path."
  (cond
    ((and (ast-var-p expr)
          (%php-class-env-class env (ast-var-name expr)))
     (%php-class-env-class env (ast-var-name expr)))
    ((and (ast-make-instance-p expr)
          (ast-var-p (ast-make-instance-class expr)))
     (ast-var-name (ast-make-instance-class expr)))
    ((ast-let-p expr)
     (let ((local-env env))
       (dolist (binding (ast-let-bindings expr))
         (setf local-env
               (%php-class-env-set
                local-env
                (car binding)
                (%php-expression-class-symbol (cdr binding) local-env))))
       (%php-expression-class-symbol (car (last (ast-let-body expr))) local-env)))
    ((ast-progn-p expr)
     (%php-expression-class-symbol (car (last (ast-progn-forms expr))) env))
    (t nil)))

(defun %php-no-discard-warning-ast (message call-node)
  "Emit E_USER_WARNING before evaluating a discarded NoDiscard call."
  (make-ast-progn
   :forms (list (%php-call 'cl-cc/php::%php-trigger-error
                           (make-ast-quote :value message)
                           (make-ast-int :value 512))
                call-node)))

(defun %php-void-cast-progn-p (node)
  "Return true when NODE is the lowering of PHP's `(void) EXPR` cast."
  (and (ast-progn-p node)
       (= (length (ast-progn-forms node)) 2)
       (let ((last-form (second (ast-progn-forms node))))
         (and (ast-quote-p last-form)
              (eq (ast-quote-value last-form) +php-null+)))))

(defun %php-apply-no-discard-warnings (stmts)
  "Wrap discarded calls to #[NoDiscard] functions and methods with warnings."
  (let ((function-messages (make-hash-table :test #'equal))
        (method-messages (make-hash-table :test #'equal)))
    (labels ((collect-method (class-name slot)
               (when (and (ast-slot-def-p slot)
                          (ast-defun-p (ast-slot-initform slot)))
                 (let* ((method (ast-slot-initform slot))
                        (attribute (or (%php-no-discard-attribute method)
                                       (%php-no-discard-attribute slot))))
                   (when attribute
                     (let* ((method-name (or (ast-defun-name method)
                                             (ast-slot-name slot)))
                            (key (%php-method-message-key class-name method-name)))
                       (setf (gethash key method-messages)
                             (%php-no-discard-method-message
                              (%php-symbol-name-key method-name)
                              attribute)))))))
             (collect (node)
               (cond
                 ((ast-defun-p node)
                  (let ((attribute (%php-no-discard-attribute node)))
                    (when attribute
                      (let ((key (%php-defun-name-key node)))
                        (setf (gethash key function-messages)
                              (%php-no-discard-function-message key attribute))))))
                 ((ast-defclass-p node)
                  (dolist (slot (ast-defclass-slots node))
                    (collect-method (ast-defclass-name node) slot)))
                 ((ast-progn-p node)
                  (dolist (form (ast-progn-forms node))
                    (collect form)))))
             (extend-env-binding (env binding)
               (%php-class-env-set
                env
                (car binding)
                (%php-expression-class-symbol (cdr binding) env)))
             (extend-env-bindings (env bindings)
               (let ((local-env env))
                 (dolist (binding bindings local-env)
                   (setf local-env (extend-env-binding local-env binding)))))
             (method-message (node env)
               (when (and (ast-call-p node)
                          (ast-slot-value-p (ast-call-func node)))
                 (let* ((callee (ast-call-func node))
                        (receiver (ast-slot-value-object callee))
                        (method-name (ast-slot-value-slot callee)))
                   (when (and method-name (ast-var-p receiver))
                     (let* ((receiver-name (ast-var-name receiver))
                            (inferred-class (%php-class-env-class env receiver-name))
                            (static-key (%php-method-message-key receiver-name
                                                                  method-name))
                            (class-name (or inferred-class
                                            (when (gethash static-key
                                                           method-messages)
                                              receiver-name))))
                       (when class-name
                         (gethash (%php-method-message-key class-name
                                                           method-name)
                                  method-messages)))))))
             (call-message (node env)
               (or (let ((key (%php-direct-call-name-key node)))
                     (when key (gethash key function-messages)))
                   (method-message node env)))
             (rewrite-list (forms env)
               (let ((rewritten nil)
                     (current-env env))
                 (dolist (form forms (values (nreverse rewritten) current-env))
                   (multiple-value-bind (new-form new-env)
                       (rewrite form current-env)
                     (push new-form rewritten)
                     (setf current-env new-env)))))
             (rewrite-callable-body (body env)
               (multiple-value-bind (rewritten-body ignored-env)
                   (rewrite-list body env)
                 (declare (ignore ignored-env))
                 rewritten-body))
             (rewrite (node env)
               (let ((message (call-message node env)))
                 (cond
                   (message
                    (values (%php-no-discard-warning-ast message node) env))
                   ((%php-void-cast-progn-p node)
                    (values node env))
                   ((ast-defun-p node)
                    (setf (ast-defun-body node)
                          (rewrite-callable-body (ast-defun-body node) nil))
                    (values node env))
                   ((ast-progn-p node)
                    (setf (ast-progn-forms node)
                          (rewrite-callable-body (ast-progn-forms node) env))
                    (values node env))
                   ((ast-let-p node)
                    (if (ast-let-body node)
                        (let ((local-env
                                (extend-env-bindings env (ast-let-bindings node))))
                          (setf (ast-let-body node)
                                (rewrite-callable-body (ast-let-body node)
                                                       local-env))
                          (values node env))
                        (values node
                                (extend-env-bindings env
                                                     (ast-let-bindings node)))))
                   ((ast-setq-p node)
                    (values node
                            (%php-class-env-set
                             env
                             (ast-setq-var node)
                             (%php-expression-class-symbol (ast-setq-value node)
                                                           env))))
                   ((ast-if-p node)
                    (multiple-value-bind (then-node ignored-then-env)
                        (rewrite (ast-if-then node) env)
                      (declare (ignore ignored-then-env))
                      (setf (ast-if-then node) then-node))
                    (multiple-value-bind (else-node ignored-else-env)
                        (rewrite (ast-if-else node) env)
                      (declare (ignore ignored-else-env))
                      (setf (ast-if-else node) else-node))
                    (values node env))
                   ((ast-block-p node)
                    (setf (ast-block-body node)
                          (rewrite-callable-body (ast-block-body node) env))
                    (values node env))
                   ((ast-catch-p node)
                    (setf (ast-catch-body node)
                          (rewrite-callable-body (ast-catch-body node) env))
                    (values node env))
                   ((ast-unwind-protect-p node)
                    (multiple-value-bind (protected ignored-protected-env)
                        (rewrite (ast-unwind-protected node) env)
                      (declare (ignore ignored-protected-env))
                      (setf (ast-unwind-protected node) protected))
                    (setf (ast-unwind-cleanup node)
                          (rewrite-callable-body (ast-unwind-cleanup node) env))
                    (values node env))
                   ((ast-multiple-value-bind-p node)
                    (setf (ast-mvb-body node)
                          (rewrite-callable-body (ast-mvb-body node) env))
                    (values node env))
                   ((ast-defclass-p node)
                    (let* ((class-name (ast-defclass-name node))
                           (method-env
                             (%php-class-env-set
                              (%php-class-env-set
                               (%php-class-env-set nil
                                                   (php-ident-sym "this")
                                                   class-name)
                               (php-ident-sym "self")
                               class-name)
                              (php-ident-sym "static")
                              class-name)))
                      (dolist (slot (ast-defclass-slots node))
                        (when (and (ast-slot-def-p slot)
                                   (ast-defun-p (ast-slot-initform slot)))
                          (let ((method (ast-slot-initform slot)))
                            (setf (ast-defun-body method)
                                  (rewrite-callable-body
                                   (ast-defun-body method)
                                   method-env))
                            (setf (ast-slot-initform slot) method)))))
                    (values node env))
                   (t (values node env))))))
      (dolist (stmt stmts)
        (collect stmt))
      (if (and (zerop (hash-table-count function-messages))
               (zerop (hash-table-count method-messages)))
          stmts
          (multiple-value-bind (rewritten ignored-env)
              (rewrite-list stmts nil)
            (declare (ignore ignored-env))
            rewritten)))))
