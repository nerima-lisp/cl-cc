(in-package :cl-cc/expand)

(defun chain-comparison-op (op args)
  "Chain a comparison (OP a b c) → (AND (OP a b) (OP b c)).
Uses gensyms for intermediate values to avoid double evaluation.
(OP) → error, (OP a) → T, (OP a b) → pass through, (OP a b c ...) → AND chain."
  (let ((len (length args)))
    (if (= len 0)
        (error "~A requires at least one argument" op)
        (if (= len 1)
            t
            (if (= len 2)
                (list op (first args) (second args))
                (let* ((temps (loop for i from 0 below len
                                    collect (gensym (format nil "CMP~D-" i))))
                       (bindings (mapcar #'list temps args))
                       (pairs (loop for (a b) on temps while b
                                    collect (list op a b))))
                  (list 'let bindings (cons 'and pairs))))))))

(defun reduce-variadic-op (op args identity)
  "Reduce a variadic arithmetic form (OP arg...) to nested binary forms.
(OP) => IDENTITY, (OP a) => a, (OP a b) => (OP a b), (OP a b c ...) => (OP (OP a b) c) ..."
  (let ((len (length args)))
    (if (= len 0)
        identity
        (if (= len 1)
            (first args)
            (if (= len 2)
                (%expander-form op (first args) (second args))
                (reduce (lambda (acc x) (%expander-form op acc x))
                        (cddr args)
                        :initial-value (%expander-form op (first args) (second args))))))))


(defun register-defclass-accessors (class-name slot-specs)
  "Register ACCESSOR → (CLASS-NAME . SLOT-NAME) in *accessor-slot-map*.
Called at expand time so later (setf (accessor obj) val) can be lowered
to (setf (slot-value ...)) without runtime lookup."
  (when (listp slot-specs)
    (dolist (spec slot-specs)
      (when (listp spec)
        (let ((accessor (getf (rest spec) :accessor)))
          (when accessor
            (setf (gethash accessor *accessor-slot-map*)
                  (cons class-name (first spec)))))))))

(defun expand-defclass-slot-spec (spec)
  "Expand only the :initform value inside a slot SPEC, leaving all other
keys (:accessor, :initarg, :reader, :writer, :type) untouched."
  (if (listp spec)
      (list* (first spec)
             (loop for kv on (rest spec) by #'cddr
                   when (cdr kv)
                     append (let ((k (car kv)) (v (cadr kv)))
                              (list k (if (eq k :initform)
                                          (compiler-macroexpand-all v)
                                          v)))))
      spec))

(defun expand-typed-defun-or-lambda (head name params rest-forms)
  "Strip type annotations from PARAMS, register the type signature, and
rebuild a plain DEFUN or LAMBDA form with check-type assertions.

HEAD is 'defun or 'lambda; NAME is the function name (nil for lambda).

Side effects:
  - For DEFUN, calls register-function-type with the resolved param/return types.

Handles return-type annotation: if the first element of REST-FORMS is a type
specifier symbol, it is treated as the declared return type and wrapped in
  (the RETURN-TYPE (progn ...)) in the output body."
  (multiple-value-bind (plain-params type-alist)
      (strip-typed-params params)
    (let* ((docstring        (and (stringp (first rest-forms))
                                  (rest rest-forms)
                                  (first rest-forms)))
           (typed-rest-forms (if docstring (rest rest-forms) rest-forms))
           (has-return-type  (and typed-rest-forms
                                  (symbolp (first typed-rest-forms))
                                  (cl-cc/type:looks-like-type-specifier-p (first typed-rest-forms))))
           (return-type-spec (when has-return-type (first typed-rest-forms)))
           (body-forms       (if has-return-type (cdr typed-rest-forms) typed-rest-forms))
           (param-types      (mapcar (lambda (e)
                                       (cl-cc/type:parse-type-specifier (cdr e)))
                                     type-alist))
            (return-type      (if return-type-spec
                                  (cl-cc/type:parse-type-specifier return-type-spec)
                                  (cl-cc/type:parse-type-specifier 't)))
           (typed-return-spec (and return-type-spec
                                   (or (cl-cc/type:lookup-type-alias return-type-spec)
                                       return-type-spec)))
           (typed-body       (if typed-return-spec
                                 `((the ,typed-return-spec (progn ,@body-forms)))
                                 body-forms))
           (checks           (mapcar (lambda (entry)
                                       `(check-type ,(car entry) ,(cdr entry)))
                                     type-alist))
            (full-body        (append (when docstring (list docstring)) checks typed-body)))
      (when (eq head 'defun)
        (register-function-type name param-types return-type))
      (compiler-macroexpand-all
       (if (eq head 'defun)
           `(defun ,name ,plain-params ,@full-body)
            `(lambda ,plain-params ,@full-body))))))

(defun %list-contains-eq (item lst)
  "Return T when ITEM is EQ to any element of LST."
  (and (member item lst :test #'eq) t))

(defun make-macro-expander (lambda-list body)
  "Build a macro expander function for a LAMBDA-LIST and BODY.
Quasiquotes in BODY are pre-expanded so the host eval can handle them.
When *macro-eval-fn* is our-eval (self-hosting mode), the returned value is
a data descriptor interpreted through the selfhost evaluator instead of a
host CL closure."
  (list :kind :macro-expander
        :lambda-list lambda-list
        :body (mapcar #'our-macroexpand-all body)))

(defun %quote-uninterned-symbol-occurrences (symbol tree)
  "Quote literal gensym occurrences in TREE without rewriting quoted data."
  (cond
    ((and (symbolp tree) (eq tree symbol) (null (symbol-package symbol)))
     `(quote ,symbol))
    ((and (consp tree) (eq (car tree) 'quote))
     tree)
    ((consp tree)
     (cons (%quote-uninterned-symbol-occurrences symbol (car tree))
           (%quote-uninterned-symbol-occurrences symbol (cdr tree))))
    (t tree)))

(defun %lambda-list-binding-names (lambda-list)
  "Return the symbols bound by LAMBDA-LIST."
  (let ((info (parse-lambda-list lambda-list))
        (names nil))
    (labels ((push-symbol (value)
               (when (symbolp value)
                 (pushnew value names :test #'eq)))
             (push-pattern (value)
               (cond
                 ((symbolp value) (push-symbol value))
                 ((consp value) (dolist (entry value) (push-pattern entry))))))
      (push-symbol (lambda-list-info-whole info))
      (push-symbol (lambda-list-info-environment info))
      (dolist (required (lambda-list-info-required info))
        (push-pattern required))
      (dolist (optional (lambda-list-info-optional info))
        (destructuring-bind (name default supplied-p) optional
          (declare (ignore default))
          (push-pattern name)
          (push-symbol supplied-p)))
      (push-symbol (lambda-list-info-rest info))
      (push-symbol (lambda-list-info-body info))
      (dolist (key-param (lambda-list-info-key-params info))
        (destructuring-bind ((keyword name) default supplied-p) key-param
          (declare (ignore keyword default))
          (push-pattern name)
          (push-symbol supplied-p)))
      (dolist (aux (lambda-list-info-aux info))
        (destructuring-bind (name init) aux
          (declare (ignore init))
          (push-pattern name))))
    names))

(defun %template-expression (form bound-names)
  "Build an expression that returns FORM as syntax with bound variables spliced."
  (cond
    ((symbolp form)
     (if (member form bound-names :test #'eq)
         form
         `(quote ,form)))
    ((atom form) form)
    ((and (consp form) (eq (car form) 'quote))
     form)
    ((%proper-list-p form)
     `(list ,@(mapcar (lambda (entry)
                        (%template-expression entry bound-names))
                      form)))
    (t
     `(cons ,(%template-expression (car form) bound-names)
            ,(%template-expression (cdr form) bound-names)))))

(defun %our-defmacro-raw-result-form-p (form)
  "Return T when FORM already constructs macro expansion data at runtime."
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(quote list list* cons append)
               :test #'eq)))

(defun %direct-our-defmacro-result-form (form bound-names)
  "Return a result expression for a directly expanded OUR-DEFMACRO body form."
  (if (%our-defmacro-raw-result-form-p form)
      form
      (%template-expression form bound-names)))

(defun %direct-our-defmacro-body (body bound-names)
  "Transform direct OUR-DEFMACRO body forms to preserve one-step expansion data."
  (let ((last-form (car (last body)))
        (prefix (butlast body)))
    (append
     prefix
     (list
      (if (and (consp last-form) (eq (car last-form) 'if))
          `(if ,(second last-form)
               ,(%direct-our-defmacro-result-form (third last-form) bound-names)
               ,(%direct-our-defmacro-result-form (fourth last-form) bound-names))
          last-form)))))

(defun %make-direct-our-defmacro-expander (lambda-list body)
  "Build the direct OUR-DEFMACRO expander used by OUR-MACROEXPAND-1."
  (let ((bound-names (%lambda-list-binding-names lambda-list)))
    (lambda (macro-form env)
      (declare (ignore env))
      (let* ((form-var (gensym "FORM"))
             (eval-form `(let ((,form-var ',macro-form))
                           ,(%nest-let-bindings
                             (generate-lambda-bindings lambda-list form-var)
                             (%direct-our-defmacro-body body bound-names)))))
        (handler-bind ((style-warning #'muffle-warning))
          (eval eval-form))))))

;; OUR-DEFMACRO is a host macro at load time, but users can also submit it to
;; OUR-MACROEXPAND-1 directly. Keep that path side-effecting like DEFMACRO.
(register-macro 'our-defmacro
  (lambda (form env)
    (declare (ignore env))
    (let ((name (second form))
          (lambda-list (third form))
          (body (cdddr form)))
      (register-macro
       name
       (%make-direct-our-defmacro-expander
        lambda-list
        (mapcar (lambda (entry)
                  (%quote-uninterned-symbol-occurrences name entry))
                body)))
      `(quote ,name))))

(defun make-host-macro-expander (lambda-list body)
  "Build a host-evaluated macro expander for local MACROLET bindings.
Quasiquotes in BODY are pre-expanded (as in MAKE-MACRO-EXPANDER) so the host
eval can construct the expansion instead of calling the undefined UNQUOTE."
  (let ((environment-sym (lambda-list-info-environment
                          (parse-lambda-list lambda-list)))
        (expanded-body (mapcar #'our-macroexpand-all body)))
    (lambda (form env)
      (let* ((form-var (gensym "FORM"))
             (effective-env (or env '(:macrolet-environment)))
             (macro-body (if environment-sym
                             `((let ((,environment-sym ',effective-env))
                                 ,@expanded-body))
                             expanded-body))
             (eval-form `(let ((,form-var ',form))
                           ,(%nest-let-bindings
                             (generate-lambda-bindings lambda-list form-var)
                             macro-body))))
        (handler-bind ((style-warning #'muffle-warning))
          (eval eval-form))))))

(defun make-compiler-macro-expander (lambda-list body)
  "Build a compiler-macro expander for a function LAMBDA-LIST and BODY."
  (list :kind :compiler-macro-expander
        :lambda-list lambda-list
        :body (mapcar #'our-macroexpand-all body)))

(defun expand-macrolet-form (bindings body)
  "Register local macro BINDINGS, expand BODY under them, then restore.
Returns the expanded BODY wrapped in PROGN."
  (let ((saved nil))
    (unwind-protect
         (progn
           (dolist (b bindings)
             (let ((name        (first b))
                   (lambda-list (second b))
                   (macro-body  (cddr b)))
               (push (cons name (lookup-macro name)) saved)
               (register-macro name (make-host-macro-expander lambda-list macro-body))))
           ;; A MACROLET body is a declaration scope, so collapse through the
           ;; helper: PROGN would leave any leading declarations where only
           ;; forms are read. Identical to (cons 'progn body) when there are
           ;; none.
           (compiler-macroexpand-all (%collapse-empty-binding-body body)))
      (dolist (s saved)
        (if (cdr s)
            (register-macro (car s) (cdr s))
            (remhash (car s) (macro-env-table *macro-environment*)))))))

(defun expand-symbol-macrolet-form (bindings body)
  "Register local symbol macro BINDINGS, expand BODY under them, then restore.
Each binding is (symbol expansion). Returns the expanded BODY wrapped in PROGN."
  (let ((saved nil))
    (dolist (b bindings)
      (let ((name (first b))
            (expansion (second b)))
        (push (cons name (gethash name *symbol-macro-table*)) saved)
        (setf (gethash name *symbol-macro-table*) expansion)))
    ;; SYMBOL-MACROLET's body is a declaration scope too; see EXPAND-MACROLET-FORM.
    (let ((result (compiler-macroexpand-all (%collapse-empty-binding-body body))))
      (dolist (s saved)
        (if (cdr s)
            (setf (gethash (car s) *symbol-macro-table*) (cdr s))
            (remhash (car s) *symbol-macro-table*)))
      result)))

(defun expand-progn-with-eager-defmacro (subforms)
  "Expand each form in SUBFORMS, eagerly registering DEFMACRO forms so
later siblings can immediately use the new macro."
  ;; OUR-DEFMACRO is registered before expansion so later siblings can use it.
  ;; DEFMACRO in the expanded output is registered afterward for the same reason.
  (cons 'progn
        (loop for sub in subforms
              do (when (and (consp sub)
                            (symbolp (car sub))
                            (string= (symbol-name (car sub)) "OUR-DEFMACRO"))
                   (register-macro (second sub)
                                   (make-macro-expander (third sub) (cdddr sub))))
              collect (let ((exp (compiler-macroexpand-all sub)))
                        (when (and (consp exp) (eq (car exp) 'defmacro))
                          (register-macro (second exp)
                                         (make-macro-expander (third exp) (cdddr exp))))
                        exp))))

(defun expand-eval-when-form (situations body)
  "Handle EVAL-WHEN phase control.
Evaluate BODY immediately for :compile-toplevel; include in output for :execute/:load-toplevel."
  (when (%list-contains-eq :compile-toplevel situations)
    (dolist (b body)
      (handler-case
          (let ((expanded (compiler-macroexpand-all b)))
            (if (fboundp 'run-string-repl)
                (run-string-repl (write-to-string expanded))
                (our-eval expanded)))
        (error () nil))))
  (if (or (%list-contains-eq :execute situations)
          (%list-contains-eq :load-toplevel situations))
      (compiler-macroexpand-all (cons 'progn body))
      nil))

(defun %build-variadic-fold-lambda (name)
  (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X"))
        (id   (variadic-fold-identity name)))
    `(lambda (&rest ,args)
       (let ((,acc ,id))
         (dolist (,x ,args ,acc)
           (setq ,acc (,name ,acc ,x)))))))

(defun %build-subtract-lambda ()
  (let ((args (gensym "ARGS")) (acc (gensym "ACC")) (x (gensym "X")))
    `(lambda (&rest ,args)
       (if (null (cdr ,args))
           (- 0 (car ,args))
           (let ((,acc (car ,args)))
             (dolist (,x (cdr ,args) ,acc)
               (setq ,acc (- ,acc ,x))))))))

(defun expand-function-builtin (name)
  "Wrap a known builtin NAME in a first-class lambda for higher-order use."
  (compiler-macroexpand-all
   (cond
     ((gethash name *variadic-fold-builtins-table*) ;; FR-130: perfect hash
      (%build-variadic-fold-lambda name))
     ((eq name '-)                           (%build-subtract-lambda))
     ((eq name 'list)
      (let ((args (gensym "ARGS")))
        `(lambda (&rest ,args) ,args)))
     ((gethash name *binary-builtins-table*) ;; FR-130: perfect hash
      (let ((a (gensym "A")) (b (gensym "B")))
        `(lambda (,a ,b) (,name ,a ,b))))
     (t
      (let ((x (gensym "X")))
        `(lambda (,x) (,name ,x)))))))

(defun expand-apply-named-fn (fn-name args-form)
  "Expand (apply 'FN-NAME args-form) where FN-NAME is a known symbol.
Normalise to (apply #'fn args) so APPLY lowering keeps spread semantics visible."
  (list 'apply (list 'function fn-name)
        (compiler-macroexpand-all args-form)))
