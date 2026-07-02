;;;; parser-class.lisp — PHP class body parser, statement dispatcher, top-level entry point
(in-package :cl-cc/php)

(declaim (special *php-interface-registry*))

;;; ─── Class Body Parser ───────────────────────────────────────────────────────

(defun %php-parse-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords and return them as metadata."
  (let ((modifiers nil))
    (flet ((visibility-modifier-p (modifier)
             (member modifier '(:public :protected :private) :test #'eq))
           (set-visibility-tail-p (tail)
             (and tail
                  (eq (php-peek-type tail) :T-KEYWORD)
                  (member (php-peek-value tail) '(:private :protected) :test #'eq)
                  (cdr tail)
                  (eq (php-peek-type (cdr tail)) :T-LPAREN)
                  (cddr tail)
                  (eq (php-peek-type (cddr tail)) :T-IDENT)
                  (string-equal (php-peek-value (cddr tail)) "set")
                  (cdddr tail)
                  (eq (php-peek-type (cdddr tail)) :T-RPAREN))))
      (loop while (and stream
                       (let ((type (php-peek-type stream))
                             (val  (php-peek-value stream)))
                         (or (and (eq type :T-KEYWORD)
                                  (member val '(:public :private :protected :static
                                                :abstract :final :readonly)))
                             ;; `static` lexes as a T-TYPE because it is also the
                             ;; `static` return-type keyword.  In modifier position
                             ;; it is the static-member modifier — accept it here so
                             ;; static properties/methods are flagged :static rather
                             ;; than parsed with a bogus `static` type hint and left
                             ;; instance-allocated (which broke C::$prop and C::m()).
                             (and (eq type :T-TYPE) (eq val :STATIC)))))
          do (let ((type (php-peek-type stream))
                   (val  (php-peek-value stream)))
               (cond
                 ;; PHP 8.5 static asymmetric property visibility permits other
                 ;; modifiers between the outer visibility and private(set), e.g.
                 ;; public static private(set) Type $prop;
                 ((and (visibility-modifier-p val)
                       (set-visibility-tail-p stream)
                       (some #'visibility-modifier-p modifiers))
                  (push (cons :set-visibility val) modifiers)
                  (setf stream (cdr (cdddr stream))))
                 ;; PHP 8.4 asymmetric property visibility:
                 ;; public private(set) Type $prop;
                 ((and (eq type :T-KEYWORD)
                       (visibility-modifier-p val)
                       (set-visibility-tail-p (cdr stream)))
                  (push val modifiers)
                  (push (cons :set-visibility (php-peek-value (cdr stream))) modifiers)
                  (setf stream (cdr (cddddr stream))))
                 (t
                  (push val modifiers)
                  (setf stream (cdr stream)))))))
    (values (nreverse modifiers) stream)))

(defun %php-slot-metadata (modifiers &key class-constant-p attributes target-type)
  "Build AST node metadata for PHP class members."
  (let ((set-visibility (cdr (find :set-visibility modifiers
                                   :key (lambda (modifier)
                                          (when (consp modifier) (car modifier)))
                                   :test #'eq)))
        (plain-modifiers (remove-if #'consp modifiers)))
  (append (when plain-modifiers (list :php-modifiers plain-modifiers))
          (when set-visibility (list :php-set-visibility set-visibility))
          (when class-constant-p (list :php-class-constant t))
          (%php-attribute-metadata attributes target-type))))

(defun %php-parse-class-constant (stream modifiers known-vars attributes)
  "Parse const [TYPE] NAME = VALUE[, NAME = VALUE]*; as class-scoped slots."
  (declare (ignore known-vars))
  (let ((current (cdr stream))
        (constant-type nil)
        (slots nil))
    (multiple-value-bind (parsed-type after-type) (php-parse-type-annotation current)
      (when (and parsed-type (eq (php-peek-type after-type) :T-IDENT))
        (setf constant-type parsed-type
              current after-type)))
    (loop
      (multiple-value-bind (name-tok rest) (php-expect :T-IDENT current)
        (let ((initform nil))
          (setf current rest)
          (when (and current (eq (php-peek-type current) :T-OP)
                     (equal "=" (php-peek-value current)))
            (multiple-value-bind (expr rest2 _) (php-parse-expr (cdr current) nil)
              (declare (ignore _))
              (setf initform expr
                    current rest2)))
          (push (make-ast-slot-def :name (php-ident-sym (php-tok-value name-tok))
                                   :type constant-type
                                   :initform initform
                                   :allocation :class
                                   :imports (%php-slot-metadata modifiers
                                                               :class-constant-p t
                                                               :attributes attributes
                                                               :target-type :constant))
                slots)))
      (unless (and current (eq (php-peek-type current) :T-COMMA))
        (return))
      (when attributes
        (error "PHP parse error: attributes on grouped const declarations require exactly one constant"))
      (setf current (cdr current)))
    (let ((ordered-slots (nreverse slots)))
      (values (first ordered-slots)
              (php-skip-semis current)
              (rest ordered-slots)))))

(defun %php-parse-trait-use-member (stream)
  "Parse use TraitName[, OtherTrait]; as class/enum metadata slots."
  (let ((current (cdr stream))
        (slots nil))
    (loop
      (multiple-value-bind (trait-name rest) (php-parse-qualified-name current)
        (let ((trait-sym (php-ident-sym (php-resolve-qualified-name trait-name :class))))
          (push (make-ast-slot-def :name trait-sym
                                   :allocation :class
                                   :imports (list :php-trait-use t))
                slots))
        (setf current rest))
      (unless (eq (php-peek-type current) :T-COMMA)
        (return))
      (setf current (cdr current)))
    (values (make-ast-slot-def :name (gensym "PHP-TRAIT-USE-")
                               :allocation :class
                               :imports (list :php-trait-uses (nreverse slots)))
            (php-skip-semis current))))

(defun %php-parse-property-slot (stream modifiers attributes)
  "Parse an untyped or typed PHP property into an AST slot definition."
  (multiple-value-bind (property-type after-type) (php-parse-type-annotation stream)
    (let ((current (if (and property-type (eq (php-peek-type after-type) :T-VAR))
                       after-type
                       stream))
          (slot-type (when (and property-type (eq (php-peek-type after-type) :T-VAR))
                       property-type)))
      (multiple-value-bind (var-tok rest) (php-consume current)
        ;; Use the SAME symbol convention as member access ($o->x): strip the
        ;; leading $ and intern via php-ident-sym (upcased). php-var-sym did not
        ;; upcase, so `public $x' declared slot |x| while $o->x looked up X, giving
        ;; "slot X is missing from the object".
        (let* ((raw  (php-tok-value var-tok))
               (bare (if (and (stringp raw) (plusp (length raw)) (char= (char raw 0) #\$))
                         (subseq raw 1) raw))
               ;; An untyped property with no initializer defaults to PHP null —
               ;; NOT the unbound-slot-marker. Without this, $o->x on `public $x;'
               ;; read as the marker, so is_null($o->x) was false and $o->x ?? d
               ;; never coalesced.
               (initform (make-ast-quote :value +php-null+)))
          ;; Default value: parse it into the slot initform (was discarded).
          (when (and rest (eq (php-peek-type rest) :T-OP)
                     (equal "=" (php-peek-value rest)))
            (multiple-value-bind (default-ast rest2) (php-parse-expr (cdr rest) nil)
              (setf initform default-ast
                    rest rest2)))
          (let ((slot (make-ast-slot-def :name (php-ident-sym bare)
                                         :type slot-type
                                         :initform initform
                                         :allocation (if (member :static modifiers :test #'eq)
                                                         :class
                                                         :instance)
                                         :imports (%php-slot-metadata modifiers
                                                                      :attributes attributes
                                                                      :target-type :property))))
            (values slot (php-skip-semis rest))))))))

(defun %php-promoted-param-assignments (param-attributes)
  "Build (ast-set-slot-value $this->prop $param) nodes for each promoted param.
PARAM-ATTRIBUTES is the :php-param-attributes plist value from an ast-defun."
  (loop for attr-plist in param-attributes
        for param = (car attr-plist)
        when (getf (cdr attr-plist) :php-promote)
          collect (make-ast-set-slot-value
                   :object (make-ast-var :name (php-var-sym "$this"))
                   :slot   (php-ident-sym (symbol-name param))
                   :value  (make-ast-var :name param))))

(defun %php-promoted-property-slots (param-attributes)
  "Build ast-slot-def nodes declaring each promoted param as a class property.
PHP 8.0 constructor promotion both declares AND initializes the property."
  (loop for attr-plist in param-attributes
        for param = (car attr-plist)
        for mods = (getf (cdr attr-plist) :php-promote)
        when mods
          collect (make-ast-slot-def
                   :name      (php-ident-sym (symbol-name param))
                   :initform  (make-ast-quote :value +php-null+)
                   :allocation (if (member :static mods :test #'eq) :class :instance)
                   :imports   (%php-slot-metadata mods :target-type :property))))

(defun %php-parse-class-body-member (stream known-vars &optional class-name)
  "Parse one class body member: a property declaration or a method definition.
Returns (values slot-def-or-nil remaining-stream extra-slots).
EXTRA-SLOTS is a list of additional slots to inject (used for constructor promotion)."
  (multiple-value-bind (attributes stream) (%php-parse-attributes stream)
  (multiple-value-bind (modifiers stream) (%php-parse-visibility-modifiers stream)
  (let ((stream (php-skip-semis stream)))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (multiple-value-bind (slots rest)
           (%php-parse-property-slot-with-hooks stream modifiers attributes class-name)
         (values (first slots) rest (rest slots))))
      ;; type $var;  — typed property, including nullable/union/intersection/DNF types
      ((or (%php-type-atom-token-p stream)
           (eq (php-peek-type stream) :T-LPAREN)
           (eq (php-peek-type stream) :T-NULLABLE))
        (multiple-value-bind (property-type rest) (php-parse-type-annotation stream)
          (if (and property-type (eq (php-peek-type rest) :T-VAR))
              (multiple-value-bind (slots rest2)
                  (%php-parse-property-slot-with-hooks stream modifiers attributes class-name)
                (values (first slots) rest2 (rest slots)))
              (values nil rest))))
      ;; const [TYPE] NAME = value; — class constants as class-scoped metadata slots.
       ((and (eq (php-peek-type stream) :T-KEYWORD)
               (eq (php-peek-value stream) :const))
        (%php-parse-class-constant stream modifiers known-vars attributes))
       ;; use TraitName[, OtherTrait] [{ insteadof/as block }]; — trait use with
       ;; optional conflict-resolution block, inside class-like bodies.
       ;; Delegate to the full use-trait parser (defined in parser-trait.lisp)
       ;; that handles { insteadof/as } blocks.  The `use` keyword was already
       ;; consumed by the caller that found :use in the token stream, but here
       ;; we are still at the `use` token — strip it before dispatching.
       ((and (eq (php-peek-type stream) :T-KEYWORD)
             (eq (php-peek-value stream) :use))
        (multiple-value-bind (slot rest kv)
            (%php-parse-use-trait-stmt (cdr stream) known-vars)
          (declare (ignore kv))
          (values slot rest)))
      ;; function name(...) { }  — method
      ((and (eq (php-peek-type stream) :T-KEYWORD)
            (eq (php-peek-value stream) :function))
        (multiple-value-bind (method-ast rest _) (php-parse-statement stream known-vars)
           (declare (ignore _))
           (%php-attach-attributes-to-node method-ast attributes :method)
           ;; Instance methods receive an implicit $this first parameter; the call
           ;; site ($o->m(args)) passes the receiver as the first argument. Static
           ;; methods have no receiver, so they are left as-is.
           (when (and method-ast
                      (ast-defun-p method-ast)
                      (not (member :static modifiers :test #'eq)))
             (setf (ast-defun-params method-ast)
                   (cons (php-var-sym "$this") (ast-defun-params method-ast))))
           ;; Constructor property promotion (PHP 8.0+): inject $this->prop = $param;
           ;; at the top of __construct for each param tagged :php-promote.
           (let (promoted-slots)
             (when (and method-ast
                        (ast-defun-p method-ast)
                        (eq (ast-defun-name method-ast) (php-ident-sym "__construct")))
               (let* ((attrs (getf (ast-defun-declarations method-ast) :php-param-attributes))
                      (promos (%php-promoted-param-assignments attrs)))
                 (when promos
                   (setf (ast-defun-body method-ast)
                         (append promos (ast-defun-body method-ast)))
                   (setf promoted-slots (%php-promoted-property-slots attrs)))))
             (values (when method-ast
                       ;; Store full ast-defun in slot-def initform to preserve method body.
                       ;; Static methods are class-allocated so their bound closure lives on
                       ;; the class object itself — C::method() then resolves via slot-value
                       ;; on the class (the same path class constants already use).  Instance
                       ;; methods stay :instance so each object carries its own closure.
                       (make-ast-slot-def :name (ast-defun-name method-ast)
                                          :initform method-ast
                                          :allocation (if (member :static modifiers :test #'eq)
                                                          :class
                                                          :instance)
                                          :imports (%php-slot-metadata modifiers
                                                                       :attributes attributes
                                                                       :target-type :method)))
                     rest
                     promoted-slots))))
      ;; Unknown class-body syntax must not be silently discarded. Silent skip
      ;; made unsupported PHP look successfully parsed while losing source.
      (t (error "PHP unsupported feature in class body near token ~S" (php-peek stream))))))))

(defun %php-parse-class-superclasses (stream)
  "Consume optional 'extends ClassName' and 'implements A, B, ...' clauses.
Returns (values superclass-list remaining-stream)."
  (let ((supers nil) (current stream))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :extends))
      (setf current (cdr current))
      (multiple-value-bind (super-name rest) (php-parse-qualified-name current)
        (push (php-ident-sym (php-resolve-qualified-name super-name :class)) supers)
        (setf current rest)))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :implements))
      (setf current (cdr current))
      (loop
        (multiple-value-bind (interface-name rest) (php-parse-qualified-name current)
          (push (php-ident-sym (php-resolve-qualified-name interface-name :class)) supers)
          (setf current rest))
        (unless (and current (eq (php-peek-type current) :T-COMMA))
          (return))
        (setf current (cdr current))))
    (values (nreverse supers) current)))

(defun %php-readonly-class-slots (slots)
  "Mark instance property slots as readonly for PHP 8.2 readonly classes."
  (mapcar (lambda (slot)
            (if (and (ast-slot-def-p slot)
                     (eq (ast-slot-allocation slot) :instance)
                     (not (ast-defun-p (ast-slot-initform slot))))
                (let* ((copy (copy-structure slot))
                       (imports (ast-imports copy))
                       (base-imports (loop for (key value) on imports by #'cddr
                                           unless (member key '(:php-modifiers :readonly-p)
                                                          :test #'eq)
                                             append (list key value)))
                       (modifiers (copy-list (getf imports :php-modifiers))))
                  (unless (member :readonly modifiers :test #'eq)
                    (setf modifiers (append modifiers (list :readonly))))
                  (setf (ast-imports copy)
                        (append base-imports
                                (list :php-modifiers modifiers
                                      :readonly-p t)))
                  copy)
                slot))
          slots))

(defun %php-parse-class-decl (rest known-vars &key readonly-p)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((class-name (php-ident-sym
                       (php-resolve-qualified-name (php-tok-value name-tok) :class))))
      (multiple-value-bind (supers rest) (%php-parse-class-superclasses rest)
        (let ((current (%php-consume-expected :T-LBRACE rest))
              (slots nil)
              ;; Expose the class + parents to expression parsing so self:: /
              ;; static:: / parent:: inside method bodies resolve correctly.
              (*php-current-class* class-name)
              (*php-current-supers* supers))
          (loop
            (setf current (php-skip-semis current))
            (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
              (return))
            (multiple-value-bind (slot rest2 extra-slots) (%php-parse-class-body-member current known-vars class-name)
              (when slot (push slot slots))
              (dolist (es extra-slots) (push es slots))
              (setf current rest2)))
          (setf slots (nreverse slots))
          (when readonly-p
            (setf slots (%php-readonly-class-slots slots)))
          (%php-record-class-trait-uses class-name slots)
          (values (make-ast-defclass :name class-name
                                      :superclasses supers
                                      :slots slots
                                      :php-kind :class)
                  (%php-consume-expected :T-RBRACE current)
                  known-vars))))))

(define-php-stmt-parser :class (rest known-vars)
  (%php-parse-class-decl rest known-vars))

(define-php-stmt-parser :readonly (rest known-vars)
  (unless (and rest
               (eq (php-peek-type rest) :T-KEYWORD)
               (eq (php-peek-value rest) :class))
    (error "PHP readonly modifier is only supported before class declarations near token ~S"
           (php-peek rest)))
  (%php-parse-class-decl (cdr rest) known-vars :readonly-p t))

;;; ─── Interface Statement Parser (overrides parser-stmt entry) ───────────────

(define-php-stmt-parser :interface (rest known-vars)
  "%php-parse-interface-decl handles the full interface syntax including
multiple-extends and abstract method signatures."
  (%php-parse-interface-decl rest known-vars))

;;; ─── Statement Dispatcher ────────────────────────────────────────────────────

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars).
Dispatches keyword statements through *php-stmt-parsers*; falls through
to %php-parse-expr-stmt for expression statements."
  (multiple-value-bind (attributes stream) (%php-parse-attributes stream)
  (cond
    ((eq (php-peek-type stream) :T-INLINE-HTML)
      (multiple-value-bind (tok rest) (php-consume stream)
        ;; Inline HTML is emitted verbatim with NO trailing newline and must
        ;; participate in PHP output buffering.
        (values (%php-attach-attributes-to-node
                 (%php-call 'cl-cc/php::%php-output-write
                            (make-ast-quote :value (php-tok-value tok)))
                 attributes)
                rest known-vars)))
    (t
      (let ((handler (when (eq (php-peek-type stream) :T-KEYWORD)
                       (gethash (php-peek-value stream) *php-stmt-parsers*))))
        (if handler
            (multiple-value-bind (_ rest) (php-consume stream)
              (multiple-value-bind (stmt rest2 kv2) (funcall handler rest known-vars)
                (let ((target-type (case (php-tok-value _)
                                     ((:class :readonly) :class)
                                     (:const :constant)
                                     (:function :function)
                                     ((:trait :interface :enum) :class)
                                     (otherwise nil))))
                  (when (and attributes
                             (eq (php-tok-value _) :const)
                             (ast-progn-p stmt))
                    (error "PHP parse error: attributes on grouped const declarations require exactly one constant"))
                  (values (%php-attach-attributes-to-node
                           stmt attributes target-type)
                          rest2 kv2))))
            (multiple-value-bind (stmt rest2 kv2) (%php-parse-expr-stmt stream known-vars)
              (values (%php-attach-attributes-to-node stmt attributes)
                      rest2 kv2))))))))

;;; ─── Top-Level Entry Point ───────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Wrap each `ast-let' with an empty body around the statements that follow it.

PHP's first assignment to a variable lowers to (ast-let ((var . value)) :body nil)
— a declaration whose scope must cover every later statement in the same block.
Walking STMTS backwards, each empty-bodied let absorbs the already-nested tail as
its body, so `$x = 10; echo $x;' becomes (let (($x 10)) (echo $x)) rather than two
siblings where echo sees an unbound $x. Statements that are not empty-bodied lets
(including lets that already carry a body) pass through as siblings.

This pass is applied at top level and in every braced block (see
php-parse-block), keeping each first assignment visible to the later statements
that share the same PHP block scope."
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
          (progn
            (setf (ast-let-body stmt) tail)
            (setf tail (list stmt)))
          (push stmt tail)))))

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

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
Analogous to parse-all-forms for CL."
  (let ((stream (tokenize-php-source source))
        (stmts nil) (kv nil)
        (*php-current-namespace* nil)
        (*php-current-imports* nil)
        (*php-pending-top-level-forms* nil)
        (*php-by-ref-param-registry* (%php-seed-by-ref-param-registry))
        (*php-named-param-registry* (make-hash-table :test #'equal)))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (cond
          (*php-pending-top-level-forms*
           (dolist (form (reverse *php-pending-top-level-forms*))
             (push form stmts))
           (setf *php-pending-top-level-forms* nil))
          (stmt
           (push (php-annotate-top-level-node stmt) stmts)))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings
     (%php-lower-reference-assignments
      (%php-validate-override-tree
       (%php-apply-no-discard-warnings (nreverse stmts)))
      nil))))
