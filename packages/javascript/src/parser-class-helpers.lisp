;;;; packages/javascript/src/parser-class-helpers.lisp — ES2026 class parser helpers
;;;;
;;;; Shared helper routines for class decorators, member names, accessors, and
;;;; member metadata.  Kept separate from parser-class.lisp so the latter can
;;;; focus on class-body lowering and stay below the giant-file threshold.

(in-package :cl-cc/javascript)

;;; ─── Symbol helpers ──────────────────────────────────────────────────────────
;;; js-tok-type, js-tok-value, js-peek*, js-consume, js-expect, js-at-eof-p,
;;; js-skip-semis, js-ident-sym — all defined in parser.lisp / parser-stmt.lisp.

(defun js-private-ident-sym (str)
  "Intern a JS private field name (without #) with a %JS-PRIV- prefix."
  (intern (concatenate 'string "%JS-PRIV-"
                       (string-upcase (if (stringp str) str (symbol-name str))))
          :cl-cc/javascript))

;;; ─── Decorator parser ────────────────────────────────────────────────────────

(defun %js-parse-decorator (stream)
  "Parse a single @expression decorator.
Returns (values decorator-ast rest) where DECORATOR-AST is an ast-call or
ast-var representing the decorator."
  ;; Consume the '@' token (already confirmed :T-AT by caller)
  (multiple-value-bind (_ rest) (js-consume stream)
    (declare (ignore _))
    ;; Decorator body: identifier optionally followed by member chain and/or call
    (multiple-value-bind (name-tok rest2) (js-expect :T-IDENT rest)
      (let ((sym (js-ident-sym (js-tok-value name-tok)))
            (current rest2))
        ;; Walk dotted member access: @foo.bar.baz
        (loop while (and current (eq (js-peek-type current) :T-DOT))
              do (setf current (cdr current))  ; consume dot
              (multiple-value-bind (mem-tok rest3) (js-expect :T-IDENT current)
                (setf sym (js-ident-sym
                           (concatenate 'string
                                        (symbol-name sym) "." (js-tok-value mem-tok)))
                      current rest3)))
        ;; Optional argument list: @decorator(args...)
        (if (and current (eq (js-peek-type current) :T-LPAREN))
            (let ((arg-rest (cdr current))  ; consume '('
                  (args nil))
              (loop until (or (js-at-eof-p arg-rest)
                              (eq (js-peek-type arg-rest) :T-RPAREN))
                    do (push (make-ast-var :name (js-ident-sym "_decorator-arg_"))
                             args)
                       ;; skip tokens until comma or closing paren
                       (loop while (and arg-rest
                                        (not (member (js-peek-type arg-rest)
                                                     '(:T-COMMA :T-RPAREN))))
                             do (setf arg-rest (cdr arg-rest)))
                       (when (and arg-rest (eq (js-peek-type arg-rest) :T-COMMA))
                         (setf arg-rest (cdr arg-rest))))
              (multiple-value-bind (_ rest4) (js-expect :T-RPAREN arg-rest)
                (declare (ignore _))
                (values (make-ast-call :func (make-ast-var :name sym)
                                       :args (nreverse args))
                        rest4)))
            (values (make-ast-var :name sym) current))))))

(defun %js-parse-decorators (stream)
  "Parse zero or more @decorator lines.
Returns (values decorators-list rest)."
  (let ((decorators nil)
        (current stream))
    (loop while (and current (eq (js-peek-type current) :T-AT))
          do (multiple-value-bind (dec rest) (%js-parse-decorator current)
               (push dec decorators)
               (setf current rest)))
    (values (nreverse decorators) current)))

;;; ─── Computed member name ────────────────────────────────────────────────────

(defun %js-parse-computed-name (stream)
  "Parse a computed property name [expr]. Consumes [ expr ].
Returns (values name-ast rest).  name-ast is wrapped in a list with
:computed-name metadata so lowering can distinguish it from a plain symbol."
  (let ((rest (cdr stream)))          ; consume '['
    ;; For now we represent the computed name as a special ast-call node
    ;; that downstream code recognises via its :imports metadata.
    ;; We skip tokens until the matching ].
    (let ((depth 1)
          (current rest)
          (expr-tokens nil))
      (loop while (and current (not (and (eq (js-peek-type current) :T-RBRACKET)
                                         (= depth 1))))
            do (cond
                 ((eq (js-peek-type current) :T-LBRACKET) (incf depth))
                 ((eq (js-peek-type current) :T-RBRACKET) (decf depth)))
               (push (car current) expr-tokens)
               (setf current (cdr current)))
      ;; consume ']'
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACKET current)
        (declare (ignore _))
        ;; Return a placeholder AST; real parsers would call js-parse-expr here
        (let ((name-node
               (make-ast-call
                :func (make-ast-var :name (js-ident-sym "%JS-COMPUTED-NAME"))
                :args (mapcar (lambda (tok)
                                (make-ast-quote :value (js-tok-value tok)))
                              (nreverse expr-tokens))
                :imports (list :js-computed-name t))))
          (values name-node rest2))))))

;;; ─── Member name parser ──────────────────────────────────────────────────────

(defun %js-parse-member-name (stream)
  "Parse a class member name. Returns (values name private-p rest orig-name).
NAME is a symbol for plain/private names or an AST node for computed names.
PRIVATE-P is T when the name was a #privateIdent. ORIG-NAME is the member name's
ORIGINAL-CASE string (js-ident-sym upcases the symbol, but JS property access is
case-sensitive and keys on the source string, so the class lowering stores
methods under ORIG-NAME); NIL for computed names."
  (cond
    ;; Computed name [expr]
    ((eq (js-peek-type stream) :T-LBRACKET)
     (multiple-value-bind (name-ast rest) (%js-parse-computed-name stream)
       (values name-ast nil rest nil)))
    ;; Private identifier #name
    ((eq (js-peek-type stream) :T-PRIVATE-IDENT)
     (multiple-value-bind (tok rest) (js-consume stream)
       (let ((v (js-tok-value tok)))
         (values (js-private-ident-sym v) t rest
                 (if (stringp v) v (symbol-name v))))))
    ;; Regular identifier — also accept keyword names used as method names
    ;; (e.g. class C { get() {} static() {} })
    (t
     (multiple-value-bind (tok rest) (js-consume stream)
       (let ((v (js-tok-value tok)))
         (values (js-ident-sym (if (stringp v) v (symbol-name v)))
                 nil rest
                 (if (stringp v) v (symbol-name v))))))))

;;; ─── Class body member kinds ─────────────────────────────────────────────────

;;; Internal helper: build a slot-def for a get/set accessor.
;;; KIND is :getter or :setter; STREAM points past the consumed 'get'/'set' token.
;;; Getters take no parameters; setters take one.
(defun %js-parse-accessor (kind static-p decorators stream)
  (multiple-value-bind (name private-p rest) (%js-parse-member-name stream)
    (multiple-value-bind (params body rest2) (%js-parse-method-params-body rest)
      (let* ((sym (if (symbolp name)
                      name
                      (gensym (if (eq kind :getter) "JS-GETTER-" "JS-SETTER-"))))
             (slot (make-ast-slot-def
                    :name sym
                    :initform (make-ast-defun :name sym
                                              :params (if (eq kind :getter) nil params)
                                              :body (list body))
                    :allocation (if static-p :class :instance)
                    :imports (%js-member-kind-metadata kind static-p private-p nil nil decorators))))
        (values slot (js-skip-semis rest2))))))

(defun %js-parse-method-params-body (stream)
  "Parse ( params ) { body } for a method. Returns (values params body rest).
Params is a list of symbols; body is an ast-progn of properly-parsed AST nodes.

Previously this function collected method body tokens verbatim and stored them
as (ast-progn (ast-quote raw-token-list)). That meant class method bodies were
never compiled - every method silently became a no-op. Now the body is parsed
via js-parse-stmt-list (the real statement parser) so methods execute normally."
  ;; Parameter list
  (let ((rest (nth-value 1 (js-expect :T-LPAREN stream)))
        (params nil))
    (loop until (or (js-at-eof-p rest) (eq (js-peek-type rest) :T-RPAREN))
          do (cond
               ;; Rest parameter ...name
               ((eq (js-peek-type rest) :T-ELLIPSIS)
                (setf rest (cdr rest))
                (multiple-value-bind (tok rest2) (js-expect :T-IDENT rest)
                  (push (list :rest (js-ident-sym (js-tok-value tok))) params)
                  (setf rest rest2)))
               ;; Normal identifier param (ignore defaults for now)
               ((member (js-peek-type rest) '(:T-IDENT :T-THIS))
                (multiple-value-bind (tok rest2) (js-consume rest)
                  (push (js-ident-sym (js-tok-value tok)) params)
                  (setf rest rest2)
                  ;; Skip default value = expr
                  (when (and rest (eq (js-peek-type rest) :T-OP)
                             (equal "=" (js-peek-value rest)))
                    (setf rest (cdr rest))
                    ;; Skip until comma or close-paren
                    (loop while (and rest
                                     (not (member (js-peek-type rest)
                                                  '(:T-COMMA :T-RPAREN))))
                          do (setf rest (cdr rest))))))
               (t
                ;; Destructuring params or other complex params - skip token
                (setf rest (cdr rest))))
          ;; consume comma separator
          (when (and rest (eq (js-peek-type rest) :T-COMMA))
            (setf rest (cdr rest))))
    (multiple-value-bind (_ rest2) (js-expect :T-RPAREN rest)
      (declare (ignore _))
      ;; Method body { stmts... }
      ;; js-parse-stmt-list consumes the opening { and closing } and returns
      ;; (values stmt-list rest-after-rbrace) - the real statement parser used
      ;; by js-parse-block and js-parse-function-body.
      (multiple-value-bind (_ rest3) (js-expect :T-LBRACE rest2)
        (declare (ignore _))
        (multiple-value-bind (body-stmts rest4) (js-parse-stmt-list rest3)
          (let ((body-ast (make-ast-progn :forms (%js-callable-body body-stmts))))
            (values (nreverse params) body-ast rest4)))))))

(defun %js-member-kind-metadata (kind static-p private-p async-p generator-p decorators
                                  &optional orig-name)
  "Build the :imports plist for a class member slot.
ORIG-NAME is the original-case method name string (case-sensitive key for prototype)."
  (append (list :js-member-kind kind)
          (when orig-name   (list :js-orig-name orig-name))
          (when static-p    (list :js-static    t))
          (when private-p   (list :js-private   t))
          (when async-p     (list :js-async     t))
          (when generator-p (list :js-generator t))
          (when decorators  (list :js-decorators decorators))))
