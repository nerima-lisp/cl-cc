;;;; packages/javascript/src/parser-class.lisp — ES2026 JavaScript class parser
;;;;
;;;; Parses ES2026 class declarations and expressions into the common CL-CC AST.
;;;;
;;;; Features covered:
;;;;   - constructor, instance methods, getters, setters
;;;;   - static methods and static fields
;;;;   - private fields and methods (#name)
;;;;   - public class fields (initialised and uninitialised)
;;;;   - static initialisation blocks (static { ... })
;;;;   - computed member names ([expr])
;;;;   - decorators (@decorator) on classes and members
;;;;   - async methods and generator methods
;;;;   - class expressions (class Foo { } assigned to a variable)
;;;;   - inheritance via `extends`
;;;;
;;;; AST lowering strategy (mirrors PHP parser-class.lisp):
;;;;   class C extends B { fields...; methods... }
;;;;   →  (ast-defclass :name C :superclasses (B) :slots ...)
;;;;       where each method slot has its ast-defun in :initform
;;;;       and public/private/static metadata is encoded in :imports

(in-package :cl-cc/javascript)

;;; ─── Class body member parser ────────────────────────────────────────────────

(defun %js-parse-class-body-member (stream)
  "Parse one member from a class body.  Returns (values slot-def rest).
Handles: methods, getters, setters, fields, static blocks, decorators."
  ;; 1. Collect leading decorators
  (multiple-value-bind (decorators stream) (%js-parse-decorators stream)
    (let ((static-p nil)
          (async-p nil)
          (generator-p nil)
          (current stream))
      ;; 2. Consume optional 'static'
      (when (and current (eq (js-peek-type current) :T-STATIC))
        (setf static-p t
              current (cdr current)))
      ;; 3. Static initialisation block: static { ... }
      ;; Parse the body via js-parse-stmt-list (real statement parser) so
      ;; static initializer code actually executes, not a raw token dump.
      (when (and static-p current (eq (js-peek-type current) :T-LBRACE))
        (let ((rest (cdr current)))   ; consume '{'
          (multiple-value-bind (body-stmts rest2) (js-parse-stmt-list rest)
            (let* ((body-ast (make-ast-progn :forms body-stmts))
                   (slot (make-ast-slot-def
                          :name (gensym "JS-STATIC-INIT-")
                          :initform body-ast
                          :allocation :class
                          :imports (list :js-member-kind :static-block
                                         :js-decorators decorators))))
              (return-from %js-parse-class-body-member
                (values slot rest2))))))
      ;; 4. 'async' modifier
      (when (and current
                 (eq (js-peek-type current) :T-ASYNC)
                 ;; Only treat as modifier when NOT followed by '=' or ';' (field named async)
                 (not (and (cdr current)
                           (member (js-peek-type (cdr current))
                                   '(:T-OP :T-SEMI :T-RBRACE) :test #'eq)
                           (or (eq (js-peek-type (cdr current)) :T-SEMI)
                               (eq (js-peek-type (cdr current)) :T-RBRACE)
                               (and (eq (js-peek-type (cdr current)) :T-OP)
                                    (equal "=" (js-peek-value (cdr current))))))))
        (setf async-p t
              current (cdr current)))
      ;; 5. Generator star
      (when (and current (eq (js-peek-type current) :T-OP)
                 (equal "*" (js-peek-value current)))
        (setf generator-p t
              current (cdr current)))
      ;; 6. Getter: get NAME () { }
      (when (and current (eq (js-peek-type current) :T-GET)
                 (cdr current)
                 (not (member (js-peek-type (cdr current))
                              '(:T-LPAREN :T-SEMI :T-RBRACE) :test #'eq)))
        (return-from %js-parse-class-body-member
          (%js-parse-accessor :getter static-p decorators (cdr current))))
      ;; 7. Setter: set NAME (param) { }
      (when (and current (eq (js-peek-type current) :T-SET)
                 (cdr current)
                 (not (member (js-peek-type (cdr current))
                              '(:T-LPAREN :T-SEMI :T-RBRACE) :test #'eq)))
        (return-from %js-parse-class-body-member
          (%js-parse-accessor :setter static-p decorators (cdr current))))
      ;; 8. Normal member: parse name, then decide method vs field
      (multiple-value-bind (name private-p rest orig-name) (%js-parse-member-name current)
        ;; Method: name ( ...
        (if (and rest (eq (js-peek-type rest) :T-LPAREN))
            (multiple-value-bind (params body rest2) (%js-parse-method-params-body rest)
              (let* ((sym (if (symbolp name) name (gensym "JS-METHOD-")))
                     (defun-ast (make-ast-defun
                                 :name sym
                                 :params params
                                 :body (list body)))
                     (slot (make-ast-slot-def
                            :name sym
                            :initform defun-ast
                            :allocation (if static-p :class :instance)
                            ;; :js-name carries the ORIGINAL-CASE method name so the
                            ;; class lowering stores it under the key obj.m accesses.
                            :imports (%js-member-kind-metadata
                                      (if (equal (symbol-name sym) "CONSTRUCTOR")
                                          :constructor
                                          :method)
                                      static-p private-p async-p generator-p decorators
                                      orig-name))))
                (values slot (js-skip-semis rest2))))
            ;; Field declaration: name [= expr] ;
            (let ((initform nil)
                  (current rest))
              (when (and current (eq (js-peek-type current) :T-OP)
                         (equal "=" (js-peek-value current)))
                (setf current (cdr current))   ; consume '='
                ;; Collect field-initialiser tokens up to the top-level ';' or
                ;; class-closing '}'. The span is brace-aware: an initialiser
                ;; may itself contain (), [] and {} — e.g. an arrow function
                ;; `() => {}` or an object literal `{a: 1}` — so we track nesting
                ;; depth and only stop on a ';'/'}' seen at depth 0. A naive
                ;; brace-blind scan stops at the arrow body's '}', truncating the
                ;; initialiser and mis-reading that '}' as the end of the class.
                (let ((expr-toks nil)
                      (depth 0))
                  (loop while (and current
                                   (not (and (zerop depth)
                                             (member (js-peek-type current)
                                                     '(:T-SEMI :T-RBRACE)))))
                        do (case (js-peek-type current)
                             ((:T-LBRACE :T-LPAREN :T-LBRACKET) (incf depth))
                             ((:T-RBRACE :T-RPAREN :T-RBRACKET) (decf depth)))
                           (push (car current) expr-toks)
                           (setf current (cdr current)))
                  ;; Parse the collected tokens into a real expression AST.  (They
                  ;; were previously wrapped in an unresolved %JS-FIELD-INIT
                  ;; placeholder, so field initializers never actually ran.)
                  (setf initform
                        (nth-value 0 (js-parse-assignment-expr (nreverse expr-toks))))))
              (let ((sym (if (symbolp name) name (gensym "JS-FIELD-"))))
                (values (make-ast-slot-def
                         :name sym
                         :initform initform
                         :allocation (if static-p :class :instance)
                         ;; Pass ORIG-NAME so the field key preserves the original
                         ;; case (downcasing the symbol broke `static VERSION = …'
                         ;; — set under "version" but read as A.VERSION).
                         :imports (%js-member-kind-metadata
                                   :field static-p private-p nil nil decorators orig-name))
                        (js-skip-semis current)))))))))

;;; ─── %js-parse-class-body ────────────────────────────────────────────────────

(defun %js-parse-class-body (stream class-name)
  "Parse { member... } class body.
Returns (values member-list rest).
Each member is an ast-slot-def whose :imports plist carries:
  :js-member-kind  (:constructor :method :getter :setter :field :static-block)
  :js-static       t (when static)
  :js-private      t (when private #name)
  :js-async        t (when async method)
  :js-generator    t (when generator method)
  :js-decorators   list"
  (declare (ignore class-name))
  (multiple-value-bind (_ rest) (js-expect :T-LBRACE stream)
    (declare (ignore _))
    (let ((members nil)
          (current rest))
      (loop
        (setf current (js-skip-semis current))
        (when (or (js-at-eof-p current)
                  (eq (js-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (slot rest2) (%js-parse-class-body-member current)
          (when slot (push slot members))
          (setf current rest2)))
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACE current)
        (declare (ignore _))
        (values (nreverse members) rest2)))))

;;; ─── AST lowering + public entry point ──────────────────────────────────────
;;; %js-slot-method-p, %js-slot-to-method-lambda, %js-super-ref,
;;; %js-wrap-method-super, %js-class-member-key, %js-lower-class-to-ast,
;;; and js-parse-class-decl are in parser-class-lower.lisp (loads after this).
