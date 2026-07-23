;;;; packages/javascript/tests/js-parser-decl-tests.lisp — JS parser: declarations
;;;;
;;;; Shared parse helpers (available to js-parser-stmt-tests via serial load):
;;;;   %js-parse SOURCE  → list of top-level AST nodes
;;;;   %js-first SOURCE  → first top-level AST node
;;;;   %js-call-name AST → upcased name string for an ast-call
;;;;
;;;; Covers: variable/const/let declarations, arrow functions, async/generator
;;;;         modifiers, class declarations with fields/methods/static/private.

(in-package :cl-cc/test)

;;; ─── Shared helpers (available to all js-parser-* files via serial load) ─────

(defun %js-parse (src)
  "Parse SRC and return the list of top-level AST nodes."
  (cl-cc/javascript:parse-js-source src))

(defun %js-first (src)
  "Parse SRC and return the first top-level AST node."
  (first (%js-parse src)))

(defun %js-call-name (ast)
  "Return the upcased symbol name for an AST-CALL whose func is an AST-VAR."
  (when (and (cl-cc:ast-call-p ast)
             (cl-cc:ast-var-p (cl-cc:ast-call-func ast)))
    (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func ast)))))

;;; ─── Variable / const / let declarations ─────────────────────────────────────

(it-sequential "js-parser-const-decl"
  (let ((ast (%js-first "const x = 42;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (= 1 (length (cl-cc:ast-let-bindings ast))) :to-be-truthy)
    (expect (cl-cc:ast-int-p (cdr (first (cl-cc:ast-let-bindings ast)))) :to-be-truthy)))

(it-sequential "js-parser-let-decl"
  (let ((ast (%js-first "let y = 'hello';")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (cl-cc:ast-quote-p (cdr (first (cl-cc:ast-let-bindings ast)))) :to-be-truthy)))

(it-sequential "js-parser-var-decl"
  (let ((ast (%js-first "var z;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)))

(it-sequential "js-parser-const-multi-decl"
  (let ((ast (%js-first "const a = 1, b = 2;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (= 1 (length (cl-cc:ast-let-bindings ast))) :to-be-truthy)
    (let ((inner (first (cl-cc:ast-let-body ast))))
      (expect (cl-cc:ast-let-p inner) :to-be-truthy)
      (expect (= 1 (length (cl-cc:ast-let-bindings inner))) :to-be-truthy))))

;;; ─── Arrow functions ──────────────────────────────────────────────────────────

(it-sequential "js-parser-arrow-function-simple"
  (let* ((ast (%js-first "const f = (x) => x + 1;"))
         (val (cdr (first (cl-cc:ast-let-bindings ast)))))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (cl-cc:ast-lambda-p val) :to-be-truthy)
    (expect (= 1 (length (cl-cc:ast-lambda-params val))) :to-be-truthy)))

(it-sequential "js-parser-arrow-function-block-body"
  (let* ((ast (%js-first "const double = (x) => { return x * 2; };"))
         (val (cdr (first (cl-cc:ast-let-bindings ast))))
         (body (cl-cc:ast-lambda-body val)))
    (expect (cl-cc:ast-lambda-p val) :to-be-truthy)
    (expect (cl-cc:ast-block-p (first body)) :to-be-truthy)
    (expect (some #'cl-cc:ast-return-from-p
                       (cl-cc:ast-block-body (first body))) :to-be-truthy)))

;;; ─── Async / generator function modifiers ────────────────────────────────────

(it-sequential "js-parser-function-modifiers async"
  (destructuring-bind (src decl) (list "async function fetch() {}" :js-async)
    (let ((ast (%js-first src)))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (member decl (cl-cc:ast-defun-declarations ast)) :to-be-truthy))))

(it-sequential "js-parser-function-modifiers generator"
  (destructuring-bind (src decl) (list "function* gen() {}" :js-generator)
    (let ((ast (%js-first src)))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (member decl (cl-cc:ast-defun-declarations ast)) :to-be-truthy))))

;;; ─── Class declarations ───────────────────────────────────────────────────────

(it-sequential "js-parser-class-decl-simple"
  (let ((ast (%js-first "class Dog {}")))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    ;; case-preserved: JS `Dog' interns as |Dog|, not DOG
    (expect (symbol-name (cl-cc:ast-defclass-name ast)) :to-equal "Dog")))

(it-sequential "js-parser-class-with-extends"
  (let ((ast (%js-first "class Cat extends Animal {}")))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (some (lambda (s) (string= "Animal" (symbol-name s)))
              (cl-cc:ast-defclass-superclasses ast)) :to-be-truthy)))

(it-sequential "js-parser-class-with-constructor"
  (let ((ast (%js-first "class Pt { constructor(x, y) { this.x = x; this.y = y; } }")))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (>= (length (cl-cc:ast-defclass-slots ast)) 1) :to-be-truthy)))

(it-sequential "js-parser-class-with-method"
  (let* ((ast (%js-first "class C { greet() { return 1; } }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (>= (length slots) 1) :to-be-truthy)))

(it-sequential "js-parser-class-with-static-method"
  (let* ((ast (%js-first "class C { static create() {} }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (some (lambda (slot)
                          (getf (cl-cc:ast-imports slot) :js-static))
                        slots) :to-be-truthy)))

(it-sequential "js-parser-class-with-private-field"
  (let* ((ast (%js-first "class C { #count = 0; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (some (lambda (slot)
                (let ((name (symbol-name (cl-cc:ast-slot-name slot))))
                  (or (search "COUNT" name) (search "PRIV" name))))
              slots) :to-be-truthy)))

(it-sequential "js-parser-class-field-arrow-initializer"
  (let ((asts (%js-parse "class C { g = () => {}; }")))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-defclass-p (first asts)) :to-be-truthy)))

(it-sequential "js-parser-class-field-private-arrow-and-method"
  (let* ((asts (%js-parse "class C { #m() {} #g = () => {}; }"))
         (ast (first asts)))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-defclass-p ast) :to-be-truthy)
    (expect (>= (length (cl-cc:ast-defclass-slots ast)) 2) :to-be-truthy)))

(it-sequential "js-parser-class-field-object-literal-initializer"
  (let ((asts (%js-parse "class C { x = {a: 1, b: 2}; }")))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-defclass-p (first asts)) :to-be-truthy)))

(it-sequential "js-parser-tagged-template plain"
  (destructuring-bind (src) (list "tag`hi`;")
    (let ((asts (%js-parse src)))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-call-p (first asts)) :to-be-truthy))))

(it-sequential "js-parser-tagged-template interpolated"
  (destructuring-bind (src) (list "tag`hi ${name}`;")
    (let ((asts (%js-parse src)))
    (expect (= 1 (length asts)) :to-be-truthy)
    (expect (cl-cc:ast-call-p (first asts)) :to-be-truthy))))

(it-sequential "js-parser-class-expression plain"
  (destructuring-bind (src) (list "const C = class { m() { return 1; } };")
    (let ((ast (%js-first src)))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy))))

(it-sequential "js-parser-class-expression extends"
  (destructuring-bind (src) (list "const C = class extends Base { constructor() { super(); } };")
    (let ((ast (%js-first src)))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy))))
