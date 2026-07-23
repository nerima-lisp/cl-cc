(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest php-parser-echo-lowers-to-output-write-call
  "echo expr; lowers to a %php-output-write call (no trailing newline, output
buffer aware) wrapping a %php-concat call (echo applies PHP string conversion to
each value); the concat's first arg is the echoed expr."
  (let ((ast (%php-first "<?php echo 42;")))
    (assert-true (cl-cc:ast-call-p ast))
    (assert-string= "%PHP-OUTPUT-WRITE" (%php-call-name ast))
    (let ((expr (first (cl-cc:ast-call-args ast))))
      (assert-true (cl-cc:ast-call-p expr))
      (assert-string= "%PHP-CONCAT" (%php-call-name expr))
      (assert-true (typep (first (cl-cc:ast-call-args expr)) 'cl-cc:ast-int)))))

(deftest php-parser-reference-assignment-lowers-to-ref-box
  "$b = &$a boxes $a, binds $b to the same box, and dereferences later reads."
  (let* ((ast (%php-first "<?php $a=1; $b=&$a; echo $b;"))
         (body (cl-cc:ast-let-body ast))
         (box-set (first body))
         (alias-let (second body))
         (echo (first (cl-cc:ast-let-body alias-let)))
         (concat (first (cl-cc:ast-call-args echo)))
         (deref (first (cl-cc:ast-call-args concat))))
    (assert-true (cl-cc:ast-let-p ast))
    (assert-true (cl-cc:ast-setq-p box-set))
    (assert-string= "%PHP-MAKE-REF" (%php-call-name (cl-cc:ast-setq-value box-set)))
    (assert-true (cl-cc:ast-let-p alias-let))
    (assert-true (cl-cc:ast-var-p (cdr (first (cl-cc:ast-let-bindings alias-let)))))
    (assert-string= "%PHP-DEREF" (%php-call-name deref))))

(deftest php-parser-settype-lowers-first-arg-by-reference
  "settype($x, ...) boxes the first argument and writes it back after the call."
  (let ((found-box nil)
        (found-writeback nil))
    (labels ((walk (node)
               (when (cl-cc:ast-call-p node)
                 (when (string= "%PHP-MAKE-REF" (%php-call-name node))
                   (setf found-box t)))
               (when (cl-cc:ast-setq-p node)
                 (when (string= (symbol-name (cl-cc:ast-setq-var node)) "x")
                   (setf found-writeback t)))
               (when (typep node 'cl-cc:ast-node)
                 (dolist (child (cl-cc:ast-children node))
                   (when child
                     (walk child))))))
      (dolist (ast (cl-cc/php:parse-php-source "<?php $x=5; settype($x,'string');"))
        (walk ast)))
    (assert-true found-box)
    (assert-true found-writeback)))

;;; ─── :return handler → ast-return-from ───────────────────────────────────

(deftest php-parser-return-with-value-lowering
  "return expr; lowers to ast-return-from with an ast-int value."
  (let ((ast (%php-first "<?php return 1;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-true (typep (cl-cc:ast-return-from-value ast) 'cl-cc:ast-int))))

(deftest php-parser-bare-return-has-nil-name
  "return; (without value) lowers to ast-return-from with nil name slot."
  (let ((ast (%php-first "<?php return;")))
    (assert-true (typep ast 'cl-cc:ast-return-from))
    (assert-null (cl-cc:ast-return-from-name ast))))

;;; ─── Simple statement → AST-type checks ─────────────────────────────────

(deftest-each php-parser-stmt-ast-type
  "Each statement construct lowers to the expected AST node type."
  :cases (("if"          "<?php if ($x) { echo 1; }"                          #'cl-cc:ast-if-p)
          ("while"       "<?php while ($x) { echo 1; }"                       #'cl-cc:ast-block-p)
          ("foreach"     "<?php foreach ($items as $item) { echo $item; }"    #'cl-cc:ast-let-p)
          ("foreach-kv"  "<?php foreach ($arr as $k => $v) { echo $v; }"      #'cl-cc:ast-let-p)
          ("function"    "<?php function greet($name) { return $name; }"       #'cl-cc:ast-defun-p))
  (src pred)
  (assert-true (funcall pred (%php-first src))))

;;; ─── :if handler → ast-if ────────────────────────────────────────────────

(deftest php-parser-if-else-branch-is-ast-progn
  "if-else lowers to ast-if where the else slot is ast-progn."
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-progn))))

(deftest php-parser-if-no-else-branch-is-nil-quote
  "if without else lowers to ast-if where the else slot is ast-quote (nil)."
  (let ((ast (%php-first "<?php if ($x) { echo 1; }")))
    (assert-true (ast-if-p ast))
    (assert-true (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-quote))))

;;; ─── :for handler → ast-progn wrapping while ─────────────────────────────

(deftest php-parser-for-produces-ast-progn
  "for ($i=0;...) lowers to an ast-progn whose single form is the init's let,
nesting the while-loop so the loop variable scopes over cond/body/increment.
(Previously the init and while-loop were two sibling forms, leaving $i unscoped
and the loop producing no output.)"
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (assert-true (typep ast 'cl-cc:ast-progn))
    ;; $i = 0 introduces a new variable, so php-finish-let-bindings nests the
    ;; while-loop inside that let — one progn form (the let), not two siblings.
    (assert-= 1 (length (cl-cc:ast-progn-forms ast)))
    (assert-true (typep (first (cl-cc:ast-progn-forms ast)) 'cl-cc:ast-let))))

;;; ─── :function handler → ast-defun ───────────────────────────────────────

(deftest php-parser-function-name-and-params-captured
  "function add($a, $b) captures upcased name ADD and 2 params."
  (let ((ast (%php-first "<?php function add($a, $b) { return $a; }")))
    (assert-equal "ADD" (symbol-name (cl-cc:ast-defun-name ast)))
    (assert-= 2 (length (cl-cc:ast-defun-params ast)))))

(deftest php-parser-function-no-params-is-nil
  "function noop() with no params produces nil params slot."
  (let ((ast (%php-first "<?php function noop() { return 0; }")))
    (assert-true (typep ast 'cl-cc:ast-defun))
    (assert-null (cl-cc:ast-defun-params ast))))

;;; ─── :class handler → ast-defclass ───────────────────────────────────────

(deftest php-parser-class-lowering
  "class declaration lowers to ast-defclass with upcased name."
  (assert-true (typep (%php-first "<?php class Dog { }") 'cl-cc:ast-defclass))
  (let ((ast (%php-first "<?php class Cat { }")))
    (assert-equal "CAT" (symbol-name (cl-cc:ast-defclass-name ast)))))

(deftest php-parser-class-with-extends
  "class Foo extends Bar captures superclass by upcased name."
  (let ((ast (%php-first "<?php class Puppy extends Dog { }")))
    (assert-true (some (lambda (s) (string= "DOG" (symbol-name s)))
                       (cl-cc:ast-defclass-superclasses ast)))))

(deftest php-parser-class-with-implements
  "class Foo implements A, B preserves interface names in class ancestry metadata."
  (let* ((ast (%php-first "<?php class Box implements IfaceA, IfaceB { }"))
         (names (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (assert-equal '("IFACEA" "IFACEB") names)))

(deftest php-parser-class-with-property
  "class with a property slot produces ast-slot-def."
  (let* ((ast   (%php-first "<?php class Point { public $x; public $y; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (assert-= 2 (length slots))
    (assert-true (every #'cl-cc:ast-slot-def-p slots))))

(deftest php-parser-bare-defclass-is-rejected
  "A raw ast-defclass without a PHP kind marker is rejected by support checks."
  (assert-signals error
    (cl-cc/php:php-check-supported-forms
     (list (cl-cc:make-ast-defclass)))))

;;; ─── Expression statement (dispatch fallthrough) ─────────────────────────

(deftest php-parser-expression-statement-assign
  "Plain assignment is parsed as an expression statement."
  (let ((ast (%php-first "<?php $x = 42;")))
    (assert-true (or (typep ast 'cl-cc:ast-setq)
                      (typep ast 'cl-cc:ast-let)
                      (typep ast 'cl-cc:ast-call)))))

(deftest php-parser-variable-names-preserve-case
  "PHP variables are case-sensitive: $foo, $FOO, and $Foo are distinct AST symbols.
After php-finish-let-bindings the 3 assignments nest into one top-level let chain."
  (let* ((asts (cl-cc/php:parse-php-source "<?php $foo = 1; $FOO = 2; $Foo = 3;"))
         (names nil))
    ;; Walk the nested let chain collecting each variable name
    (labels ((collect (nodes)
               (dolist (node nodes)
                 (when (cl-cc:ast-let-p node)
                   (push (symbol-name (car (first (cl-cc:ast-let-bindings node)))) names)
                   (collect (cl-cc:ast-let-body node))))))
      (collect asts))
    (setf names (nreverse names))
    (assert-equal '("foo" "FOO" "Foo") names)
    (assert-= 3 (length (remove-duplicates names :test #'string=)))))

;;; ─── Multiple top-level statements ───────────────────────────────────────

(deftest php-parser-multi-statement-source
  "cl-cc/php:parse-php-source returns all top-level statements in order."
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; echo 2; echo 3;")))
    (assert-= 3 (length asts))
    (assert-true (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts))))

;;; ─── Characterization tests for unsupported PHP support gaps ───────────────

(deftest php-parser-null-distinct-from-false
  "null and false produce different AST quote values."
  (let ((null-ast (%php-first "<?php $x = null;"))
        (false-ast (%php-first "<?php $x = false;")))
    (let ((null-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings null-ast)))))
          (false-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings false-ast))))))
      (assert-false (eql null-val false-val)))))

(deftest php-parser-truthiness-rules
  "PHP conditionals should lower conditions through PHP truthiness rules."
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (assert-true (cl-cc:ast-if-p ast))
    (let ((cond (cl-cc:ast-if-cond ast)))
      (assert-true (cl-cc:ast-call-p cond))
      (assert-string= "%PHP-TRUTHY" (%php-call-name cond)))))

(deftest php-parser-variable-case-sensitive
  "$foo and $FOO produce different variable symbols."
  (let ((ast (%php-first "<?php $foo = $FOO;")))
    (assert-true (cl-cc:ast-let-p ast))
    (let* ((bindings (cl-cc:ast-let-bindings ast))
           (lhs (car (first bindings)))
           (rhs (cdr (first bindings))))
      (assert-true (cl-cc:ast-var-p rhs))
      (assert-false (string= (symbol-name lhs) (symbol-name (cl-cc:ast-var-name rhs)))))))

(deftest php-parser-count-builtin-lowering
  "count($arr) should lower to %php-count helper, not raw function call."
  (let ((ast (%php-first "<?php $n = count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-string= "%PHP-COUNT" (%php-call-name call)))))

(deftest php-parser-absolute-count-builtin-lowering
  "\\count($arr) should lower to the global %php-count helper."
  (let ((ast (%php-first "<?php namespace App\\Lib; $n = \\count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-string= "%PHP-COUNT" (%php-call-name call)))))

(deftest php-parser-namespaced-count-call-does-not-force-global-builtin
  "Unqualified count() inside a namespace must remain fallback-safe, not force %php-count."
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; function count($xs) { return 99; } $n = count($arr);"))
         (call (cdr (first (cl-cc:ast-let-bindings (second asts))))))
    (assert-true (cl-cc:ast-call-p call))
    (assert-string= "COUNT" (%php-call-name call))))

(deftest php-parser-isset-syntax-lowering
  "isset($x) should be lowered without treating $x as a variable reference."
  (let ((ast (%php-first "<?php $result = isset($x);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-call-p call))
      (assert-true (search "ISSET" (%php-call-name call))))))

(deftest php-parser-empty-variable-syntax-lowering
  "empty($x) should avoid evaluating an undefined variable operand."
  (let ((value (%php-first-binding-value "<?php $result = empty($x);")))
    (assert-true (cl-cc:ast-quote-p value))
    (assert-eq t (cl-cc:ast-quote-value value)))
  (let* ((let-x (%php-first "<?php $x = 0; $result = empty($x);"))
         (let-result (first (cl-cc:ast-let-body let-x)))
         (value (cdr (first (cl-cc:ast-let-bindings let-result)))))
    (assert-true (cl-cc:ast-call-p value))
    (assert-true (search "EMPTY" (%php-call-name value)))))

(deftest php-parser-match-strict-comparison
  "match should use strict equality, not EQUAL."
  (let ((ast (%php-first "<?php $x = match($v) { 1 => 'one', 2 => 'two' };")))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (assert-true (cl-cc:ast-let-p val))
      (let ((if-chain (first (cl-cc:ast-let-body val))))
        (assert-true (cl-cc:ast-if-p if-chain))
        (let ((cond (cl-cc:ast-if-cond if-chain)))
          (assert-true (cl-cc:ast-call-p cond))
          (assert-false (string= "EQUAL" (%php-call-name cond))))))))

(deftest php-parser-foreach-ordered-iteration
  "foreach should bind key and value and iterate array order."
  (let ((ast (%php-first "<?php foreach ($arr as $k => $v) { echo $v; }")))
    (assert-true (cl-cc:ast-let-p ast))
    (let ((bindings (cl-cc:ast-let-bindings ast)))
      (assert-= 2 (length bindings)))))

(deftest php-parser-throw-catch-consistency
  "throw inside try should produce catchable exception structure."
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo 'caught'; }")))
    (assert-true (cl-cc:ast-unwind-protect-p ast))
    (assert-true (cl-cc:ast-let-p (cl-cc:ast-unwind-protected ast)))))

(deftest php-parser-match-expression
  "match lowers to a subject let with nested conditional dispatch."
  (let ((value (%php-first-binding-value
                "<?php $result = match($x) { 1 => 'one', 2 => 'two', default => 'other' };")))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))))
    (assert-false (string= "MATCH" (or (%php-call-name value) "")))))

(deftest php-parser-null-coalesce-expression
  "?? lowers to a temp let so the left-hand side is evaluated only once."
  (let ((value (%php-first-binding-value "<?php $result = $a ?? $b;")))
    (assert-true (cl-cc:ast-let-p value))
    (assert-true (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))))))

(deftest php-parser-ternary-expression
  "Characterization: ternary ?: should parse as ast-if with cond/then/else."
  (let ((value (%php-first-binding-value "<?php $result = $cond ? $yes : $no;")))
    (assert-true (cl-cc:ast-if-p value))))

;;; ─── Operator helper lowering ────────────────────────────────────────────


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
