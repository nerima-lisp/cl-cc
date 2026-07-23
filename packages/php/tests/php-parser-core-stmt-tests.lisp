(in-package :cl-cc/test)

(it-sequential "php-parser-echo-lowers-to-output-write-call"
  (let ((ast (%php-first "<?php echo 42;")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (%php-call-name ast) :to-equal "%PHP-OUTPUT-WRITE")
    (let ((expr (first (cl-cc:ast-call-args ast))))
      (expect (cl-cc:ast-call-p expr) :to-be-truthy)
      (expect (%php-call-name expr) :to-equal "%PHP-CONCAT")
      (expect (typep (first (cl-cc:ast-call-args expr)) 'cl-cc:ast-int) :to-be-truthy))))

(it-sequential "php-parser-reference-assignment-lowers-to-ref-box"
  (let* ((ast (%php-first "<?php $a=1; $b=&$a; echo $b;"))
         (body (cl-cc:ast-let-body ast))
         (box-set (first body))
         (alias-let (second body))
         (echo (first (cl-cc:ast-let-body alias-let)))
         (concat (first (cl-cc:ast-call-args echo)))
         (deref (first (cl-cc:ast-call-args concat))))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (expect (cl-cc:ast-setq-p box-set) :to-be-truthy)
    (expect (%php-call-name (cl-cc:ast-setq-value box-set)) :to-equal "%PHP-MAKE-REF")
    (expect (cl-cc:ast-let-p alias-let) :to-be-truthy)
    (expect (cl-cc:ast-var-p (cdr (first (cl-cc:ast-let-bindings alias-let)))) :to-be-truthy)
    (expect (%php-call-name deref) :to-equal "%PHP-DEREF")))

(it-sequential "php-parser-settype-lowers-first-arg-by-reference"
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
    (expect found-box :to-be-truthy)
    (expect found-writeback :to-be-truthy)))

;;; ─── :return handler → ast-return-from ───────────────────────────────────

(it-sequential "php-parser-return-with-value-lowering"
  (let ((ast (%php-first "<?php return 1;")))
    (expect (typep ast 'cl-cc:ast-return-from) :to-be-truthy)
    (expect (typep (cl-cc:ast-return-from-value ast) 'cl-cc:ast-int) :to-be-truthy)))

(it-sequential "php-parser-bare-return-has-nil-name"
  (let ((ast (%php-first "<?php return;")))
    (expect (typep ast 'cl-cc:ast-return-from) :to-be-truthy)
    (expect (cl-cc:ast-return-from-name ast) :to-be-null)))

;;; ─── Simple statement → AST-type checks ─────────────────────────────────

(it-sequential "php-parser-stmt-ast-type if"
  (destructuring-bind (src pred) (list "<?php if ($x) { echo 1; }" #'cl-cc:ast-if-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-stmt-ast-type while"
  (destructuring-bind (src pred) (list "<?php while ($x) { echo 1; }" #'cl-cc:ast-block-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-stmt-ast-type foreach"
  (destructuring-bind (src pred) (list "<?php foreach ($items as $item) { echo $item; }" #'cl-cc:ast-let-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-stmt-ast-type foreach-kv"
  (destructuring-bind (src pred) (list "<?php foreach ($arr as $k => $v) { echo $v; }" #'cl-cc:ast-let-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

(it-sequential "php-parser-stmt-ast-type function"
  (destructuring-bind (src pred) (list "<?php function greet($name) { return $name; }" #'cl-cc:ast-defun-p)
    (expect (funcall pred (%php-first src)) :to-be-truthy)))

;;; ─── :if handler → ast-if ────────────────────────────────────────────────

(it-sequential "php-parser-if-else-branch-is-ast-progn"
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (expect (ast-if-p ast) :to-be-truthy)
    (expect (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-progn) :to-be-truthy)))

(it-sequential "php-parser-if-no-else-branch-is-nil-quote"
  (let ((ast (%php-first "<?php if ($x) { echo 1; }")))
    (expect (ast-if-p ast) :to-be-truthy)
    (expect (typep (cl-cc:ast-if-else ast) 'cl-cc:ast-quote) :to-be-truthy)))

;;; ─── :for handler → ast-progn wrapping while ─────────────────────────────

(it-sequential "php-parser-for-produces-ast-progn"
  (let ((ast (%php-first "<?php for ($i = 0; $i < 10; $i++) { echo $i; }")))
    (expect (typep ast 'cl-cc:ast-progn) :to-be-truthy)
    ;; $i = 0 introduces a new variable, so php-finish-let-bindings nests the
    ;; while-loop inside that let — one progn form (the let), not two siblings.
    (expect (= 1 (length (cl-cc:ast-progn-forms ast))) :to-be-truthy)
    (expect (typep (first (cl-cc:ast-progn-forms ast)) 'cl-cc:ast-let) :to-be-truthy)))

;;; ─── :function handler → ast-defun ───────────────────────────────────────

(it-sequential "php-parser-function-name-and-params-captured"
  (let ((ast (%php-first "<?php function add($a, $b) { return $a; }")))
    (expect (symbol-name (cl-cc:ast-defun-name ast)) :to-equal "ADD")
    (expect (= 2 (length (cl-cc:ast-defun-params ast))) :to-be-truthy)))

(it-sequential "php-parser-function-no-params-is-nil"
  (let ((ast (%php-first "<?php function noop() { return 0; }")))
    (expect (typep ast 'cl-cc:ast-defun) :to-be-truthy)
    (expect (cl-cc:ast-defun-params ast) :to-be-null)))

;;; ─── :class handler → ast-defclass ───────────────────────────────────────

(it-sequential "php-parser-class-lowering"
  (expect (typep (%php-first "<?php class Dog { }") 'cl-cc:ast-defclass) :to-be-truthy)
  (let ((ast (%php-first "<?php class Cat { }")))
    (expect (symbol-name (cl-cc:ast-defclass-name ast)) :to-equal "CAT")))

(it-sequential "php-parser-class-with-extends"
  (let ((ast (%php-first "<?php class Puppy extends Dog { }")))
    (expect (some (lambda (s) (string= "DOG" (symbol-name s)))
                       (cl-cc:ast-defclass-superclasses ast)) :to-be-truthy)))

(it-sequential "php-parser-class-with-implements"
  (let* ((ast (%php-first "<?php class Box implements IfaceA, IfaceB { }"))
         (names (mapcar #'symbol-name (cl-cc:ast-defclass-superclasses ast))))
    (expect names :to-equal '("IFACEA" "IFACEB"))))

(it-sequential "php-parser-class-with-property"
  (let* ((ast   (%php-first "<?php class Point { public $x; public $y; }"))
         (slots (cl-cc:ast-defclass-slots ast)))
    (expect (= 2 (length slots)) :to-be-truthy)
    (expect (every #'cl-cc:ast-slot-def-p slots) :to-be-truthy)))

(it-sequential "php-parser-bare-defclass-is-rejected"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/php:php-check-supported-forms
     (list (cl-cc:make-ast-defclass)))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

;;; ─── Expression statement (dispatch fallthrough) ─────────────────────────

(it-sequential "php-parser-expression-statement-assign"
  (let ((ast (%php-first "<?php $x = 42;")))
    (expect (or (typep ast 'cl-cc:ast-setq)
                      (typep ast 'cl-cc:ast-let)
                      (typep ast 'cl-cc:ast-call)) :to-be-truthy)))

(it-sequential "php-parser-variable-names-preserve-case"
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
    (expect names :to-equal '("foo" "FOO" "Foo"))
    (expect (= 3 (length (remove-duplicates names :test #'string=))) :to-be-truthy)))

;;; ─── Multiple top-level statements ───────────────────────────────────────

(it-sequential "php-parser-multi-statement-source"
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; echo 2; echo 3;")))
    (expect (= 3 (length asts)) :to-be-truthy)
    (expect (every (lambda (a)
                          (and (cl-cc:ast-call-p a)
                               (string= "%PHP-OUTPUT-WRITE" (%php-call-name a))))
                        asts) :to-be-truthy)))

;;; ─── Characterization tests for unsupported PHP support gaps ───────────────

(it-sequential "php-parser-null-distinct-from-false"
  (let ((null-ast (%php-first "<?php $x = null;"))
        (false-ast (%php-first "<?php $x = false;")))
    (let ((null-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings null-ast)))))
          (false-val (cl-cc:ast-quote-value (cdr (first (cl-cc:ast-let-bindings false-ast))))))
      (expect (eql null-val false-val) :to-be-falsy))))

(it-sequential "php-parser-truthiness-rules"
  (let ((ast (%php-first "<?php if ($x) { echo 1; } else { echo 2; }")))
    (expect (cl-cc:ast-if-p ast) :to-be-truthy)
    (let ((cond (cl-cc:ast-if-cond ast)))
      (expect (cl-cc:ast-call-p cond) :to-be-truthy)
      (expect (%php-call-name cond) :to-equal "%PHP-TRUTHY"))))

(it-sequential "php-parser-variable-case-sensitive"
  (let ((ast (%php-first "<?php $foo = $FOO;")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let* ((bindings (cl-cc:ast-let-bindings ast))
           (lhs (car (first bindings)))
           (rhs (cdr (first bindings))))
      (expect (cl-cc:ast-var-p rhs) :to-be-truthy)
      (expect (string= (symbol-name lhs) (symbol-name (cl-cc:ast-var-name rhs))) :to-be-falsy))))

(it-sequential "php-parser-count-builtin-lowering"
  (let ((ast (%php-first "<?php $n = count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (expect (cl-cc:ast-call-p call) :to-be-truthy)
      (expect (%php-call-name call) :to-equal "%PHP-COUNT"))))

(it-sequential "php-parser-absolute-count-builtin-lowering"
  (let ((ast (%php-first "<?php namespace App\\Lib; $n = \\count($arr);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (expect (cl-cc:ast-call-p call) :to-be-truthy)
      (expect (%php-call-name call) :to-equal "%PHP-COUNT"))))

(it-sequential "php-parser-namespaced-count-call-does-not-force-global-builtin"
  (let* ((asts (cl-cc/php:parse-php-source
                "<?php namespace App\\Lib; function count($xs) { return 99; } $n = count($arr);"))
         (call (cdr (first (cl-cc:ast-let-bindings (second asts))))))
    (expect (cl-cc:ast-call-p call) :to-be-truthy)
    (expect (%php-call-name call) :to-equal "COUNT")))

(it-sequential "php-parser-isset-syntax-lowering"
  (let ((ast (%php-first "<?php $result = isset($x);")))
    (let ((call (cdr (first (cl-cc:ast-let-bindings ast)))))
      (expect (cl-cc:ast-call-p call) :to-be-truthy)
      (expect (search "ISSET" (%php-call-name call)) :to-be-truthy))))

(it-sequential "php-parser-empty-variable-syntax-lowering"
  (let ((value (%php-first-binding-value "<?php $result = empty($x);")))
    (expect (cl-cc:ast-quote-p value) :to-be-truthy)
    (expect (cl-cc:ast-quote-value value) :to-be t))
  (let* ((let-x (%php-first "<?php $x = 0; $result = empty($x);"))
         (let-result (first (cl-cc:ast-let-body let-x)))
         (value (cdr (first (cl-cc:ast-let-bindings let-result)))))
    (expect (cl-cc:ast-call-p value) :to-be-truthy)
    (expect (search "EMPTY" (%php-call-name value)) :to-be-truthy)))

(it-sequential "php-parser-match-strict-comparison"
  (let ((ast (%php-first "<?php $x = match($v) { 1 => 'one', 2 => 'two' };")))
    (let ((val (cdr (first (cl-cc:ast-let-bindings ast)))))
      (expect (cl-cc:ast-let-p val) :to-be-truthy)
      (let ((if-chain (first (cl-cc:ast-let-body val))))
        (expect (cl-cc:ast-if-p if-chain) :to-be-truthy)
        (let ((cond (cl-cc:ast-if-cond if-chain)))
          (expect (cl-cc:ast-call-p cond) :to-be-truthy)
          (expect (string= "EQUAL" (%php-call-name cond)) :to-be-falsy))))))

(it-sequential "php-parser-foreach-ordered-iteration"
  (let ((ast (%php-first "<?php foreach ($arr as $k => $v) { echo $v; }")))
    (expect (cl-cc:ast-let-p ast) :to-be-truthy)
    (let ((bindings (cl-cc:ast-let-bindings ast)))
      (expect (= 2 (length bindings)) :to-be-truthy))))

(it-sequential "php-parser-throw-catch-consistency"
  (let ((ast (%php-first "<?php try { throw new Ex(); } catch (Ex $e) { echo 'caught'; }")))
    (expect (cl-cc:ast-unwind-protect-p ast) :to-be-truthy)
    (expect (cl-cc:ast-let-p (cl-cc:ast-unwind-protected ast)) :to-be-truthy)))

(it-sequential "php-parser-match-expression"
  (let ((value (%php-first-binding-value
                "<?php $result = match($x) { 1 => 'one', 2 => 'two', default => 'other' };")))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))) :to-be-truthy)
    (expect (string= "MATCH" (or (%php-call-name value) "")) :to-be-falsy)))

(it-sequential "php-parser-null-coalesce-expression"
  (let ((value (%php-first-binding-value "<?php $result = $a ?? $b;")))
    (expect (cl-cc:ast-let-p value) :to-be-truthy)
    (expect (cl-cc:ast-if-p (first (cl-cc:ast-let-body value))) :to-be-truthy)))

(it-sequential "php-parser-ternary-expression"
  (let ((value (%php-first-binding-value "<?php $result = $cond ? $yes : $no;")))
    (expect (cl-cc:ast-if-p value) :to-be-truthy)))

;;; ─── Operator helper lowering ────────────────────────────────────────────


(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
