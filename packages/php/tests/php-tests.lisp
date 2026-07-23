;;;; tests/unit/frontend/php-tests.lisp
;;;; Unit tests for PHP frontend: lexer and parser

(in-package :cl-cc/test)


;;; Lexer helpers (local, not imported from cl-cc)

(defun php-tok-type  (tok) (getf tok :type))
(defun php-tok-value (tok) (getf tok :value))

;;; ─── Lexer Tests ────────────────────────────────────────────────────────────

(it-sequential "php-lex-empty-source-produces-eof"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php ")))
    (expect (= 1 (length tokens)) :to-be-truthy)
    (expect (php-tok-type (first tokens)) :to-be :T-EOF)))

(it-sequential "php-lex-integer-literal"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php 42;")))
    (expect (php-tok-type (first tokens)) :to-be :T-INT)
    (expect (= 42 (php-tok-value (first tokens))) :to-be-truthy)))

(it-sequential "php-lex-float-literal"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php 3.14;")))
    (expect (php-tok-type (first tokens)) :to-be :T-FLOAT)
    (expect (< (abs (- 3.14 (php-tok-value (first tokens)))) 1e-6) :to-be-truthy)))

(it-sequential "php-lex-string-literals double-quote"
  (destructuring-bind (source expected-val) (list "<?php \"hello\";" "hello")
    (let ((tok (first (cl-cc/php:tokenize-php-source source))))
    (expect (php-tok-type tok) :to-be :T-STRING)
    (expect (php-tok-value tok) :to-equal expected-val))))

(it-sequential "php-lex-string-literals single-quote"
  (destructuring-bind (source expected-val) (list "<?php 'world';" "world")
    (let ((tok (first (cl-cc/php:tokenize-php-source source))))
    (expect (php-tok-type tok) :to-be :T-STRING)
    (expect (php-tok-value tok) :to-equal expected-val))))

(it-sequential "php-lex-braced-string-interpolation curly-variable"
  (destructuring-bind (source expected-var) (list "<?php \"Hello {$name}\";" "$name")
    (let* ((tok (first (cl-cc/php:tokenize-php-source source)))
         (value (php-tok-value tok)))
    (expect (php-tok-type tok) :to-be :T-STRING)
    (expect (first value) :to-be :php-interpolated-string)
    (expect (second (second (second value))) :to-equal expected-var))))

(it-sequential "php-lex-braced-string-interpolation dollar-curly"
  (destructuring-bind (source expected-var) (list "<?php \"Hello ${name}\";" "$name")
    (let* ((tok (first (cl-cc/php:tokenize-php-source source)))
         (value (php-tok-value tok)))
    (expect (php-tok-type tok) :to-be :T-STRING)
    (expect (first value) :to-be :php-interpolated-string)
    (expect (second (second (second value))) :to-equal expected-var))))

(it-sequential "php-lex-variable-produces-t-var"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php $x;")))
    (expect (php-tok-type (first tokens)) :to-be :T-VAR)
    (expect (php-tok-value (first tokens)) :to-equal "$x")))

(it-sequential "php-lex-type-keyword-produces-t-type"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php int")))
    (expect (php-tok-type (first tokens)) :to-be :T-TYPE)
    (expect (php-tok-value (first tokens)) :to-be :int)))

(it-sequential "php-lex-keywords if"
  (destructuring-bind (source expected-kw) (list "<?php if" :if)
    (let ((tok (first (cl-cc/php:tokenize-php-source source))))
    (expect (php-tok-type tok) :to-be :T-KEYWORD)
    (expect (php-tok-value tok) :to-be expected-kw))))

(it-sequential "php-lex-keywords function"
  (destructuring-bind (source expected-kw) (list "<?php function" :function)
    (let ((tok (first (cl-cc/php:tokenize-php-source source))))
    (expect (php-tok-type tok) :to-be :T-KEYWORD)
    (expect (php-tok-value tok) :to-be expected-kw))))

(it-sequential "php-lex-punctuation arrow"
  (destructuring-bind (source expected-type) (list "<?php ->" :T-ARROW)
    (expect (php-tok-type (first (cl-cc/php:tokenize-php-source source))) :to-be expected-type)))

(it-sequential "php-lex-punctuation nullsafe-arrow"
  (destructuring-bind (source expected-type) (list "<?php ?->" :T-NULLSAFE-ARROW)
    (expect (php-tok-type (first (cl-cc/php:tokenize-php-source source))) :to-be expected-type)))

(it-sequential "php-lex-punctuation semicolon"
  (destructuring-bind (source expected-type) (list "<?php ;" :T-SEMI)
    (expect (php-tok-type (first (cl-cc/php:tokenize-php-source source))) :to-be expected-type)))

(it-sequential "php-lex-line-comment-is-skipped"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php // this is a comment
42;")))
    (expect (php-tok-type (first tokens)) :to-be :T-INT)
    (expect (= 42 (php-tok-value (first tokens))) :to-be-truthy)))

(it-sequential "php-lex-arithmetic-operators-are-t-op"
  (let ((tokens (cl-cc/php:tokenize-php-source "<?php + - * /")))
    (expect (every (lambda (tok) (eq :T-OP (php-tok-type tok)))
                        (butlast tokens)) :to-be-truthy)))  ; exclude :T-EOF

(it-sequential "php-lex-php85-pipe-and-void-cast-tokens"
  (let ((pipe-tokens (cl-cc/php:tokenize-php-source "<?php $value |> trim(...);"))
        (void-tokens (cl-cc/php:tokenize-php-source "<?php (void) $value;")))
    (let ((pipe-token (find "|>" pipe-tokens :key #'php-tok-value :test #'equal)))
      (expect pipe-token :to-be-truthy)
      (expect (php-tok-type pipe-token) :to-be :T-OP))
    (expect (php-tok-type (first void-tokens)) :to-be :T-LPAREN)
    (expect (php-tok-type (second void-tokens)) :to-be :T-TYPE)
    (expect (php-tok-value (second void-tokens)) :to-be :void)
    (expect (php-tok-type (third void-tokens)) :to-be :T-RPAREN)))

(it-sequential "php-parser-expression-gap-operator-tokens"
  (let ((tokens (cl-cc/php:tokenize-php-source
                 "<?php $a ?? $b; $c ? $d : $e; fn($x) => $x;")))
    (expect (find "??" tokens :key #'php-tok-value :test #'equal) :to-be-truthy)
    (expect (find "?" tokens :key #'php-tok-value :test #'equal) :to-be-truthy)
    (expect (find "=>" tokens :key #'php-tok-value :test #'equal) :to-be-truthy)
    (expect (find :fn tokens :key #'php-tok-value :test #'eq) :to-be-truthy)))

(it-sequential "php-parser-array-gap-lexer-tokens"
  (let ((tokens (cl-cc/php:tokenize-php-source
                 "<?php [1,2,3]; [\"a\"=>1,\"b\"=>2]; array(1,2,3);")))
    (expect (find :T-LBRACKET tokens :key #'php-tok-type :test #'eq) :to-be-truthy)
    (expect (find "=>" tokens :key #'php-tok-value :test #'equal) :to-be-truthy)
    (expect (find :array tokens :key #'php-tok-value :test #'eq) :to-be-truthy)))

;;; ─── Parser Tests ───────────────────────────────────────────────────────────

(it-sequential "php-parse-single-forms"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php 42;"))))
    (expect (ast-int-p ast) :to-be-truthy)
    (expect (= 42 (ast-int-value ast)) :to-be-truthy))
  (let ((ast (first (cl-cc/php:parse-php-source "<?php echo 42;"))))
    ;; echo lowers to a PRINC call (no trailing newline) wrapping a %php-concat
    ;; call (PHP string conversion); the echoed int is the concat's first arg.
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (let ((concat (first (cl-cc:ast-call-args ast))))
      (expect (cl-cc:ast-call-p concat) :to-be-truthy)
      (expect (ast-int-p (first (cl-cc:ast-call-args concat))) :to-be-truthy)))
  (let ((ast (first (cl-cc/php:parse-php-source "<?php $x = 42;"))))
    (expect (or (ast-let-p ast) (ast-setq-p ast)) :to-be-truthy))
  (let ((ast (first (cl-cc/php:parse-php-source "<?php if ($x) { echo 1; }"))))
    (expect (ast-if-p ast) :to-be-truthy)))

(it-sequential "php-parse-function-produces-ast-defun"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php function f($x) { return $x; }"))))
    (expect (ast-defun-p ast) :to-be-truthy)
    (expect (symbol-name (ast-defun-name ast)) :to-equal "F")))

(it-sequential "php-parse-class-produces-ast-defclass"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php class Foo {}"))))
    (expect (ast-defclass-p ast) :to-be-truthy)
    (expect (symbol-name (ast-defclass-name ast)) :to-equal "FOO")))

(it-sequential "php-parse-multiple-statements"
  (let ((asts (cl-cc/php:parse-php-source "<?php echo 1; echo 2;")))
    (expect (= 2 (length asts)) :to-be-truthy)))

(it-sequential "php-parse-binary-op-produces-ast-binop-or-call"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php $a + $b;"))))
    (expect (or (ast-binop-p ast) (ast-call-p ast)) :to-be-truthy)))

(it-sequential "php-parse-string-literal-produces-ast-quote"
  (let ((ast (first (cl-cc/php:parse-php-source "<?php \"hello\";"))))
    (expect (ast-quote-p ast) :to-be-truthy)
    (expect (ast-quote-value ast) :to-equal "hello")))

(eval-when (:load-toplevel :execute)
  (%run-registered-tests-from-source-file
   (or *compile-file-pathname* *load-pathname*)))
