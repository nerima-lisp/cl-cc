;;;; tests/unit/parse/lexer-tests.lisp — CL lexer unit tests
;;;;
;;;; Tests: integers, floats, ratios, strings, symbols, keywords,
;;;; parens, quote macros, hash dispatch, radix, comments, position
;;;; tracking, lex-all for full forms.

(in-package :cl-cc/test)


;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun first-token-type (source)
  "Lex SOURCE and return the type of the first token."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc:lexer-token-type (first tokens))))

(defun first-token-value (source)
  "Lex SOURCE and return the value of the first token."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc:lexer-token-value (first tokens))))

(defun token-types (source)
  "Lex SOURCE and return list of token types (excluding :T-EOF)."
  (let ((tokens (cl-cc:lex-all source)))
    (remove :T-EOF (mapcar #'cl-cc:lexer-token-type tokens))))

;;; ─── Integer Tokens ──────────────────────────────────────────────────────────

(it-sequential "lexer-integer-literals simple"
  (destructuring-bind (source expected-value) (list "42" 42)
    (expect (first-token-type source) :to-be :T-INT) (expect (= expected-value (first-token-value source)) :to-be-truthy)))

(it-sequential "lexer-integer-literals zero"
  (destructuring-bind (source expected-value) (list "0" 0)
    (expect (first-token-type source) :to-be :T-INT) (expect (= expected-value (first-token-value source)) :to-be-truthy)))

(it-sequential "lexer-integer-literals negative"
  (destructuring-bind (source expected-value) (list "-7" -7)
    (expect (first-token-type source) :to-be :T-INT) (expect (= expected-value (first-token-value source)) :to-be-truthy)))

;;; ─── Float Tokens ────────────────────────────────────────────────────────────

(it-sequential "lexer-float-simple-type-and-value"
  (expect (first-token-type "3.14") :to-be :T-FLOAT)
  (let ((val (first-token-value "3.14")))
    (expect (< (abs (- val 3.14d0)) 0.001) :to-be-truthy)))

(it-sequential "lexer-float-exponent-notation-is-float"
  (expect (first-token-type "1.0e5") :to-be :T-FLOAT))

;;; ─── Ratio Tokens ────────────────────────────────────────────────────────────

(it-sequential "lexer-ratio"
  (expect (first-token-type "3/4") :to-be :T-RATIO)
  (expect (first-token-value "3/4") :to-be 3/4))

;;; ─── String Tokens ──────────────────────────────────────────────────────────

(it-sequential "lexer-string-type-and-value"
  (expect (first-token-type "\"hello\"") :to-be :T-STRING)
  (expect (first-token-value "\"hello\"") :to-equal "hello"))

(it-sequential "lexer-string-escape-sequences newline"
  (destructuring-bind (input expected-middle) (list "\"a\\nb\"" #\Newline)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-sequences tab"
  (destructuring-bind (input expected-middle) (list "\"a\\tb\"" #\Tab)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-sequences return"
  (destructuring-bind (input expected-middle) (list "\"a\\rb\"" #\Return)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-sequences backslash"
  (destructuring-bind (input expected-middle) (list "\"a\\\\b\"" #\\)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-sequences nul"
  (destructuring-bind (input expected-middle) (list "\"a\\0b\"" #\Nul)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-sequences unknown"
  (destructuring-bind (input expected-middle) (list "\"a\\xb\"" #\x)
    (let ((val (first-token-value input)))
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (char= (char val 1) expected-middle) :to-be-truthy))))

(it-sequential "lexer-string-escape-table-has-six-entries"
  (expect (= 6 (length cl-cc/parse::*lex-string-escape-table*)) :to-be-truthy))

;;; ─── Symbol Tokens ──────────────────────────────────────────────────────────

(it-sequential "lexer-symbol-plain-is-upcased"
  (expect (first-token-type "foo") :to-be :T-IDENT)
  (expect (symbol-name (first-token-value "foo")) :to-equal "FOO"))

(it-sequential "lexer-symbol-pipe-quoted-preserves-case"
  (let ((val (first-token-value "|MixedCase|")))
    (expect (symbol-name val) :to-equal "MixedCase")))

(it-sequential "lexer-bool-tokens true"
  (destructuring-bind (source expected-type) (list "t" :T-BOOL-TRUE)
    (expect (first-token-type source) :to-be expected-type)))

(it-sequential "lexer-bool-tokens false"
  (destructuring-bind (source expected-type) (list "nil" :T-BOOL-FALSE)
    (expect (first-token-type source) :to-be expected-type)))

;;; ─── Keyword Tokens ─────────────────────────────────────────────────────────

(it-sequential "lexer-keyword"
  (expect (first-token-type ":foo") :to-be :T-KEYWORD)
  (expect (first-token-value ":foo") :to-be :FOO))

;;; ─── Parens ─────────────────────────────────────────────────────────────────

(it-sequential "lexer-parens"
  (let ((types (token-types "()")))
    (expect types :to-equal '(:T-LPAREN :T-RPAREN))))

;;; ─── Quote Macros ───────────────────────────────────────────────────────────

(it-sequential "lexer-quote-macros quote"
  (destructuring-bind (input expected-token-type) (list "'x" :T-QUOTE)
    (expect (first-token-type input) :to-be expected-token-type)))

(it-sequential "lexer-quote-macros backquote"
  (destructuring-bind (input expected-token-type) (list "`x" :T-BACKQUOTE)
    (expect (first-token-type input) :to-be expected-token-type)))

(it-sequential "lexer-quote-macros unquote"
  (destructuring-bind (input expected-token-type) (list ",x" :T-UNQUOTE)
    (expect (first-token-type input) :to-be expected-token-type)))

(it-sequential "lexer-quote-macros splice"
  (destructuring-bind (input expected-token-type) (list ",@x" :T-UNQUOTE-SPLICING)
    (expect (first-token-type input) :to-be expected-token-type)))

;;; ─── Hash Dispatch ──────────────────────────────────────────────────────────

(it-sequential "lexer-hash-function-shorthand"
  (expect (first-token-type "#'foo") :to-be :T-FUNCTION))

(it-sequential "lexer-hash-vector-start"
  (expect (first-token-type "#(1 2)") :to-be :T-VECTOR-START))

(it-sequential "lexer-hash-char-dispatch letter"
  (destructuring-bind (source expected-char) (list "#\\a" #\a)
    (expect (first-token-type source) :to-be :T-CHAR) (expect (char= expected-char (first-token-value source)) :to-be-truthy)))

(it-sequential "lexer-hash-char-dispatch space"
  (destructuring-bind (source expected-char) (list "#\\Space" #\Space)
    (expect (first-token-type source) :to-be :T-CHAR) (expect (char= expected-char (first-token-value source)) :to-be-truthy)))


;;; ─── Radix Dispatch ─────────────────────────────────────────────────────────

(it-sequential "lexer-radix-dispatch binary"
  (destructuring-bind (input expected-value) (list "#b101" 5)
    (expect (= expected-value (first-token-value input)) :to-be-truthy)))

(it-sequential "lexer-radix-dispatch octal"
  (destructuring-bind (input expected-value) (list "#o10" 8)
    (expect (= expected-value (first-token-value input)) :to-be-truthy)))

(it-sequential "lexer-radix-dispatch hex"
  (destructuring-bind (input expected-value) (list "#xFF" 255)
    (expect (= expected-value (first-token-value input)) :to-be-truthy)))

;;; ─── Comments ───────────────────────────────────────────────────────────────

(it-sequential "lexer-comment-forms line"
  (destructuring-bind (source) (list (format nil "; comment~%42"))
    (expect (token-types source) :to-equal '(:T-INT))))

(it-sequential "lexer-comment-forms block"
  (destructuring-bind (source) (list "#| block |# 99")
    (expect (token-types source) :to-equal '(:T-INT))))

;;; ─── Position Tracking ──────────────────────────────────────────────────────

(it-sequential "lexer-position-start-byte"
  (let* ((tokens (cl-cc:lex-all "  42"))
         (tok (first tokens)))
    (expect (= 2 (cl-cc:lexer-token-start-byte tok)) :to-be-truthy)))

;;; ─── Dot Token ──────────────────────────────────────────────────────────────

(it-sequential "lexer-dot"
  (let ((types (token-types "(a . b)")))
    (expect types :to-equal '(:T-LPAREN :T-IDENT :T-DOT :T-IDENT :T-RPAREN))))

;;; ─── lex-all Full Forms ─────────────────────────────────────────────────────

(it-sequential "lexer-full-form-token-types simple-list"
  (destructuring-bind (source expected-types) (list "(+ 1 2)" '(:T-LPAREN :T-IDENT :T-INT :T-INT :T-RPAREN))
    (expect (token-types source) :to-equal expected-types)))

(it-sequential "lexer-full-form-token-types quoted-list"
  (destructuring-bind (source expected-types) (list "'(a b)" '(:T-QUOTE :T-LPAREN :T-IDENT :T-IDENT :T-RPAREN))
    (expect (token-types source) :to-equal expected-types)))

(it-sequential "lexer-full-form-defun"
  (let ((types (token-types "(defun f (x) x)")))
    (expect (= 8 (length types)) :to-be-truthy)
    (expect (first types) :to-be :T-LPAREN)
    (expect (car (last types)) :to-be :T-RPAREN)))

(it-sequential "lexer-eof-always-present non-empty"
  (destructuring-bind (source expected-count) (list "42" nil)
    (let ((tokens (cl-cc:lex-all source)))
    (expect (cl-cc:lexer-token-type (car (last tokens))) :to-be :T-EOF)
    (when expected-count
      (expect (= expected-count (length tokens)) :to-be-truthy)))))

(it-sequential "lexer-eof-always-present empty"
  (destructuring-bind (source expected-count) (list "" 1)
    (let ((tokens (cl-cc:lex-all source)))
    (expect (cl-cc:lexer-token-type (car (last tokens))) :to-be :T-EOF)
    (when expected-count
      (expect (= expected-count (length tokens)) :to-be-truthy)))))
