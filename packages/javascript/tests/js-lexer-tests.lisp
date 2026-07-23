;;;; packages/javascript/tests/js-lexer-tests.lisp — ES2026 JavaScript Lexer Tests
;;;;
;;;; Token format: (:type :T-XXX :value val)

(in-package :cl-cc/test)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %js-lex (src)
  "Tokenize SRC and return the token list (including :T-EOF)."
  (cl-cc/javascript:tokenize-js-source src))

(defun %js-lex-types (src)
  "Return only the :type field of each token produced from SRC."
  (mapcar (lambda (tok) (getf tok :type)) (%js-lex src)))

(defun %js-lex-values (src)
  "Return only the :value field of each token produced from SRC."
  (mapcar (lambda (tok) (getf tok :value)) (%js-lex src)))

(defun %js-first-token (src)
  "Return the first token produced from SRC."
  (first (%js-lex src)))

(defun %js-first-type (src)
  "Return the :type of the first token produced from SRC."
  (getf (%js-first-token src) :type))

(defun %js-first-value (src)
  "Return the :value of the first token produced from SRC."
  (getf (%js-first-token src) :value))

;;; ─── Numeric literals ─────────────────────────────────────────────────────────

(it-sequential "lex-integer-literal decimal-42"
  (destructuring-bind (src expected) (list "42" 42)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal decimal-0"
  (destructuring-bind (src expected) (list "0" 0)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal decimal-255"
  (destructuring-bind (src expected) (list "255" 255)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal hex-0xff"
  (destructuring-bind (src expected) (list "0xFF" 255)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal octal-0o777"
  (destructuring-bind (src expected) (list "0o777" 511)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal binary-0b1010"
  (destructuring-bind (src expected) (list "0b1010" 10)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-integer-literal separator"
  (destructuring-bind (src expected) (list "1_000_000" 1000000)
    (expect (%js-first-type src) :to-be :T-NUMBER) (expect (= expected (%js-first-value src)) :to-be-truthy)))

(it-sequential "lex-float-pi"
  (expect (%js-first-type "3.14") :to-be :T-NUMBER)
  (expect (typep (%js-first-value "3.14") 'double-float) :to-be-truthy)
  (expect (< (abs (- 3.14d0 (%js-first-value "3.14"))) 1.0d-10) :to-be-truthy))

(it-sequential "lex-float-scientific"
  (expect (%js-first-type "1.5e-3") :to-be :T-NUMBER)
  (expect (< (abs (- 1.5d-3 (%js-first-value "1.5e-3"))) 1.0d-15) :to-be-truthy))

(it-sequential "lex-bigint-literal"
  (expect (%js-first-type "42n") :to-be :T-BIGINT)
  (expect (= 42 (%js-first-value "42n")) :to-be-truthy))

;;; ─── String literals ──────────────────────────────────────────────────────────

(it-sequential "lex-string-literal single"
  (destructuring-bind (src expected) (list "'hello'" "hello")
    (expect (%js-first-type src) :to-be :T-STRING) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-string-literal double"
  (destructuring-bind (src expected) (list "\"world\"" "world")
    (expect (%js-first-type src) :to-be :T-STRING) (expect (%js-first-value src) :to-equal expected)))

;;; ─── Keyword tokens ───────────────────────────────────────────────────────────

(it-sequential "lex-keyword if"
  (destructuring-bind (src expected-type) (list "if" :T-IF)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword for"
  (destructuring-bind (src expected-type) (list "for" :T-FOR)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword class"
  (destructuring-bind (src expected-type) (list "class" :T-CLASS)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword async"
  (destructuring-bind (src expected-type) (list "async" :T-ASYNC)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword await"
  (destructuring-bind (src expected-type) (list "await" :T-AWAIT)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword using"
  (destructuring-bind (src expected-type) (list "using" :T-USING)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword true"
  (destructuring-bind (src expected-type) (list "true" :T-TRUE)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword false"
  (destructuring-bind (src expected-type) (list "false" :T-FALSE)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword null"
  (destructuring-bind (src expected-type) (list "null" :T-NULL)
    (expect (%js-first-type src) :to-be expected-type)))

(it-sequential "lex-keyword undefined"
  (destructuring-bind (src expected-type) (list "undefined" :T-UNDEFINED)
    (expect (%js-first-type src) :to-be expected-type)))

;;; ─── Identifiers ──────────────────────────────────────────────────────────────

(it-sequential "lex-identifier simple"
  (destructuring-bind (src expected) (list "foo" "foo")
    (expect (%js-first-type src) :to-be :T-IDENT) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-identifier underscore"
  (destructuring-bind (src expected) (list "_bar" "_bar")
    (expect (%js-first-type src) :to-be :T-IDENT) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-identifier dollar"
  (destructuring-bind (src expected) (list "$baz" "$baz")
    (expect (%js-first-type src) :to-be :T-IDENT) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-identifier camel"
  (destructuring-bind (src expected) (list "camelCase" "camelCase")
    (expect (%js-first-type src) :to-be :T-IDENT) (expect (%js-first-value src) :to-equal expected)))

;;; ─── Private identifiers ──────────────────────────────────────────────────────

(it-sequential "lex-private-identifier field"
  (destructuring-bind (src expected) (list "#field" "field")
    (expect (%js-first-type src) :to-be :T-PRIVATE-IDENT) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-private-identifier method"
  (destructuring-bind (src expected) (list "#privateMethod" "privateMethod")
    (expect (%js-first-type src) :to-be :T-PRIVATE-IDENT) (expect (%js-first-value src) :to-equal expected)))

;;; ─── Decorator ────────────────────────────────────────────────────────────────

(it-sequential "lex-decorator-at"
  (let ((types (%js-lex-types "@decorator")))
    (expect (first  types) :to-be :T-AT)
    (expect (second types) :to-be :T-IDENT)))

;;; ─── Multi-character operators ────────────────────────────────────────────────

(it-sequential "lex-operator strict-eq"
  (destructuring-bind (src expected) (list "===" "===")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator strict-neq"
  (destructuring-bind (src expected) (list "!==" "!==")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator nullish"
  (destructuring-bind (src expected) (list "??" "??")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator optional-chain"
  (destructuring-bind (src expected) (list "?." "?.")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator logic-and-asgn"
  (destructuring-bind (src expected) (list "&&=" "&&=")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator logic-or-asgn"
  (destructuring-bind (src expected) (list "||=" "||=")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator nullish-asgn"
  (destructuring-bind (src expected) (list "??=" "??=")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

(it-sequential "lex-operator exp-asgn"
  (destructuring-bind (src expected) (list "**=" "**=")
    (expect (%js-first-type src) :to-be :T-OP) (expect (%js-first-value src) :to-equal expected)))

;;; ─── Special punctuation ──────────────────────────────────────────────────────

(it-sequential "lex-ellipsis"
  (expect (%js-first-type "...") :to-be :T-ELLIPSIS)
  (expect (%js-first-value "...") :to-equal "..."))

(it-sequential "lex-arrow"
  (expect (%js-first-type "=>") :to-be :T-ARROW)
  (expect (%js-first-value "=>") :to-equal "=>"))

;;; ─── Comments ─────────────────────────────────────────────────────────────────

(it-sequential "lex-line-comment-skipped"
  (let ((types (%js-lex-types "// this is a comment")))
    (expect (= 1 (length types)) :to-be-truthy)
    (expect (first types) :to-be :T-EOF)))

(it-sequential "lex-line-comment-newline-consumed"
  (let ((tokens (%js-lex (format nil "// comment~%foo"))))
    (expect (getf (first tokens) :type) :to-be :T-IDENT)
    (expect (getf (first tokens) :value) :to-equal "foo")))

(it-sequential "lex-block-comment-skipped"
  (let ((tokens (%js-lex "/* skip me */ foo")))
    (expect (getf (first tokens) :type) :to-be :T-IDENT)
    (expect (getf (first tokens) :value) :to-equal "foo")))

(it-sequential "lex-block-comment-unterminated"
  (let ((%%signaled1 nil)) (handler-case (progn (%js-lex "/* unterminated")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "lex-hashbang-skipped-at-start"
  (let ((tokens (%js-lex (format nil "#!/usr/bin/env js~%foo"))))
    (expect (getf (first tokens) :type) :to-be :T-IDENT)
    (expect (getf (first tokens) :value) :to-equal "foo")))

(it-sequential "lex-string-escape-branches standard"
  (destructuring-bind (src expected) (list "\\n\\r\\t\\\\\\'\\\"\\0z'" (coerce (list #\Newline #\Return #\Tab #\\ #\' #\" #\Null #\z) 'string))
    (multiple-value-bind (value new-pos)
      (cl-cc/javascript::lex-js-string src 0 #\')
    (expect value :to-equal expected)
    (expect (= (length src) new-pos) :to-be-truthy))))

(it-sequential "lex-string-escape-branches unicode-short"
  (destructuring-bind (src expected) (list "\\u0041'" "A")
    (multiple-value-bind (value new-pos)
      (cl-cc/javascript::lex-js-string src 0 #\')
    (expect value :to-equal expected)
    (expect (= (length src) new-pos) :to-be-truthy))))

(it-sequential "lex-string-escape-branches unicode-braced"
  (destructuring-bind (src expected) (list "\\u{263A}'" (string (code-char #x263A)))
    (multiple-value-bind (value new-pos)
      (cl-cc/javascript::lex-js-string src 0 #\')
    (expect value :to-equal expected)
    (expect (= (length src) new-pos) :to-be-truthy))))

(it-sequential "lex-string-escape-branches hex"
  (destructuring-bind (src expected) (list "\\x41'" "A")
    (multiple-value-bind (value new-pos)
      (cl-cc/javascript::lex-js-string src 0 #\')
    (expect value :to-equal expected)
    (expect (= (length src) new-pos) :to-be-truthy))))

(it-sequential "lex-string-escape-branches normal"
  (destructuring-bind (src expected) (list "abc'" "abc")
    (multiple-value-bind (value new-pos)
      (cl-cc/javascript::lex-js-string src 0 #\')
    (expect value :to-equal expected)
    (expect (= (length src) new-pos) :to-be-truthy))))

(it-sequential "lex-string-error-branches trailing-backslash"
  (destructuring-bind (src) (list "\\")
    (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::lex-js-string src 0 #\')) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))))

(it-sequential "lex-string-error-branches newline"
  (destructuring-bind (src) (list (format nil "line1~%line2'"))
    (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::lex-js-string src 0 #\')) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))))

(it-sequential "lex-string-error-branches unterminated"
  (destructuring-bind (src) (list "abc")
    (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::lex-js-string src 0 #\')) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))))

;;; ─── Multi-token sequence ─────────────────────────────────────────────────────

(it-sequential "lex-const-statement-sequence"
  (expect (%js-lex-types "const x = 42 + y;") :to-equal '(:T-CONST :T-IDENT :T-OP :T-NUMBER :T-OP :T-IDENT :T-SEMI :T-EOF)))

;;; ─── Regex literals ───────────────────────────────────────────────────────────

(it-sequential "lex-regex-pattern with-flags"
  (destructuring-bind (src expected-pattern expected-flags) (list "/ab+c/gi" "ab+c" "gi")
    (let* ((tok (first (%js-lex src)))
         (val (getf tok :value)))
    (expect (getf tok :type) :to-be :T-REGEX)
    (expect (second val) :to-equal expected-pattern)
    (expect (third  val) :to-equal expected-flags))))

(it-sequential "lex-regex-pattern no-flags"
  (destructuring-bind (src expected-pattern expected-flags) (list "/hello/" "hello" "")
    (let* ((tok (first (%js-lex src)))
         (val (getf tok :value)))
    (expect (getf tok :type) :to-be :T-REGEX)
    (expect (second val) :to-equal expected-pattern)
    (expect (third  val) :to-equal expected-flags))))

(it-sequential "lex-regex-pattern char-class"
  (destructuring-bind (src expected-pattern expected-flags) (list "/[a-z]+/" "[a-z]+" "")
    (let* ((tok (first (%js-lex src)))
         (val (getf tok :value)))
    (expect (getf tok :type) :to-be :T-REGEX)
    (expect (second val) :to-equal expected-pattern)
    (expect (third  val) :to-equal expected-flags))))

(it-sequential "lex-regex-pattern all-flags"
  (destructuring-bind (src expected-pattern expected-flags) (list "/x/dgimsuy" "x" "dgimsuy")
    (let* ((tok (first (%js-lex src)))
         (val (getf tok :value)))
    (expect (getf tok :type) :to-be :T-REGEX)
    (expect (second val) :to-equal expected-pattern)
    (expect (third  val) :to-equal expected-flags))))

(it-sequential "lex-division-not-regex"
  (let ((types (%js-lex-types "a / b")))
    (expect (member :T-OP    types) :to-be-truthy)
    (expect (member :T-REGEX types) :to-be-falsy)))

;;; ─── Template literals ───────────────────────────────────────────────────────

(it-sequential "lex-template-simple"
  (let* ((tok (first (%js-lex "`hello`")))
         (parts (getf tok :value)))
    (expect (getf tok :type) :to-be :T-TEMPLATE-PARTS)
    (expect parts :to-equal '("hello"))))

(it-sequential "lex-template-escaped-cook"
  (let* ((tok (first (%js-lex "`a\\n\\r\\t\\\\\\`\\$\\0z`")))
         (parts (getf tok :value))
         (expected (coerce (list #\a #\Newline #\Return #\Tab #\\ #\` #\$ #\Null #\z)
                           'string)))
    (expect (getf tok :type) :to-be :T-TEMPLATE-PARTS)
    (expect parts :to-equal (list expected))))

(it-sequential "lex-template-interpolated"
  (let* ((tok (first (%js-lex "`hi ${name + 1}!`")))
         (parts (getf tok :value))
         (expr-part (second parts))
         (inner (second expr-part))
         (inner-types (mapcar (lambda (tk) (getf tk :type))
                              inner)))
    (expect (getf tok :type) :to-be :T-TEMPLATE-PARTS)
    (expect (first parts) :to-equal "hi ")
    (expect (and (consp expr-part)
                      (eq (first expr-part) :template-expr)) :to-be-truthy)
    (expect (third parts) :to-equal "!")
    (expect inner-types :to-equal '(:T-IDENT :T-OP :T-NUMBER))))

(it-sequential "lex-template-nested-interpolation"
  (let* ((tok (first (%js-lex "`a ${\"{\" + `inner ${x}` + \"}\"} b`")))
         (parts (getf tok :value))
         (inner (second (second parts)))
         (inner-types (mapcar (lambda (tk) (getf tk :type)) inner)))
    (expect (getf tok :type) :to-be :T-TEMPLATE-PARTS)
    (expect (first parts) :to-equal "a ")
    (expect (third parts) :to-equal " b")
    (expect (member :T-TEMPLATE-PARTS inner-types) :to-be-truthy)))

(it-sequential "lex-template-escape-processor newline"
  (destructuring-bind (src pos expected-char expected-pos) (list "n" 0 #\Newline 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor return"
  (destructuring-bind (src pos expected-char expected-pos) (list "r" 0 #\Return 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor tab"
  (destructuring-bind (src pos expected-char expected-pos) (list "t" 0 #\Tab 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor backslash"
  (destructuring-bind (src pos expected-char expected-pos) (list "\\" 0 #\\ 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor backtick"
  (destructuring-bind (src pos expected-char expected-pos) (list "`" 0 #\` 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor dollar"
  (destructuring-bind (src pos expected-char expected-pos) (list "$" 0 #\$ 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor null"
  (destructuring-bind (src pos expected-char expected-pos) (list "0" 0 #\Null 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor unicode-short"
  (destructuring-bind (src pos expected-char expected-pos) (list "u0041" 0 #\A 5)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor unicode-braced"
  (destructuring-bind (src pos expected-char expected-pos) (list "u{1F600}" 0 (code-char #x1F600) 8)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor hex"
  (destructuring-bind (src pos expected-char expected-pos) (list "x41" 0 #\A 3)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-processor fallback"
  (destructuring-bind (src pos expected-char expected-pos) (list "a" 0 #\a 1)
    (multiple-value-bind (ch new-pos)
      (cl-cc/javascript::js-lex-template-escape src pos)
    (expect ch :to-be expected-char)
    (expect (= expected-pos new-pos) :to-be-truthy))))

(it-sequential "lex-template-escape-errors trailing-backslash"
  (destructuring-bind (src) (list "")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors incomplete-unicode"
  (destructuring-bind (src) (list "u")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors empty-braced-unicode"
  (destructuring-bind (src) (list "u{}")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors unterminated-braced-unicode"
  (destructuring-bind (src) (list "u{1F4")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors missing-brace"
  (destructuring-bind (src) (list "u{1F4x")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors out-of-range"
  (destructuring-bind (src) (list "u{110000}")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors short-hex"
  (destructuring-bind (src) (list "x4")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors bad-unicode-digit"
  (destructuring-bind (src) (list "u12x4")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))

(it-sequential "lex-template-escape-errors bad-hex-digit"
  (destructuring-bind (src) (list "xg1")
    (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::js-lex-template-escape src 0)) (error () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy))))
