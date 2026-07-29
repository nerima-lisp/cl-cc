;;;; tests/unit/parse/lexer-dispatch-tests.lisp
;;;; Unit tests for src/parse/lexer-dispatch.lisp
;;;;
;;;; Covers:
;;;;   Skip helpers    — %lex-skip-string-chars, %lex-skip-list-body, %lex-skip-atom
;;;;                     (via lex-read-form-text and lex-skip-form)
;;;;   Feature logic   — lex-feature-present-p (pure boolean combinators)
;;;;   Hash dispatch   — #b/#o/#x radix, #' function, #\\ char, #( vector,
;;;;                     #* bit-vector, #+ / #- feature conditionals, #| block comment
;;;;   lex-read-form-text — reads balanced forms as raw strings

(in-package :cl-cc/test)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun lexer-dispatch-lex-types (source)
  "Tokenize SOURCE and return list of token types (excluding :T-EOF)."
  (remove :t-eof (mapcar #'cl-cc:lexer-token-type (cl-cc:lex-all source))))

(defun lexer-dispatch-first-value (source)
  "Tokenize SOURCE and return the value of the first non-EOF token."
  (cl-cc:lexer-token-value (first (cl-cc:lex-all source))))

(defun lexer-dispatch-lex-types-with-features (features source)
  "Tokenize SOURCE with FEATURES bound in *FEATURES* and return token types."
  (let ((*features* features))
    (lexer-dispatch-lex-types source)))

(defun lexer-dispatch-first-value-with-features (features source)
  "Tokenize SOURCE with FEATURES bound in *FEATURES* and return the first value."
  (let ((*features* features))
    (lexer-dispatch-first-value source)))

(defun lexer-dispatch-read-form-text (source)
  "Use internal lex-read-form-text to extract the raw form text from SOURCE."
  (let ((state (cl-cc/parse:make-lexer source)))
    (cl-cc/parse::lex-read-form-text state)))

;;; ─── lex-feature-present-p ───────────────────────────────────────────────────

(it-sequential "lex-feature-present-p-keyword-lookup present"
  (destructuring-bind (feature expected) (list :cl-cc-present t)
    (let ((*features* '(:cl-cc-present :common-lisp)))
    (assert-parse-boolean-case expected
        (cl-cc/parse::lex-feature-present-p feature)))))

(it-sequential "lex-feature-present-p-keyword-lookup absent"
  (destructuring-bind (feature expected) (list :no-such-feature nil)
    (let ((*features* '(:cl-cc-present :common-lisp)))
    (assert-parse-boolean-case expected
        (cl-cc/parse::lex-feature-present-p feature)))))

(it-sequential "lex-feature-present-p-or-any-present"
  (let ((*features* '(:cl-cc-present)))
      (expect (cl-cc/parse::lex-feature-present-p '(:or :cl-cc-present :ccl)) :to-be-truthy)
      (expect (cl-cc/parse::lex-feature-present-p '(:or :ccl :ecl)) :to-be-null)))

(it-sequential "lex-feature-present-p-and-all-required"
  (let ((*features* '(:cl-cc-present :common-lisp)))
      (expect (cl-cc/parse::lex-feature-present-p '(:and :cl-cc-present :common-lisp)) :to-be-truthy)
      (expect (cl-cc/parse::lex-feature-present-p '(:and :cl-cc-present :ccl)) :to-be-null)))

(it-sequential "lex-feature-present-p-not-negates"
  (let ((*features* '(:cl-cc-present)))
      (expect (cl-cc/parse::lex-feature-present-p '(:not :cl-cc-present)) :to-be-null)
      (expect (cl-cc/parse::lex-feature-present-p '(:not :ccl)) :to-be-truthy)))

(it-sequential "lex-feature-present-p-unknown-returns-nil"
  (expect (cl-cc/parse::lex-feature-present-p 42) :to-be-null)
  (expect (cl-cc/parse::lex-feature-present-p '(:unknown :foo)) :to-be-null))

;;; ─── lex-read-form-text ──────────────────────────────────────────────────────

(it-sequential "lex-read-form-text-cases integer"
  (destructuring-bind (source expected) (list "42" "42")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-cases symbol"
  (destructuring-bind (source expected) (list "foo" "foo")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-cases keyword"
  (destructuring-bind (source expected) (list ":bar" ":bar")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-cases string"
  (destructuring-bind (source expected) (list "\"hello\"" "\"hello\"")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-cases list"
  (destructuring-bind (source expected) (list "(+ 1 2)" "(+ 1 2)")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-cases nested-list"
  (destructuring-bind (source expected) (list "(a (b c) d)" "(a (b c) d)")
    (expect (lexer-dispatch-read-form-text source) :to-equal expected)))

(it-sequential "lex-read-form-text-list-with-string"
  (let ((text (lexer-dispatch-read-form-text "(f \")\")")))
    ;; The ) inside the string must not close the list
    (expect text :to-equal "(f \")\")")))

;;; ─── Hash Dispatch: Radix Integers ──────────────────────────────────────────

(it-sequential "lexer-dispatch-radix-integers binary"
  (destructuring-bind (source expected) (list "#b1010" 10)
    (expect (first (lexer-dispatch-lex-types source)) :to-be :t-int) (expect (= expected (lexer-dispatch-first-value source)) :to-be-truthy)))

(it-sequential "lexer-dispatch-radix-integers octal"
  (destructuring-bind (source expected) (list "#o17" 15)
    (expect (first (lexer-dispatch-lex-types source)) :to-be :t-int) (expect (= expected (lexer-dispatch-first-value source)) :to-be-truthy)))

(it-sequential "lexer-dispatch-radix-integers hex"
  (destructuring-bind (source expected) (list "#xff" 255)
    (expect (first (lexer-dispatch-lex-types source)) :to-be :t-int) (expect (= expected (lexer-dispatch-first-value source)) :to-be-truthy)))

(it-sequential "lexer-dispatch-radix-integers hex-cap"
  (destructuring-bind (source expected) (list "#xFF" 255)
    (expect (first (lexer-dispatch-lex-types source)) :to-be :t-int) (expect (= expected (lexer-dispatch-first-value source)) :to-be-truthy)))

;;; ─── Hash Dispatch: Function Reference ──────────────────────────────────────

(it-sequential "lexer-dispatch-function-reference"
  (let ((types (lexer-dispatch-lex-types "#'foo")))
    (expect types :to-equal '(:t-function :t-ident))))

;;; ─── Hash Dispatch: Bit Vector ───────────────────────────────────────────────

(it-sequential "lexer-dispatch-bit-vector-with-bits"
  (let ((val (lexer-dispatch-first-value "#*101")))
    (expect (bit-vector-p val) :to-be-truthy)
    (expect (= 3 (length val)) :to-be-truthy)
    (expect (= 1 (sbit val 0)) :to-be-truthy)
    (expect (= 0 (sbit val 1)) :to-be-truthy)
    (expect (= 1 (sbit val 2)) :to-be-truthy)))

(it-sequential "lexer-dispatch-bit-vector-empty"
  (let ((val (lexer-dispatch-first-value "#*")))
    (expect (bit-vector-p val) :to-be-truthy)
    (expect (= 0 (length val)) :to-be-truthy)))

;;; ─── Hash Dispatch: Block Comment ───────────────────────────────────────────

(it-sequential "lexer-dispatch-block-comment-before-integer"
  (expect (lexer-dispatch-lex-types "#| this is a comment |# 42") :to-equal '(:t-int)))

(it-sequential "lexer-dispatch-block-comment-before-ident"
  (expect (lexer-dispatch-lex-types "#| comment |# symbol") :to-equal '(:t-ident)))

;;; ─── Hash Dispatch: Feature Conditionals #+/# ──────────────────────────────

(it-sequential "lexer-dispatch-hash-plus-includes-when-feature-present"
  (expect (lexer-dispatch-lex-types-with-features
                 '(:cl-cc-present)
                 "#+cl-cc-present 42") :to-equal '(:t-int)))

(it-sequential "lexer-dispatch-hash-plus-skips-when-feature-absent"
  (expect (lexer-dispatch-lex-types-with-features
                 '()
                 "#+no-such-feature 42 99") :to-equal '(:t-int))
  (expect (= 99 (lexer-dispatch-first-value-with-features
             '()
             "#+no-such-feature 42 99")) :to-be-truthy))

(it-sequential "lexer-dispatch-hash-minus-skips-when-feature-present"
  (expect (lexer-dispatch-lex-types-with-features
                 '(:cl-cc-present)
                 "#-cl-cc-present 42 99") :to-equal '(:t-int))
  (expect (= 99 (lexer-dispatch-first-value-with-features
             '(:cl-cc-present)
             "#-cl-cc-present 42 99")) :to-be-truthy))

(it-sequential "lexer-dispatch-hash-minus-includes-when-feature-absent"
  (expect (lexer-dispatch-lex-types-with-features
                 '()
                 "#-no-such-feature 42") :to-equal '(:t-int)))

;;; ─── Hash Dispatch: Arbitrary Radix #nR ─────────────────────────────────────

(it-sequential "lexer-dispatch-arbitrary-radix"
  (expect (= 255 (lexer-dispatch-first-value "#16rFF")) :to-be-truthy)
  (expect (= 10 (lexer-dispatch-first-value "#2r1010")) :to-be-truthy))

;;; ─── Hash Dispatch: Vector #( ───────────────────────────────────────────────

(it-sequential "lexer-dispatch-vector-start"
  (let ((types (lexer-dispatch-lex-types "#(1 2)")))
    (expect (member :t-vector-start types) :to-be-truthy)))

;;; ─── Skip helpers via feature skip (indirect) ───────────────────────────────

;; For #- skip tests: #-feature skips the form when feature IS PRESENT.
;; Bind *features* to contain :skip-me so #-skip-me actually skips.

(it-sequential "lex-skip-form-cases list"
  (destructuring-bind (source expected) (list "#-skip-me (+ 1 2) 99" 99)
    (expect (= expected (lexer-dispatch-first-value-with-features '(:skip-me) source)) :to-be-truthy)))

(it-sequential "lex-skip-form-cases string"
  (destructuring-bind (source expected) (list "#-skip-me \"hello (world)\" 42" 42)
    (expect (= expected (lexer-dispatch-first-value-with-features '(:skip-me) source)) :to-be-truthy)))

(it-sequential "lex-skip-form-cases atom"
  (destructuring-bind (source expected) (list "#-skip-me some-symbol 77" 77)
    (expect (= expected (lexer-dispatch-first-value-with-features '(:skip-me) source)) :to-be-truthy)))

(it-sequential "lex-skip-list-with-comment"
  (let ((val (lexer-dispatch-first-value-with-features
              '(:skip-me)
              "#-skip-me (foo ; comment
               bar) 55")))
    (expect (= 55 val) :to-be-truthy)))
