;;;; tests/unit/parse/pratt-tests.lisp — Pratt parser engine unit tests
;;;;
;;;; Tests: pratt-context stream ops, NUD dispatch, CL grammar round-trips,
;;;; parse-cl-source CST structure, parse-all-forms s-expression output,
;;;; error recovery via cst-error, and parse-and-lower pipeline.

(in-package :cl-cc/test)


;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-test-ctx (source)
  "Create a generic pratt-context from SOURCE string for parser-engine tests."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc/parse::make-pratt-context
     :tokens tokens
     :source source
     :source-file nil
     :tok-type-fn #'cl-cc:lexer-token-type
     :tok-value-fn #'cl-cc:lexer-token-value
     :tok-start-fn #'cl-cc:lexer-token-start-byte
     :tok-end-fn #'cl-cc:lexer-token-end-byte
     :nud-table (make-hash-table :test #'eq)
     :led-table (make-hash-table :test #'eq))))

(defun parse-one-cst (source)
  "Parse SOURCE and return the first CST node."
  (first (nth-value 0 (cl-cc:parse-cl-source source))))

(defun parse-one-sexp (source)
  "Parse SOURCE and return the first s-expression."
  (first (cl-cc:parse-all-forms source)))

;;; ─── Stream Operations ───────────────────────────────────────────────────────

(it-sequential "pratt-peek-behavior"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc/parse::pratt-peek ctx)))
      (expect (not (null tok)) :to-be-truthy)
      (expect (cl-cc:lexer-token-type tok) :to-be :T-INT)))
  (let ((ctx (make-test-ctx "42")))
    (let ((tok1 (cl-cc/parse::pratt-peek ctx))
          (tok2 (cl-cc/parse::pratt-peek ctx)))
      (expect tok2 :to-be tok1))))

(it-sequential "pratt-advance-consumes-token"
  (let ((ctx (make-test-ctx "1 2")))
    (let ((tok1 (cl-cc/parse::pratt-advance ctx)))
      (expect (cl-cc:lexer-token-type tok1) :to-be :T-INT)
      (expect (= 1 (cl-cc:lexer-token-value tok1)) :to-be-truthy))
    (let ((tok2 (cl-cc/parse::pratt-advance ctx)))
      (expect (cl-cc:lexer-token-type tok2) :to-be :T-INT)
      (expect (= 2 (cl-cc:lexer-token-value tok2)) :to-be-truthy))))

(it-sequential "pratt-at-end-p-states empty-source"
  (destructuring-bind (source expected advance-first) (list "" t nil)
    (let ((ctx (make-test-ctx source)))
    (when advance-first
      (cl-cc/parse::pratt-advance ctx))
    (if expected
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-truthy)
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-falsy)))))

(it-sequential "pratt-at-end-p-states non-empty"
  (destructuring-bind (source expected advance-first) (list "42" nil nil)
    (let ((ctx (make-test-ctx source)))
    (when advance-first
      (cl-cc/parse::pratt-advance ctx))
    (if expected
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-truthy)
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-falsy)))))

(it-sequential "pratt-at-end-p-states after-advance"
  (destructuring-bind (source expected advance-first) (list "x" t t)
    (let ((ctx (make-test-ctx source)))
    (when advance-first
      (cl-cc/parse::pratt-advance ctx))
    (if expected
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-truthy)
        (expect (cl-cc/parse::pratt-at-end-p ctx) :to-be-falsy)))))

;;; ─── Token Accessors ─────────────────────────────────────────────────────────

(it-sequential "pratt-tok-type-via-context"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc/parse::pratt-peek ctx)))
    (expect (cl-cc/parse::pratt-tok-type ctx tok) :to-be :T-IDENT)))

(it-sequential "pratt-tok-value-via-context"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc/parse::pratt-peek ctx)))
    (expect (string (cl-cc/parse::pratt-tok-value ctx tok)) :to-equal "HELLO")))

(it-sequential "pratt-tok-type-nil-safe"
  (let ((ctx (make-test-ctx "")))
    (expect (cl-cc/parse::pratt-tok-type ctx nil) :to-be nil)))

;;; ─── Diagnostics: pratt-expect ───────────────────────────────────────────────

(it-sequential "pratt-expect-matching-type"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc/parse::pratt-expect ctx :T-INT "test")))
      (expect (not (null tok)) :to-be-truthy)
      (expect (cl-cc:lexer-token-type tok) :to-be :T-INT))))

(it-sequential "pratt-expect-adds-diagnostic-on-failure type-mismatch"
  (destructuring-bind (source expected-type label) (list "42" :T-STRING "test")
    (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-expect ctx expected-type label)
    (expect (not (null (cl-cc/parse::pratt-context-diagnostics ctx))) :to-be-truthy))))

(it-sequential "pratt-expect-adds-diagnostic-on-failure eof"
  (destructuring-bind (source expected-type label) (list "" :T-INT "eof-test")
    (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-expect ctx expected-type label)
    (expect (not (null (cl-cc/parse::pratt-context-diagnostics ctx))) :to-be-truthy))))

(it-sequential "pratt-expect-mismatch-returns-nil"
  (let ((ctx (make-test-ctx "42")))
    (let ((result (cl-cc/parse::pratt-expect ctx :T-STRING)))
      (expect result :to-be nil))))

;;; ─── parse-cl-source: CST Structure ─────────────────────────────────────────

(it-sequential "parse-literal-kind integer"
  (destructuring-bind (source expected-kind expected-num expected-str) (list "42" :T-INT 42 nil)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (when expected-num
      (expect (= expected-num (cl-cc:cst-token-value node)) :to-be-truthy))
    (when expected-str
      (expect (cl-cc:cst-token-value node) :to-equal expected-str)))))

(it-sequential "parse-literal-kind float"
  (destructuring-bind (source expected-kind expected-num expected-str) (list "3.14" :T-FLOAT nil nil)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (when expected-num
      (expect (= expected-num (cl-cc:cst-token-value node)) :to-be-truthy))
    (when expected-str
      (expect (cl-cc:cst-token-value node) :to-equal expected-str)))))

(it-sequential "parse-literal-kind symbol"
  (destructuring-bind (source expected-kind expected-num expected-str) (list "foo" :T-IDENT nil nil)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (when expected-num
      (expect (= expected-num (cl-cc:cst-token-value node)) :to-be-truthy))
    (when expected-str
      (expect (cl-cc:cst-token-value node) :to-equal expected-str)))))

(it-sequential "parse-literal-kind keyword"
  (destructuring-bind (source expected-kind expected-num expected-str) (list ":foo" :T-KEYWORD nil nil)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (when expected-num
      (expect (= expected-num (cl-cc:cst-token-value node)) :to-be-truthy))
    (when expected-str
      (expect (cl-cc:cst-token-value node) :to-equal expected-str)))))

(it-sequential "parse-literal-kind string"
  (destructuring-bind (source expected-kind expected-num expected-str) (list "\"hello\"" :T-STRING nil "hello")
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (when expected-num
      (expect (= expected-num (cl-cc:cst-token-value node)) :to-be-truthy))
    (when expected-str
      (expect (cl-cc:cst-token-value node) :to-equal expected-str)))))

(it-sequential "parse-empty-list-produces-interior"
  (let ((node (parse-one-cst "()")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be :list)
    (expect (= 0 (length (cl-cc:cst-children node))) :to-be-truthy)))

(it-sequential "parse-simple-list-produces-children"
  (let ((node (parse-one-cst "(a b c)")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (= 3 (length (cl-cc:cst-children node))) :to-be-truthy)))

(it-sequential "parse-nested-list"
  (let ((node (parse-one-cst "(a (b c))")))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (= 2 (length (cl-cc:cst-children node))) :to-be-truthy)
    (let ((inner (second (cl-cc:cst-children node))))
      (expect (cl-cc:cst-interior-p inner) :to-be-truthy)
      (expect (= 2 (length (cl-cc:cst-children inner))) :to-be-truthy))))

(it-sequential "parse-dispatch-interior-kind quote"
  (destructuring-bind (source expected-kind) (list "'x" :quote)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-dispatch-interior-kind quasiquote"
  (destructuring-bind (source expected-kind) (list "`x" :quasiquote)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-dispatch-interior-kind function"
  (destructuring-bind (source expected-kind) (list "#'foo" :function)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "parse-dispatch-interior-kind vector"
  (destructuring-bind (source expected-kind) (list "#(1 2 3)" :vector)
    (let ((node (parse-one-cst source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))
