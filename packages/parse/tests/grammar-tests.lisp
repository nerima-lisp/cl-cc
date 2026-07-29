;;;; tests/unit/parse/grammar-tests.lisp — CL grammar parser unit tests
;;;;
;;;; Tests: token-stream operations, parse-cl-atom, parse-cl-form via
;;;; parse-cl-source, multi-form parsing, error recovery, and Pratt bridge.

(in-package :cl-cc/test)


;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun make-grammar-token (type value &key (start 0) (end 1))
  "Create a lexer-token for grammar token-stream tests."
  (cl-cc/parse::make-lexer-token :type type :value value
                           :start-byte start :end-byte end
                           :line 1 :column 0))

(defun make-grammar-ts (&rest token-specs)
  "Create a token-stream from TOKEN-SPECS: each is (type value) or (type value :start s :end e)."
  (let ((tokens (mapcar (lambda (spec)
                          (apply #'make-grammar-token spec))
                        token-specs)))
    (cl-cc/parse::make-token-stream :tokens tokens :source "" :source-file nil)))

(defun parse-first-form (source)
  "Parse SOURCE and return the first CST node."
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (first forms)))

(defun parse-first-kind (source)
  "Parse SOURCE and return the kind of the first CST node."
  (let ((node (parse-first-form source)))
    (when node (cl-cc:cst-node-kind node))))

(defun parse-first-value (source)
  "Parse SOURCE and return the value of the first CST token node."
  (let ((node (parse-first-form source)))
    (when (and node (cl-cc:cst-token-p node))
      (cl-cc:cst-token-value node))))

;;; ─── Token Stream: ts-peek ─────────────────────────────────────────────────

(it-sequential "grammar-ts-peek-non-empty-stream"
  (let* ((ts (make-grammar-ts '(:T-INT 42) '(:T-INT 99)))
         (tok (cl-cc/parse::ts-peek ts)))
    (expect (cl-cc/parse::lexer-token-type tok) :to-be :T-INT)
    (expect (= 42 (cl-cc/parse::lexer-token-value tok)) :to-be-truthy)
    (expect (= 42 (cl-cc/parse::lexer-token-value (cl-cc/parse::ts-peek ts))) :to-be-truthy)))

(it-sequential "grammar-ts-peek-empty-stream-returns-nil"
  (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
    (expect (cl-cc/parse::ts-peek ts) :to-be-null)))

;;; ─── Token Stream: ts-advance ──────────────────────────────────────────────

(it-sequential "grammar-ts-advance-consumes-token"
  (let ((ts (make-grammar-ts '(:T-INT 1) '(:T-INT 2) '(:T-INT 3))))
    (let ((tok1 (cl-cc/parse::ts-advance ts)))
      (expect (= 1 (cl-cc/parse::lexer-token-value tok1)) :to-be-truthy))
    (let ((tok2 (cl-cc/parse::ts-advance ts)))
      (expect (= 2 (cl-cc/parse::lexer-token-value tok2)) :to-be-truthy))
    (let ((tok3 (cl-cc/parse::ts-advance ts)))
      (expect (= 3 (cl-cc/parse::lexer-token-value tok3)) :to-be-truthy))
    ;; Stream now exhausted
    (expect (cl-cc/parse::ts-advance ts) :to-be-null)))

;;; ─── Token Stream: ts-peek-type ────────────────────────────────────────────

(it-sequential "grammar-ts-peek-type-cases non-empty"
  (destructuring-bind (scenario expected) (list :non-empty :T-STRING)
    (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-STRING "hello")))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (expect (cl-cc/parse::ts-peek-type ts) :to-equal expected))))

(it-sequential "grammar-ts-peek-type-cases empty"
  (destructuring-bind (scenario expected) (list :empty nil)
    (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-STRING "hello")))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (expect (cl-cc/parse::ts-peek-type ts) :to-equal expected))))

;;; ─── Token Stream: ts-expect ───────────────────────────────────────────────

(it-sequential "grammar-ts-expect-behavior"
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (tok (cl-cc/parse::ts-expect ts :T-INT)))
    (expect (not (null tok)) :to-be-truthy)
    (expect (= 42 (cl-cc/parse::lexer-token-value tok)) :to-be-truthy)
    ;; Stream is empty after consuming the token
    (expect (cl-cc/parse::ts-peek ts) :to-be-null))
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (result (cl-cc/parse::ts-expect ts :T-STRING)))
    (expect result :to-be-null)
    (expect (not (null (cl-cc/parse::token-stream-diagnostics ts))) :to-be-truthy)
    ;; Token was NOT consumed
    (expect (= 42 (cl-cc/parse::lexer-token-value (cl-cc/parse::ts-peek ts))) :to-be-truthy))
  (let* ((ts (cl-cc/parse::make-token-stream :tokens nil :source ""))
         (result (cl-cc/parse::ts-expect ts :T-RPAREN)))
    (expect result :to-be-null)
    (expect (not (null (cl-cc/parse::token-stream-diagnostics ts))) :to-be-truthy)))

;;; ─── Token Stream: ts-at-end-p ─────────────────────────────────────────────

(it-sequential "grammar-ts-at-end-p-cases empty"
  (destructuring-bind (scenario expected) (list :empty t)
    (let ((ts (ecase scenario
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source ""))
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:eof       (make-grammar-ts '(:T-EOF nil))))))
    (if expected
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-truthy)
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-falsy)))))

(it-sequential "grammar-ts-at-end-p-cases non-empty"
  (destructuring-bind (scenario expected) (list :non-empty nil)
    (let ((ts (ecase scenario
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source ""))
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:eof       (make-grammar-ts '(:T-EOF nil))))))
    (if expected
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-truthy)
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-falsy)))))

(it-sequential "grammar-ts-at-end-p-cases eof-token"
  (destructuring-bind (scenario expected) (list :eof t)
    (let ((ts (ecase scenario
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source ""))
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:eof       (make-grammar-ts '(:T-EOF nil))))))
    (if expected
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-truthy)
        (expect (cl-cc/parse::ts-at-end-p ts) :to-be-falsy)))))

;;; ─── Token Stream: ts-token-value ──────────────────────────────────────────

(it-sequential "grammar-ts-token-value-cases non-empty"
  (destructuring-bind (scenario expected) (list :non-empty 42)
    (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (expect (cl-cc/parse::ts-token-value ts) :to-equal expected))))

(it-sequential "grammar-ts-token-value-cases empty"
  (destructuring-bind (scenario expected) (list :empty nil)
    (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (expect (cl-cc/parse::ts-token-value ts) :to-equal expected))))

;;; ─── parse-cl-atom ─────────────────────────────────────────────────────────

(it-sequential "grammar-parse-cl-atom-value-types integer"
  (destructuring-bind (tok-type tok-value expected) (list :T-INT 42 42)
    (let* ((ts (make-grammar-ts (list tok-type tok-value)))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-token-value node) :to-equal expected))))

(it-sequential "grammar-parse-cl-atom-value-types string"
  (destructuring-bind (tok-type tok-value expected) (list :T-STRING "hello" "hello")
    (let* ((ts (make-grammar-ts (list tok-type tok-value)))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-token-value node) :to-equal expected))))

(it-sequential "grammar-parse-cl-atom-value-types keyword"
  (destructuring-bind (tok-type tok-value expected) (list :T-KEYWORD :foo :foo)
    (let* ((ts (make-grammar-ts (list tok-type tok-value)))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-token-value node) :to-equal expected))))

(it-sequential "grammar-parse-cl-atom-nil-cases non-atom"
  (destructuring-bind (scenario) (list :non-atom)
    (let* ((ts (ecase scenario
               (:non-atom (make-grammar-ts (list :T-LPAREN nil)))
               (:empty    (cl-cc/parse::make-token-stream :tokens nil :source ""))))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (expect node :to-be-null))))

(it-sequential "grammar-parse-cl-atom-nil-cases empty"
  (destructuring-bind (scenario) (list :empty)
    (let* ((ts (ecase scenario
               (:non-atom (make-grammar-ts (list :T-LPAREN nil)))
               (:empty    (cl-cc/parse::make-token-stream :tokens nil :source ""))))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (expect node :to-be-null))))

;;; ─── parse-cl-source: atoms ────────────────────────────────────────────────

(it-sequential "grammar-parse-atomic-tokens integer"
  (destructuring-bind (source expected-value) (list "42" 42)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-token-value node) :to-equal expected-value))))

(it-sequential "grammar-parse-atomic-tokens string"
  (destructuring-bind (source expected-value) (list "\"hello\"" "hello")
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-token-value node) :to-equal expected-value))))

;;; ─── parse-cl-source: quote sugar ──────────────────────────────────────────

(it-sequential "grammar-quote-sugar-forms quote"
  (destructuring-bind (source expected-kind expected-children) (list "'x" :quote 1)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-quote-sugar-forms backquote"
  (destructuring-bind (source expected-kind expected-children) (list "`x" :quasiquote 1)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-quote-sugar-forms unquote"
  (destructuring-bind (source expected-kind expected-children) (list ",x" :unquote 1)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-quote-sugar-forms function"
  (destructuring-bind (source expected-kind expected-children) (list "#'foo" :function 1)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-quote-sugar-forms vector"
  (destructuring-bind (source expected-kind expected-children) (list "#(1 2 3)" :vector 3)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind)
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

;;; ─── parse-cl-source: list forms ───────────────────────────────────────────

(it-sequential "grammar-list-forms empty-list"
  (destructuring-bind (source expected-kind expected-children) (list "()" :list 0)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind))
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-list-forms simple-list"
  (destructuring-bind (source expected-kind expected-children) (list "(a b c)" nil 3)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind))
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

(it-sequential "grammar-list-forms dotted-list"
  (destructuring-bind (source expected-kind expected-children) (list "(a . b)" :dotted-list 2)
    (let ((node (parse-first-form source)))
    (expect (cl-cc:cst-interior-p node) :to-be-truthy)
    (when expected-kind
      (expect (cl-cc:cst-node-kind node) :to-be expected-kind))
    (expect (= expected-children (length (cl-cc:cst-interior-children node))) :to-be-truthy))))

;;; ─── parse-cl-source: multi-form ───────────────────────────────────────────

(it-sequential "grammar-multi-form-cases three-atoms"
  (destructuring-bind (source expected-count expected-first-kind) (list "1 2 3" 3 nil)
    (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (when expected-first-kind
      (expect (cl-cc:cst-node-kind (first forms)) :to-be expected-first-kind)))))

(it-sequential "grammar-multi-form-cases defun-and-call"
  (destructuring-bind (source expected-count expected-first-kind) (list "(defun f () 1) (f)" 2 :defun)
    (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (when expected-first-kind
      (expect (cl-cc:cst-node-kind (first forms)) :to-be expected-first-kind)))))

(it-sequential "grammar-multi-form-cases empty-input"
  (destructuring-bind (source expected-count expected-first-kind) (list "" 0 nil)
    (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (when expected-first-kind
      (expect (cl-cc:cst-node-kind (first forms)) :to-be expected-first-kind)))))

;;; ─── Error Recovery ────────────────────────────────────────────────────────

(it-sequential "grammar-error-recovery-behavior"
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b c")
    (declare (ignore forms))
    (expect (> (length diags) 0) :to-be-truthy))
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b")
    (declare (ignore forms))
    (expect (listp diags) :to-be-truthy)
    (expect (> (length diags) 0) :to-be-truthy))
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(+ 1 2)")
    (declare (ignore forms))
    (expect (= 0 (length diags)) :to-be-truthy)))

;;; ─── CST byte positions ────────────────────────────────────────────────────

(it-sequential "grammar-byte-positions-cases atom"
  (destructuring-bind (source expected-start expected-end) (list "  42" 2 4)
    (let ((node (parse-first-form source)))
    (expect (= expected-start (cl-cc:cst-node-start-byte node)) :to-be-truthy)
    (expect (= expected-end (cl-cc:cst-node-end-byte   node)) :to-be-truthy))))

(it-sequential "grammar-byte-positions-cases list"
  (destructuring-bind (source expected-start expected-end) (list "(+ 1 2)" 0 7)
    (let ((node (parse-first-form source)))
    (expect (= expected-start (cl-cc:cst-node-start-byte node)) :to-be-truthy)
    (expect (= expected-end (cl-cc:cst-node-end-byte   node)) :to-be-truthy))))
