;;;; tests/unit/parse/cl/grammar-tokenstream-tests.lisp
;;;; Unit tests for src/parse/cl/grammar.lisp — Token Stream helpers
;;;;
;;;; Covers: token-stream struct (make-token-stream, predicates),
;;;;   ts-peek, ts-advance, ts-at-end-p, ts-peek-type, ts-token-value,
;;;;   ts-expect (success, EOF, type mismatch),
;;;;   %tok-to-cst, %make-list-cst,
;;;;   parse-cl-source (simple integer, symbol, list).

(in-package :cl-cc/test)

;;; ─── Token construction helpers ──────────────────────────────────────────

(defun make-test-token (type value &optional (start 0) (end 1))
  "Build a lexer-token for testing."
  (cl-cc/parse::make-lexer-token :type type :value value
                            :start-byte start :end-byte end :trivia nil))

(defun make-test-ts (token-list &optional (source ""))
  "Build a token-stream from a list of lexer-tokens."
  (cl-cc/parse::make-token-stream :tokens token-list :source source))

;;; ─── ts-peek ─────────────────────────────────────────────────────────────

(it-sequential "ts-peek-behavior"
  (let* ((tok (make-test-token :T-INT 42))
         (ts  (make-test-ts (list tok))))
    (expect (cl-cc/parse::ts-peek ts) :to-be tok)
    (expect (cl-cc/parse::ts-peek ts) :to-be tok))
  (expect (cl-cc/parse::ts-peek (make-test-ts nil)) :to-be-null))

;;; ─── ts-advance ──────────────────────────────────────────────────────────

(it-sequential "ts-advance-behavior"
  (let* ((t1  (make-test-token :T-INT 1))
         (t2  (make-test-token :T-INT 2))
         (ts  (make-test-ts (list t1 t2))))
    (let ((consumed (cl-cc/parse::ts-advance ts)))
      (expect consumed :to-be t1)
      (expect (cl-cc/parse::ts-peek ts) :to-be t2)))
  (expect (cl-cc/parse::ts-advance (make-test-ts nil)) :to-be-null))

;;; ─── ts-peek-type ────────────────────────────────────────────────────────

(it-sequential "ts-peek-type-behavior"
  (let* ((tok (make-test-token :T-IDENT 'foo))
         (ts  (make-test-ts (list tok))))
    (expect (cl-cc/parse::ts-peek-type ts) :to-be :T-IDENT))
  (expect (cl-cc/parse::ts-peek-type (make-test-ts nil)) :to-be-null))

;;; ─── ts-at-end-p ─────────────────────────────────────────────────────────

(it-sequential "ts-at-end-p-behavior"
  (expect (cl-cc/parse::ts-at-end-p (make-test-ts nil)) :to-be-truthy)
  (expect (cl-cc/parse::ts-at-end-p (make-test-ts (list (make-test-token :T-INT 1)))) :to-be-falsy)
  (expect (cl-cc/parse::ts-at-end-p (make-test-ts (list (make-test-token :T-EOF nil)))) :to-be-truthy))

;;; ─── ts-token-value ──────────────────────────────────────────────────────

(it-sequential "ts-token-value-behavior"
  (let* ((tok (make-test-token :T-INT 99))
         (ts  (make-test-ts (list tok))))
    (expect (= 99 (cl-cc/parse::ts-token-value ts)) :to-be-truthy))
  (expect (cl-cc/parse::ts-token-value (make-test-ts nil)) :to-be-null))

;;; ─── ts-expect ───────────────────────────────────────────────────────────

(it-sequential "ts-expect-behavior"
  (let* ((tok (make-test-token :T-INT 5))
         (ts  (make-test-ts (list tok))))
    (let ((result (cl-cc/parse::ts-expect ts :T-INT)))
      (expect result :to-be tok)
      (expect (cl-cc/parse::ts-at-end-p ts) :to-be-truthy)))
  (let* ((tok (make-test-token :T-IDENT 'x))
         (ts  (make-test-ts (list tok))))
    (cl-cc/parse::ts-expect ts :T-INT "ctx")
    (expect (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0) :to-be-truthy))
  (let ((ts (make-test-ts nil)))
    (cl-cc/parse::ts-expect ts :T-INT "end")
    (expect (> (length (cl-cc/parse::token-stream-diagnostics ts)) 0) :to-be-truthy)))

;;; ─── %tok-to-cst ─────────────────────────────────────────────────────────

(it-sequential "tok-to-cst-converts-lexer-token"
  (let* ((tok (make-test-token :T-INT 7 0 3))
         (cst (cl-cc/parse::%tok-to-cst tok)))
    (expect (cl-cc/parse:cst-token-p cst) :to-be-truthy)
    (expect (cl-cc/parse::cst-node-kind cst) :to-be :T-INT)
    (expect (= 7 (cl-cc/parse:cst-token-value cst)) :to-be-truthy)
    (expect (= 0 (cl-cc/parse:cst-node-start-byte cst)) :to-be-truthy)
    (expect (= 3 (cl-cc/parse:cst-node-end-byte cst)) :to-be-truthy)))

(it-sequential "tok-to-cst-returns-nil-for-nil-input"
  (expect (cl-cc/parse::%tok-to-cst nil) :to-be-null))

;;; ─── %make-list-cst ──────────────────────────────────────────────────────

(it-sequential "make-list-cst-creates-interior-with-kind-and-span"
  (let ((node (cl-cc/parse::%make-list-cst :defun '() 0 10)))
    (expect (cl-cc/parse:cst-interior-p node) :to-be-truthy)
    (expect (cl-cc/parse::cst-node-kind node) :to-be :defun)
    (expect (= 0 (cl-cc/parse:cst-node-start-byte node)) :to-be-truthy)
    (expect (= 10 (cl-cc/parse:cst-node-end-byte node)) :to-be-truthy)))

(it-sequential "make-list-cst-stores-children"
  (let* ((child (cl-cc/parse::%tok-to-cst (make-test-token :T-INT 1)))
         (node  (cl-cc/parse::%make-list-cst :call (list child) 0 5)))
    (expect (= 1 (length (cl-cc/parse:cst-interior-children node))) :to-be-truthy)
    (expect (first (cl-cc/parse:cst-interior-children node)) :to-be child)))

;;; ─── parse-cl-source ─────────────────────────────────────────────────────

(it-sequential "parse-cl-source-non-empty-for-valid-forms"
  (dolist (src '("42" "foo" "(+ 1 2)"))
    (let ((result (cl-cc/parse:parse-cl-source src)))
      (expect (listp result) :to-be-truthy)
      (expect (> (length result) 0) :to-be-truthy))))

(it-sequential "parse-cl-source-empty-string-returns-list"
  (expect (listp (cl-cc/parse:parse-cl-source "")) :to-be-truthy))
