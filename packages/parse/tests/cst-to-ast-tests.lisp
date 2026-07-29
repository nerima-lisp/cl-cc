;;;; tests/unit/parse/cst-to-ast-tests.lisp — CST-to-AST Lowering Tests
;;;;
;;;; Tests for CST → AST conversion: lower-cst-to-ast, lower-cst-list-to-ast,
;;;; parse-and-lower, parse-and-lower-one.

(in-package :cl-cc/test)



;;; ─── lower-cst-to-ast ──────────────────────────────────────────────────────

(it-sequential "cst-ast-nil-input"
  (expect (cl-cc/parse:lower-cst-to-ast nil) :to-be-null))

(it-sequential "cst-ast-token-types integer"
  (destructuring-bind (kind value source pred) (list :T-INT 42 "42" #'cl-cc/ast:ast-int-p)
    (let* ((cst (cl-cc/parse::make-cst-token :kind kind :value value
                                     :start-byte 0 :end-byte (length source)))
         (ast (cl-cc/parse:lower-cst-to-ast cst :source source)))
    (expect (cl-cc/ast:ast-node-p ast) :to-be-truthy)
    (expect (funcall pred ast) :to-be-truthy))))

(it-sequential "cst-ast-token-types symbol"
  (destructuring-bind (kind value source pred) (list :T-SYMBOL 'hello "hello" #'cl-cc/ast:ast-node-p)
    (let* ((cst (cl-cc/parse::make-cst-token :kind kind :value value
                                     :start-byte 0 :end-byte (length source)))
         (ast (cl-cc/parse:lower-cst-to-ast cst :source source)))
    (expect (cl-cc/ast:ast-node-p ast) :to-be-truthy)
    (expect (funcall pred ast) :to-be-truthy))))

(it-sequential "cst-ast-token-types string"
  (destructuring-bind (kind value source pred) (list :T-STRING "hi" "\"hi\"" #'cl-cc/ast:ast-quote-p)
    (let* ((cst (cl-cc/parse::make-cst-token :kind kind :value value
                                     :start-byte 0 :end-byte (length source)))
         (ast (cl-cc/parse:lower-cst-to-ast cst :source source)))
    (expect (cl-cc/ast:ast-node-p ast) :to-be-truthy)
    (expect (funcall pred ast) :to-be-truthy))))

(it-sequential "cst-ast-source-file-propagation"
  (let* ((cst (cl-cc/parse::make-cst-token :kind :T-INT :value 1
                                      :start-byte 0 :end-byte 1))
         (ast (cl-cc/parse:lower-cst-to-ast cst :source "1" :source-file "test.lisp")))
    (expect (cl-cc/ast:ast-node-p ast) :to-be-truthy)))

;;; ─── lower-cst-list-to-ast ─────────────────────────────────────────────────

(it-sequential "cst-ast-list-empty"
  (expect (cl-cc/parse:lower-cst-list-to-ast nil) :to-be-null))

(it-sequential "cst-ast-list-multiple"
  (let* ((cst1 (cl-cc/parse::make-cst-token :kind :T-INT :value 1
                                       :start-byte 0 :end-byte 1))
         (cst2 (cl-cc/parse::make-cst-token :kind :T-INT :value 2
                                       :start-byte 2 :end-byte 3))
         (result (cl-cc/parse:lower-cst-list-to-ast (list cst1 cst2) :source "1 2")))
    (expect (length result) :to-equal 2)
    (expect (cl-cc/ast:ast-int-p (first result)) :to-be-truthy)
    (expect (cl-cc/ast:ast-int-p (second result)) :to-be-truthy)))

;;; ─── parse-and-lower ────────────────────────────────────────────────────────

(it-sequential "cst-ast-parse-and-lower-cases integer"
  (destructuring-bind (source expected-len pred) (list "42" 1 #'cl-cc/ast:ast-int-p)
    (let ((result (cl-cc/parse:parse-and-lower source)))
    (expect (length result) :to-equal expected-len)
    (expect (funcall pred (first result)) :to-be-truthy))))

(it-sequential "cst-ast-parse-and-lower-cases string"
  (destructuring-bind (source expected-len pred) (list "\"hello\"" 1 #'cl-cc/ast:ast-quote-p)
    (let ((result (cl-cc/parse:parse-and-lower source)))
    (expect (length result) :to-equal expected-len)
    (expect (funcall pred (first result)) :to-be-truthy))))

(it-sequential "cst-ast-parse-and-lower-cases multiple"
  (destructuring-bind (source expected-len pred) (list "1 2 3" 3 #'cl-cc/ast:ast-node-p)
    (let ((result (cl-cc/parse:parse-and-lower source)))
    (expect (length result) :to-equal expected-len)
    (expect (funcall pred (first result)) :to-be-truthy))))

(it-sequential "cst-ast-parse-and-lower-cases list"
  (destructuring-bind (source expected-len pred) (list "(+ 1 2)" 1 #'cl-cc/ast:ast-node-p)
    (let ((result (cl-cc/parse:parse-and-lower source)))
    (expect (length result) :to-equal expected-len)
    (expect (funcall pred (first result)) :to-be-truthy))))

(it-sequential "cst-ast-parse-and-lower-source-file"
  (let ((result (cl-cc/parse:parse-and-lower "42" "test.lisp")))
    (expect (length result) :to-equal 1)))

;;; ─── parse-and-lower-one ────────────────────────────────────────────────────

(it-sequential "cst-ast-parse-and-lower-one"
  (let ((ast (cl-cc/parse:parse-and-lower-one "42")))
    (expect (cl-cc/ast:ast-int-p ast) :to-be-truthy)
    (expect (cl-cc/ast:ast-int-value ast) :to-equal 42))
  (let ((ast (cl-cc/parse:parse-and-lower-one "(if t 1 2)")))
    (expect (cl-cc/ast:ast-node-p ast) :to-be-truthy)))

(it-sequential "cst-ast-parse-and-lower-one-empty-error"
  (signals error (cl-cc/parse:parse-and-lower-one "")))
