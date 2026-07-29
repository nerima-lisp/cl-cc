;;;; tests/unit/parse/cst-tests.lisp — CST node unit tests
;;;;
;;;; Tests: node creation, predicates, cst-walk, cst-to-sexp, sexp-to-cst
;;;; roundtrip, and sexp-head-to-kind.

(in-package :cl-cc/test)


;;; ─── Node Creation ──────────────────────────────────────────────────────────

(it-sequential "cst-token-creation"
  (let ((tok (cl-cc:make-cst-token :kind :int :value 42 :start-byte 0 :end-byte 2)))
    (expect (cl-cc:cst-node-kind tok) :to-be :int)
    (expect (= 42 (cl-cc:cst-token-value tok)) :to-be-truthy)
    (expect (= 0 (cl-cc:cst-node-start-byte tok)) :to-be-truthy)
    (expect (= 2 (cl-cc:cst-node-end-byte tok)) :to-be-truthy)))

(it-sequential "cst-interior-creation"
  (let* ((child1 (cl-cc:make-cst-token :kind :int :value 1))
         (child2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :list :children (list child1 child2))))
    (expect (cl-cc:cst-node-kind node) :to-be :list)
    (expect (= 2 (length (cl-cc:cst-children node))) :to-be-truthy)))

(it-sequential "cst-error-creation"
  (let ((err (cl-cc:make-cst-error :message "bad syntax" :start-byte 5 :end-byte 10)))
    (expect (cl-cc:cst-error-message err) :to-equal "bad syntax")
    (expect (= 5 (cl-cc:cst-node-start-byte err)) :to-be-truthy)))

;;; ─── Predicates ─────────────────────────────────────────────────────────────

(it-sequential "cst-type-predicates token-p-true"
  (destructuring-bind (pred node expected) (list #'cl-cc:cst-token-p (cl-cc:make-cst-token :kind :int :value 0) t)
    (if expected
      (expect (funcall pred node) :to-be-truthy)
      (expect (funcall pred node) :to-be-falsy))))

(it-sequential "cst-type-predicates token-p-false"
  (destructuring-bind (pred node expected) (list #'cl-cc:cst-token-p (cl-cc:make-cst-interior :kind :list) nil)
    (if expected
      (expect (funcall pred node) :to-be-truthy)
      (expect (funcall pred node) :to-be-falsy))))

(it-sequential "cst-type-predicates interior-p-true"
  (destructuring-bind (pred node expected) (list #'cl-cc:cst-interior-p (cl-cc:make-cst-interior :kind :list) t)
    (if expected
      (expect (funcall pred node) :to-be-truthy)
      (expect (funcall pred node) :to-be-falsy))))

(it-sequential "cst-type-predicates error-p-true"
  (destructuring-bind (pred node expected) (list #'cl-cc:cst-error-p (cl-cc:make-cst-error :message "err") t)
    (if expected
      (expect (funcall pred node) :to-be-truthy)
      (expect (funcall pred node) :to-be-falsy))))

;;; ─── cst-child ──────────────────────────────────────────────────────────────

(it-sequential "cst-child-access"
  (let* ((c0 (cl-cc:make-cst-token :kind :int :value 10))
         (c1 (cl-cc:make-cst-token :kind :int :value 20))
         (node (cl-cc:make-cst-interior :kind :list :children (list c0 c1))))
    (expect (= 10 (cl-cc:cst-token-value (cl-cc:cst-child node 0))) :to-be-truthy)
    (expect (= 20 (cl-cc:cst-token-value (cl-cc:cst-child node 1))) :to-be-truthy)))

;;; ─── cst-walk ───────────────────────────────────────────────────────────────

(it-sequential "cst-walk-behavior"
  (let* ((leaf1 (cl-cc:make-cst-token :kind :int :value 1))
         (leaf2 (cl-cc:make-cst-token :kind :int :value 2))
         (inner (cl-cc:make-cst-interior :kind :list :children (list leaf1 leaf2)))
         (visited nil))
    (cl-cc:cst-walk inner (lambda (n) (push n visited)))
    (expect (= 3 (length visited)) :to-be-truthy)
    ;; Pre-order: inner first
    (expect (cl-cc:cst-interior-p (car (last visited))) :to-be-truthy))
  (let ((count 0)
        (leaf (cl-cc:make-cst-token :kind :int :value 42)))
    (cl-cc:cst-walk leaf (lambda (n) (declare (ignore n)) (incf count)))
    (expect (= 1 count) :to-be-truthy)))

;;; ─── cst-collect-errors ─────────────────────────────────────────────────────

(it-sequential "cst-collect-errors-finds-errors"
  (let* ((good (cl-cc:make-cst-token :kind :int :value 1))
         (bad (cl-cc:make-cst-error :message "oops"))
         (tree (cl-cc:make-cst-interior :kind :list :children (list good bad))))
    (expect (= 1 (length (cl-cc:cst-collect-errors tree))) :to-be-truthy)))

;;; ─── cst-to-sexp ────────────────────────────────────────────────────────────

(it-sequential "cst-to-sexp-token"
  (expect (= 42 (cl-cc:cst-to-sexp (cl-cc:make-cst-token :kind :int :value 42))) :to-be-truthy))

(it-sequential "cst-to-sexp-list"
  (let* ((c1 (cl-cc:make-cst-token :kind :int :value 1))
         (c2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :list :children (list c1 c2))))
    (expect (cl-cc:cst-to-sexp node) :to-equal '(1 2))))

(it-sequential "cst-to-sexp-quote"
  (let* ((inner (cl-cc:make-cst-token :kind :var :value 'foo))
         (node (cl-cc:make-cst-interior :kind :quote :children (list inner))))
    (expect (cl-cc:cst-to-sexp node) :to-equal '(quote foo))))

(it-sequential "cst-to-sexp-vector"
  (let* ((c1 (cl-cc:make-cst-token :kind :int :value 1))
         (c2 (cl-cc:make-cst-token :kind :int :value 2))
         (node (cl-cc:make-cst-interior :kind :vector :children (list c1 c2))))
    (let ((result (cl-cc:cst-to-sexp node)))
      (expect (vectorp result) :to-be-truthy)
      (expect (= 2 (length result)) :to-be-truthy))))

;;; ─── sexp-to-cst ────────────────────────────────────────────────────────────

(it-sequential "sexp-to-cst-literal-types integer"
  (destructuring-bind (input expected-kind) (list 42 :int)
    (let ((node (cl-cc:sexp-to-cst input)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "sexp-to-cst-literal-types string"
  (destructuring-bind (input expected-kind) (list "hello" :string)
    (let ((node (cl-cc:sexp-to-cst input)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "sexp-to-cst-literal-types nil"
  (destructuring-bind (input expected-kind) (list nil :nil)
    (let ((node (cl-cc:sexp-to-cst input)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

(it-sequential "sexp-to-cst-literal-types keyword"
  (destructuring-bind (input expected-kind) (list :foo :keyword)
    (let ((node (cl-cc:sexp-to-cst input)))
    (expect (cl-cc:cst-token-p node) :to-be-truthy)
    (expect (cl-cc:cst-node-kind node) :to-be expected-kind))))

;;; ─── Roundtrip ──────────────────────────────────────────────────────────────

(it-sequential "cst-roundtrip integer"
  (destructuring-bind (form) (list 42)
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip string"
  (destructuring-bind (form) (list "hello")
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip symbol"
  (destructuring-bind (form) (list 'foo)
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip keyword"
  (destructuring-bind (form) (list :bar)
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip t"
  (destructuring-bind (form) (list t)
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip list"
  (destructuring-bind (form) (list '(+ 1 2))
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip nested"
  (destructuring-bind (form) (list '(if (> x 0) x (- x)))
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

(it-sequential "cst-roundtrip quote"
  (destructuring-bind (form) (list '(quote a))
    (expect (cl-cc:cst-to-sexp (cl-cc:sexp-to-cst form)) :to-equal form)))

;;; ─── sexp-head-to-kind ──────────────────────────────────────────────────────

(it-sequential "cst-head-to-kind defun"
  (destructuring-bind (sym expected-kind) (list 'defun :defun)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))

(it-sequential "cst-head-to-kind let"
  (destructuring-bind (sym expected-kind) (list 'let :let)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))

(it-sequential "cst-head-to-kind if"
  (destructuring-bind (sym expected-kind) (list 'if :if)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))

(it-sequential "cst-head-to-kind lambda"
  (destructuring-bind (sym expected-kind) (list 'lambda :lambda)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))

(it-sequential "cst-head-to-kind progn"
  (destructuring-bind (sym expected-kind) (list 'progn :progn)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))

(it-sequential "cst-head-to-kind unknown"
  (destructuring-bind (sym expected-kind) (list 'my-func :call)
    (expect (cl-cc:sexp-head-to-kind sym) :to-be expected-kind)))
