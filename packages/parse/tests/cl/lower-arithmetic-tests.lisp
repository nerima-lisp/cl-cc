;;;; tests/unit/parse/cl/lower-arithmetic-tests.lisp
;;;; Additional coverage for src/parse/cl/lower.lisp
;;;;
;;;; Tests n-ary arithmetic lowering, comparison lowering, sequence,
;;;; let, lambda, function reference, block, return-from, tagbody, go,
;;;; multi-var setq, and setf (symbol and compound place forms).
;;;; The cl-lower-suite defsuite is in lower-tests.lisp (loaded first).

(in-package :cl-cc/test)

;;; ─── Nullary and unary arithmetic (*arith-op-descriptors* table) ────────

(it-sequential "lower-nullary-arithmetic plus"
  (destructuring-bind (form expected) (list '(+) 0)
    (let ((node (lower form)))
    (expect (cl-cc::ast-int-p node) :to-be-truthy)
    (expect (= expected (cl-cc::ast-int-value node)) :to-be-truthy))))

(it-sequential "lower-nullary-arithmetic mul"
  (destructuring-bind (form expected) (list '(*) 1)
    (let ((node (lower form)))
    (expect (cl-cc::ast-int-p node) :to-be-truthy)
    (expect (= expected (cl-cc::ast-int-value node)) :to-be-truthy))))

(it-sequential "lower-unary-plus-returns-arg"
  (let ((node (lower '(+ 42))))
    (expect (cl-cc::ast-int-p node) :to-be-truthy)
    (expect (= 42 (cl-cc::ast-int-value node)) :to-be-truthy)))

(it-sequential "lower-unary-divide-becomes-reciprocal"
  (let ((node (lower '(/ 2))))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be '/)
    (expect (= 1 (cl-cc::ast-int-value (cl-cc::ast-binop-lhs node))) :to-be-truthy)))

;;; ─── N-ary arithmetic left-fold ──────────────────────────────────────────

(it-sequential "lower-nary-plus-folds-left"
  (let ((node (lower '(+ 1 2 3))))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be '+)
    ;; rhs is 3
    (expect (= 3 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs node))) :to-be-truthy)
    ;; lhs is (+ 1 2)
    (expect (cl-cc::ast-binop-p (cl-cc::ast-binop-lhs node)) :to-be-truthy)))

(it-sequential "lower-comparison-ops-produce-binop eq"
  (destructuring-bind (form expected-op) (list '(= 3 4) '=)
    (let ((node (lower form)))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be expected-op))))

(it-sequential "lower-comparison-ops-produce-binop lt"
  (destructuring-bind (form expected-op) (list '(< 1 2) '<)
    (let ((node (lower form)))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be expected-op))))

(it-sequential "lower-comparison-ops-produce-binop gt"
  (destructuring-bind (form expected-op) (list '(> 2 1) '>)
    (let ((node (lower form)))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be expected-op))))

(it-sequential "lower-comparison-ops-produce-binop le"
  (destructuring-bind (form expected-op) (list '(<= 1 1) '<=)
    (let ((node (lower form)))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be expected-op))))

(it-sequential "lower-comparison-ops-produce-binop ge"
  (destructuring-bind (form expected-op) (list '(>= 2 1) '>=)
    (let ((node (lower form)))
    (expect (cl-cc::ast-binop-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-op node) :to-be expected-op))))

;;; ─── Sequence (progn) ────────────────────────────────────────────────────

(it-sequential "lower-empty-progn-returns-nil-quote"
  (let ((node (lower '(progn))))
    (expect (cl-cc::ast-quote-p node) :to-be-truthy)
    (expect (cl-cc::ast-quote-value node) :to-be-null)))

(it-sequential "lower-progn-form-count single"
  (destructuring-bind (form expected-count) (list '(progn 42) 1)
    (let ((node (lower form)))
    (expect (cl-cc::ast-progn-p node) :to-be-truthy)
    (expect (= expected-count (length (cl-cc::ast-progn-forms node))) :to-be-truthy))))

(it-sequential "lower-progn-form-count multiple"
  (destructuring-bind (form expected-count) (list '(progn 1 2 3) 3)
    (let ((node (lower form)))
    (expect (cl-cc::ast-progn-p node) :to-be-truthy)
    (expect (= expected-count (length (cl-cc::ast-progn-forms node))) :to-be-truthy))))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(it-sequential "lower-let-produces-ast-let"
  (let ((node (lower '(let ((x 1)) x))))
    (expect (cl-cc::ast-let-p node) :to-be-truthy)
    (expect (= 1 (length (cl-cc::ast-let-bindings node))) :to-be-truthy)
    (expect (car (first (cl-cc::ast-let-bindings node))) :to-be 'x)))

(it-sequential "lower-let-default-nil-binding-forms bare-symbol"
  (destructuring-bind (form) (list '(let (x) x))
    (let* ((node (lower form))
         (binding (first (cl-cc::ast-let-bindings node))))
    (expect (cl-cc::ast-let-p node) :to-be-truthy)
    (expect (car binding) :to-be 'x)
    (expect (cl-cc::ast-quote-p (cdr binding)) :to-be-truthy))))

(it-sequential "lower-let-default-nil-binding-forms single-element"
  (destructuring-bind (form) (list '(let ((x)) x))
    (let* ((node (lower form))
         (binding (first (cl-cc::ast-let-bindings node))))
    (expect (cl-cc::ast-let-p node) :to-be-truthy)
    (expect (car binding) :to-be 'x)
    (expect (cl-cc::ast-quote-p (cdr binding)) :to-be-truthy))))

;;; ─── Lambda ───────────────────────────────────────────────────────────────

(it-sequential "lower-lambda-cases with-params"
  (destructuring-bind (form expected-count expected-first) (list '(lambda (a b) (+ a b)) 2 'a)
    (let ((node (lower form)))
    (expect (cl-cc::ast-lambda-p node) :to-be-truthy)
    (expect (= expected-count (length (cl-cc::ast-lambda-params node))) :to-be-truthy)
    (when expected-first
      (expect (first (cl-cc::ast-lambda-params node)) :to-be expected-first)))))

(it-sequential "lower-lambda-cases empty-params"
  (destructuring-bind (form expected-count expected-first) (list '(lambda () 42) 0 nil)
    (let ((node (lower form)))
    (expect (cl-cc::ast-lambda-p node) :to-be-truthy)
    (expect (= expected-count (length (cl-cc::ast-lambda-params node))) :to-be-truthy)
    (when expected-first
      (expect (first (cl-cc::ast-lambda-params node)) :to-be expected-first)))))

;;; ─── Function reference ───────────────────────────────────────────────────

(it-sequential "lower-function-reference-cases symbol"
  (destructuring-bind (form pred) (list '(function cons) #'cl-cc::ast-function-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-function-reference-cases lambda"
  (destructuring-bind (form pred) (list '(function (lambda (x) x)) #'cl-cc::ast-lambda-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

;;; ─── Block / return-from ─────────────────────────────────────────────────

(it-sequential "lower-block-cases with-body"
  (destructuring-bind (form expected-name empty-body-p) (list '(block outer (+ 1 2)) 'outer nil)
    (let ((node (lower form)))
    (expect (cl-cc::ast-block-p node) :to-be-truthy)
    (expect (cl-cc::ast-block-name node) :to-be expected-name)
    (when empty-body-p
      (expect (cl-cc::ast-quote-p (first (cl-cc::ast-block-body node))) :to-be-truthy)))))

(it-sequential "lower-block-cases empty-body"
  (destructuring-bind (form expected-name empty-body-p) (list '(block foo) 'foo t)
    (let ((node (lower form)))
    (expect (cl-cc::ast-block-p node) :to-be-truthy)
    (expect (cl-cc::ast-block-name node) :to-be expected-name)
    (when empty-body-p
      (expect (cl-cc::ast-quote-p (first (cl-cc::ast-block-body node))) :to-be-truthy)))))

(it-sequential "lower-return-from-cases with-value"
  (destructuring-bind (form has-value-p) (list '(return-from outer 42) t)
    (let ((node (lower form)))
    (expect (cl-cc::ast-return-from-p node) :to-be-truthy)
    (expect (cl-cc::ast-return-from-name node) :to-be 'outer)
    (if has-value-p
        (expect (= 42 (cl-cc::ast-int-value (cl-cc::ast-return-from-value node))) :to-be-truthy)
        (expect (cl-cc::ast-quote-p (cl-cc::ast-return-from-value node)) :to-be-truthy)))))

(it-sequential "lower-return-from-cases without-value"
  (destructuring-bind (form has-value-p) (list '(return-from outer) nil)
    (let ((node (lower form)))
    (expect (cl-cc::ast-return-from-p node) :to-be-truthy)
    (expect (cl-cc::ast-return-from-name node) :to-be 'outer)
    (if has-value-p
        (expect (= 42 (cl-cc::ast-int-value (cl-cc::ast-return-from-value node))) :to-be-truthy)
        (expect (cl-cc::ast-quote-p (cl-cc::ast-return-from-value node)) :to-be-truthy)))))

;;; ─── Tagbody / go ────────────────────────────────────────────────────────

(it-sequential "lower-go-and-tagbody"
  (let ((go-node (lower '(go top))))
    (expect (cl-cc::ast-go-p go-node) :to-be-truthy)
    (expect (cl-cc::ast-go-tag go-node) :to-be 'top))
  (let ((tb-node (lower '(tagbody top (go top)))))
    (expect (cl-cc::ast-tagbody-p tb-node) :to-be-truthy)
    (expect (= 1 (length (cl-cc::ast-tagbody-tags tb-node))) :to-be-truthy)
    (expect (car (first (cl-cc::ast-tagbody-tags tb-node))) :to-be 'top)))

;;; ─── setf (compound places) ──────────────────────────────────────────────

(it-sequential "lower-setf-symbol-place-produces-setq"
  (let ((node (lower '(setf x 5))))
    (expect (cl-cc::ast-setq-p node) :to-be-truthy)
    (expect (cl-cc::ast-setq-var node) :to-be 'x)))

(it-sequential "lower-empty-setf-returns-nil-quote"
  (let ((node (lower '(setf))))
    (expect (cl-cc::ast-quote-p node) :to-be-truthy)
    (expect (cl-cc::ast-quote-value node) :to-be-null)))

(it-sequential "lower-setf-multiple-symbol-places-produces-progn"
  (let ((node (lower '(setf a 1 b 2))))
    (expect (cl-cc::ast-progn-p node) :to-be-truthy)
    (expect (= 2 (length (cl-cc::ast-progn-forms node))) :to-be-truthy)
    (expect (every #'cl-cc::ast-setq-p (cl-cc::ast-progn-forms node)) :to-be-truthy)))

(it-sequential "lower-setf-compound-place-cases gethash"
  (destructuring-bind (form pred) (list '(setf (gethash :k ht) v) #'cl-cc::ast-set-gethash-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-setf-compound-place-cases slot-value"
  (destructuring-bind (form pred) (list '(setf (slot-value obj 'x) 5) #'cl-cc::ast-set-slot-value-p)
    (expect (funcall pred (lower form)) :to-be-truthy)))

(it-sequential "lower-setf-list-accessor-rewrites first"
  (destructuring-bind (form expected) (list '(setf (first xs) v) '(rplaca xs v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites rest"
  (destructuring-bind (form expected) (list '(setf (rest xs) v) '(rplacd xs v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites second"
  (destructuring-bind (form expected) (list '(setf (second xs) v) '(rplaca (cdr xs) v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites tenth"
  (destructuring-bind (form expected) (list '(setf (tenth xs) v) '(rplaca (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs))))))))) v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites cadr"
  (destructuring-bind (form expected) (list '(setf (cadr xs) v) '(rplaca (cdr xs) v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites cddr"
  (destructuring-bind (form expected) (list '(setf (cddr xs) v) '(rplacd (cdr xs) v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-list-accessor-rewrites caddr"
  (destructuring-bind (form expected) (list '(setf (caddr xs) v) '(rplaca (cdr (cdr xs)) v))
    (expect (cl-cc::ast-to-sexp (lower form)) :to-equal expected)))

(it-sequential "lower-setf-multiple-compound-places-produces-progn"
  (let* ((node (lower '(setf (gethash :a ht) 1 (slot-value obj 'x) 2)))
         (forms (cl-cc::ast-progn-forms node)))
    (expect (cl-cc::ast-progn-p node) :to-be-truthy)
    (expect (= 2 (length forms)) :to-be-truthy)
    (expect (cl-cc::ast-set-gethash-p (first forms)) :to-be-truthy)
    (expect (cl-cc::ast-set-slot-value-p (second forms)) :to-be-truthy)))

(it-sequential "lower-incf-symbol-place-produces-setq-update"
  (let* ((node (lower '(incf x)))
         (value (cl-cc::ast-setq-value node)))
    (expect (cl-cc::ast-setq-p node) :to-be-truthy)
    (expect (cl-cc::ast-setq-var node) :to-be 'x)
    (expect (cl-cc::ast-binop-p value) :to-be-truthy)
    (expect (cl-cc::ast-binop-op value) :to-be '+)
    (expect (cl-cc::ast-var-name (cl-cc::ast-binop-lhs value)) :to-be 'x)
    (expect (= 1 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs value))) :to-be-truthy)))

(it-sequential "lower-decf-symbol-place-with-delta-produces-setq-update"
  (let* ((node (lower '(decf x 3)))
         (value (cl-cc::ast-setq-value node)))
    (expect (cl-cc::ast-setq-p node) :to-be-truthy)
    (expect (cl-cc::ast-setq-var node) :to-be 'x)
    (expect (cl-cc::ast-binop-p value) :to-be-truthy)
    (expect (cl-cc::ast-binop-op value) :to-be '-)
    (expect (cl-cc::ast-var-name (cl-cc::ast-binop-lhs value)) :to-be 'x)
    (expect (= 3 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs value))) :to-be-truthy)))

(it-sequential "lower-incf-gethash-place-produces-set-gethash-update"
  (let* ((node (lower '(incf (gethash k ht) 2)))
         (value (cl-cc::ast-set-gethash-value node)))
    (expect (cl-cc::ast-set-gethash-p node) :to-be-truthy)
    (expect (cl-cc::ast-binop-p value) :to-be-truthy)
    (expect (cl-cc::ast-binop-op value) :to-be '+)
    (expect (cl-cc::ast-call-p (cl-cc::ast-binop-lhs value)) :to-be-truthy)
    (expect (= 2 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs value))) :to-be-truthy)))
