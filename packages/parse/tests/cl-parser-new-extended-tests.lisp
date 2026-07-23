;;;; tests/unit/parse/cl-parser-new-extended-tests.lisp — Extended parser tests: roundtrip, slot-spec, lambda edge cases, pipeline
;;;;
;;;; Tests: ast-to-sexp roundtrip additional forms, parse-slot-spec,
;;;; parse-compiler-lambda-list edge cases, full parse-then-lower pipeline.
;;;; Requires helpers (parse-one, parse-many, lower, ast-roundtrip) from earlier files (loaded first).

(in-package :cl-cc/test)


;;; ─── NEW: ast-to-sexp roundtrip additional forms ──────────────────────────

(it-sequential "ast-roundtrip-head-preserved return-from"
  (destructuring-bind (form expected-head) (list '(return-from blk 42) 'return-from)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved go"
  (destructuring-bind (form expected-head) (list '(go my-tag) 'go)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved catch"
  (destructuring-bind (form expected-head) (list '(catch 'tag 1 2) 'catch)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved throw"
  (destructuring-bind (form expected-head) (list '(throw 'tag 99) 'throw)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved the"
  (destructuring-bind (form expected-head) (list '(the fixnum x) 'the)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved values"
  (destructuring-bind (form expected-head) (list '(values 1 2 3) 'values)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved print"
  (destructuring-bind (form expected-head) (list '(print 42) 'print)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-head-preserved make-instance"
  (destructuring-bind (form expected-head) (list '(make-instance 'point :x 1 :y 2) 'make-instance)
    (expect (first (ast-roundtrip form)) :to-be expected-head)))

(it-sequential "ast-roundtrip-local-fn-forms flet"
  (destructuring-bind (expected-head form) (list 'flet '(flet ((f (x) x)) (f 1)))
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (= 1 (length (second result))) :to-be-truthy))))

(it-sequential "ast-roundtrip-local-fn-forms labels"
  (destructuring-bind (expected-head form) (list 'labels '(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 5)))
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (= 1 (length (second result))) :to-be-truthy))))

(it-sequential "ast-roundtrip-condition-control handler-case"
  (destructuring-bind (form expected-head expected-min-len) (list '(handler-case (risky) (error (e) (print e))) 'handler-case 3)
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (>= (length result) expected-min-len) :to-be-truthy))))

(it-sequential "ast-roundtrip-condition-control unwind-protect"
  (destructuring-bind (form expected-head expected-min-len) (list '(unwind-protect (risky) (cleanup1) (cleanup2)) 'unwind-protect 4)
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (>= (length result) expected-min-len) :to-be-truthy))))

(it-sequential "ast-roundtrip-definition-forms defvar-no-value"
  (destructuring-bind (form expected-head expected-second expected-third) (list '(defvar *x*) 'defvar '*x* nil)
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (second result) :to-be expected-second)
    (when expected-third
      (expect (third result) :to-equal expected-third)))))

(it-sequential "ast-roundtrip-definition-forms defclass"
  (destructuring-bind (form expected-head expected-second expected-third) (list '(defclass point (shape) (x y)) 'defclass 'point '(shape))
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (second result) :to-be expected-second)
    (when expected-third
      (expect (third result) :to-equal expected-third)))))

(it-sequential "ast-roundtrip-definition-forms defmethod"
  (destructuring-bind (form expected-head expected-second expected-third) (list '(defmethod area ((s circle)) (* 3 (slot-value s 'r))) 'defmethod 'area nil)
    (let ((result (ast-roundtrip form)))
    (expect (first result) :to-be expected-head)
    (expect (second result) :to-be expected-second)
    (when expected-third
      (expect (third result) :to-equal expected-third)))))

;;; ─── NEW: parse-slot-spec ──────────────────────────────────────────────────

(it-sequential "parse-slot-spec-bare-symbol"
  (let ((slot (cl-cc/parse:parse-slot-spec 'x)))
    (expect (cl-cc::ast-slot-name slot) :to-be 'x)
    (expect (cl-cc::ast-slot-initarg slot) :to-be-null)
    (expect (cl-cc::ast-slot-reader slot) :to-be-null)))

(it-sequential "parse-slot-spec-full-options"
  (let ((slot (cl-cc/parse:parse-slot-spec
               '(x :initarg :x :reader get-x :writer set-x :accessor x-acc :type integer))))
    (expect (cl-cc::ast-slot-name     slot) :to-be 'x)
    (expect (cl-cc::ast-slot-initarg   slot) :to-be :x)
    (expect (cl-cc::ast-slot-reader    slot) :to-be 'get-x)
    (expect (cl-cc::ast-slot-writer    slot) :to-be 'set-x)
    (expect (cl-cc::ast-slot-accessor  slot) :to-be 'x-acc)
    (expect (cl-cc::ast-slot-type      slot) :to-be 'integer)))

(it-sequential "parse-slot-spec-initform"
  (let ((slot (cl-cc/parse:parse-slot-spec '(count :initform 0))))
    (expect (cl-cc::ast-slot-name slot) :to-be 'count)
    (expect (cl-cc/ast:ast-int-p (cl-cc::ast-slot-initform slot)) :to-be-truthy)
    (expect (= 0 (cl-cc/ast:ast-int-value (cl-cc::ast-slot-initform slot))) :to-be-truthy)))

;;; ─── NEW: parse-compiler-lambda-list edge cases ────────────────────────────

(it-sequential "parser-lambda-list-rest-and-key"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(x &rest args &key verbose))
    (expect required :to-equal '(x))
    (expect optional :to-be-null)
    (expect rest-param :to-be 'args)
    (expect (= 1 (length key-params)) :to-be-truthy)
    (expect (first (first key-params)) :to-be 'verbose)))

(it-sequential "parser-lambda-list-allow-other-keys"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(&key x &allow-other-keys))
    (expect required :to-be-null)
    (expect optional :to-be-null)
    (expect rest-param :to-be-null)
    (expect (= 1 (length key-params)) :to-be-truthy)))

(it-sequential "parser-lambda-list-aux-after-key"
  (multiple-value-bind (required optional rest-param key-params aux-params)
      (cl-cc/parse:parse-compiler-lambda-list '(x &key verbose &aux (count 0) flag))
    (expect required :to-equal '(x))
    (expect optional :to-be-null)
    (expect rest-param :to-be-null)
    (expect (= 1 (length key-params)) :to-be-truthy)
    (expect (first (first key-params)) :to-be 'verbose)
    (expect (= 2 (length aux-params)) :to-be-truthy)
    (expect (first (first aux-params)) :to-be 'count)
    (expect (= 0 (second (first aux-params))) :to-be-truthy)
    (expect (first (second aux-params)) :to-be 'flag)
    (expect (second (second aux-params)) :to-be-null)))

(it-sequential "parser-lambda-list-body-treated-as-rest"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(x &body forms))
    (declare (ignore optional))
    (expect required :to-equal '(x))
    (expect rest-param :to-be 'forms)
    (expect key-params :to-be-null)))

(it-sequential "parser-lambda-list-bare-optional"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc/parse:parse-compiler-lambda-list '(&optional x))
    (declare (ignore rest-param key-params))
    (expect required :to-be-null)
    (expect (= 1 (length optional)) :to-be-truthy)
    (expect (first (first optional)) :to-be 'x)
    (expect (second (first optional)) :to-be-null)))

(it-sequential "parser-lambda-list-whole-and-environment"
  (multiple-value-bind (required optional rest-param key-params aux-params whole-param environment-param)
      (cl-cc/parse:parse-compiler-lambda-list '(&whole whole-form x &environment env y &key verbose))
    (expect whole-param :to-be 'whole-form)
    (expect environment-param :to-be 'env)
    (expect required :to-equal '(x y))
    (expect optional :to-be-null)
    (expect rest-param :to-be-null)
    (expect (= 1 (length key-params)) :to-be-truthy)
    (expect aux-params :to-be-null)))

;;; ─── NEW: full parse-then-lower pipeline ───────────────────────────────────

(it-sequential "parse-lower-pipeline integer"
  (destructuring-bind (source pred) (list "42" #'cl-cc/ast:ast-int-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline string"
  (destructuring-bind (source pred) (list "\"hi\"" #'cl-cc/ast:ast-quote-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline nil"
  (destructuring-bind (source pred) (list "nil" #'cl-cc/ast:ast-quote-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline t"
  (destructuring-bind (source pred) (list "t" #'cl-cc/ast:ast-quote-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline symbol"
  (destructuring-bind (source pred) (list "foo" #'cl-cc/ast:ast-var-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline hole"
  (destructuring-bind (source pred) (list "_" #'cl-cc/ast:ast-hole-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline if"
  (destructuring-bind (source pred) (list "(if x 1 2)" #'cl-cc/ast:ast-if-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline let"
  (destructuring-bind (source pred) (list "(let ((x 1)) x)" #'cl-cc/ast:ast-let-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline lambda"
  (destructuring-bind (source pred) (list "(lambda (x) x)" #'cl-cc/ast:ast-lambda-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline defun"
  (destructuring-bind (source pred) (list "(defun f (x) x)" #'cl-cc/ast:ast-defun-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline quote"
  (destructuring-bind (source pred) (list "'hello" #'cl-cc/ast:ast-quote-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline progn"
  (destructuring-bind (source pred) (list "(progn 1 2)" #'cl-cc/ast:ast-progn-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline setq"
  (destructuring-bind (source pred) (list "(setq x 1)" #'cl-cc/ast:ast-setq-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline block"
  (destructuring-bind (source pred) (list "(block b 1)" #'cl-cc/ast:ast-block-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "parse-lower-pipeline call"
  (destructuring-bind (source pred) (list "(foo 1 2)" #'cl-cc/ast:ast-call-p)
    (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (expect (funcall pred node) :to-be-truthy))))

(it-sequential "lower-lambda-aux-wraps-body"
  (let* ((node (lower '(lambda (x &aux (y 1) z) (+ x y))))
         (outer (first (cl-cc::ast-lambda-body node)))
         (inner (first (cl-cc/ast:ast-let-body outer))))
    (expect (cl-cc/ast:ast-lambda-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-let-p outer) :to-be-truthy)
    (expect (car (first (cl-cc/ast:ast-let-bindings outer))) :to-be 'y)
    (expect (cl-cc/ast:ast-int-p (cdr (first (cl-cc/ast:ast-let-bindings outer)))) :to-be-truthy)
    (expect (cl-cc/ast:ast-let-p inner) :to-be-truthy)
    (expect (car (first (cl-cc/ast:ast-let-bindings inner))) :to-be 'z)
    (expect (cl-cc/ast:ast-quote-p (cdr (first (cl-cc/ast:ast-let-bindings inner)))) :to-be-truthy)
    (expect (= 1 (length (cl-cc/ast:ast-let-body inner))) :to-be-truthy)))

(it-sequential "lower-defun-aux-wraps-block-body"
  (let* ((node (lower '(defun f (x &aux (y 1)) (+ x y))))
         (outer (first (cl-cc::ast-defun-body node))))
    (expect (cl-cc/ast:ast-defun-p node) :to-be-truthy)
    (expect (cl-cc/ast:ast-let-p outer) :to-be-truthy)
    (expect (car (first (cl-cc/ast:ast-let-bindings outer))) :to-be 'y)
    (expect (= 1 (length (cl-cc/ast:ast-let-body outer))) :to-be-truthy)))
