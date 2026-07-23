(in-package :cl-cc/test)



(it-sequential "macro-lambda-list-required-only"
  (let ((info (cl-cc/expand:parse-lambda-list '(a b c))))
    (expect (cl-cc/expand::lambda-list-info-required info) :to-equal '(a b c))))

(it-sequential "macro-lambda-list-key-and-aux"
  (let ((info (cl-cc/expand:parse-lambda-list '(&key verbose &aux (count 0)))))
    (expect (cl-cc/expand::lambda-list-info-key-params info) :to-be-truthy)
    (expect (cl-cc/expand::lambda-list-info-aux info) :to-equal '((count 0)))))

(it-sequential "macro-lambda-list-whole-section"
  (let ((info (cl-cc/expand:parse-lambda-list '(&whole whole-name x &optional y))))
    (expect (cl-cc/expand::lambda-list-info-whole info) :to-be 'whole-name)
    (expect (cl-cc/expand::lambda-list-info-required info) :to-equal '(x))
    (expect (cl-cc/expand::lambda-list-info-optional info) :to-equal '((y nil nil)))))

(it-sequential "macro-lambda-list-bindings-shape"
  (expect (assoc 'args (cl-cc/expand:generate-lambda-bindings '(&rest args) 'form)) :to-be-truthy)
  (expect (assoc 'x (cl-cc/expand:destructure-lambda-list '(x &optional y) 'form)) :to-be-truthy))

(it-sequential "macro-lambda-list-optional-rest-body-environment"
  (let ((info (cl-cc/expand:parse-lambda-list '(a &optional (b 10 b-p) &body body &environment env))))
    (expect (cl-cc/expand::lambda-list-info-required info) :to-equal '(a))
    (expect (cl-cc/expand::lambda-list-info-optional info) :to-equal '((b 10 b-p)))
    (expect (cl-cc/expand::lambda-list-info-body info) :to-be 'body)
    (expect (cl-cc/expand:lambda-list-info-environment info) :to-be 'env)))

(it-sequential "macro-lambda-list-allow-other-keys-and-key-spec"
  (let ((info (cl-cc/expand:parse-lambda-list '(&key ((:size n) 3 supplied-p) &allow-other-keys))))
    (expect (cl-cc/expand::lambda-list-info-allow-other-keys info) :to-be-truthy)
    (expect (mapcar (lambda (spec) (list (first spec) (second spec) (third spec)))
                          (cl-cc/expand::lambda-list-info-key-params info)) :to-equal '(((:size n) 3 supplied-p)))))

(it-sequential "macro-lambda-list-generate-bindings-covers-key-and-aux"
  (let ((bindings (cl-cc/expand:generate-lambda-bindings
                   '(x &key ((:size n) 3 n-p) &aux (count 0))
                   'form)))
    (expect (assoc 'x bindings) :to-be-truthy)
    (expect (assoc 'n bindings) :to-be-truthy)
    (expect (assoc 'n-p bindings) :to-be-truthy)
    (expect (assoc 'count bindings) :to-be-truthy)))

(it-sequential "macro-lambda-list-destructure-covers-nested-required-and-key"
  (let ((bindings (cl-cc/expand:destructure-lambda-list
                   '((head tail) &key ((:limit lim) 5 lim-p) &aux (done nil))
                   'form)))
    (expect (assoc 'head bindings) :to-be-truthy)
    (expect (assoc 'tail bindings) :to-be-truthy)
    (expect (assoc 'lim bindings) :to-be-truthy)
    (expect (assoc 'lim-p bindings) :to-be-truthy)
    (expect (assoc 'done bindings) :to-be-truthy)))

(it-sequential "macro-lambda-list-destructure-whole"
  (let ((bindings (cl-cc/expand:destructure-lambda-list '(&whole whole x) 'input-form)))
    (expect (cdr (assoc 'whole bindings)) :to-be 'input-form)
    (expect (assoc 'x bindings) :to-be-truthy)))

;;; ── %push-required-bindings ──────────────────────────────────────────────

(it-sequential "push-required-bindings-single"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-required-bindings '(x) 'args bindings gsl))
    (expect (= 2 (length bindings)) :to-be-truthy)
    (expect (assoc 'x bindings) :to-be-truthy)))

(it-sequential "push-required-bindings-advances-cursor"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-required-bindings '(x y) 'args bindings gsl))
    (expect result-arg :to-equal '(cdr (cdr args)))))

;;; ── %push-optional-bindings ──────────────────────────────────────────────

(it-sequential "push-optional-bindings-with-supplied-p"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-optional-bindings
       '((b 10 b-p)) 'remaining bindings gsl))
    (expect (assoc 'b bindings) :to-be-truthy)
    (expect (assoc 'b-p bindings) :to-be-truthy)))

(it-sequential "push-optional-bindings-without-supplied-p"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result-arg nil))
    (multiple-value-setq (result-arg bindings)
      (cl-cc/expand::%push-optional-bindings
       '((b 10 nil)) 'remaining bindings gsl))
    (expect (assoc 'b bindings) :to-be-truthy)
    (expect (= 2 (length bindings)) :to-be-truthy)))

;;; ── %push-key-bindings ───────────────────────────────────────────────────

(it-sequential "push-key-bindings-basic"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result (cl-cc/expand::%push-key-bindings
                  '(((:size n) 3 nil)) 'kwargs bindings gsl)))
    (expect (assoc 'n result) :to-be-truthy)))

(it-sequential "push-key-bindings-with-supplied-p"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (bindings nil)
         (result (cl-cc/expand::%push-key-bindings
                  '(((:size n) 3 n-p)) 'kwargs bindings gsl)))
    (expect (assoc 'n result) :to-be-truthy)
    (expect (assoc 'n-p result) :to-be-truthy)))

;;; ── %push-aux-bindings ───────────────────────────────────────────────────

(it-sequential "push-aux-bindings-single"
  (let* ((bindings nil)
         (result (cl-cc/expand::%push-aux-bindings '((count 0)) bindings)))
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (first result) :to-equal '(count 0))))

(it-sequential "push-aux-bindings-multiple"
  (let* ((bindings nil)
         (result (cl-cc/expand::%push-aux-bindings '((x 1) (y 2)) bindings)))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (assoc 'x result) :to-be-truthy)
    (expect (assoc 'y result) :to-be-truthy)))

;;; ── %push-destructured-required-bindings ─────────────────────────────────

(it-sequential "push-destructured-required-simple-name"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-required-bindings '(x) 'args nil gsl)))
    (expect (assoc 'x result) :to-be-truthy)))

(it-sequential "push-destructured-required-nested-list"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-required-bindings '((a b)) 'args nil gsl)))
    (expect (assoc 'a result) :to-be-truthy)
    (expect (assoc 'b result) :to-be-truthy)))

;;; ── %push-destructured-key-bindings ──────────────────────────────────────

(it-sequential "push-destructured-key-bindings-emits-getf"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-key-bindings
                  '(((:count n) 0 nil)) 'kw-args nil gsl)))
    (expect (assoc 'n result) :to-be-truthy)))

(it-sequential "push-destructured-key-bindings-with-supplied-p"
  (let* ((gsl (cl-cc/expand::%make-gensym-local))
         (result (cl-cc/expand::%push-destructured-key-bindings
                  '(((:count n) 0 n-p)) 'kw-args nil gsl)))
    (expect (assoc 'n result) :to-be-truthy)
    (expect (assoc 'n-p result) :to-be-truthy)))

;;; ── *lambda-list-keyword-transitions* data table ─────────────────────────

(it-sequential "lambda-list-keyword-transitions-completeness &whole"
  (destructuring-bind (keyword expected-state) (list '&whole :whole)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &optional"
  (destructuring-bind (keyword expected-state) (list '&optional :optional)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &rest"
  (destructuring-bind (keyword expected-state) (list '&rest :rest)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &body"
  (destructuring-bind (keyword expected-state) (list '&body :body)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &key"
  (destructuring-bind (keyword expected-state) (list '&key :key)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &aux"
  (destructuring-bind (keyword expected-state) (list '&aux :aux)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-completeness &environment"
  (destructuring-bind (keyword expected-state) (list '&environment :environment)
    (expect (cdr (assoc keyword cl-cc/expand::*lambda-list-keyword-transitions*)) :to-be expected-state)))

(it-sequential "lambda-list-keyword-transitions-excludes-allow-other-keys"
  (expect (assoc '&allow-other-keys cl-cc/expand::*lambda-list-keyword-transitions*) :to-be-null))
