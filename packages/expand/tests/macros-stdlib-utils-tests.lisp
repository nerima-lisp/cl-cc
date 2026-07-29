;;;; tests/unit/expand/macros-stdlib-utils-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib-utils.lisp
;;;;
;;;; Covers: tailp, ldiff, copy-alist, tree-equal, get-properties,
;;;; nunion/nintersection/nset-difference/nset-exclusive-or,
;;;; nsubst/nsubst-if/nsubst-if-not, nstring-upcase/downcase/capitalize,
;;;; bit-vector-p, simple-string-p, simple-bit-vector-p,
;;;; array-element-type, array-in-bounds-p, upgraded-array-element-type.

(in-package :cl-cc/test)



;;; ─── tailp ────────────────────────────────────────────────────────────────

(it-sequential "tailp-expansion"
  (let* ((result (our-macroexpand-1 '(tailp obj lst)))
         (end-clause (third result)))
    (expect (car result) :to-be 'do)
    (expect (consp end-clause) :to-be-truthy)))

;;; ─── ldiff ────────────────────────────────────────────────────────────────

(it-sequential "ldiff-expansion"
  (let* ((result (our-macroexpand-1 '(ldiff lst obj)))
         (body (cddr result)))
    (expect (car result) :to-be 'let)
    (expect (%tree-contains-p 'nreverse body) :to-be-truthy)))

;;; ─── copy-alist ───────────────────────────────────────────────────────────

(it-sequential "copy-alist-expansion"
  (let* ((result (our-macroexpand-1 '(copy-alist alist)))
         (body (cddr result)))
    (expect (car result) :to-be 'let)
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'dolist))) body) :to-be-truthy)))

;;; ─── tree-equal ───────────────────────────────────────────────────────────

(it-sequential "tree-equal-expands-to-labels"
  (let ((result (our-macroexpand-1 '(tree-equal x y))))
    (expect (car result) :to-be 'labels)))

(defun %tree-contains-p (target form)
  "True if TARGET appears anywhere inside FORM (nested)."
  (cond ((eq form target) t)
        ((consp form) (or (%tree-contains-p target (car form))
                          (%tree-contains-p target (cdr form))))
        (t nil)))

(it-sequential "tree-equal-uses-default-eql-test"
  (let* ((result   (our-macroexpand-1 '(tree-equal x y)))
         (bindings (second result))
         (fn-body  (cddr (first bindings))))
    ;; eql appears anywhere in the recursive fn body (nested ok)
    (expect (%tree-contains-p 'eql fn-body) :to-be-truthy)))

(it-sequential "tree-equal-respects-test-keyword"
  (let* ((result (our-macroexpand-1 '(tree-equal x y :test #'equal)))
         (fn-body (cddr (first (second result)))))
    (expect (%tree-contains-p 'equal fn-body) :to-be-truthy)))

;;; ─── get-properties ───────────────────────────────────────────────────────

(it-sequential "get-properties-expansion"
  (let* ((result (our-macroexpand-1 '(get-properties plist '(:a :b))))
         (end-clause (third result)))
    (expect (car result) :to-be 'do)
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'values))) end-clause) :to-be-truthy)))

;;; ─── Destructive set operations ───────────────────────────────────────────

(it-sequential "destructive-set-ops-delegate nunion"
  (destructuring-bind (form expected-head) (list '(nunion a b) 'union)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "destructive-set-ops-delegate nintersection"
  (destructuring-bind (form expected-head) (list '(nintersection a b) 'intersection)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "destructive-set-ops-delegate nset-difference"
  (destructuring-bind (form expected-head) (list '(nset-difference a b) 'set-difference)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "destructive-set-ops-delegate nset-exclusive-or"
  (destructuring-bind (form expected-head) (list '(nset-exclusive-or a b) 'set-exclusive-or)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

;;; ─── nsubst / nsubst-if / nsubst-if-not ──────────────────────────────────

(it-sequential "nsubst-delegation-cases no-test"
  (destructuring-bind (form expected-head) (list '(nsubst new old tree) 'subst)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nsubst-delegation-cases with-test"
  (destructuring-bind (form expected-head) (list '(nsubst new old tree :test #'equal) 'subst-if)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nsubst-if-variants-delegate nsubst-if"
  (destructuring-bind (form expected-head) (list '(nsubst-if new pred tree) 'subst-if)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nsubst-if-variants-delegate nsubst-if-not"
  (destructuring-bind (form expected-head) (list '(nsubst-if-not new pred tree) 'subst-if-not)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

;;; ─── nstring-upcase / nstring-downcase / nstring-capitalize ──────────────

(it-sequential "nstring-case-variants-no-bounds-delegate nstring-upcase"
  (destructuring-bind (form expected-head) (list '(nstring-upcase s) 'string-upcase)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nstring-case-variants-no-bounds-delegate nstring-downcase"
  (destructuring-bind (form expected-head) (list '(nstring-downcase s) 'string-downcase)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nstring-case-variants-no-bounds-delegate nstring-capitalize"
  (destructuring-bind (form expected-head) (list '(nstring-capitalize s) 'string-capitalize)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "nstring-upcase-with-bounds"
  (let ((result-start (our-macroexpand-1 '(nstring-upcase s :start 2)))
        (result-end   (our-macroexpand-1 '(nstring-upcase s :end 5))))
    (expect (car result-start) :to-be 'string-upcase)
    (expect (member :start result-start) :to-be-truthy)
    (expect (car result-end) :to-be 'string-upcase)
    (expect (member :end result-end) :to-be-truthy)))

;;; ─── Array predicate macros ───────────────────────────────────────────────

(it-sequential "bit-vector-p-expands-to-let"
  (let ((result (our-macroexpand-1 '(bit-vector-p x))))
    (expect (car result) :to-be 'let)))

(it-sequential "simple-predicate-delegation-cases simple-string-p"
  (destructuring-bind (form expected-head) (list '(simple-string-p x) 'stringp)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

(it-sequential "simple-predicate-delegation-cases simple-bit-vector-p"
  (destructuring-bind (form expected-head) (list '(simple-bit-vector-p x) 'bit-vector-p)
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be expected-head))))

;;; ─── Array utility macros ─────────────────────────────────────────────────

(it-sequential "array-element-type-returns-t"
  (let ((result (our-macroexpand-1 '(array-element-type arr))))
    (expect (car result) :to-be 'progn)
    ;; last element is quoted T
    (expect (car (last result)) :to-equal ''t)))

(it-sequential "array-in-bounds-p-expands-to-let"
  (let ((result (our-macroexpand-1 '(array-in-bounds-p arr 0 1))))
    (expect (car result) :to-be 'let)))

(it-sequential "array-in-bounds-p-uses-every"
  (let* ((result (our-macroexpand-1 '(array-in-bounds-p arr 0 1)))
         (body (cddr result)))
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'and))) body) :to-be-truthy)))

(it-sequential "upgraded-array-element-type-returns-t"
  (let ((result (our-macroexpand-1 '(cl:upgraded-array-element-type 'integer))))
    (expect (car result) :to-be 'progn)
    (expect (car (last result)) :to-equal ''t)))
