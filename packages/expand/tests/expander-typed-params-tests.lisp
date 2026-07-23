;;;; tests/unit/expand/expander-typed-params-tests.lisp
;;;; Unit tests for src/expand/expander-typed-params.lisp
;;;;
;;;; Covers: lambda-list-has-typed-p, strip-typed-params,
;;;;         register-function-type / *function-type-registry*,
;;;;         expand-type-alias.

(in-package :cl-cc/test)



;;; ── lambda-list-has-typed-p ─────────────────────────────────────────────────

(it-sequential "typed-params-has-typed-plain-symbol-list"
  (expect (cl-cc/expand:lambda-list-has-typed-p '(x y z)) :to-be-falsy))

(it-sequential "typed-params-has-typed-with-typed-pair"
  (expect (cl-cc/expand:lambda-list-has-typed-p '((x fixnum) (y string))) :to-be-truthy))

(it-sequential "typed-params-has-typed-mixed-params"
  (expect (cl-cc/expand:lambda-list-has-typed-p '((x integer) y z)) :to-be-truthy))

(it-sequential "typed-params-has-typed-stops-at-lambda-keywords"
  (expect (cl-cc/expand:lambda-list-has-typed-p '(&optional (x fixnum))) :to-be-falsy))

(it-sequential "typed-params-has-typed-empty-list"
  (expect (cl-cc/expand:lambda-list-has-typed-p '()) :to-be-falsy))

(it-sequential "typed-params-has-typed-builtin-types fixnum"
  (destructuring-bind (params) (list '((n fixnum)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types integer"
  (destructuring-bind (params) (list '((n integer)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types string"
  (destructuring-bind (params) (list '((s string)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types boolean"
  (destructuring-bind (params) (list '((b boolean)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types character"
  (destructuring-bind (params) (list '((c character)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types list"
  (destructuring-bind (params) (list '((l list)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

(it-sequential "typed-params-has-typed-builtin-types function"
  (destructuring-bind (params) (list '((f function)))
    (expect (cl-cc/expand:lambda-list-has-typed-p params) :to-be-truthy)))

;;; ── strip-typed-params ──────────────────────────────────────────────────────

(it-sequential "strip-typed-params-extracts-names"
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand:strip-typed-params '((x fixnum) (y string)))
    (declare (ignore type-alist))
    (expect plain :to-equal '(x y))))

(it-sequential "strip-typed-params-extracts-type-alist"
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand:strip-typed-params '((x fixnum) (y string)))
    (declare (ignore plain))
    (expect (= 2 (length type-alist)) :to-be-truthy)
    (expect (cdr (assoc 'x type-alist)) :to-be 'fixnum)
    (expect (cdr (assoc 'y type-alist)) :to-be 'string)))

(it-sequential "strip-typed-params-preserves-plain-symbols"
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand:strip-typed-params '((x fixnum) z))
    (expect plain :to-equal '(x z))
    (expect (= 1 (length type-alist)) :to-be-truthy)))

(it-sequential "strip-typed-params-preserves-lambda-keywords"
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand:strip-typed-params '(x &rest args))
    (declare (ignore type-alist))
    (expect (member '&rest plain) :to-be-truthy)))

(it-sequential "strip-typed-params-empty-list"
  (multiple-value-bind (plain type-alist)
      (cl-cc/expand:strip-typed-params '())
    (expect plain :to-be-null)
    (expect type-alist :to-be-null)))

;;; ── register-function-type / *function-type-registry* ──────────────────────

(it-sequential "register-function-type-stores-entry"
  (let ((cl-cc/expand:*function-type-registry*
         (make-hash-table :test #'eq)))
    (cl-cc/expand:register-function-type 'my-fn '(fixnum) 'string)
    (let ((entry (gethash 'my-fn cl-cc/expand:*function-type-registry*)))
      (expect entry :to-be-truthy)
      (expect (car entry) :to-equal '(fixnum))
      (expect (cdr entry) :to-be 'string))))

(it-sequential "register-function-type-overwrites-previous"
  (let ((cl-cc/expand:*function-type-registry*
         (make-hash-table :test #'eq)))
    (cl-cc/expand:register-function-type 'f '(fixnum) 'fixnum)
    (cl-cc/expand:register-function-type 'f '(string) 'string)
    (let ((entry (gethash 'f cl-cc/expand:*function-type-registry*)))
      (expect (car entry) :to-equal '(string))
      (expect (cdr entry) :to-be 'string))))

(it-sequential "function-type-registry-unknown-name-returns-nil"
  (let ((cl-cc/expand:*function-type-registry*
         (make-hash-table :test #'eq)))
    (expect (gethash 'no-such-fn cl-cc/expand:*function-type-registry*) :to-be-null)))
