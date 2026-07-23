;;;; tests/unit/expand/macros-setops-tests.lisp — Set operations macro tests

(in-package :cl-cc/test)



(defun %quoted-form (value)
  `(quote ,value))

(it-sequential "macros-setops-expand-to-let remove"
  (destructuring-bind (form) (list '(remove x xs))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let member"
  (destructuring-bind (form) (list '(member x xs))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let remove-duplicates"
  (destructuring-bind (form) (list '(remove-duplicates xs))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let union"
  (destructuring-bind (form) (list '(union xs ys))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let set-difference"
  (destructuring-bind (form) (list '(set-difference xs ys))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let intersection"
  (destructuring-bind (form) (list '(intersection xs ys))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let subsetp"
  (destructuring-bind (form) (list '(subsetp xs ys))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let adjoin"
  (destructuring-bind (form) (list '(adjoin x xs))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let rassoc"
  (destructuring-bind (form) (list '(rassoc x alist))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

(it-sequential "macros-setops-expand-to-let pairlis"
  (destructuring-bind (form) (list '(pairlis keys data))
    (expect (car (our-macroexpand-1 form)) :to-be 'let)))

;;; ── %keyword-test-args ───────────────────────────────────────────────────

(it-sequential "keyword-test-args-cases test-given"
  (destructuring-bind (test test-not expected) (list '#'eq nil '(:test #'eq))
    (expect (cl-cc/expand::%keyword-test-args test test-not) :to-equal expected)))

(it-sequential "keyword-test-args-cases test-not-given"
  (destructuring-bind (test test-not expected) (list nil '#'equal '(:test-not #'equal))
    (expect (cl-cc/expand::%keyword-test-args test test-not) :to-equal expected)))

(it-sequential "keyword-test-args-cases neither"
  (destructuring-bind (test test-not expected) (list nil nil nil)
    (expect (cl-cc/expand::%keyword-test-args test test-not) :to-equal expected)))

;;; ── %keyword-test-key-args ───────────────────────────────────────────────

(it-sequential "keyword-test-key-args-cases test-and-key"
  (destructuring-bind (test test-not key expected) (list '#'eq nil '#'car '(:test #'eq :key #'car))
    (expect (cl-cc/expand::%keyword-test-key-args test test-not key) :to-equal expected)))

(it-sequential "keyword-test-key-args-cases test-not-and-key"
  (destructuring-bind (test test-not key expected) (list nil '#'equal '#'cdr '(:test-not #'equal :key #'cdr))
    (expect (cl-cc/expand::%keyword-test-key-args test test-not key) :to-equal expected)))

(it-sequential "keyword-test-key-args-cases no-test-no-key"
  (destructuring-bind (test test-not key expected) (list nil nil nil nil)
    (expect (cl-cc/expand::%keyword-test-key-args test test-not key) :to-equal expected)))

(it-sequential "keyword-test-key-args-cases test-only"
  (destructuring-bind (test test-not key expected) (list '#'eql nil nil '(:test #'eql))
    (expect (cl-cc/expand::%keyword-test-key-args test test-not key) :to-equal expected)))

;;; ── %test-predicate-form ─────────────────────────────────────────────────

(it-sequential "test-predicate-form-cases test-not"
  (destructuring-bind (test test-not expected) (list nil '#'equal '(complement #'equal))
    (expect (cl-cc/expand::%test-predicate-form test test-not) :to-equal expected)))

(it-sequential "test-predicate-form-cases test"
  (destructuring-bind (test test-not expected) (list '#'eq nil '#'eq)
    (expect (cl-cc/expand::%test-predicate-form test test-not) :to-equal expected)))

(it-sequential "test-predicate-form-cases default"
  (destructuring-bind (test test-not expected) (list nil nil '#'eql)
    (expect (cl-cc/expand::%test-predicate-form test test-not) :to-equal expected)))

;;; ── macro runtime behaviour ──────────────────────────────────────────────

(it-sequential "macros-setops-member-finds-element"
  (expect (run-string "(member 2 '(1 2 3))") :to-be-truthy))

(it-sequential "macros-setops-remove-filters-element"
  (expect (run-string "(remove 2 '(1 2 3))") :to-equal '(1 3)))

(it-sequential "macros-setops-union-no-duplicates"
  (let ((result (run-string "(sort (union '(1 2) '(2 3)) #'<)")))
    (expect result :to-equal '(1 2 3))))

(it-sequential "macros-setops-intersection-common-elements"
  (let ((result (run-string "(sort (intersection '(1 2 3) '(2 3 4)) #'<)")))
    (expect result :to-equal '(2 3))))

(it-sequential "macros-setops-set-difference-removes-second-set"
  (let ((result (run-string "(sort (set-difference '(1 2 3) '(2)) #'<)")))
    (expect result :to-equal '(1 3))))

(it-sequential "macros-setops-hash-fast-path-expands union"
  (destructuring-bind (form) (list `(union ,(%quoted-form (loop for i below 24 collect i))
                   ,(%quoted-form '(24 25))
                   :test #'eql))
    (let ((expanded (our-macroexpand-1 form)))
    (expect (%tree-contains-head-p 'make-hash-table expanded) :to-be-truthy))))

(it-sequential "macros-setops-hash-fast-path-expands set-difference"
  (destructuring-bind (form) (list `(set-difference ,(%quoted-form (loop for i below 24 collect i))
                            ,(%quoted-form '(1 2))
                            :test #'eql))
    (let ((expanded (our-macroexpand-1 form)))
    (expect (%tree-contains-head-p 'make-hash-table expanded) :to-be-truthy))))

(it-sequential "macros-setops-hash-fast-path-expands intersection"
  (destructuring-bind (form) (list `(intersection ,(%quoted-form (loop for i below 24 collect i))
                          ,(%quoted-form '(1 2))
                          :test #'eql))
    (let ((expanded (our-macroexpand-1 form)))
    (expect (%tree-contains-head-p 'make-hash-table expanded) :to-be-truthy))))
