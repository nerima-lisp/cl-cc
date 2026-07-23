;;;; tests/unit/expand/expander-test-support.lisp — Shared expansion test helpers

(in-package :cl-cc/test)

(defmacro assert-expansion-head (form expected-head)
  `(let ((result (cl-cc/expand:compiler-macroexpand-all ,form)))
     (expect (car result) :to-be ,expected-head)
     result))

(defmacro assert-no-expansion (form)
  `(expect (our-macroexpand-1 ,form) :to-equal ,form))

(defmacro assert-form-string-contains (form expected-substring)
  `(expect (search ,expected-substring (format nil "~S" ,form)) :to-be-truthy))

(defmacro assert-expanded-string-contains (form expected-substring)
  `(assert-form-string-contains (cl-cc/expand:compiler-macroexpand-all ,form)
     ,expected-substring))

(defmacro assert-form-string-not-contains (form unexpected-substring)
  `(expect (search ,unexpected-substring (format nil "~S" ,form)) :to-be-falsy))

(defmacro assert-expanded-string-not-contains (form unexpected-substring)
  `(assert-form-string-not-contains
       (cl-cc/expand:compiler-macroexpand-all ,form)
       ,unexpected-substring))

(defun %tree-contains-head-p (head form)
  "True if FORM contains any cons whose CAR is HEAD."
  (cond ((consp form)
         (or (eq (car form) head)
             (%tree-contains-head-p head (car form))
             (%tree-contains-head-p head (cdr form))))
        (t nil)))
