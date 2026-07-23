;;;; tests/unit/expand/expander-data-tests.lisp — Expander data table tests

(in-package :cl-cc/test)



(it-sequential "builtin-table-members variadic-+"
  (destructuring-bind (sym table) (list '+ cl-cc/expand::*variadic-fold-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members variadic-*"
  (destructuring-bind (sym table) (list '* cl-cc/expand::*variadic-fold-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members variadic-append"
  (destructuring-bind (sym table) (list 'append cl-cc/expand::*variadic-fold-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members variadic-nconc"
  (destructuring-bind (sym table) (list 'nconc cl-cc/expand::*variadic-fold-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members binary-cons"
  (destructuring-bind (sym table) (list 'cons cl-cc/expand::*binary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members binary-="
  (destructuring-bind (sym table) (list '= cl-cc/expand::*binary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members binary-mod"
  (destructuring-bind (sym table) (list 'mod cl-cc/expand::*binary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members binary-ash"
  (destructuring-bind (sym table) (list 'ash cl-cc/expand::*binary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members binary-bit-ior"
  (destructuring-bind (sym table) (list 'bit-ior cl-cc/expand::*binary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members unary-car"
  (destructuring-bind (sym table) (list 'car cl-cc/expand::*unary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members unary-cdr"
  (destructuring-bind (sym table) (list 'cdr cl-cc/expand::*unary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members unary-not"
  (destructuring-bind (sym table) (list 'not cl-cc/expand::*unary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "builtin-table-members unary-length"
  (destructuring-bind (sym table) (list 'length cl-cc/expand::*unary-builtins*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "cxr-builtins-completeness"
  (expect (length cl-cc/expand::*cxr-builtins*) :to-equal 28)
  (expect (member 'caar cl-cc/expand::*cxr-builtins*) :to-be-truthy)
  (expect (member 'cddddr cl-cc/expand::*cxr-builtins*) :to-be-truthy))

(it-sequential "all-builtin-names-members variadic-+"
  (destructuring-bind (sym table) (list '+ cl-cc/expand::*all-builtin-names*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "all-builtin-names-members binary-cons"
  (destructuring-bind (sym table) (list 'cons cl-cc/expand::*all-builtin-names*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "all-builtin-names-members unary-car"
  (destructuring-bind (sym table) (list 'car cl-cc/expand::*all-builtin-names*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "all-builtin-names-members cxr-caar"
  (destructuring-bind (sym table) (list 'caar cl-cc/expand::*all-builtin-names*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "all-builtin-names-members special-list"
  (destructuring-bind (sym table) (list 'list cl-cc/expand::*all-builtin-names*)
    (expect (member sym table) :to-be-truthy)))

(it-sequential "expander-data-registry-sanity"
  (expect (hash-table-p cl-cc/expand:*accessor-slot-map*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand:*defstruct-slot-registry*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand:*defstruct-type-registry*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand:*symbol-macro-table*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand:*constant-table*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand:*compiler-macro-table*) :to-be-truthy)
  (expect (hash-table-p cl-cc/expand::*setf-compound-place-handlers*) :to-be-truthy)
  (expect (functionp cl-cc/expand:*macro-eval-fn*) :to-be-truthy)
  (expect (cl-cc/expand::compiler-special-form-p 'if) :to-be-truthy)
  (expect (cl-cc/expand::compiler-special-form-p 'not-a-special-form) :to-be-falsy)
  (expect (cl-cc/expand::builtin-name-p 'append) :to-be-truthy)
  (expect (cl-cc/expand::builtin-name-p 'bit-ior) :to-be-truthy)
  (expect (cl-cc/expand::builtin-name-p 'bit-or) :to-be-falsy)
  (expect (cl-cc/expand::builtin-name-p 'not-a-builtin) :to-be-falsy)
  (expect (cl-cc/expand::variadic-fold-identity '+) :to-equal 0)
  (expect (cl-cc/expand::variadic-fold-identity '*) :to-equal 1)
  (expect (cl-cc/expand::variadic-fold-identity 'length) :to-equal nil))

(it-sequential "bootstrap-macro-eval-errors-without-our-eval"
  (let ((original (when (fboundp 'cl-cc/expand::our-eval)
                    (symbol-function 'cl-cc/expand::our-eval))))
    (unwind-protect
         (progn
           (when original
             (fmakunbound 'cl-cc/expand::our-eval))
           (signals error (cl-cc/expand::%bootstrap-macro-eval '(+ 1 2))))
      (when original
        (setf (symbol-function 'cl-cc/expand::our-eval) original)))))
