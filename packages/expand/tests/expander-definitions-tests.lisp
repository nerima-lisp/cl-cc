;;;; tests/unit/expand/expander-definitions-tests.lisp — Definition-form expander tests

(in-package :cl-cc/test)



(it-sequential "expander-docstring-helpers"
  (let* ((body '("Doc" (foo) (bar)))
         (stripped (cl-cc/expand::%strip-docstring body)))
    (expect stripped :to-equal '((foo) (bar)))
    (expect (cl-cc/expand::%extract-docstring body) :to-equal "Doc")))

(it-sequential "expander-defsetf-short-form-registers-updater"
  (let ((accessor 'expander-defsetf-short-accessor)
        (updater 'expander-defsetf-short-updater))
    (cl-cc/expand:compiler-macroexpand-all `(defsetf ,accessor ,updater))
    (expect (gethash accessor cl-cc/expand::*setf-compound-place-handlers*) :to-be-truthy)
    (let ((result (cl-cc/expand:compiler-macroexpand-all `(setf (,accessor obj arg) value))))
      (expect (car result) :to-be updater)
      (expect (cdr result) :to-equal '(obj arg value)))))

(it-sequential "expander-defsetf-long-form-uses-full-lambda-list"
  (let ((accessor 'expander-defsetf-long-form-accessor))
    (cl-cc/expand:compiler-macroexpand-all
     '(defsetf expander-defsetf-long-form-accessor
          (&optional (x 1 x-p) &key ((:scale scale) 2 scale-p))
          (new-value)
        (list x x-p scale scale-p new-value)))
    (let* ((result (cl-cc/expand:compiler-macroexpand-all
                    '(setf (expander-defsetf-long-form-accessor 10 :scale 7)
                           42)))
           (printed (format nil "~S" result)))
      (expect (car result) :to-be 'let)
      (expect (search "X-P" printed) :to-be-truthy)
      (expect (search "SCALE-P" printed) :to-be-truthy)
      (expect (search "NEW-VALUE" printed) :to-be-truthy)
      (expect (gethash accessor cl-cc/expand::*setf-compound-place-handlers*) :to-be-truthy))))

(it-sequential "expander-defconstant-populates-constant-table"
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(defconstant +expander-def-constant+ 42))))
    (expect (car result) :to-be 'defparameter)
    (expect (third result) :to-equal 42)
    (expect (gethash '+expander-def-constant+ cl-cc/expand:*constant-table*) :to-equal 42)))

(it-sequential "expander-define-symbol-macro-and-get-decoded-time"
  (let* ((name 'expander-definitions-symbol-macro)
         (expansion '(+ 1 2))
         (result (cl-cc/expand:compiler-macroexpand-all `(define-symbol-macro ,name ,expansion))))
    (expect (car result) :to-be 'quote)
    (expect (second result) :to-be name)
    (expect (gethash name cl-cc/expand:*symbol-macro-table*) :to-equal expansion))
  (expect (cl-cc/expand:compiler-macroexpand-all '(get-decoded-time)) :to-equal '(decode-universal-time (get-universal-time))))
