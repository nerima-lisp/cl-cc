;;;; tests/unit/expand/macros-stdlib-io-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)


(describe-sequential "macros-stdlib-io-suite"
  (before-each
    (clrhash cl-cc/expand::*load-time-value-cache*)
    (setf cl-cc/expand:*macro-eval-fn* #'eval))


(defparameter *load-time-value-hit* 0)

(it-sequential "export-is-not-a-macro"
  (multiple-value-bind (expansion expanded-p) (our-macroexpand-1 '(export '(foo bar)))
    (declare (ignore expansion))
    (expect (not expanded-p) :to-be-truthy)))

(it-sequential "warn-expansion"
  (let* ((result   (our-macroexpand-1 '(warn "oops ~A" x)))
         (fmt-call (second result)))
    (expect (car result) :to-be 'progn)
    (expect (car fmt-call) :to-be 'format)
    (expect (second fmt-call) :to-be 't)))

(it-sequential "copy-hash-table-expansion"
  (let* ((result       (our-macroexpand-1 '(cl-cc/expand:copy-hash-table ht)))
         (inner-let    (caddr result))
         (maphash-call (caddr inner-let)))
    (expect (car result) :to-be 'let)
    (expect (car maphash-call) :to-be 'maphash)))

(it-sequential "with-open-file-expansion"
  (let* ((result    (our-macroexpand-1 '(with-open-file (s "/tmp/f") body)))
         (body-form (caddr result))
         (cleanup   (third body-form)))
    (expect (car result) :to-be 'let)
    (expect (car body-form) :to-be 'unwind-protect)
    (expect (car cleanup) :to-be 'close)))

(it-sequential "load-time-value-expansion-preserves-form-without-evaluation"
  (let ((evaluations 0)
        (cl-cc/expand:*macro-eval-fn*
          (lambda (form)
            (declare (ignore form))
            (incf evaluations))))
    (expect (our-macroexpand-1 '(load-time-value (+ 1 2) t))
            :to-equal '(cl-cc/expand::%load-time-value (+ 1 2) t))
    (expect (= 0 evaluations) :to-be-truthy)))

(it-sequential "load-time-value-expansion-preserves-default-read-only-argument"
  (expect (our-macroexpand-1 '(load-time-value (list 1 2)))
          :to-equal '(cl-cc/expand::%load-time-value (list 1 2))))

(it-sequential "provide-require-expansion-structure provide"
  (destructuring-bind (form expected-inner-op) (list '(provide :my-lib) 'pushnew)
    (let* ((result (our-macroexpand-1 form))
         (inner  (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (car inner) :to-be expected-inner-op))))

(it-sequential "provide-require-expansion-structure require"
  (destructuring-bind (form expected-inner-op) (list '(require :my-lib) 'unless)
    (let* ((result (our-macroexpand-1 form))
         (inner  (caddr result)))
    (expect (car result) :to-be 'let)
    (expect (car inner) :to-be expected-inner-op))))

)
