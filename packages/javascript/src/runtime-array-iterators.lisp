;;;; packages/javascript/src/runtime-array-iterators.lisp — JS Array iterator helpers
;;;;
;;;; Load order: after runtime-array-core.lisp.

(in-package :cl-cc/javascript)

(defmacro define-js-array-iterator (name docstring value-form)
  `(defun ,name (arr)
     ,docstring
     (let ((i 0)
           (n (length arr)))
       (%js-make-cl-iterator
        (lambda ()
          (if (>= i n)
              :done
              (let ((value ,value-form))
                (incf i)
                (cons value nil))))))))

(define-js-array-iterator %js-array-entries
  "Return an Array Iterator yielding [index, value] pairs."
  (%js-make-array i (aref arr i)))

(define-js-array-iterator %js-array-keys
  "Return an Array Iterator yielding indices."
  i)
