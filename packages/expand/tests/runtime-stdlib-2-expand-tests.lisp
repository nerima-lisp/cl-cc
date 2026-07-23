(in-package :cl-cc/test)



(it-sequential "runtime-stdlib-2-expand-system-loads"
  :timeout
  30
  (expect (asdf:find-system :cl-cc-expand nil) :to-be-truthy))

(it-sequential "runtime-stdlib-2-delay-force-caches-nil"
  :timeout
  10
  (let* ((calls 0)
         (promise (cl-cc/expand::%make-promise (lambda () (incf calls) nil) nil nil)))
    (expect (cl-cc/expand:promisep promise) :to-be-truthy)
    (expect (cl-cc/expand:force promise) :to-be-null)
    (expect (cl-cc/expand:force promise) :to-be-null)
    (expect (= 1 calls) :to-be-truthy)))

(it-sequential "runtime-stdlib-2-memoize-stats-and-clear"
  :timeout
  10
  (let* ((calls 0)
         (fn (cl-cc/expand:memoize (lambda (x) (incf calls) (* x x)))))
    (expect (= 9 (funcall fn 3)) :to-be-truthy)
    (expect (= 9 (funcall fn 3)) :to-be-truthy)
    (let ((stats (cl-cc/expand:memoize-stats fn)))
      (expect (= 1 (getf stats :hits)) :to-be-truthy)
      (expect (= 1 (getf stats :misses)) :to-be-truthy)
      (expect (= 1 (getf stats :size)) :to-be-truthy))
    (expect (cl-cc/expand:memoize-clear fn) :to-be-truthy)
    (expect (= 9 (funcall fn 3)) :to-be-truthy)
    (expect (= 2 calls) :to-be-truthy)))

(it-sequential "runtime-stdlib-2-quasiquote-folds-static-splice-nil"
  :timeout
  10
  (expect (cl-cc/expand:our-macroexpand-all '(backquote (a b (unquote-splicing nil) c))) :to-equal (cl-cc/expand:our-macroexpand-all '(backquote (a b c)))))

(it-sequential "runtime-stdlib-2-quasiquote-single-splice-copy-list"
  :timeout
  10
  (expect (cl-cc/expand:our-macroexpand-all '(backquote ((unquote-splicing xs)))) :to-equal '(copy-list xs)))
