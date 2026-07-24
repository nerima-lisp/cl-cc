;;;; packages/javascript/tests/js-runtime-collections-tests.lisp
;;;;
;;;; Set, Iterator, Promise, Map, Generator, BigInt, URI/base64,
;;;; and AggregateError/WeakRef runtime tests.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list, %jr-set)

(in-package :cl-cc/test)

;;; ─── Set built-ins ───────────────────────────────────────────────────────────

(defun %jr-set-like (&rest values)
  (let ((entries (make-array (length values)
                             :element-type t
                             :adjustable t
                             :fill-pointer (length values)
                             :initial-contents values)))
    (cl-cc/javascript::%js-make-object
     "size" (length values)
     "has" (lambda (candidate &rest _)
             (declare (ignore _))
             (loop for value across entries
                   thereis (cl-cc/javascript::%js-same-value-zero value candidate)))
     "keys" (lambda (&rest _)
              (declare (ignore _))
              entries))))

(defun %jr-assert-set-has-all (set expected-values)
  (expect (= (length expected-values) (cl-cc/javascript::%js-set-size set)) :to-be-truthy)
  (dolist (value expected-values)
    (expect (cl-cc/javascript::%js-set-has set value) :to-be-truthy)))

(defun %jr-assert-set-keys (expected-values set)
  (expect (%jr-list (cl-cc/javascript::%js-set-keys set)) :to-equal expected-values))

(it-sequential "js-rt-set-basic"
  (let ((s (%jr-set 1 2 3)))
    (expect (= 3 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s 2) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s 9) :to-be-falsy)
    (cl-cc/javascript::%js-set-delete s 2)
    (expect (= 2 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)
    (cl-cc/javascript::%js-set-clear s)
    (expect (= 0 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)))

(it-sequential "js-rt-set-same-value-zero-values"
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+)
        (s (cl-cc/javascript::%js-make-set)))
    (cl-cc/javascript::%js-set-add s nan-a)
    (expect (cl-cc/javascript::%js-set-has s nan-b) :to-be-truthy)
    (cl-cc/javascript::%js-set-add s nan-b)
    (expect (= 1 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-delete s nan-b) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)
    (cl-cc/javascript::%js-set-add s 0.0d0)
    (cl-cc/javascript::%js-set-add s -0.0d0)
    (expect (= 1 (cl-cc/javascript::%js-set-size s)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s 0.0d0) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s -0.0d0) :to-be-truthy)
    (let ((with-nan (%jr-set nan-b 1)))
      (cl-cc/javascript::%js-set-add s nan-a)
      (expect (cl-cc/javascript::%js-set-is-disjoint-from s with-nan) :to-be-falsy)
      (expect (cl-cc/javascript::%js-set-is-superset-of s (%jr-set nan-b -0.0d0)) :to-be-truthy))))

(it-sequential "js-rt-set-union"
  (let* ((a (%jr-set 1 2))
         (b (%jr-set 2 3))
         (u (cl-cc/javascript::%js-set-union a b)))
    (%jr-assert-set-has-all u '(1 2 3))))

(it-sequential "js-rt-set-union-set-like"
  (let* ((a (%jr-set 1 2))
         (b (%jr-set-like 2 3))
         (u (cl-cc/javascript::%js-set-union a b)))
    (%jr-assert-set-keys '(1 2 3) u)))

(it-sequential "js-rt-set-intersection"
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set 2 3 4))
         (i (cl-cc/javascript::%js-set-intersection a b)))
    (%jr-assert-set-has-all i '(2 3))
    (expect (cl-cc/javascript::%js-set-has i 1) :to-be-falsy)))

(it-sequential "js-rt-set-difference"
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set 2))
         (d (cl-cc/javascript::%js-set-difference a b)))
    (%jr-assert-set-has-all d '(1 3))
    (expect (cl-cc/javascript::%js-set-has d 2) :to-be-falsy)))

(it-sequential "js-rt-set-filter-ops-set-like"
  (let* ((a (%jr-set 1 2 3))
         (b (%jr-set-like 2 4))
         (intersection (cl-cc/javascript::%js-set-intersection a b))
         (difference (cl-cc/javascript::%js-set-difference a b))
         (symmetric (cl-cc/javascript::%js-set-symmetric-difference a b)))
    (%jr-assert-set-keys '(2) intersection)
    (%jr-assert-set-keys '(1 3) difference)
    (%jr-assert-set-keys '(1 3 4) symmetric)))

(it-sequential "js-rt-set-subset-disjoint"
  (let ((a (%jr-set 1 2))
        (b (%jr-set 1 2 3))
        (c (%jr-set 4 5)))
    (expect (cl-cc/javascript::%js-set-is-subset-of   a b) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-is-subset-of   b a) :to-be-falsy)
    (expect (cl-cc/javascript::%js-set-is-disjoint-from a c) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-is-disjoint-from a b) :to-be-falsy)))

(it-sequential "js-rt-set-predicates-set-like"
  (let ((a (%jr-set 1 2))
        (b (%jr-set-like 1 2 3))
        (c (%jr-set 1 2 3))
        (d (%jr-set-like 2 3))
        (e (%jr-set-like 4 5))
        (f (%jr-set-like 2 5)))
    (expect (cl-cc/javascript::%js-set-is-subset-of a b) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-is-superset-of c d) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-is-disjoint-from a e) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-is-disjoint-from a f) :to-be-falsy)))

;;; ─── Iterator helpers ────────────────────────────────────────────────────────

(it-sequential "js-rt-iterator-map-filter"
  (let* ((iter  (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3 4)))
         (evens (cl-cc/javascript::%js-iterator-filter
                 iter (lambda (x &rest _) (declare (ignore _)) (evenp x))))
         (doubled (cl-cc/javascript::%js-iterator-map
                   (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3))
                   (lambda (x &rest _) (declare (ignore _)) (* x 2)))))
    (expect (%jr-list (cl-cc/javascript::%js-iterator-to-array evens)) :to-equal '(2 4))
    (expect (%jr-list (cl-cc/javascript::%js-iterator-to-array doubled)) :to-equal '(2 4 6))))

(it-sequential "js-rt-iterator-take-drop"
  (let* ((src (%jr-arr 10 20 30 40 50))
         (taken (cl-cc/javascript::%js-iterator-to-array
                 (cl-cc/javascript::%js-iterator-take
                  (cl-cc/javascript::%js-vec-to-iter src) 3)))
         (dropped (cl-cc/javascript::%js-iterator-to-array
                   (cl-cc/javascript::%js-iterator-drop
                    (cl-cc/javascript::%js-vec-to-iter src) 2))))
    (expect (%jr-list taken) :to-equal '(10 20 30))
    (expect (%jr-list dropped) :to-equal '(30 40 50))))

(it-sequential "js-rt-iterator-reduce"
  (let* ((iter (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3 4)))
         (sum  (cl-cc/javascript::%js-iterator-reduce
                iter (lambda (a b &rest _) (declare (ignore _)) (+ a b)) 0)))
    (expect (= 10 sum) :to-be-truthy)))

(it-sequential "js-rt-iterator-some-every some-found"
  (destructuring-bind (fn vals pred expected) (list #'cl-cc/javascript::%js-iterator-some '(1 3 4) #'evenp t)
    (let ((iter (cl-cc/javascript::%js-vec-to-iter (apply #'%jr-arr vals))))
    (expect (funcall fn iter (lambda (x &rest _) (declare (ignore _)) (funcall pred x))) :to-equal expected))))

(it-sequential "js-rt-iterator-some-every some-not"
  (destructuring-bind (fn vals pred expected) (list #'cl-cc/javascript::%js-iterator-some '(1 3 5) #'evenp nil)
    (let ((iter (cl-cc/javascript::%js-vec-to-iter (apply #'%jr-arr vals))))
    (expect (funcall fn iter (lambda (x &rest _) (declare (ignore _)) (funcall pred x))) :to-equal expected))))

(it-sequential "js-rt-iterator-some-every every-true"
  (destructuring-bind (fn vals pred expected) (list #'cl-cc/javascript::%js-iterator-every '(2 4 6) #'evenp t)
    (let ((iter (cl-cc/javascript::%js-vec-to-iter (apply #'%jr-arr vals))))
    (expect (funcall fn iter (lambda (x &rest _) (declare (ignore _)) (funcall pred x))) :to-equal expected))))

(it-sequential "js-rt-iterator-some-every every-false"
  (destructuring-bind (fn vals pred expected) (list #'cl-cc/javascript::%js-iterator-every '(2 3 6) #'evenp nil)
    (let ((iter (cl-cc/javascript::%js-vec-to-iter (apply #'%jr-arr vals))))
    (expect (funcall fn iter (lambda (x &rest _) (declare (ignore _)) (funcall pred x))) :to-equal expected))))

(it-sequential "js-rt-iterator-find"
  (let* ((found    (cl-cc/javascript::%js-iterator-find
                    (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 3 4 5))
                    (lambda (x &rest _) (declare (ignore _)) (evenp x))))
         (not-found (cl-cc/javascript::%js-iterator-find
                     (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 3 5))
                     (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (expect (= 4 found) :to-be-truthy)
    (expect not-found :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-iterator-for-each"
  (let ((seen nil))
    (cl-cc/javascript::%js-iterator-for-each
     (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10 20 30))
     (lambda (x &rest _) (declare (ignore _)) (push x seen)))
    (expect (nreverse seen) :to-equal '(10 20 30))))

(it-sequential "js-rt-iterator-flat-map"
  (let* ((iter   (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3)))
         (result (cl-cc/javascript::%js-iterator-to-array
                  (cl-cc/javascript::%js-iterator-flat-map
                   iter
                   (lambda (x &rest _) (declare (ignore _)) (%jr-arr x (* x 10)))))))
    (expect (%jr-list result) :to-equal '(1 10 2 20 3 30))))

(it-sequential "js-rt-iterator-concat"
  (let* ((result (cl-cc/javascript::%js-iterator-to-array
                  (cl-cc/javascript::%js-iterator-concat
                   (%jr-arr 1 2)
                   "ab"
                   (%jr-set 9))))
         (values (%jr-list result)))
    (expect values :to-equal '(1 2 "a" "b" 9))))

(it-sequential "js-rt-iterator-concat-empty"
  (multiple-value-bind (value done)
      (cl-cc/javascript::%js-iter-next (cl-cc/javascript::%js-iterator-concat))
    (declare (ignore value))
    (expect done :to-be-truthy)))

(it-sequential "js-rt-iterator-zip"
  (let* ((rows (cl-cc/javascript::%js-iterator-to-array
                (cl-cc/javascript::%js-iterator-zip
                 (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3))
                           (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10 20))))))
         (lists (mapcar #'%jr-list (%jr-list rows))))
    (expect lists :to-equal '((1 10) (2 20)))))

(it-sequential "js-rt-iterator-zip-longest"
  (let* ((rows (cl-cc/javascript::%js-iterator-to-array
                (cl-cc/javascript::%js-iterator-zip
                 (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2 3))
                          (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10 20)))
                 (cl-cc/javascript::%js-make-object
                  "mode" "longest"
                  "padding" (%jr-arr 0 9)))))
         (lists (mapcar #'%jr-list (%jr-list rows))))
    (expect lists :to-equal '((1 10) (2 20) (3 9)))))

(it-sequential "js-rt-iterator-zip-longest-default-padding"
  (let* ((rows (cl-cc/javascript::%js-iterator-to-array
                (cl-cc/javascript::%js-iterator-zip
                 (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2))
                          (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10)))
                 (cl-cc/javascript::%js-make-object "mode" "longest"))))
         (first (aref rows 0))
         (second (aref rows 1)))
    (expect (= 1 (aref first 0)) :to-be-truthy)
    (expect (= 10 (aref first 1)) :to-be-truthy)
    (expect (= 2 (aref second 0)) :to-be-truthy)
    (expect (aref second 1) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-iterator-zip-invalid-options"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/javascript::%js-iterator-zip
     (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1)))
     42.0d0)) (cl-cc/javascript:js-exception () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "js-rt-iterator-zip-invalid-mode"
  (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::%js-iterator-zip
     (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1)))
     (cl-cc/javascript::%js-make-object "mode" "middle"))) (cl-cc/javascript:js-exception () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy)))

(it-sequential "js-rt-iterator-zip-primitive-padding"
  (let ((%%signaled3 nil)) (handler-case (progn (cl-cc/javascript::%js-iterator-zip
     (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2))
              (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10)))
     (cl-cc/javascript::%js-make-object
      "mode" "longest"
      "padding" 1.0d0))) (cl-cc/javascript:js-exception () (setf %%signaled3 t))) (expect %%signaled3 :to-be-truthy)))

(it-sequential "js-rt-iterator-zip-strict-signals"
  (let ((iter (cl-cc/javascript::%js-iterator-zip
               (%jr-arr (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2))
                        (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10)))
               (cl-cc/javascript::%js-make-object "mode" "strict"))))
    (multiple-value-bind (row done) (cl-cc/javascript::%js-iter-next iter)
      (declare (ignore row))
      (expect done :to-be-falsy))
    (let ((%%signaled4 nil)) (handler-case (progn (cl-cc/javascript::%js-iter-next iter)) (cl-cc/javascript:js-exception () (setf %%signaled4 t))) (expect %%signaled4 :to-be-truthy))))

(it-sequential "js-rt-iterator-zip-keyed"
  (let* ((sources (cl-cc/javascript::%js-make-object
                   "left" (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2))
                   "right" (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10))))
         (rows (cl-cc/javascript::%js-iterator-to-array
                (cl-cc/javascript::%js-iterator-zip-keyed
                 sources
                 (cl-cc/javascript::%js-make-object
                  "mode" "longest"
                  "padding" (cl-cc/javascript::%js-make-object "left" 0 "right" 9)))))
         (first (aref rows 0))
         (second (aref rows 1)))
    (expect (cl-cc/javascript::%js-object-get-prototype-of first) :to-be cl-cc/javascript::+js-null+)
    (expect (= 1 (gethash "left" first)) :to-be-truthy)
    (expect (= 10 (gethash "right" first)) :to-be-truthy)
    (expect (= 2 (gethash "left" second)) :to-be-truthy)
    (expect (= 9 (gethash "right" second)) :to-be-truthy)))

(it-sequential "js-rt-iterator-zip-keyed-longest-missing-padding"
  (let* ((sources (cl-cc/javascript::%js-make-object
                   "left" (cl-cc/javascript::%js-vec-to-iter (%jr-arr 1 2))
                   "right" (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10))))
         (rows (cl-cc/javascript::%js-iterator-to-array
                (cl-cc/javascript::%js-iterator-zip-keyed
                 sources
                 (cl-cc/javascript::%js-make-object
                  "mode" "longest"
                  "padding" (cl-cc/javascript::%js-make-object "left" 0)))))
         (first (aref rows 0))
         (second (aref rows 1)))
    (expect (= 1 (gethash "left" first)) :to-be-truthy)
    (expect (= 10 (gethash "right" first)) :to-be-truthy)
    (expect (= 2 (gethash "left" second)) :to-be-truthy)
    (expect (gethash "right" second) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-iterator-zip-keyed-undefined-source"
  (let ((%%signaled5 nil)) (handler-case (progn (cl-cc/javascript::%js-iterator-zip-keyed
     (cl-cc/javascript::%js-make-object
      "left" cl-cc/javascript::+js-undefined+
      "right" (cl-cc/javascript::%js-vec-to-iter (%jr-arr 10)))
     (cl-cc/javascript::%js-make-object "mode" "shortest"))) (cl-cc/javascript:js-exception () (setf %%signaled5 t))) (expect %%signaled5 :to-be-truthy)))

(it-sequential "js-rt-iterator-zip-keyed-non-object"
  (let ((%%signaled6 nil)) (handler-case (progn (cl-cc/javascript::%js-iterator-zip-keyed
     42.0d0
     (cl-cc/javascript::%js-make-object "mode" "shortest"))) (cl-cc/javascript:js-exception () (setf %%signaled6 t))) (expect %%signaled6 :to-be-truthy)))

;;; ─── Promise built-ins ───────────────────────────────────────────────────────

(it-sequential "js-rt-promise-resolve-await"
  (let* ((p (cl-cc/javascript::%js-promise-resolve 42))
         (v (cl-cc/javascript::%js-await p)))
    (expect (= 42 v) :to-be-truthy)))

(it-sequential "js-rt-promise-reject-await"
  (let ((p (cl-cc/javascript::%js-promise-reject "oops")))
    (let ((%%signaled7 nil)) (handler-case (progn (cl-cc/javascript::%js-await p)) (cl-cc/javascript:js-exception () (setf %%signaled7 t))) (expect %%signaled7 :to-be-truthy))))

(it-sequential "js-rt-promise-then"
  (let* ((p (cl-cc/javascript::%js-promise-resolve 5))
         (p2 (cl-cc/javascript::%js-promise-then
              p (lambda (v &rest _) (declare (ignore _)) (* v 2)))))
    (expect (= 10 (cl-cc/javascript::%js-await p2)) :to-be-truthy)))

(it-sequential "js-rt-promise-all"
  (let* ((promises (%jr-arr (cl-cc/javascript::%js-promise-resolve 1)
                            (cl-cc/javascript::%js-promise-resolve 2)
                            (cl-cc/javascript::%js-promise-resolve 3)))
         (result (cl-cc/javascript::%js-await
                  (cl-cc/javascript::%js-promise-all promises))))
    (expect (%jr-list result) :to-equal '(1 2 3))))

(it-sequential "js-rt-promise-any-first-fulfilled"
  (let* ((p1 (cl-cc/javascript::%js-promise-reject "e1"))
         (p2 (cl-cc/javascript::%js-promise-resolve 42))
         (arr (%jr-arr p1 p2))
         (r   (cl-cc/javascript::%js-promise-any arr)))
    (expect (cl-cc/javascript::js-promise-rejected-p r) :to-be-falsy)
    (expect (= 42 (cl-cc/javascript::js-promise-value r)) :to-be-truthy)))

(it-sequential "js-rt-promise-any-all-rejected"
  (let* ((p1 (cl-cc/javascript::%js-promise-reject "e1"))
         (p2 (cl-cc/javascript::%js-promise-reject "e2"))
         (arr (%jr-arr p1 p2))
         (r   (cl-cc/javascript::%js-promise-any arr)))
    (expect (cl-cc/javascript::js-promise-rejected-p r) :to-be-truthy)))

(it-sequential "js-rt-promise-with-resolvers"
  (let* ((trio    (cl-cc/javascript::%js-promise-with-resolvers))
         (promise (gethash "promise" trio))
         (resolve (gethash "resolve" trio))
         (reject  (gethash "reject"  trio)))
    (expect (cl-cc/javascript::js-promise-p promise) :to-be-truthy)
    (expect (functionp resolve) :to-be-truthy)
    (expect (functionp reject) :to-be-truthy)
    (funcall resolve 99)
    (expect (= 99 (cl-cc/javascript::js-promise-value promise)) :to-be-truthy)))

;;; ─── Map built-ins ───────────────────────────────────────────────────────────

(it-sequential "js-rt-map-set-get-has-size"
  (let ((m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m "k" 42)
    (expect (= 1 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-map-has m "k") :to-be-truthy)
    (expect (cl-cc/javascript::%js-map-has m "x") :to-be-falsy)
    (expect (= 42 (cl-cc/javascript::%js-map-get m "k")) :to-be-truthy)
    (cl-cc/javascript::%js-map-delete m "k")
    (expect (= 0 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)))

(it-sequential "js-rt-map-same-value-zero-keys"
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+)
        (m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m nan-a "first")
    (expect (cl-cc/javascript::%js-map-has m nan-b) :to-be-truthy)
    (expect (cl-cc/javascript::%js-map-get m nan-b) :to-equal "first")
    (cl-cc/javascript::%js-map-set m nan-b "second")
    (expect (= 1 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-map-get m nan-a) :to-equal "second")
    (expect (cl-cc/javascript::%js-map-delete m nan-b) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)
    (cl-cc/javascript::%js-map-set m 0.0d0 "zero")
    (cl-cc/javascript::%js-map-set m -0.0d0 "neg-zero")
    (expect (= 1 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-map-get m 0.0d0) :to-equal "neg-zero")
    (expect (cl-cc/javascript::%js-map-get m -0.0d0) :to-equal "neg-zero")))

(it-sequential "js-rt-map-for-each-order"
  (let ((m    (cl-cc/javascript::%js-make-map))
        (seen nil))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-set m "b" 2)
    (cl-cc/javascript::%js-map-set m "c" 3)
    (cl-cc/javascript::%js-map-for-each m
      (lambda (v k &rest _) (declare (ignore _)) (push (cons k v) seen)))
    (expect (nreverse seen) :to-equal '(("a" . 1) ("b" . 2) ("c" . 3)))))

(it-sequential "js-rt-map-clear"
  (let ((m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-clear m)
    (expect (= 0 (cl-cc/javascript::%js-map-size m)) :to-be-truthy)))

(it-sequential "js-rt-map-get-or-insert"
  (let ((m (cl-cc/javascript::%js-make-map)))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (expect (= 1 (cl-cc/javascript::%js-map-get-or-insert m "a" 99)) :to-be-truthy)
    (expect (= 1 (cl-cc/javascript::%js-map-get m "a")) :to-be-truthy)
    (expect (= 7 (cl-cc/javascript::%js-map-get-or-insert m "b" 7)) :to-be-truthy)
    (expect (= 7 (cl-cc/javascript::%js-map-get m "b")) :to-be-truthy)))

(it-sequential "js-rt-map-get-or-insert-computed"
  (let ((m (cl-cc/javascript::%js-make-map))
        (calls 0))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (expect (= 1 (cl-cc/javascript::%js-map-get-or-insert-computed
               m "a"
               (lambda (k)
                 (declare (ignore k))
                 (incf calls)
                 99))) :to-be-truthy)
    (expect (= 0 calls) :to-be-truthy)
    (expect (= 10 (cl-cc/javascript::%js-map-get-or-insert-computed
               m "b"
               (lambda (k)
                 (declare (ignore k))
                 (incf calls)
                 10))) :to-be-truthy)
    (expect (= 1 calls) :to-be-truthy)
    (expect (= 10 (cl-cc/javascript::%js-map-get m "b")) :to-be-truthy)))

(it-sequential "js-rt-map-get-or-insert-computed-reentrant"
  (let ((m (cl-cc/javascript::%js-make-map))
        (calls 0))
    (expect (= 20 (cl-cc/javascript::%js-map-get-or-insert-computed
               m "a"
               (lambda (k)
                 (incf calls)
                 (cl-cc/javascript::%js-map-set m k 20)
                 99))) :to-be-truthy)
    (expect (= 1 calls) :to-be-truthy)
    (expect (= 20 (cl-cc/javascript::%js-map-get m "a")) :to-be-truthy)))

(it-sequential "js-rt-map-group-by"
  (let* ((arr (%jr-arr 1 2 3 4 6))
         (result (cl-cc/javascript::%js-map-group-by
                  arr
                  (lambda (x) (svref #("even" "odd") (mod x 2)))))
         (evens (cl-cc/javascript::%js-map-get result "even"))
         (odds  (cl-cc/javascript::%js-map-get result "odd")))
    (expect (= 3 (length evens)) :to-be-truthy)
    (expect (= 2 (length odds)) :to-be-truthy)))

;;; ─── Generator / yield ───────────────────────────────────────────────────────

(it-sequential "js-rt-generator-basic"
  (let* ((gen (cl-cc/javascript::%js-make-generator
               (lambda ()
                 (cl-cc/javascript::%js-yield 10)
                 (cl-cc/javascript::%js-yield 20)
                 (cl-cc/javascript::%js-yield 30))))
         (arr (cl-cc/javascript::%js-iterator-to-array gen)))
    (expect (%jr-list arr) :to-equal '(10 20 30))))

(it-sequential "js-rt-generator-done-after-exhaust"
  (let* ((gen  (cl-cc/javascript::%js-make-generator
                (lambda () (cl-cc/javascript::%js-yield 1))))
         (r1   (cl-cc/javascript::%js-generator-next gen))
         (r2   (cl-cc/javascript::%js-generator-next gen)))
    (expect (cl-cc/javascript::%js-get-prop r1 "done") :to-be-falsy)
    (expect (cl-cc/javascript::%js-get-prop r2 "done") :to-be-truthy)))

;;; ─── BigInt operations ───────────────────────────────────────────────────────

(it-sequential "js-rt-bigint-val bigint-pos"
  (destructuring-bind (raw expected) (list 42 42)
    (let ((bi (cl-cc/javascript::%make-js-bigint raw)))
    (expect (= expected (cl-cc/javascript::%js-bigint-val bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-val bigint-neg"
  (destructuring-bind (raw expected) (list -7 -7)
    (let ((bi (cl-cc/javascript::%make-js-bigint raw)))
    (expect (= expected (cl-cc/javascript::%js-bigint-val bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-val plain-int"
  (destructuring-bind (raw expected) (list 10 10)
    (let ((bi (cl-cc/javascript::%make-js-bigint raw)))
    (expect (= expected (cl-cc/javascript::%js-bigint-val bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-val float"
  (destructuring-bind (raw expected) (list 3 3)
    (let ((bi (cl-cc/javascript::%make-js-bigint raw)))
    (expect (= expected (cl-cc/javascript::%js-bigint-val bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic add"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-add 3 4 7)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic sub"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-sub 9 4 5)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic mul"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-mul 3 4 12)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic pow"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-pow 2 8 256)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic band"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-bitwise-and #b1010 #b1100 #b1000)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic bor"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-bitwise-or #b1010 #b1100 #b1110)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-arithmetic bxor"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-bitwise-xor #b1010 #b1100 #b0110)
    (let ((result (funcall fn (cl-cc/javascript::%make-js-bigint a)
                            (cl-cc/javascript::%make-js-bigint b))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-as-int-n-uint-n int-n-positive"
  (destructuring-bind (fn width val expected) (list #'cl-cc/javascript::%js-bigint-as-int-n 8 127 127)
    (let ((result (funcall fn width (cl-cc/javascript::%make-js-bigint val))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-as-int-n-uint-n int-n-wrap"
  (destructuring-bind (fn width val expected) (list #'cl-cc/javascript::%js-bigint-as-int-n 8 128 -128)
    (let ((result (funcall fn width (cl-cc/javascript::%make-js-bigint val))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-as-int-n-uint-n uint-n"
  (destructuring-bind (fn width val expected) (list #'cl-cc/javascript::%js-bigint-as-uint-n 8 300 44)
    (let ((result (funcall fn width (cl-cc/javascript::%make-js-bigint val))))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

;;; ─── URI encoding / base64 ───────────────────────────────────────────────────

(it-sequential "js-rt-encode-uri-component space"
  (destructuring-bind (s expected) (list "hello world" "hello%20world")
    (expect (cl-cc/javascript::%js-encode-uri-component s) :to-equal expected)))

(it-sequential "js-rt-encode-uri-component slash"
  (destructuring-bind (s expected) (list "a/b" "a%2Fb")
    (expect (cl-cc/javascript::%js-encode-uri-component s) :to-equal expected)))

(it-sequential "js-rt-encode-uri-component plain"
  (destructuring-bind (s expected) (list "abc123" "abc123")
    (expect (cl-cc/javascript::%js-encode-uri-component s) :to-equal expected)))

(it-sequential "js-rt-decode-uri-component"
  (expect (cl-cc/javascript::%js-decode-uri-component "hello%20world") :to-equal "hello world"))

(it-sequential "js-rt-btoa-atob-roundtrip"
  (let* ((s "Hello, World!")
         (encoded (cl-cc/javascript::%js-btoa s))
         (decoded (cl-cc/javascript::%js-atob encoded)))
    (expect decoded :to-equal s)))

;;; ─── AggregateError / WeakRef / RegExp.escape ────────────────────────────────

(it-sequential "js-rt-aggregate-error-make"
  (let* ((errs (%jr-arr "e1" "e2"))
         (obj  (cl-cc/javascript::%js-make-aggregate-error errs "some errors")))
    (expect (gethash "message" obj) :to-equal "some errors")
    (expect (gethash "name"    obj) :to-equal "AggregateError")
    (expect (gethash "errors"  obj) :to-be errs)
    (expect (cl-cc/javascript::%js-instanceof
                  obj cl-cc/javascript::*js-aggregate-error-class*) :to-be-truthy)))

(it-sequential "js-rt-weak-ref-make-deref"
  (let* ((target   (cl-cc/javascript::%js-make-object "k" 1))
         (wr       (cl-cc/javascript::%js-make-weak-ref target))
         (dereffed (cl-cc/javascript::%js-weak-ref-deref wr)))
    (expect dereffed :to-be target)))

(it-sequential "js-rt-weak-ref-deref-method"
  (let* ((target (cl-cc/javascript::%js-make-object "k" 1))
         (wr     (cl-cc/javascript::%js-make-weak-ref target))
         (deref  (cl-cc/javascript::%js-get-prop wr "deref")))
    (expect (funcall deref) :to-be target)))

(it-sequential "js-rt-regexp-escape"
  (expect (cl-cc/javascript::%js-regexp-escape "a.b+c?") :to-equal "\\x61\\.b\\+c\\?")
  (expect (cl-cc/javascript::%js-regexp-escape "foo-bar") :to-equal "\\x66oo\\x2Dbar")
  (expect (cl-cc/javascript::%js-regexp-escape (format nil " ~%~C" #\Tab)) :to-equal "\\x20\\n\\t"))

;;; ─── Map iterators — keys / values / entries ─────────────────────────────────

(it-sequential "js-rt-map-keys-iterator"
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "a" 1)
    (cl-cc/javascript::%js-map-set m "b" 2)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-keys m)
     (lambda (k) (push k acc)))
    (expect acc :to-equal '("b" "a"))))

(it-sequential "js-rt-map-values-iterator"
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "x" 10)
    (cl-cc/javascript::%js-map-set m "y" 20)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-values m)
     (lambda (v) (push v acc)))
    (expect acc :to-equal '(20 10))))

(it-sequential "js-rt-map-entries-iterator"
  (let* ((m   (cl-cc/javascript::%js-make-map))
         (acc nil))
    (cl-cc/javascript::%js-map-set m "k" 99)
    (cl-cc/javascript::%js-for-of
     (cl-cc/javascript::%js-map-entries m)
     (lambda (e) (push (list (aref e 0) (aref e 1)) acc)))
    (expect acc :to-equal '(("k" 99)))))

;;; ─── WeakMap ─────────────────────────────────────────────────────────────────

(it-sequential "js-rt-weak-map-lifecycle"
  (let* ((wm  (cl-cc/javascript::%js-make-weak-map))
         (key (cl-cc/javascript::%js-make-object "x" 1)))
    (expect (cl-cc/javascript::%js-weak-map-p wm) :to-be-truthy)
    (cl-cc/javascript::%js-weak-map-set wm key 42)
    (expect (= 42 (cl-cc/javascript::%js-weak-map-get wm key)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-weak-map-has wm key) :to-be-truthy)
    (expect (cl-cc/javascript::%js-weak-map-delete wm key) :to-be-truthy)
    (expect (cl-cc/javascript::%js-weak-map-has wm key) :to-be-falsy)
    (expect (cl-cc/javascript::%js-weak-map-get wm key) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-weak-map-get-or-insert"
  (let* ((wm  (cl-cc/javascript::%js-make-weak-map))
         (key (cl-cc/javascript::%js-make-object "x" 1)))
    (cl-cc/javascript::%js-weak-map-set wm key 12)
    (expect (= 12 (cl-cc/javascript::%js-weak-map-get-or-insert wm key 99)) :to-be-truthy)
    (expect (= 12 (cl-cc/javascript::%js-weak-map-get wm key)) :to-be-truthy)))

(it-sequential "js-rt-weak-map-get-or-insert-computed"
  (let* ((wm  (cl-cc/javascript::%js-make-weak-map))
         (key (cl-cc/javascript::%js-make-object "x" 1))
         (calls 0))
    (cl-cc/javascript::%js-weak-map-set wm key 3)
    (expect (= 3 (cl-cc/javascript::%js-weak-map-get-or-insert-computed
               wm key
               (lambda (k)
                 (declare (ignore k))
                 (incf calls)
                 99))) :to-be-truthy)
    (expect (= 0 calls) :to-be-truthy)
    (let ((other-key (cl-cc/javascript::%js-make-object "y" 2)))
      (expect (= 10 (cl-cc/javascript::%js-weak-map-get-or-insert-computed
                 wm other-key
                 (lambda (k)
                   (declare (ignore k))
                   (incf calls)
                   10))) :to-be-truthy)
      (expect (= 1 calls) :to-be-truthy)
      (expect (= 10 (cl-cc/javascript::%js-weak-map-get wm other-key)) :to-be-truthy))))

(it-sequential "js-rt-weak-map-get-or-insert-computed-reentrant"
  (let* ((wm  (cl-cc/javascript::%js-make-weak-map))
         (key (cl-cc/javascript::%js-make-object "x" 1))
         (calls 0))
    (expect (= 20 (cl-cc/javascript::%js-weak-map-get-or-insert-computed
               wm key
               (lambda (k)
                 (incf calls)
                 (cl-cc/javascript::%js-weak-map-set wm k 20)
                 99))) :to-be-truthy)
    (expect (= 1 calls) :to-be-truthy)
    (expect (= 20 (cl-cc/javascript::%js-weak-map-get wm key)) :to-be-truthy)))

;;; ─── WeakSet ─────────────────────────────────────────────────────────────────

(it-sequential "js-rt-weak-set-lifecycle"
  (let* ((ws  (cl-cc/javascript::%js-make-weak-set))
         (obj (cl-cc/javascript::%js-make-object "y" 2)))
    (expect (cl-cc/javascript::%js-weak-set-p ws) :to-be-truthy)
    (cl-cc/javascript::%js-weak-set-add ws obj)
    (expect (cl-cc/javascript::%js-weak-set-has ws obj) :to-be-truthy)
    (expect (cl-cc/javascript::%js-weak-set-has ws (cl-cc/javascript::%js-make-object)) :to-be-falsy)
    (cl-cc/javascript::%js-weak-set-delete ws obj)
    (expect (cl-cc/javascript::%js-weak-set-has ws obj) :to-be-falsy)))

;;; ─── FinalizationRegistry ────────────────────────────────────────────────────

(it-sequential "js-rt-finalization-registry-register-unregister"
  (let* ((reg    (cl-cc/javascript::%js-make-finalization-registry (lambda (hv) (declare (ignore hv)))))
         (tgt    (cl-cc/javascript::%js-make-object))
         (token  (cl-cc/javascript::%js-make-object "token" t))
         (token2 (cl-cc/javascript::%js-make-object "token" 2)))
    (expect (cl-cc/javascript::%js-finreg-register reg tgt "held" token) :to-be cl-cc/javascript::+js-undefined+)
    (expect (cl-cc/javascript::%js-finreg-register reg tgt "held-again" token) :to-be cl-cc/javascript::+js-undefined+)
    (expect (cl-cc/javascript::%js-finreg-register reg tgt "held-without-token") :to-be cl-cc/javascript::+js-undefined+)
    (expect (cl-cc/javascript::%js-finreg-unregister reg token2) :to-be-falsy)
    (expect (cl-cc/javascript::%js-finreg-unregister reg token) :to-be-truthy)
    (expect (cl-cc/javascript::%js-finreg-unregister reg token) :to-be-falsy)))

(it-sequential "js-rt-finalization-registry-methods"
  (let* ((reg        (cl-cc/javascript::%js-make-finalization-registry (lambda (hv) (declare (ignore hv)))))
         (tgt        (cl-cc/javascript::%js-make-object))
         (token      (cl-cc/javascript::%js-make-object))
         (register   (cl-cc/javascript::%js-get-prop reg "register"))
         (unregister (cl-cc/javascript::%js-get-prop reg "unregister")))
    (expect (funcall register tgt "held" token) :to-be cl-cc/javascript::+js-undefined+)
    (expect (funcall unregister token) :to-be-truthy)
    (expect (funcall unregister token) :to-be-falsy)))
