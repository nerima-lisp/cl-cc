;;;; packages/javascript/tests/js-runtime-array-tests.lisp
;;;;
;;;; Array operations (push/pop/shift/map/filter/reduce/sort/flat/splice …),
;;;; ES2023 non-mutating variants (toReversed/toSorted/with/toSpliced),
;;;; and TypedArray construction and element access.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)

;;; ─── Array core ──────────────────────────────────────────────────────────────

(it-sequential "js-rt-array-make-and-index"
  (let ((a (%jr-arr 10 20 30)))
    (expect (= 3 (length a)) :to-be-truthy)
    (expect (= 10 (aref a 0)) :to-be-truthy)
    (expect (= 30 (aref a 2)) :to-be-truthy)))

(it-sequential "js-rt-array-push-pop"
  (let ((a (%jr-arr 1 2)))
    (expect (= 3 (cl-cc/javascript::%js-array-push a 3)) :to-be-truthy)
    (expect (= 3 (aref a 2)) :to-be-truthy)
    (expect (= 3 (cl-cc/javascript::%js-array-pop a)) :to-be-truthy)
    (expect (= 2 (length a)) :to-be-truthy)))

(it-sequential "js-rt-array-pop-empty-is-undefined"
  (expect (cl-cc/javascript::%js-array-pop (%jr-arr)) :to-be cl-cc/javascript::+js-undefined+))

(it-sequential "js-rt-array-map-filter"
  (let ((doubled (cl-cc/javascript::%js-array-map
                  (%jr-arr 1 2 3)
                  (lambda (x &rest _) (declare (ignore _)) (* x 2))))
        (evens   (cl-cc/javascript::%js-array-filter
                  (%jr-arr 1 2 3 4)
                  (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (expect (%jr-list doubled) :to-equal '(2 4 6))
    (expect (%jr-list evens) :to-equal '(2 4))))

(it-sequential "js-rt-array-reduce"
  (expect (= 10 (cl-cc/javascript::%js-array-reduce
             (%jr-arr 1 2 3 4)
             (lambda (acc x &rest _) (declare (ignore _)) (+ acc x))
             0)) :to-be-truthy))

(it-sequential "js-rt-array-includes found"
  (destructuring-bind (needle expected) (list 2 t)
    (expect (cl-cc/javascript::%js-array-includes (%jr-arr 1 2 3) needle) :to-equal expected)))

(it-sequential "js-rt-array-includes missing"
  (destructuring-bind (needle expected) (list 9 nil)
    (expect (cl-cc/javascript::%js-array-includes (%jr-arr 1 2 3) needle) :to-equal expected)))

(it-sequential "js-rt-array-includes-same-value-zero"
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+))
    (expect (cl-cc/javascript::%js-array-includes (%jr-arr nan-a) nan-b) :to-be-truthy)
    (expect (cl-cc/javascript::%js-array-includes (%jr-arr -0.0d0) 0.0d0) :to-be-truthy)))

(it-sequential "js-rt-array-index-of found"
  (destructuring-bind (needle expected) (list 6 1)
    (expect (= expected (cl-cc/javascript::%js-array-index-of (%jr-arr 5 6 7) needle)) :to-be-truthy)))

(it-sequential "js-rt-array-index-of missing"
  (destructuring-bind (needle expected) (list 9 -1)
    (expect (= expected (cl-cc/javascript::%js-array-index-of (%jr-arr 5 6 7) needle)) :to-be-truthy)))

(it-sequential "js-rt-array-index-of-does-not-match-nan"
  (let ((nan-a cl-cc/javascript::*js-nan-float*)
        (nan-b cl-cc/javascript::+js-nan+))
    (expect (= -1 (cl-cc/javascript::%js-array-index-of (%jr-arr nan-a) nan-b)) :to-be-truthy)
    (expect (= -1 (cl-cc/javascript::%js-array-last-index-of (%jr-arr nan-a) nan-b)) :to-be-truthy)))

(it-sequential "js-rt-array-join"
  (expect (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3)) :to-equal "1,2,3")
  (expect (cl-cc/javascript::%js-array-join (%jr-arr 1 2 3) "-") :to-equal "1-2-3"))

(it-sequential "js-rt-array-shift-unshift"
  (let ((a (%jr-arr 1 2 3)))
    (expect (= 1 (cl-cc/javascript::%js-array-shift a)) :to-be-truthy)
    (expect (%jr-list a) :to-equal '(2 3))
    (expect (= 4 (cl-cc/javascript::%js-array-unshift a 0 1)) :to-be-truthy)
    (expect (%jr-list a) :to-equal '(0 1 2 3))))

(it-sequential "js-rt-array-some-every"
  (let ((pred (lambda (x &rest _) (declare (ignore _)) (evenp x))))
    (expect (cl-cc/javascript::%js-array-some  (%jr-arr 1 2 3) pred) :to-be-truthy)
    (expect (cl-cc/javascript::%js-array-some  (%jr-arr 1 3 5) pred) :to-be-falsy)
    (expect (cl-cc/javascript::%js-array-every (%jr-arr 2 4 6) pred) :to-be-truthy)
    (expect (cl-cc/javascript::%js-array-every (%jr-arr 2 3 4) pred) :to-be-falsy)))

(it-sequential "js-rt-array-find-and-find-index"
  (let ((arr (%jr-arr 1 4 9 16)))
    (expect (= 4 (cl-cc/javascript::%js-array-find
                   arr (lambda (x &rest _) (declare (ignore _)) (> x 3)))) :to-be-truthy)
    (expect (= 1 (cl-cc/javascript::%js-array-find-index
                   arr (lambda (x &rest _) (declare (ignore _)) (> x 3)))) :to-be-truthy)
    (expect (cl-cc/javascript::%js-array-find
                arr (lambda (x &rest _) (declare (ignore _)) (> x 100))) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-array-slice"
  (let* ((a   (%jr-arr 10 20 30 40))
         (s1  (cl-cc/javascript::%js-array-slice a 1 3))
         (s2  (cl-cc/javascript::%js-array-slice a 2)))
    (expect (%jr-list s1) :to-equal '(20 30))
    (expect (%jr-list s2) :to-equal '(30 40))
    (expect (= 4 (length a)) :to-be-truthy)))    ; original untouched

(it-sequential "js-rt-array-slice-coerces-relative-indices"
  (let* ((a (%jr-arr 10 20 30 40))
         (r (cl-cc/javascript::%js-array-slice a "-3" -1.2d0)))
    (expect (%jr-list r) :to-equal '(20 30))))

(it-sequential "js-rt-array-splice-delete-insert"
  (let ((a (%jr-arr 1 2 3 4 5)))
    (cl-cc/javascript::%js-array-splice a 1 2 9 9)
    (expect (%jr-list a) :to-equal '(1 9 9 4 5))))

(it-sequential "js-rt-array-splice-coerces-indices"
  (let* ((coerced (%jr-arr 1 2 3 4))
         (omitted (%jr-arr 1 2 3 4))
         (undefined-count (%jr-arr 1 2 3 4))
         (removed (cl-cc/javascript::%js-array-splice coerced "-3" 1.9d0 9))
         (removed-tail (cl-cc/javascript::%js-array-splice omitted "2"))
         (removed-none (cl-cc/javascript::%js-array-splice
                        undefined-count 1 cl-cc/javascript::+js-undefined+ 9)))
    (expect (%jr-list removed) :to-equal '(2))
    (expect (%jr-list coerced) :to-equal '(1 9 3 4))
    (expect (%jr-list removed-tail) :to-equal '(3 4))
    (expect (%jr-list omitted) :to-equal '(1 2))
    (expect (%jr-list removed-none) :to-equal '())
    (expect (%jr-list undefined-count) :to-equal '(1 9 2 3 4))))

(it-sequential "js-rt-array-concat"
  (let* ((a (%jr-arr 1 2))
         (b (%jr-arr 3 4))
         (r (cl-cc/javascript::%js-array-concat a b (%jr-arr 5))))
    (expect (%jr-list r) :to-equal '(1 2 3 4 5))
    (expect (= 2 (length a)) :to-be-truthy)))   ; originals untouched

(it-sequential "js-rt-array-reverse"
  (let ((a (%jr-arr 1 2 3)))
    (let ((r (cl-cc/javascript::%js-array-reverse a)))
      (expect (%jr-list r) :to-equal '(3 2 1))
      (expect r :to-be a))))         ; same object

(it-sequential "js-rt-array-sort-default"
  (let* ((a (%jr-arr 9 10 2))
         (r (cl-cc/javascript::%js-array-sort a)))
    (expect (%jr-list r) :to-equal '(10 2 9))   ; lexicographic
    (expect r :to-be a)))

(it-sequential "js-rt-array-sort-numeric"
  (let* ((a (%jr-arr 10 2 30))
         (r (cl-cc/javascript::%js-array-sort
             a (lambda (x y &rest _) (declare (ignore _)) (- x y)))))
    (expect (%jr-list r) :to-equal '(2 10 30))))

(it-sequential "js-rt-array-flat"
  (let* ((nested (%jr-arr 1 (%jr-arr 2 3) (%jr-arr 4 (%jr-arr 5)))))
    (expect (%jr-list (cl-cc/javascript::%js-array-flat nested 2)) :to-equal '(1 2 3 4 5))
    (let ((shallow (%jr-arr 1 (%jr-arr 2 3) (%jr-arr 4 (%jr-arr 5)))))
      ;; depth 1: [1, 2, 3, 4, [5]] — inner [5] stays nested
      (expect (= 5 (length (%jr-list (cl-cc/javascript::%js-array-flat shallow 1)))) :to-be-truthy))))

(it-sequential "js-rt-array-last-index-of"
  (let ((a (%jr-arr 1 2 3 2 1)))
    (expect (= 3 (cl-cc/javascript::%js-array-last-index-of a 2)) :to-be-truthy)
    (expect (= -1 (cl-cc/javascript::%js-array-last-index-of a 9)) :to-be-truthy)))

(it-sequential "js-rt-array-search-from-coerces-indices"
  (let ((a (%jr-arr 1 2 3 2 1)))
    (expect (cl-cc/javascript::%js-array-includes a 2 "-4") :to-be-truthy)
    (expect (= 3 (cl-cc/javascript::%js-array-index-of a 2 2.8d0)) :to-be-truthy)
    (expect (= 1 (cl-cc/javascript::%js-array-last-index-of a 2 -3.2d0)) :to-be-truthy)))

(it-sequential "js-rt-array-last-index-of-from-before-start"
  (let ((a (%jr-arr 1 2 1)))
    (expect (= -1 (cl-cc/javascript::%js-array-last-index-of a 1 -99)) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-array-last-index-of
                 a 1 cl-cc/javascript::+js-undefined+)) :to-be-truthy)))

(it-sequential "js-rt-array-fill"
  (let* ((a (%jr-arr 1 2 3 4 5))
         (r (cl-cc/javascript::%js-array-fill a 0 1 3)))
    (expect (%jr-list r) :to-equal '(1 0 0 4 5))
    (expect r :to-be a)))           ; mutated in place

(it-sequential "js-rt-array-fill-coerces-relative-indices"
  (let ((a (%jr-arr 1 2 3 4)))
    (cl-cc/javascript::%js-array-fill a "x" "-3" 3.8d0)
    (expect (%jr-list a) :to-equal '(1 "x" "x" 4))))

(it-sequential "js-rt-array-reduce-right"
  (let ((result (cl-cc/javascript::%js-array-reduce-right
                 (%jr-arr 1 2 3 4)
                 (lambda (acc x &rest _) (declare (ignore _)) (cons x acc))
                 nil)))
    (expect result :to-equal '(1 2 3 4))))

;;; ─── ES2023 non-mutating array methods ──────────────────────────────────────

(it-sequential "js-rt-array-to-reversed"
  (let* ((orig (%jr-arr 1 2 3))
         (rev  (cl-cc/javascript::%js-array-to-reversed orig)))
    (expect (%jr-list rev) :to-equal '(3 2 1))
    (expect (%jr-list orig) :to-equal '(1 2 3))))

(it-sequential "js-rt-array-to-sorted"
  (let* ((orig (%jr-arr 3 1 2))
         (srt  (cl-cc/javascript::%js-array-to-sorted orig)))
    (expect (%jr-list srt) :to-equal '(1 2 3))
    (expect (%jr-list orig) :to-equal '(3 1 2))))

(it-sequential "js-rt-array-with mid-index"
  (destructuring-bind (idx val expected) (list 1 99 '(10 99 30))
    (expect (%jr-list (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx val)) :to-equal expected)))

(it-sequential "js-rt-array-with neg-index"
  (destructuring-bind (idx val expected) (list -1 99 '(10 20 99))
    (expect (%jr-list (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx val)) :to-equal expected)))

(it-sequential "js-rt-array-with string-index"
  (destructuring-bind (idx val expected) (list "1" 99 '(10 99 30))
    (expect (%jr-list (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx val)) :to-equal expected)))

(it-sequential "js-rt-array-with fractional-negative-index"
  (destructuring-bind (idx val expected) (list -1.8d0 99 '(10 20 99))
    (expect (%jr-list (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx val)) :to-equal expected)))

(it-sequential "js-rt-array-with-out-of-bounds too-large"
  (destructuring-bind (idx) (list 3)
    (handler-case
      (progn
        (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx 99)
        (expect t :to-be-falsy))
    (cl-cc/javascript:js-exception (c)
      (let ((err (cl-cc/javascript:js-exception-value c)))
        (expect (gethash "name" err) :to-equal "RangeError"))))))

(it-sequential "js-rt-array-with-out-of-bounds too-negative"
  (destructuring-bind (idx) (list -4)
    (handler-case
      (progn
        (cl-cc/javascript::%js-array-with (%jr-arr 10 20 30) idx 99)
        (expect t :to-be-falsy))
    (cl-cc/javascript:js-exception (c)
      (let ((err (cl-cc/javascript:js-exception-value c)))
        (expect (gethash "name" err) :to-equal "RangeError"))))))

(it-sequential "js-rt-array-at positive"
  (destructuring-bind (idx expected) (list 1 20)
    (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (expect got :to-be undef)
          (expect (= expected got) :to-be-truthy))))))

(it-sequential "js-rt-array-at negative"
  (destructuring-bind (idx expected) (list -1 30)
    (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (expect got :to-be undef)
          (expect (= expected got) :to-be-truthy))))))

(it-sequential "js-rt-array-at string-index"
  (destructuring-bind (idx expected) (list "1" 20)
    (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (expect got :to-be undef)
          (expect (= expected got) :to-be-truthy))))))

(it-sequential "js-rt-array-at fractional-negative-index"
  (destructuring-bind (idx expected) (list -1.8d0 30)
    (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (expect got :to-be undef)
          (expect (= expected got) :to-be-truthy))))))

(it-sequential "js-rt-array-at oob"
  (destructuring-bind (idx expected) (list 9 :js-undefined)
    (let ((a (%jr-arr 10 20 30))
        (undef cl-cc/javascript::+js-undefined+))
    (let ((got (cl-cc/javascript::%js-array-at a idx)))
      (if (eq expected :js-undefined)
          (expect got :to-be undef)
          (expect (= expected got) :to-be-truthy))))))

(it-sequential "js-rt-array-find-last"
  (let ((result (cl-cc/javascript::%js-array-find-last
                 (%jr-arr 1 2 3 4)
                 (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (expect (= 4 result) :to-be-truthy)))

(it-sequential "js-rt-array-find-last-index"
  (let ((result (cl-cc/javascript::%js-array-find-last-index
                 (%jr-arr 1 2 3 4)
                 (lambda (x &rest _) (declare (ignore _)) (evenp x)))))
    (expect (= 3 result) :to-be-truthy)))

(it-sequential "js-rt-array-to-spliced"
  (let* ((orig (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-array-to-spliced orig 1 2 9 9)))
    (expect (%jr-list result) :to-equal '(1 9 9 4))
    (expect (%jr-list orig) :to-equal '(1 2 3 4))))

(it-sequential "js-rt-array-to-spliced-coerces-indices"
  (let* ((orig (%jr-arr 1 2 3 4))
         (coerced (cl-cc/javascript::%js-array-to-spliced orig "1" "2" 9))
         (omitted (cl-cc/javascript::%js-array-to-spliced orig "2")))
    (expect (%jr-list coerced) :to-equal '(1 9 4))
    (expect (%jr-list omitted) :to-equal '(1 2))
    (expect (%jr-list orig) :to-equal '(1 2 3 4))))

(it-sequential "js-rt-array-of"
  (expect (%jr-list (cl-cc/javascript::%js-array-of 5 6 7)) :to-equal '(5 6 7)))

;;; ─── TypedArray basic operations ─────────────────────────────────────────────

(it-sequential "js-rt-typed-array-make-get-set"
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" 3)))
    (cl-cc/javascript::%js-ta-set ta 0 10)
    (cl-cc/javascript::%js-ta-set ta 2 99)
    (expect (= 10 (cl-cc/javascript::%js-ta-get ta 0)) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-ta-get ta 1)) :to-be-truthy)
    (expect (= 99 (cl-cc/javascript::%js-ta-get ta 2)) :to-be-truthy)))

(it-sequential "js-rt-typed-array-length"
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Float64Array" 4)))
    (expect (= 4 (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)))

(it-sequential "js-rt-typed-array-types Int8Array"
  (destructuring-bind (type-name length expected-length) (list "Int8Array" 3 3)
    (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (expect (= expected-length (cl-cc/javascript::js-ta-length ta)) :to-be-truthy))))

(it-sequential "js-rt-typed-array-types Uint8Array"
  (destructuring-bind (type-name length expected-length) (list "Uint8Array" 5 5)
    (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (expect (= expected-length (cl-cc/javascript::js-ta-length ta)) :to-be-truthy))))

(it-sequential "js-rt-typed-array-types Int32Array"
  (destructuring-bind (type-name length expected-length) (list "Int32Array" 2 2)
    (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (expect (= expected-length (cl-cc/javascript::js-ta-length ta)) :to-be-truthy))))

(it-sequential "js-rt-typed-array-types Float16Array"
  (destructuring-bind (type-name length expected-length) (list "Float16Array" 4 4)
    (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (expect (= expected-length (cl-cc/javascript::js-ta-length ta)) :to-be-truthy))))

(it-sequential "js-rt-typed-array-types Float64Array"
  (destructuring-bind (type-name length expected-length) (list "Float64Array" 1 1)
    (let ((ta (cl-cc/javascript::%js-make-typed-array type-name length)))
    (expect (= expected-length (cl-cc/javascript::js-ta-length ta)) :to-be-truthy))))

(it-sequential "js-rt-method-resolution-array"
  (let* ((arr (%jr-arr 10 20 30))
         (join-fn (cl-cc/javascript::%js-get-prop arr "join")))
    (expect (functionp join-fn) :to-be-truthy)
    (expect (funcall join-fn ",") :to-equal "10,20,30")))

(it-sequential "js-rt-method-resolution-length"
  (let* ((arr (%jr-arr 1 2 3)))
    (expect (= 3 (cl-cc/javascript::%js-get-prop arr "length")) :to-be-truthy)))

(it-sequential "js-rt-array-values-via-get-prop"
  (let* ((arr   (%jr-arr 10 20))
         (fn    (cl-cc/javascript::%js-get-prop arr "values"))
         (iter  (funcall fn))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (done  (funcall next)))
    (expect (= 10 (gethash "value" r1)) :to-be-truthy)
    (expect (= 20 (gethash "value" r2)) :to-be-truthy)
    (expect (gethash "done"  done) :to-be-truthy)))

(it-sequential "js-rt-array-@@iterator-via-get-prop"
  (let* ((arr   (%jr-arr 5 6))
         (fn    (cl-cc/javascript::%js-get-prop arr "@@iterator"))
         (iter  (funcall fn))
         (next  (gethash "next" iter)))
    (expect (functionp next) :to-be-truthy)
    (expect (= 5 (gethash "value" (funcall next))) :to-be-truthy)
    (expect (= 6 (gethash "value" (funcall next))) :to-be-truthy)))

;;; ─── Uncovered array methods ─────────────────────────────────────────────────

(it-sequential "js-rt-array-flat-map"
  (let* ((arr    (%jr-arr 1 2 3))
         (result (cl-cc/javascript::%js-array-flat-map
                  arr (lambda (x &rest _) (declare (ignore _)) (%jr-arr x (* x 10))))))
    (expect (%jr-list result) :to-equal '(1 10 2 20 3 30))))

(it-sequential "js-rt-array-entries"
  (let* ((arr     (%jr-arr "a" "b"))
         (entries (cl-cc/javascript::%js-array-entries arr))
         (next    (gethash "next" entries)))
    (let* ((r0 (funcall next))
           (e0 (gethash "value" r0))
           (r1 (funcall next))
           (e1 (gethash "value" r1))
           (r2 (funcall next)))
      (expect (gethash "done" r0) :to-be-falsy)
      (expect (= 0 (aref e0 0)) :to-be-truthy)
      (expect (aref e0 1) :to-equal "a")
      (expect (gethash "done" r1) :to-be-falsy)
      (expect (= 1 (aref e1 0)) :to-be-truthy)
      (expect (aref e1 1) :to-equal "b")
      (expect (gethash "done" r2) :to-be-truthy))))

(it-sequential "js-rt-array-keys"
  (let* ((arr  (%jr-arr "x" "y" "z"))
         (ks   (cl-cc/javascript::%js-array-keys arr))
         (acc  nil))
    (cl-cc/javascript::%js-for-of ks (lambda (k) (push k acc)))
    (expect (nreverse acc) :to-equal '(0 1 2))))

(it-sequential "js-rt-array-copy-within"
  (let ((arr (%jr-arr 1 2 3 4 5)))
    (cl-cc/javascript::%js-array-copy-within arr 0 3 5)
    (expect (= 4 (aref arr 0)) :to-be-truthy)
    (expect (= 5 (aref arr 1)) :to-be-truthy)
    (expect (= 3 (aref arr 2)) :to-be-truthy)))

(it-sequential "js-rt-array-copy-within-coerces-relative-indices"
  (let ((arr (%jr-arr 1 2 3 4)))
    (cl-cc/javascript::%js-array-copy-within arr "-2" "0" 2.9d0)
    (expect (%jr-list arr) :to-equal '(1 2 1 2))))

;;; ─── Coverage: forEach / isArray / Array.from ────────────────────────────────

(it-sequential "js-rt-array-for-each"
  (let ((collected nil))
    (cl-cc/javascript::%js-array-for-each
     (%jr-arr 10 20 30)
     (lambda (x i &rest _) (declare (ignore _)) (push (list i x) collected)))
    (expect collected :to-equal '((2 30) (1 20) (0 10)))
    (expect (cl-cc/javascript::%js-array-for-each (%jr-arr 1) (constantly nil)) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-array-is-array"
  (expect (cl-cc/javascript::%js-array-is-array (%jr-arr 1 2)) :to-be-truthy)
  (expect (cl-cc/javascript::%js-array-is-array "not-an-array") :to-be-falsy)
  (expect (cl-cc/javascript::%js-array-is-array 42) :to-be-falsy)
  (expect (cl-cc/javascript::%js-array-is-array cl-cc/javascript::+js-undefined+) :to-be-falsy))

(it-sequential "js-rt-array-from-plain"
  (let ((result (cl-cc/javascript::%js-array-from (%jr-arr 1 2 3))))
    (expect (%jr-list result) :to-equal '(1 2 3))))

(it-sequential "js-rt-array-from-undefined-map-fn"
  (let ((result (cl-cc/javascript::%js-array-from
                 (%jr-arr 1 2 3)
                 cl-cc/javascript::+js-undefined+)))
    (expect (%jr-list result) :to-equal '(1 2 3))))

(it-sequential "js-rt-array-from-with-map"
  (let ((result (cl-cc/javascript::%js-array-from
                 (%jr-arr 1 2 3)
                 (lambda (x &rest _) (declare (ignore _)) (* x 10)))))
    (expect (%jr-list result) :to-equal '(10 20 30))))

(it-sequential "js-rt-array-from-map-fn-receives-index"
  (let ((result (cl-cc/javascript::%js-array-from
                 (%jr-arr 10 20 30)
                 (lambda (x i) (+ x i)))))
    (expect (%jr-list result) :to-equal '(10 21 32))))

(it-sequential "js-rt-array-from-array-like"
  (let* ((source (cl-cc/javascript::%js-make-object "0" "a" "1" "b" "length" 2))
         (result (cl-cc/javascript::%js-array-from source)))
    (expect (%jr-list result) :to-equal '("a" "b"))))

(it-sequential "js-rt-array-from-array-like-missing-index"
  (let* ((source (cl-cc/javascript::%js-make-object "0" "a" "length" 2))
         (result (cl-cc/javascript::%js-array-from source)))
    (expect (%jr-list result) :to-equal (list "a" cl-cc/javascript::+js-undefined+))))

(it-sequential "js-rt-array-from-map-fn-this-arg"
  (let* ((this (cl-cc/javascript::%js-make-object "scale" 10))
         (result (cl-cc/javascript::%js-array-from
                  (%jr-arr 1 2)
                  (lambda (x i)
                    (+ (* x (cl-cc/javascript::%js-get-prop cl-cc/javascript::%js-this "scale"))
                       i))
                  this)))
    (expect (%jr-list result) :to-equal '(10 21))))

(it-sequential "js-rt-array-from-async-awaits-elements"
  (let* ((input  (%jr-arr (cl-cc/javascript::%js-promise-resolve 1)
                          (cl-cc/javascript::%js-promise-resolve 2)
                          3))
         (result (cl-cc/javascript::%js-await
                  (cl-cc/javascript::%js-array-from-async input))))
    (expect (%jr-list result) :to-equal '(1 2 3))))

(it-sequential "js-rt-array-from-async-awaits-map-results"
  (let* ((input  (%jr-arr (cl-cc/javascript::%js-promise-resolve 10)
                          (cl-cc/javascript::%js-promise-resolve 20)))
         (result (cl-cc/javascript::%js-await
                  (cl-cc/javascript::%js-array-from-async
                   input
                   (lambda (x i &rest _) (declare (ignore _))
                     (cl-cc/javascript::%js-promise-resolve (+ x i)))))))
    (expect (%jr-list result) :to-equal '(10 21))))

(it-sequential "js-rt-array-from-async-array-like"
  (let* ((source (cl-cc/javascript::%js-make-object
                  "0" (cl-cc/javascript::%js-promise-resolve 3)
                  "1" 4
                  "length" 2))
         (result (cl-cc/javascript::%js-await
                  (cl-cc/javascript::%js-array-from-async
                   source
                   (lambda (x i)
                     (cl-cc/javascript::%js-promise-resolve (+ x i)))))))
    (expect (%jr-list result) :to-equal '(3 5))))

;;; ─── Coverage: ES2024 group / groupToMap ─────────────────────────────────────

(it-sequential "js-rt-array-group"
  (let* ((arr    (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-array-group
                  arr (lambda (x &rest _) (declare (ignore _))
                        (if (evenp x) "even" "odd")))))
    (expect (hash-table-p result) :to-be-truthy)
    (expect (%jr-list (gethash "odd"  result)) :to-equal '(1 3))
    (expect (%jr-list (gethash "even" result)) :to-equal '(2 4))))

(it-sequential "js-rt-array-group-to-map"
  (let* ((arr    (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-array-group-to-map
                  arr (lambda (x &rest _) (declare (ignore _))
                        (if (evenp x) "even" "odd")))))
    (expect (cl-cc/javascript::%js-map-p result) :to-be-truthy)
    (expect (%jr-list (cl-cc/javascript::%js-map-get result "odd")) :to-equal '(1 3))
    (expect (%jr-list (cl-cc/javascript::%js-map-get result "even")) :to-equal '(2 4))))
