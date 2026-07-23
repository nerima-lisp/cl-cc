;;;; packages/javascript/tests/js-runtime-typed-array-methods-tests.lisp
;;;;
;;;; Unit tests for runtime-typed-arrays-methods.lisp:
;;;; ES2023 non-mutating methods (toReversed, toSorted, with, at, findLast…),
;;;; mutating methods (reverse, sort, copyWithin), iterator methods
;;;; (values, keys, entries), and ES2025 Uint8Array hex/base64 encoding.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun %make-int32-ta (&rest values)
  "Build an Int32Array TypedArray filled with VALUES."
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" (length values))))
    (loop for v in values for i from 0
          do (cl-cc/javascript::%js-ta-set ta i v))
    ta))

(defun %make-float64-ta (&rest values)
  "Build a Float64Array TypedArray filled with VALUES."
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Float64Array" (length values))))
    (loop for v in values for i from 0
          do (cl-cc/javascript::%js-ta-set ta i v))
    ta))

(defun %make-bigint-ta (type-name &rest values)
  "Build a BigInt TypedArray filled with VALUES."
  (let ((ta (cl-cc/javascript::%js-make-typed-array type-name (length values))))
    (loop for v in values for i from 0
          do (cl-cc/javascript::%js-ta-set ta i v))
    ta))

(defun %ta-to-list (ta)
  "Convert a TypedArray's buffer to a CL list of numbers."
  (loop for i below (cl-cc/javascript::js-ta-length ta)
        collect (cl-cc/javascript::%js-ta-get ta i)))

;;; ─── at (negative indexing) ──────────────────────────────────────────────────

(it-sequential "js-rt-ta-at first"
  (destructuring-bind (idx expected) (list 0 10.0d0)
    (let ((ta (%make-int32-ta 10 20 30)))
    (expect (= expected (cl-cc/javascript::%js-ta-at ta idx)) :to-be-truthy))))

(it-sequential "js-rt-ta-at last"
  (destructuring-bind (idx expected) (list -1 30.0d0)
    (let ((ta (%make-int32-ta 10 20 30)))
    (expect (= expected (cl-cc/javascript::%js-ta-at ta idx)) :to-be-truthy))))

(it-sequential "js-rt-ta-at middle"
  (destructuring-bind (idx expected) (list 1 20.0d0)
    (let ((ta (%make-int32-ta 10 20 30)))
    (expect (= expected (cl-cc/javascript::%js-ta-at ta idx)) :to-be-truthy))))

;;; ─── find / findIndex ────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-find"
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (expect (= 7.0d0 (cl-cc/javascript::%js-ta-find ta (lambda (v &rest _) (declare (ignore _)) (> v 5)))) :to-be-truthy)
    (expect (cl-cc/javascript::%js-ta-find ta (lambda (v &rest _) (declare (ignore _)) (> v 100))) :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-ta-find-index"
  (let ((ta (%make-int32-ta 3 7 4)))
    (expect (= 1.0d0 (cl-cc/javascript::%js-ta-find-index ta (lambda (v &rest _) (declare (ignore _)) (> v 5)))) :to-be-truthy)
    (expect (= -1.0d0 (cl-cc/javascript::%js-ta-find-index ta (lambda (v &rest _) (declare (ignore _)) (> v 100)))) :to-be-truthy)))

(it-sequential "js-rt-ta-find-last"
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (expect (= 9.0d0 (cl-cc/javascript::%js-ta-find-last ta (lambda (v &rest _) (declare (ignore _)) (> v 5)))) :to-be-truthy)))

(it-sequential "js-rt-ta-find-last-index"
  (let ((ta (%make-int32-ta 3 7 4 9)))
    (expect (= 3.0d0 (cl-cc/javascript::%js-ta-find-last-index ta (lambda (v &rest _) (declare (ignore _)) (> v 5)))) :to-be-truthy)))

;;; ─── every / some ────────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-every-some every-all-positive"
  (destructuring-bind (fn values pred expected) (list #'cl-cc/javascript::%js-ta-every '(1 2 3) (lambda (v &rest _) (declare (ignore _)) (> v 0)) t)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (funcall fn ta pred) :to-equal expected))))

(it-sequential "js-rt-ta-every-some every-some-negative"
  (destructuring-bind (fn values pred expected) (list #'cl-cc/javascript::%js-ta-every '(1 -2 3) (lambda (v &rest _) (declare (ignore _)) (> v 0)) nil)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (funcall fn ta pred) :to-equal expected))))

(it-sequential "js-rt-ta-every-some some-found"
  (destructuring-bind (fn values pred expected) (list #'cl-cc/javascript::%js-ta-some '(1 -2 3) (lambda (v &rest _) (declare (ignore _)) (< v 0)) t)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (funcall fn ta pred) :to-equal expected))))

(it-sequential "js-rt-ta-every-some some-not-found"
  (destructuring-bind (fn values pred expected) (list #'cl-cc/javascript::%js-ta-some '(1 2 3) (lambda (v &rest _) (declare (ignore _)) (< v 0)) nil)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (funcall fn ta pred) :to-equal expected))))

;;; ─── reverse / toReversed ────────────────────────────────────────────────────

(it-sequential "js-rt-ta-reverse-mutating"
  (let ((ta (%make-int32-ta 1 2 3)))
    (let ((ret (cl-cc/javascript::%js-ta-reverse ta)))
      (expect ret :to-be ta)
      (expect (%ta-to-list ta) :to-equal '(3 2 1)))))

(it-sequential "js-rt-ta-to-reversed-non-mutating"
  (let* ((ta  (%make-int32-ta 1 2 3))
         (rev (cl-cc/javascript::%js-ta-to-reversed ta)))
    (expect (eq ta rev) :to-be-falsy)
    (expect (%ta-to-list ta) :to-equal '(1 2 3))
    (expect (%ta-to-list rev) :to-equal '(3 2 1))))

;;; ─── sort / toSorted ─────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-sort-mutating"
  (let ((ta (%make-int32-ta 3 1 2)))
    (let ((ret (cl-cc/javascript::%js-ta-sort ta)))
      (expect ret :to-be ta)
      (expect (%ta-to-list ta) :to-equal '(1 2 3)))))

(it-sequential "js-rt-ta-to-sorted-non-mutating"
  (let* ((ta     (%make-int32-ta 3 1 2))
         (sorted (cl-cc/javascript::%js-ta-to-sorted ta)))
    (expect (eq ta sorted) :to-be-falsy)
    (expect (%ta-to-list ta) :to-equal '(3 1 2))
    (expect (%ta-to-list sorted) :to-equal '(1 2 3))))

;;; ─── with ────────────────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-with-non-mutating"
  (let* ((ta  (%make-int32-ta 1 2 3))
         (w   (cl-cc/javascript::%js-ta-with ta 1 99)))
    (expect (eq ta w) :to-be-falsy)
    (expect (%ta-to-list ta) :to-equal '(1 2 3))
    (expect (%ta-to-list w) :to-equal '(1 99 3))))

;;; ─── copyWithin ──────────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-copy-within"
  (let ((ta (%make-int32-ta 1 2 3 4 5)))
    (cl-cc/javascript::%js-ta-copy-within ta 0 3 5)
    (expect (%ta-to-list ta) :to-equal '(4 5 3 4 5))))

(it-sequential "js-rt-ta-slice-non-mutating"
  (let* ((ta  (%make-int32-ta 1 2 3 4 5))
         (out (cl-cc/javascript::%js-ta-slice ta 1 4)))
    (expect (eq ta out) :to-be-falsy)
    (expect (%ta-to-list ta) :to-equal '(1 2 3 4 5))
    (expect (%ta-to-list out) :to-equal '(2 3 4))))

(it-sequential "js-rt-ta-fill-mutating"
  (let* ((ta  (%make-int32-ta 1 2 3 4 5))
         (out (cl-cc/javascript::%js-ta-fill ta 9 1 4)))
    (expect (eq ta out) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal '(1 9 9 9 5))))

(it-sequential "js-rt-ta-negative-ranges-and-clamping"
  (let ((ta (%make-int32-ta 1 2 3 4 5)))
    (let ((sub (cl-cc/javascript::%js-ta-subarray ta -4 -1)))
      (expect (%ta-to-list sub) :to-equal '(2 3 4)))
    (let ((slice (cl-cc/javascript::%js-ta-slice ta -4 -1)))
      (expect (%ta-to-list slice) :to-equal '(2 3 4)))
    (cl-cc/javascript::%js-ta-fill ta 9 -3 -1)
    (expect (%ta-to-list ta) :to-equal '(1 2 9 9 5)))
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Uint8ClampedArray" 4)))
    (cl-cc/javascript::%js-ta-set ta 0 -12.5d0)
    (cl-cc/javascript::%js-ta-set ta 1 127.6d0)
    (cl-cc/javascript::%js-ta-set ta 2 300)
    (cl-cc/javascript::%js-ta-set ta 3 255.9d0)
    (expect (%ta-to-list ta) :to-equal '(0 127 255 255)))
  (let ((signed   (%make-bigint-ta "BigInt64Array"
                                    (cl-cc/javascript::%make-js-bigint -1)
                                    9223372036854775808))
        (unsigned (%make-bigint-ta "BigUint64Array"
                                    (cl-cc/javascript::%make-js-bigint -1)
                                    18446744073709551616)))
    (expect (= -1 (cl-cc/javascript::%js-ta-get signed 0)) :to-be-truthy)
    (expect (= -9223372036854775808 (cl-cc/javascript::%js-ta-get signed 1)) :to-be-truthy)
    (expect (= 18446744073709551615 (cl-cc/javascript::%js-ta-get unsigned 0)) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-ta-get unsigned 1)) :to-be-truthy)))

;;; ─── lastIndexOf ─────────────────────────────────────────────────────────────

(it-sequential "js-rt-ta-last-index-of found"
  (destructuring-bind (values target expected) (list '(1 2 3 2) 2 3.0d0)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (= expected (cl-cc/javascript::%js-ta-last-index-of ta target)) :to-be-truthy))))

(it-sequential "js-rt-ta-last-index-of absent"
  (destructuring-bind (values target expected) (list '(1 2 3) 9 -1.0d0)
    (let ((ta (apply #'%make-int32-ta values)))
    (expect (= expected (cl-cc/javascript::%js-ta-last-index-of ta target)) :to-be-truthy))))

(it-sequential "js-rt-ta-search-from-index-coercion"
  (let ((ta (%make-int32-ta 1 2 3 2 1)))
    (expect (cl-cc/javascript::%js-ta-includes ta 2 "-4") :to-be-truthy)
    (expect (cl-cc/javascript::%js-ta-includes ta 2 4) :to-be-falsy)
    (expect (= 3 (cl-cc/javascript::%js-ta-index-of ta 2 2.8d0)) :to-be-truthy)
    (expect (= -1 (cl-cc/javascript::%js-ta-index-of ta 1 99)) :to-be-truthy)
    (expect (= 1.0d0 (cl-cc/javascript::%js-ta-last-index-of ta 2 -3.2d0)) :to-be-truthy)
    (expect (= -1.0d0 (cl-cc/javascript::%js-ta-last-index-of ta 1 -99)) :to-be-truthy)
    (expect (= 0.0d0 (cl-cc/javascript::%js-ta-last-index-of
                     ta 1 cl-cc/javascript::+js-undefined+)) :to-be-truthy)))

(it-sequential "js-rt-ta-includes-same-value-zero"
  (let ((ta (%make-float64-ta cl-cc/javascript::*js-nan-float* -0.0d0)))
    (expect (cl-cc/javascript::%js-ta-includes ta cl-cc/javascript::+js-nan+) :to-be-truthy)
    (expect (cl-cc/javascript::%js-ta-includes ta 0.0d0) :to-be-truthy)))

(it-sequential "js-rt-ta-index-of-does-not-match-nan"
  (let ((ta (%make-float64-ta cl-cc/javascript::*js-nan-float*)))
    (expect (= -1 (cl-cc/javascript::%js-ta-index-of ta cl-cc/javascript::+js-nan+)) :to-be-truthy)
    (expect (= -1.0d0 (cl-cc/javascript::%js-ta-last-index-of ta cl-cc/javascript::+js-nan+)) :to-be-truthy)))

(it-sequential "js-rt-ta-float64-preserves-fractional-values"
  (let ((ta (%make-float64-ta 1.5d0)))
    (expect (= 1.5d0 (cl-cc/javascript::%js-ta-get ta 0)) :to-be-truthy)))

(it-sequential "js-rt-ta-float16-rounds-to-binary16"
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Float16Array" 3)))
    (cl-cc/javascript::%js-ta-set ta 0 1.337d0)
    (cl-cc/javascript::%js-ta-set ta 1 5.05d0)
    (cl-cc/javascript::%js-ta-set ta 2 65520.0d0)
    (expect (cl-cc/javascript::js-ta-type-name ta) :to-equal "Float16Array")
    (expect (= 2 (cl-cc/javascript::js-ta-element-size ta)) :to-be-truthy)
    (expect (= 1.3369140625d0 (cl-cc/javascript::%js-ta-get ta 0)) :to-be-truthy)
    (expect (= 5.05078125d0 (cl-cc/javascript::%js-ta-get ta 1)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-float-infinity-p
                  (cl-cc/javascript::%js-ta-get ta 2)) :to-be-truthy)))

;;; ─── values / keys / entries iterators ──────────────────────────────────────

(it-sequential "js-rt-ta-values-iterator"
  (let* ((ta    (%make-int32-ta 10 20))
         (iter  (cl-cc/javascript::%js-ta-values ta))
         (next  (gethash "next" iter))
         (r1    (funcall next))
         (r2    (funcall next))
         (done  (funcall next)))
    (expect (= 10.0d0 (gethash "value" r1)) :to-be-truthy)
    (expect (= 20.0d0 (gethash "value" r2)) :to-be-truthy)
    (expect (gethash "done"  done) :to-be-truthy)))

(it-sequential "js-rt-ta-keys-iterator"
  (let* ((ta   (%make-int32-ta 10 20))
         (iter (cl-cc/javascript::%js-ta-keys ta))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (done (funcall next)))
    (expect (= 0.0d0 (gethash "value" r1)) :to-be-truthy)
    (expect (= 1.0d0 (gethash "value" r2)) :to-be-truthy)
    (expect (gethash "done"  done) :to-be-truthy)))

(it-sequential "js-rt-ta-entries-iterator"
  (let* ((ta   (%make-int32-ta 5))
         (iter (cl-cc/javascript::%js-ta-entries ta))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (done (funcall next)))
    (let ((pair (gethash "value" r1)))
      (expect (= 0.0d0 (aref pair 0)) :to-be-truthy)
      (expect (= 5.0d0 (aref pair 1)) :to-be-truthy))
    (expect (gethash "done" done) :to-be-truthy)))

;;; ─── clone-with-buffer ───────────────────────────────────────────────────────

(it-sequential "js-rt-ta-clone-with-buffer"
  (let* ((orig    (%make-int32-ta 1 2 3))
         (new-buf (make-array 2 :initial-contents (list 7 8) :element-type t))
         (clone   (cl-cc/javascript::%js-ta-clone-with-buffer orig new-buf)))
    (expect (eq orig clone) :to-be-falsy)
    (expect (cl-cc/javascript::js-ta-type-name clone) :to-equal "Int32Array")
    (expect (= 2 (cl-cc/javascript::js-ta-length clone)) :to-be-truthy)))

;;; ─── constructor / methods ──────────────────────────────────────────────────

(it-sequential "js-rt-ta-make-typed-array-from-arg undefined"
  (destructuring-bind (arg expected-len expected-values) (list cl-cc/javascript::+js-undefined+ 0 '())
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-make-typed-array-from-arg null"
  (destructuring-bind (arg expected-len expected-values) (list cl-cc/javascript::+js-null+ 0 '())
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-make-typed-array-from-arg number"
  (destructuring-bind (arg expected-len expected-values) (list 3.9d0 3 '(0 0 0))
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-make-typed-array-from-arg vector"
  (destructuring-bind (arg expected-len expected-values) (list #(4 5 6) 3 '(4 5 6))
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-make-typed-array-from-arg typed-array"
  (destructuring-bind (arg expected-len expected-values) (list (%make-int32-ta 1 2 3) 3 '(1 2 3))
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-make-typed-array-from-arg fallback"
  (destructuring-bind (arg expected-len expected-values) (list :oops 0 '())
    (let ((ta (cl-cc/javascript::%js-make-typed-array "Int32Array" arg)))
    (expect (= expected-len (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal expected-values))))

(it-sequential "js-rt-ta-set-from-vector-and-typed-array"
  (let ((from-vector (%make-int32-ta 0 0 0 0))
        (from-ta     (%make-int32-ta 0 0 0 0)))
    (cl-cc/javascript::%js-ta-set-from from-vector #(7 8) 1)
    (cl-cc/javascript::%js-ta-set-from from-ta (%make-int32-ta 9 10) 2)
    (expect (%ta-to-list from-vector) :to-equal '(0 7 8 0))
    (expect (%ta-to-list from-ta) :to-equal '(0 0 9 10))))

(it-sequential "js-rt-ta-get-set-and-to-array"
  (let ((ta (%make-int32-ta 10 20)))
    (expect (cl-cc/javascript::%js-ta-get ta 2) :to-be cl-cc/javascript::+js-undefined+)
    (expect (cl-cc/javascript::%js-ta-get ta -1) :to-be cl-cc/javascript::+js-undefined+)
    (expect (= 99.0d0 (cl-cc/javascript::%js-ta-set ta 5 99)) :to-be-truthy)
    (expect (%ta-to-list ta) :to-equal '(10 20))
    (expect (equalp #(10.0d0 20.0d0) (cl-cc/javascript::%js-ta-to-array ta)) :to-be-truthy)))

(it-sequential "js-rt-ta-join"
  (let ((ta (%make-int32-ta 1 2 3)))
    (expect (cl-cc/javascript::%js-ta-join ta "-") :to-equal "1-2-3")))

(it-sequential "js-rt-ta-for-each"
  (let ((ta (%make-int32-ta 1 2 3))
        (seen nil))
    (cl-cc/javascript::%js-ta-for-each
     ta
     (lambda (v i source &rest _)
       (declare (ignore i source _))
       (push v seen)))
    (expect (nreverse seen) :to-equal '(1.0d0 2.0d0 3.0d0))))

(it-sequential "js-rt-ta-map"
  (let* ((ta (%make-int32-ta 1 2 3))
         (mapped (cl-cc/javascript::%js-ta-map
                  ta
                  (lambda (v i source &rest _)
                    (declare (ignore i source _))
                    (* v 2)))))
    (expect (%ta-to-list mapped) :to-equal '(2 4 6))))

(it-sequential "js-rt-ta-filter"
  (let* ((ta (%make-int32-ta 1 2 3))
         (filtered (cl-cc/javascript::%js-ta-filter
                    ta
                    (lambda (v i source &rest _)
                      (declare (ignore i source _))
                      (> v 1)))))
    (expect (%ta-to-list filtered) :to-equal '(2 3))))

(it-sequential "js-rt-ta-reduce implicit-init"
  (destructuring-bind (init expected) (list nil 6)
    (let ((ta (%make-int32-ta 1 2 3)))
    (if init
        (expect (= expected (cl-cc/javascript::%js-ta-reduce
                   ta (lambda (acc v &rest _) (+ acc v)) init)) :to-be-truthy)
        (expect (= expected (cl-cc/javascript::%js-ta-reduce
                   ta (lambda (acc v &rest _) (+ acc v)))) :to-be-truthy)))))

(it-sequential "js-rt-ta-reduce explicit-init"
  (destructuring-bind (init expected) (list 10 16)
    (let ((ta (%make-int32-ta 1 2 3)))
    (if init
        (expect (= expected (cl-cc/javascript::%js-ta-reduce
                   ta (lambda (acc v &rest _) (+ acc v)) init)) :to-be-truthy)
        (expect (= expected (cl-cc/javascript::%js-ta-reduce
                   ta (lambda (acc v &rest _) (+ acc v)))) :to-be-truthy)))))

;;; ─── ES2025 Uint8Array hex / base64 ─────────────────────────────────────────

(defun %make-u8-ta (&rest bytes)
  (let ((ta (cl-cc/javascript::%js-make-typed-array "Uint8Array" (length bytes))))
    (loop for b in bytes for i from 0
          do (cl-cc/javascript::%js-ta-set ta i b))
    ta))

(it-sequential "js-rt-uint8-to-hex"
  (let ((ta (%make-u8-ta 0 15 255)))
    (expect (cl-cc/javascript::%js-uint8-to-hex ta) :to-equal "000fff")))

(it-sequential "js-rt-uint8-from-hex"
  (let* ((ta  (cl-cc/javascript::%js-uint8-from-hex "000fff"))
         (buf (cl-cc/javascript::js-ta-buffer ta)))
    (expect (= 3 (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)
    (expect (= 0 (aref buf 0)) :to-be-truthy)
    (expect (= 15 (aref buf 1)) :to-be-truthy)
    (expect (= 255 (aref buf 2)) :to-be-truthy)))

(it-sequential "js-rt-uint8-base64-roundtrip"
  (let* ((orig   (%make-u8-ta 1 2 3))
         (b64    (cl-cc/javascript::%js-uint8-to-base64 orig))
         (result (cl-cc/javascript::%js-uint8-from-base64 b64)))
    (expect (= 3 (cl-cc/javascript::js-ta-length result)) :to-be-truthy)
    (expect (= 1 (cl-cc/javascript::%js-ta-get result 0)) :to-be-truthy)
    (expect (= 2 (cl-cc/javascript::%js-ta-get result 1)) :to-be-truthy)
    (expect (= 3 (cl-cc/javascript::%js-ta-get result 2)) :to-be-truthy)))

(it-sequential "js-rt-uint8-set-from-hex"
  (let* ((ta (%make-u8-ta 0 0 0 9))
         (result (cl-cc/javascript::%js-uint8-set-from-hex ta "000fff")))
    (expect (%ta-to-list ta) :to-equal '(0 15 255 9))
    (expect (= 6.0d0 (gethash "read" result)) :to-be-truthy)
    (expect (= 3.0d0 (gethash "written" result)) :to-be-truthy)))

(it-sequential "js-rt-uint8-set-from-base64"
  (let* ((ta (%make-u8-ta 0 0 0 9))
         (result (cl-cc/javascript::%js-uint8-set-from-base64 ta "AQID")))
    (expect (%ta-to-list ta) :to-equal '(1 2 3 9))
    (expect (= 4.0d0 (gethash "read" result)) :to-be-truthy)
    (expect (= 3.0d0 (gethash "written" result)) :to-be-truthy)))
