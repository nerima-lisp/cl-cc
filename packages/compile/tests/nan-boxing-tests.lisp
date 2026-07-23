;;;; packages/compile/tests/nan-boxing-tests.lisp
;;;; Unit tests for NaN-boxing value encoding in nan-boxing.lisp
;;;;
;;;; Covers: nan-box-fixnum, nan-box-pointer, nan-box-unbox round-trips,
;;;;   boundary cases (48-bit payload mask), tag discrimination.

(in-package :cl-cc/test)

(it-sequential "nan-box-fixnum-round-trips-zero"
  (multiple-value-bind (tag payload)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 0))
    (expect (= cl-cc/compile::+nan-box-tag-fixnum+ tag) :to-be-truthy)
    (expect (= 0 payload) :to-be-truthy)))

(it-sequential "nan-box-fixnum-round-trips-positive"
  (multiple-value-bind (tag payload)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 42))
    (expect (= cl-cc/compile::+nan-box-tag-fixnum+ tag) :to-be-truthy)
    (expect (= 42 payload) :to-be-truthy)))

(it-sequential "nan-box-pointer-embeds-ptr-tag"
  (let ((ptr #x1234567890AB))
    (multiple-value-bind (tag payload)
        (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-pointer ptr))
      (expect (= cl-cc/compile::+nan-box-tag-ptr+ tag) :to-be-truthy)
      (expect (= ptr payload) :to-be-truthy))))

(it-sequential "nan-box-float-preserves-ieee754-bits"
  (let ((boxed (cl-cc/compile::nan-box-float 1.0d0)))
    (expect (= #x3FF0000000000000 boxed) :to-be-truthy)))

(it-sequential "nan-box-fixnum-payload-masked-to-48-bits"
  (let* ((big-val (+ #xFFFFFFFFFFFF 1))
         (boxed (cl-cc/compile::nan-box-fixnum big-val)))
    (multiple-value-bind (tag payload)
        (cl-cc/compile::nan-box-unbox boxed)
      (expect (= cl-cc/compile::+nan-box-tag-fixnum+ tag) :to-be-truthy)
      (expect (= (logand big-val #xFFFFFFFFFFFF) payload) :to-be-truthy))))

(it-sequential "nan-box-unbox-distinguishes-fixnum-from-pointer"
  (multiple-value-bind (fixnum-tag ignore-f)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 1))
    (declare (ignore ignore-f))
    (multiple-value-bind (ptr-tag ignore-p)
        (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-pointer 1))
      (declare (ignore ignore-p))
      (expect (= fixnum-tag ptr-tag) :to-be-falsy))))
