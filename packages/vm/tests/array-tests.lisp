;;;; tests/unit/vm/array-tests.lisp — VM Array and Bit Array Instruction Tests
;;;;
;;;; Tests for array, vector, and bit-array instructions executed via the VM.

(in-package :cl-cc/test)

;;; ─── Array operations ──────────────────────────────────────────────────────

(it-sequential "vm-array-make-creates-array-of-given-size"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 5)
    (exec1 (cl-cc:make-vm-make-array :dst 0 :size-reg 1) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (expect (arrayp arr) :to-be-truthy)
      (expect (= 5 (length arr)) :to-be-truthy))))

(it-sequential "vm-array-make-with-initial-element-fills-all-slots"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 3)
    (cl-cc:vm-reg-set s 2 42)
    (exec1 (cl-cc:make-vm-make-array :dst 0 :size-reg 1 :initial-element 2) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (expect (= 42 (aref arr 0)) :to-be-truthy)
      (expect (= 42 (aref arr 2)) :to-be-truthy))))

(it-sequential "vm-array-make-with-element-type-character-uses-character-default"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 2)
    (exec1 (cl-cc:make-vm-make-array :dst 0 :size-reg 1 :element-type 'character) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      ;; The VM may create a host simple-array or a vm-specialized-array.
      (expect (or (typep arr '(simple-array character (*)))
                       (cl-cc/vm:vm-specialized-array-p arr)) :to-be-truthy)
      (expect (char= #\Nul (if (cl-cc/vm:vm-specialized-array-p arr)
                                    (cl-cc/vm:vm-specialized-array-ref arr 0)
                                    (aref arr 0))) :to-be-truthy)
      (expect (char= #\Nul (if (cl-cc/vm:vm-specialized-array-p arr)
                                     (cl-cc/vm:vm-specialized-array-ref arr 1)
                                     (aref arr 1))) :to-be-truthy))))

(it-sequential "vm-array-make-with-dynamic-keyword-registers"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 4)
    (cl-cc:vm-reg-set s 2 2)
    (cl-cc:vm-reg-set s 3 t)
    (cl-cc:vm-reg-set s 4 t)
    (exec1 (cl-cc:make-vm-make-array :dst 0 :size-reg 1
                                     :fill-pointer-reg 2
                                     :adjustable-reg 3
                                     :element-type-reg 4)
           s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (expect (= 4 (array-total-size arr)) :to-be-truthy)
      (expect (= 2 (fill-pointer arr)) :to-be-truthy)
      (expect (adjustable-array-p arr) :to-be-truthy))))

(it-sequential "vm-array-make-with-displaced-to-register"
  (let ((s (make-test-vm))
        (base (vector 'a 'b 'c 'd)))
    (cl-cc:vm-reg-set s 1 2)
    (cl-cc:vm-reg-set s 2 base)
    (exec1 (cl-cc:make-vm-make-array :dst 0 :size-reg 1 :displaced-to-reg 2) s)
    (let ((arr (cl-cc:vm-reg-get s 0)))
      (multiple-value-bind (target offset) (array-displacement arr)
        (expect target :to-be base)
        (expect (= 0 offset) :to-be-truthy))
      (expect (aref arr 0) :to-be 'a)
      (expect (aref arr 1) :to-be 'b))))

(it-sequential "vm-array-make-sexps-read-legacy-slot-order"
  (let ((inst (cl-cc:sexp->instruction '(:make-array 0 1 2 3 t character))))
    (expect (= 0 (cl-cc:vm-dst inst)) :to-be-truthy)
    (expect (= 1 (cl-cc:vm-size-reg inst)) :to-be-truthy)
    (expect (= 2 (cl-cc:vm-initial-element inst)) :to-be-truthy)
    (expect (= 3 (cl-cc:vm-fill-pointer inst)) :to-be-truthy)
    (expect (cl-cc:vm-adjustable inst) :to-be t)
    (expect (cl-cc:vm-element-type inst) :to-be 'character)
    (expect (cl-cc:vm-fill-pointer-reg inst) :to-be-null)
    (expect (cl-cc:vm-adjustable-reg inst) :to-be-null)
    (expect (cl-cc:vm-element-type-reg inst) :to-be-null)
    (expect (cl-cc:vm-displaced-to-reg inst) :to-be-null)))

(it-sequential "vm-array-aref-and-aset"
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 1)
    (cl-cc:vm-reg-set s 3 99)
    (exec1 (cl-cc:make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s)
    (exec1 (cl-cc:make-vm-aref :dst 0 :array-reg 1 :index-reg 2) s)
    (expect (= 99 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

(it-sequential "vm-array-fill-writes-all-elements"
  (let ((s (make-test-vm))
        (arr (make-array 4 :initial-element 0)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 7)
    (exec1 (cl-cc:make-vm-fill :array-reg 1 :val-reg 2) s)
    (loop for i below 4
          do (expect (= 7 (aref arr i)) :to-be-truthy))))

(it-sequential "vm-array-vector-push-extend-grows"
  (let ((s (make-test-vm))
        (v (make-array 2 :fill-pointer 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 'hello)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc:make-vm-vector-push-extend :dst 0 :val-reg 1 :array-reg 2) s)
    (expect (= 0 (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 1 (fill-pointer v)) :to-be-truthy)
    (expect (aref v 0) :to-be 'hello)))

(it-sequential "vm-array-length-vector"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(a b c d))
    (exec1 (cl-cc:make-vm-array-length :dst 0 :src 1) s)
    (expect (= 4 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

(it-sequential "vm-array-vectorp vector"
  (destructuring-bind (value expected) (list #(1 2) 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 value)
    (exec1 (cl-cc:make-vm-vectorp :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-vectorp non-vector"
  (destructuring-bind (value expected) (list '(a b) 0)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 value)
    (exec1 (cl-cc:make-vm-vectorp :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

;;; ─── Array dimension queries ────────────────────────────────────────────────

(it-sequential "vm-array-rank 1d"
  (destructuring-bind (arr expected) (list #(1 2 3) 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc:make-vm-array-rank :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-rank 2d"
  (destructuring-bind (arr expected) (list (make-array '(2 3)) 2)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc:make-vm-array-rank :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-dim-queries"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 4)))
    (exec1 (cl-cc:make-vm-array-total-size :dst 0 :src 1) s)
    (expect (= 12 (cl-cc:vm-reg-get s 0)) :to-be-truthy))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(2 5)))
    (exec1 (cl-cc:make-vm-array-dimensions :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal '(2 5)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (make-array '(3 7)))
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc:make-vm-array-dimension :dst 0 :lhs 1 :rhs 2) s)
    (expect (= 7 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

;;; ─── Row-major access ───────────────────────────────────────────────────────

(it-sequential "vm-array-row-major-operations"
  (let ((s   (make-test-vm))
        (arr (make-array '(2 3) :initial-contents '((10 20 30) (40 50 60)))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 4)
    (exec1 (cl-cc:make-vm-row-major-aref :dst 0 :lhs 1 :rhs 2) s)
    (expect (= 50 (cl-cc:vm-reg-get s 0)) :to-be-truthy))
  (let ((s   (make-test-vm))
        (arr (make-array '(2 3))))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 '(1 2))
    (exec1 (cl-cc:make-vm-array-row-major-index :dst 0 :arr 1 :subs 2) s)
    (expect (= 5 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

;;; ─── svref ──────────────────────────────────────────────────────────────────

(it-sequential "vm-array-svref-and-svset"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 #(a b c))
    (cl-cc:vm-reg-set s 2 1)
    (exec1 (cl-cc:make-vm-svref :dst 0 :lhs 1 :rhs 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be 'b))
  (let ((s (make-test-vm))
        (v (vector 'a 'b 'c)))
    (cl-cc:vm-reg-set s 1 v)
    (cl-cc:vm-reg-set s 2 0)
    (cl-cc:vm-reg-set s 3 'z)
    (exec1 (cl-cc:make-vm-svset :dst 0 :array-reg 1 :index-reg 2 :val-reg 3) s)
    (expect (svref v 0) :to-be 'z)
    (expect (cl-cc:vm-reg-get s 0) :to-be 'z)))

;;; ─── Fill-pointer operations ────────────────────────────────────────────────

(it-sequential "vm-array-fill-pointer-query fill-pointer"
  (destructuring-bind (arr ctor expected) (list (make-array 5 :fill-pointer 3) #'cl-cc:make-vm-fill-pointer-inst 3)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-fill-pointer-query has-fill-pointer"
  (destructuring-bind (arr ctor expected) (list (make-array 5 :fill-pointer 0) #'cl-cc:make-vm-array-has-fill-pointer-p 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-fill-pointer-query adjustable"
  (destructuring-bind (arr ctor expected) (list (make-array 5 :adjustable t) #'cl-cc:make-vm-array-adjustable-p 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-array-vector-push-and-pop"
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 'x)
    (cl-cc:vm-reg-set s 2 v)
    (exec1 (cl-cc:make-vm-vector-push :dst 0 :val-reg 1 :array-reg 2) s)
    (expect (= 0 (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 1 (fill-pointer v)) :to-be-truthy))
  (let ((s (make-test-vm))
        (v (make-array 5 :fill-pointer 2 :initial-contents '(a b 0 0 0))))
    (cl-cc:vm-reg-set s 1 v)
    (exec1 (cl-cc:make-vm-vector-pop :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be 'b)
    (expect (= 1 (fill-pointer v)) :to-be-truthy)))

(it-sequential "vm-array-set-fill-pointer"
  (let ((s (make-test-vm))
        (v (make-array 10 :fill-pointer 0)))
    (cl-cc:vm-reg-set s 1 v)
    (cl-cc:vm-reg-set s 2 7)
    (exec1 (cl-cc:make-vm-set-fill-pointer :dst 0 :array-reg 1 :val-reg 2) s)
    (expect (= 7 (fill-pointer v)) :to-be-truthy)
    (expect (= 7 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

;;; ─── Bit array operations ──────────────────────────────────────────────────

(it-sequential "vm-bit-simple-reads bit-access"
  (destructuring-bind (ctor ba idx expected) (list #'cl-cc:make-vm-bit-access (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0)) 2 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 idx)
    (exec1 (funcall ctor :dst 0 :arr 1 :idx 2) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-bit-simple-reads sbit"
  (destructuring-bind (ctor ba idx expected) (list #'cl-cc:make-vm-sbit (make-array 3 :element-type 'bit :initial-contents '(0 1 0)) 1 1)
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 idx)
    (exec1 (funcall ctor :dst 0 :arr 1 :idx 2) s)
    (expect (= expected (cl-cc:vm-reg-get s 0)) :to-be-truthy))))

(it-sequential "vm-bit-set-writes"
  (let ((s (make-test-vm))
        (ba (make-array 4 :element-type 'bit :initial-element 0)))
    (cl-cc:vm-reg-set s 1 ba)
    (cl-cc:vm-reg-set s 2 1)
    (cl-cc:vm-reg-set s 3 1)
    (exec1 (cl-cc:make-vm-bit-set :dst 0 :arr 1 :idx 2 :val 3) s)
    (expect (= 1 (bit ba 1)) :to-be-truthy)
    (expect (= 1 (cl-cc:vm-reg-get s 0)) :to-be-truthy)))

(it-sequential "vm-bit-and-elementwise"
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 1 0 0)))
        (b (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (cl-cc:vm-reg-set s 2 b)
    (exec1 (cl-cc:make-vm-bit-and :dst 0 :lhs 1 :rhs 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 1) (1 0) (2 0) (3 0))
            do (expect (= expected (bit result idx)) :to-be-truthy)))))

(it-sequential "vm-bit-or-elementwise"
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 1 0 0)))
        (b (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (cl-cc:vm-reg-set s 2 b)
    (exec1 (cl-cc:make-vm-bit-or :dst 0 :lhs 1 :rhs 2) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 1) (1 1) (2 1) (3 0))
            do (expect (= expected (bit result idx)) :to-be-truthy)))))

(it-sequential "vm-bit-not-inverts"
  (let ((s (make-test-vm))
        (a (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
    (cl-cc:vm-reg-set s 1 a)
    (exec1 (cl-cc:make-vm-bit-not :dst 0 :src 1) s)
    (let ((result (cl-cc:vm-reg-get s 0)))
      (loop for (idx expected) in '((0 0) (1 1) (2 0) (3 1))
            do (expect (= expected (bit result idx)) :to-be-truthy)))))

;;; ─── adjust-array / array-displacement ──────────────────────────────────────

(it-sequential "vm-adjust-array-grows-array-to-new-size"
  (let ((s (make-test-vm))
        (arr (make-array 3 :initial-element 0 :adjustable t)))
    (cl-cc:vm-reg-set s 1 arr)
    (cl-cc:vm-reg-set s 2 5)
    (exec1 (cl-cc:make-vm-adjust-array :dst 0 :arr 1 :dims 2) s)
    (expect (= 5 (length (cl-cc:vm-reg-get s 0))) :to-be-truthy)))

(it-sequential "vm-array-displacement-returns-nil-for-non-displaced"
  (let ((s (make-test-vm))
        (arr (make-array 3)))
    (cl-cc:vm-reg-set s 1 arr)
    (exec1 (cl-cc:make-vm-array-displacement :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be-null)))

(it-sequential "vm-array-cow-copy-seq-shares-then-aset-detaches" (let* ((s (make-test-vm)) (source (vector 10 20 30)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (expect (cl-cc/vm::copy-on-write-array-p copy) :to-be-truthy) (expect (cl-cc/vm::vm-cow-vector-backing copy) :to-be source) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 1) (cl-cc:vm-reg-set s 3 99) (exec1 (cl-cc:make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s) (expect (aref source 1) :to-equal 20) (expect (aref (cl-cc/vm::vm-cow-vector-backing copy) 1) :to-equal 99) (expect (cl-cc/vm::vm-cow-vector-backing copy) :not :to-be source)))

(it-sequential "vm-array-cow-bit-vector-copy-detaches-on-bit-set" (let* ((s (make-test-vm)) (source (cl-cc/vm:vm-make-specialized-array 4 :bit)) (copy (cl-cc/vm::vm-bit-vector-copy source))) (setf (cl-cc/vm:vm-specialized-array-ref source 1) 1) (expect (cl-cc/vm::vm-cow-vector-backing copy) :to-be source) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 1) (cl-cc:vm-reg-set s 3 0) (exec1 (cl-cc:make-vm-bit-set :dst 0 :arr 1 :idx 2 :val 3) s) (expect (cl-cc/vm:vm-specialized-array-ref source 1) :to-equal 1) (expect (cl-cc/vm:vm-specialized-array-ref (cl-cc/vm::vm-cow-vector-backing copy) 1) :to-equal 0) (expect (cl-cc/vm::vm-cow-vector-backing copy) :not :to-be source)))

(it-sequential "vm-array-cow-displaced-copy-preserves-offset-then-detaches" (let* ((s (make-test-vm)) (base (vector 0 10 20 30)) (source (make-array 2 :displaced-to base :displaced-index-offset 1)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (multiple-value-bind (target offset) (array-displacement (cl-cc/vm::vm-cow-vector-backing copy)) (expect target :to-be base) (expect offset :to-equal 1)) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 0) (cl-cc:vm-reg-set s 3 77) (exec1 (cl-cc:make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s) (expect (aref source 0) :to-equal 10) (expect (aref (cl-cc/vm::vm-cow-vector-backing copy) 0) :to-equal 77) (multiple-value-bind (target offset) (array-displacement (cl-cc/vm::vm-cow-vector-backing copy)) (expect target :to-be-null) (expect offset :to-equal 0))))

(it-sequential "vm-array-cow-adjust-array-dissolves-wrapper" (let* ((s (make-test-vm)) (source (make-array 2 :initial-element 5 :adjustable t)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 4) (exec1 (cl-cc:make-vm-adjust-array :dst 0 :arr 1 :dims 2) s) (let ((adjusted (cl-cc:vm-reg-get s 0))) (expect (cl-cc/vm::copy-on-write-array-p copy) :to-be-truthy) (expect (cl-cc/vm::copy-on-write-array-p adjusted) :to-be-falsy) (expect (cl-cc/vm::vm-adjustable-array-p-value copy) :to-be-truthy) (expect (array-total-size adjusted) :to-equal 4) (expect (aref source 0) :to-equal 5))))
