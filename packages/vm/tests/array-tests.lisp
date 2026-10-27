;;;; tests/unit/vm/array-tests.lisp — VM Array and Bit Array Instruction Tests
;;;;
;;;; Tests for array, vector, and bit-array instructions executed via the VM.

(in-package :cl-cc/test)

;;; ─── Array operations ──────────────────────────────────────────────────────

;;; ─── Array dimension queries ────────────────────────────────────────────────

;;; ─── Row-major access ───────────────────────────────────────────────────────

;;; ─── svref ──────────────────────────────────────────────────────────────────

;;; ─── Fill-pointer operations ────────────────────────────────────────────────

;;; ─── Bit array operations ──────────────────────────────────────────────────

;;; ─── adjust-array / array-displacement ──────────────────────────────────────

(it-sequential "vm-array-cow-copy-seq-shares-then-aset-detaches" (let* ((s (make-test-vm)) (source (vector 10 20 30)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (expect (cl-cc/vm::copy-on-write-array-p copy) :to-be-truthy) (expect (cl-cc/vm::vm-cow-vector-backing copy) :to-be source) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 1) (cl-cc:vm-reg-set s 3 99) (exec1 (cl-cc:make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s) (expect (aref source 1) :to-equal 20) (expect (aref (cl-cc/vm::vm-cow-vector-backing copy) 1) :to-equal 99) (expect (cl-cc/vm::vm-cow-vector-backing copy) :not :to-be source)))

(it-sequential "vm-array-cow-bit-vector-copy-detaches-on-bit-set" (let* ((s (make-test-vm)) (source (cl-cc/vm:vm-make-specialized-array 4 :bit)) (copy (cl-cc/vm::vm-bit-vector-copy source))) (setf (cl-cc/vm:vm-specialized-array-ref source 1) 1) (expect (cl-cc/vm::vm-cow-vector-backing copy) :to-be source) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 1) (cl-cc:vm-reg-set s 3 0) (exec1 (cl-cc:make-vm-bit-set :dst 0 :arr 1 :idx 2 :val 3) s) (expect (cl-cc/vm:vm-specialized-array-ref source 1) :to-equal 1) (expect (cl-cc/vm:vm-specialized-array-ref (cl-cc/vm::vm-cow-vector-backing copy) 1) :to-equal 0) (expect (cl-cc/vm::vm-cow-vector-backing copy) :not :to-be source)))

(it-sequential "vm-array-cow-displaced-copy-preserves-offset-then-detaches" (let* ((s (make-test-vm)) (base (vector 0 10 20 30)) (source (make-array 2 :displaced-to base :displaced-index-offset 1)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (multiple-value-bind (target offset) (array-displacement (cl-cc/vm::vm-cow-vector-backing copy)) (expect target :to-be base) (expect offset :to-equal 1)) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 0) (cl-cc:vm-reg-set s 3 77) (exec1 (cl-cc:make-vm-aset :array-reg 1 :index-reg 2 :val-reg 3) s) (expect (aref source 0) :to-equal 10) (expect (aref (cl-cc/vm::vm-cow-vector-backing copy) 0) :to-equal 77) (multiple-value-bind (target offset) (array-displacement (cl-cc/vm::vm-cow-vector-backing copy)) (expect target :to-be-null) (expect offset :to-equal 0))))

(it-sequential "vm-array-cow-adjust-array-dissolves-wrapper" (let* ((s (make-test-vm)) (source (make-array 2 :initial-element 5 :adjustable t)) (copy (cl-cc/vm::vm-cow-copy-seq source))) (cl-cc:vm-reg-set s 1 copy) (cl-cc:vm-reg-set s 2 4) (exec1 (cl-cc:make-vm-adjust-array :dst 0 :arr 1 :dims 2) s) (let ((adjusted (cl-cc:vm-reg-get s 0))) (expect (cl-cc/vm::copy-on-write-array-p copy) :to-be-truthy) (expect (cl-cc/vm::copy-on-write-array-p adjusted) :to-be-falsy) (expect (cl-cc/vm::vm-adjustable-array-p-value copy) :to-be-truthy) (expect (array-total-size adjusted) :to-equal 4) (expect (aref source 0) :to-equal 5))))
