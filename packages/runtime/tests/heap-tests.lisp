;;;; tests/vm-heap-tests.lisp - VM Heap Operations Tests
;;;
;;; This module provides tests for VM heap operations including:
;;; - Cons cell allocation and access
;;; - Car/cdr operations
;;; - Rplaca/rplacd mutation
;;; - Closure creation and access
;;;
;;; Instruction serialization, roundtrip, and integration list-building tests
;;; continue in heap-gc-tests.lisp.

(in-package :cl-cc/test)


;;; Heap Allocation Tests

(it-sequential "vm-heap-alloc-first-returns-addr-1"
  (let* ((state (make-instance 'vm-io-state))
         (addr (vm-heap-alloc state nil)))
    (expect (= addr 1) :to-be-truthy)
    (expect (= (vm-heap-counter state) 1) :to-be-truthy)))

(it-sequential "vm-heap-alloc-multiple-yields-unique-addrs"
  (let* ((state (make-instance 'vm-io-state))
         (addr1 (vm-heap-alloc state nil))
         (addr2 (vm-heap-alloc state nil))
         (addr3 (vm-heap-alloc state nil)))
    (expect (/= addr1 addr2) :to-be-truthy)
    (expect (/= addr2 addr3) :to-be-truthy)
    (expect (= (vm-heap-counter state) 3) :to-be-truthy)))

(it-sequential "vm-heap-get-set-roundtrip"
  (let* ((state (make-instance 'vm-io-state))
         (addr (incf (vm-heap-counter state))))
    (vm-heap-set state addr :test-value)
    (expect :test-value :to-be (vm-heap-get state addr))))

;;; Cons Cell Tests

(it-sequential "vm-cons-creation-with-non-nil-cdr"
  (let* ((state (make-instance 'vm-io-state))
         (inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (consp cell) :to-be-truthy)
      (expect (= (car cell) 10) :to-be-truthy)
      (expect (= (cdr cell) 20) :to-be-truthy))))

(it-sequential "vm-cons-creation-with-nil-cdr"
  (let* ((state (make-instance 'vm-io-state))
         (inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (vm-reg-set state 1 42)
    (vm-reg-set state 2 nil)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (consp cell) :to-be-truthy)
      (expect (= (car cell) 42) :to-be-truthy)
      (expect (cdr cell) :to-be-null))))

(it-sequential "vm-cons-nested"
  (let* ((state (make-instance 'vm-io-state))
         ;; Create first cons: (2 . nil)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         ;; Create second cons: (1 . <addr of first cons>)
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0)))
    (vm-reg-set state 1 2)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 1)
    (execute-instruction inst2 state 1 (make-hash-table))
    ;; Verify structure
    (let* ((outer-cell (vm-reg-get state 3))
           (inner-cell (cdr outer-cell)))
      (expect (= (car outer-cell) 1) :to-be-truthy)
      (expect (= (car inner-cell) 2) :to-be-truthy)
      (expect (cdr inner-cell) :to-be-null))))

;;; Car/Cdr Tests

(it-sequential "vm-car-extracts-car-of-cons"
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (car-inst (make-vm-car :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction car-inst state 1 (make-hash-table))
    (expect (= (vm-reg-get state 3) 123) :to-be-truthy)))

(it-sequential "vm-cdr-extracts-cdr-of-cons"
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (cdr-inst (make-vm-cdr :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction cdr-inst state 1 (make-hash-table))
    (expect (= (vm-reg-get state 3) 456) :to-be-truthy)))

(it-sequential "vm-car-traverses-nested-cons"
  (let* ((state (make-instance 'vm-io-state))
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (inst2 (make-vm-cons :dst 3 :car-src 0 :cdr-src 4))
         (car-inst (make-vm-car :dst 5 :src 3))
         (car-inst2 (make-vm-car :dst 6 :src 5)))
    (vm-reg-set state 1 2)
    (vm-reg-set state 2 3)
    (vm-reg-set state 4 4)
    (execute-instruction inst1 state 0 (make-hash-table))
    (execute-instruction inst2 state 1 (make-hash-table))
    (execute-instruction car-inst state 2 (make-hash-table))
    (execute-instruction car-inst2 state 3 (make-hash-table))
    (expect (= (vm-reg-get state 6) 2) :to-be-truthy)))

;;; Rplaca/Rplacd Tests

(it-sequential "vm-rplaca-mutates-car"
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplaca-inst (make-vm-rplaca :cons 0 :val 3)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (= 10 (car cell)) :to-be-truthy))
    (vm-reg-set state 3 99)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (= 99 (car cell)) :to-be-truthy)
      (expect (= 20 (cdr cell)) :to-be-truthy))))

(it-sequential "vm-rplacd-mutates-cdr"
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplacd-inst (make-vm-rplacd :cons 0 :val 3)))
    (vm-reg-set state 1 11)
    (vm-reg-set state 2 21)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (= 21 (cdr cell)) :to-be-truthy))
    (vm-reg-set state 3 88)
    (execute-instruction rplacd-inst state 1 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (= 11 (car cell)) :to-be-truthy)
      (expect (= 88 (cdr cell)) :to-be-truthy))))

(it-sequential "vm-rplaca-and-rplacd-together"
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplaca-inst (make-vm-rplaca :cons 0 :val 3))
         (rplacd-inst (make-vm-rplacd :cons 0 :val 4)))
    (vm-reg-set state 1 1)
    (vm-reg-set state 2 2)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (vm-reg-set state 3 100)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    (vm-reg-set state 4 200)
    (execute-instruction rplacd-inst state 2 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (expect (= (car cell) 100) :to-be-truthy)
      (expect (= (cdr cell) 200) :to-be-truthy))))

;;; Closure Heap Operations Tests

(it-sequential "vm-make-closure-heap-alloc"
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (inst (make-vm-make-closure
                              :dst 0
                              :label :func
                              :params '(:x)
                              :env-regs '(1 2))))
    (setf (gethash :func labels) 10)
    (vm-reg-set state 1 42)
    (vm-reg-set state 2 99)
    (execute-instruction inst state 0 labels)
    (let* ((addr (vm-reg-get state 0))
           (closure (vm-heap-get state addr)))
      (expect (typep closure 'vm-closure-object) :to-be-truthy)
      (expect :func :to-be (vm-closure-entry-label closure))
      (expect '(:x) :to-equal (vm-closure-params closure)))))

(it-sequential "vm-closure-ref-idx-accesses-value"
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst 0
                                   :label :func
                                   :params nil
                                   :env-regs '(1 2 3)))
         (ref-inst-0 (make-vm-closure-ref-idx
                                    :dst 4
                                    :closure 0
                                    :index 0))
         (ref-inst-1 (make-vm-closure-ref-idx
                                    :dst 5
                                    :closure 0
                                    :index 1))
         (ref-inst-2 (make-vm-closure-ref-idx
                                    :dst 6
                                    :closure 0
                                    :index 2)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (vm-reg-set state 3 30)
    (execute-instruction make-inst state 0 labels)
    (execute-instruction ref-inst-0 state 1 labels)
    (execute-instruction ref-inst-1 state 2 labels)
    (execute-instruction ref-inst-2 state 3 labels)
    (expect (= (vm-reg-get state 4) 10) :to-be-truthy)
    (expect (= (vm-reg-get state 5) 20) :to-be-truthy)
    (expect (= (vm-reg-get state 6) 30) :to-be-truthy)))

(it-sequential "vm-closure-ref-idx-out-of-bounds"
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst 0
                                   :label :func
                                   :params nil
                                   :env-regs '(1)))
         (ref-inst (make-vm-closure-ref-idx
                                  :dst 2
                                  :closure 0
                                  :index 99)))
    (vm-reg-set state 1 42)
    (execute-instruction make-inst state 0 labels)
    (handler-case
        (progn
          (execute-instruction ref-inst state 1 labels)
          (%fail-test "Should have signaled an error"))
      (error () t))))
