;;;; tests/unit/vm/vm-tests.lisp — VM core helper tests

(in-package :cl-cc/test)



;; SKIP (Nix sandbox): *features* initialization differs in sandbox
(it-sequential "vm-heap-address-normalization"
  (let ((wrapped (cl-cc/vm::make-vm-heap-address :value 7)))
    (expect (= 9 (cl-cc/vm::vm-heap-address 9)) :to-be-truthy)
    (expect (= 7 (cl-cc/vm::vm-heap-address wrapped)) :to-be-truthy)
    (expect (cl-cc/vm::vm-heap-address nil) :to-be-null)))

(it-sequential "vm-build-list-stack-allocates-values"
  (let* ((values (list 1 2 3))
          (result (cl-cc/vm::vm-build-list nil values :stack-allocate-p t)))
    (expect result :to-equal values)
    (expect result :to-be values)))

(it-sequential "vm-arg-slot-name-helper zero"
  (destructuring-bind (index expected) (list 0 :ARG0)
    (expect (cl-cc/vm::vm-arg-slot-name index) :to-be expected)))

(it-sequential "vm-arg-slot-name-helper one"
  (destructuring-bind (index expected) (list 1 :ARG1)
    (expect (cl-cc/vm::vm-arg-slot-name index) :to-be expected)))

(it-sequential "vm-arg-slot-name-helper seven"
  (destructuring-bind (index expected) (list 7 :ARG7)
    (expect (cl-cc/vm::vm-arg-slot-name index) :to-be expected)))

(it-sequential "vm-bind-arg-slots-binds-leading-args"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((slots (cl-cc/vm::vm-bind-arg-slots state '(10 20 30 40 50 60 70 80 90))))
      (expect slots :to-equal '(:ARG0 :ARG1 :ARG2 :ARG3 :ARG4 :ARG5 :ARG6 :ARG7))
      (expect (= 10 (cl-cc:vm-reg-get state :ARG0)) :to-be-truthy)
      (expect (= 80 (cl-cc:vm-reg-get state :ARG7)) :to-be-truthy))))

(it-sequential "vm-bind-closure-args-populates-arg-slots"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state))
        (closure (make-instance 'cl-cc/vm::vm-closure-object
                                :entry-label "f"
                                :params '(:R10 :R11)
                                :captured-regs #()
                                :captured-vals #())))
    (cl-cc/vm::vm-bind-closure-args closure state '(1 2 3))
    (expect (= 1 (cl-cc:vm-reg-get state :ARG0)) :to-be-truthy)
    (expect (= 2 (cl-cc:vm-reg-get state :ARG1)) :to-be-truthy)
    (expect (= 3 (cl-cc:vm-reg-get state :ARG2)) :to-be-truthy)
    (expect (= 1 (cl-cc:vm-reg-get state :R10)) :to-be-truthy)
    (expect (= 2 (cl-cc:vm-reg-get state :R11)) :to-be-truthy)))

(it-sequential "vm-host-bridge-registration"
  (let ((sym (gensym "VM-BRIDGE-")))
    (unwind-protect
        (progn
          (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-null)
          (cl-cc/vm::vm-register-host-bridge sym (lambda () :ok))
          (expect (gethash sym cl-cc/vm::*vm-host-bridge-functions*) :to-be-truthy))
      (remhash sym cl-cc/vm::*vm-host-bridge-functions*))))

;;; ─── vm runtime objects and heap helpers ────────────────────────────────────

(it-sequential "vm-closure-object"
  (let ((c (make-instance 'cl-cc/vm::vm-closure-object
                           :entry-label "my-fn"
                           :params (list :r1 :r2)
                           :captured-regs #()
                           :captured-vals #())))
    (expect (cl-cc/vm::vm-closure-entry-label c) :to-equal "my-fn")
    (expect (cl-cc/vm::vm-closure-params c) :to-equal (list :r1 :r2))
    (expect (vectorp (cl-cc/vm::vm-closure-captured-regs c)) :to-be-truthy)
    (expect (= 0 (length (cl-cc/vm::vm-closure-captured-regs c))) :to-be-truthy)
    (expect (vectorp (cl-cc/vm::vm-closure-captured-vals c)) :to-be-truthy)
    (expect (= 0 (length (cl-cc/vm::vm-closure-captured-vals c))) :to-be-truthy)
    (expect (typep c 'cl-cc/vm::vm-closure-object) :to-be-truthy))
  (let ((c (make-instance 'cl-cc/vm::vm-closure-object
                           :entry-label "adder"
                           :params (list :r1)
                           :captured-regs (vector :r0)
                           :captured-vals (vector 10))))
    (expect (vectorp (cl-cc/vm::vm-closure-captured-regs c)) :to-be-truthy)
    (expect (= 1 (length (cl-cc/vm::vm-closure-captured-regs c))) :to-be-truthy)
    (expect (aref (cl-cc/vm::vm-closure-captured-regs c) 0) :to-equal :r0)
    (expect (vectorp (cl-cc/vm::vm-closure-captured-vals c)) :to-be-truthy)
    (expect (= 1 (length (cl-cc/vm::vm-closure-captured-vals c))) :to-be-truthy)
    (expect (aref (cl-cc/vm::vm-closure-captured-vals c) 0) :to-equal 10)))

(it-sequential "vm-cons-cell"
  (let ((cell (make-instance 'cl-cc/vm::vm-cons-cell :car 1 :cdr 2)))
    (expect (= 1 (cl-cc/vm::vm-cons-cell-car cell)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::vm-cons-cell-cdr cell)) :to-be-truthy)
    (setf (cl-cc/vm::vm-cons-cell-car cell) 99)
    (expect (= 99 (cl-cc/vm::vm-cons-cell-car cell)) :to-be-truthy)
    (expect (typep cell 'cl-cc/vm::vm-heap-object) :to-be-truthy)))

(it-sequential "vm-heap-address-struct"
  (let ((ha (cl-cc/vm::make-vm-heap-address :value 42)))
    (expect (= 42 (cl-cc/vm::vm-heap-address-value ha)) :to-be-truthy)
    (expect (cl-cc/vm::vm-heap-address-p ha) :to-be-truthy)))

(it-sequential "rt-plist-put"
  (expect (getf (cl-cc/bootstrap::rt-plist-put nil :foo 42) :foo) :to-equal 42)
  (let ((result (cl-cc/bootstrap::rt-plist-put '(:foo 1 :bar 2) :foo 99)))
    (expect (getf result :foo) :to-equal 99)
    (expect (getf result :bar) :to-equal 2))
  (let ((result (cl-cc/bootstrap::rt-plist-put '(:a 1 :b 2 :c 3) :b 20)))
    (expect (getf result :a) :to-equal 1)
    (expect (getf result :b) :to-equal 20)
    (expect (getf result :c) :to-equal 3))
  (let ((orig '(:x 10)))
    (cl-cc/bootstrap::rt-plist-put orig :x 99)
    (expect (getf orig :x) :to-equal 10)))

(it-sequential "vm-heap-alloc-operations"
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((addr (cl-cc/vm::vm-heap-alloc s :some-object)))
      (expect (integerp addr) :to-be-truthy)
      (expect (> addr 0) :to-be-truthy)))
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((addr (cl-cc/vm::vm-heap-alloc s "original")))
      (cl-cc/vm::vm-heap-set s addr "replaced")
      (expect (cl-cc/vm::vm-heap-get s addr) :to-equal "replaced")))
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((a1 (cl-cc/vm::vm-heap-alloc s 1))
          (a2 (cl-cc/vm::vm-heap-alloc s 2))
          (a3 (cl-cc/vm::vm-heap-alloc s 3)))
      (expect (/= a1 a2) :to-be-truthy)
      (expect (/= a2 a3) :to-be-truthy)
      (expect (/= a1 a3) :to-be-truthy))))

(it-sequential "vm-heap-alloc-roundtrip string"
  (destructuring-bind (value) (list "test-payload")
    (let* ((s    (make-instance 'cl-cc/vm::vm-io-state))
         (addr (cl-cc/vm::vm-heap-alloc s value)))
    (expect (cl-cc/vm::vm-heap-get s addr) :to-equal value))))

(it-sequential "vm-heap-alloc-roundtrip list"
  (destructuring-bind (value) (list '(a b c))
    (let* ((s    (make-instance 'cl-cc/vm::vm-io-state))
         (addr (cl-cc/vm::vm-heap-alloc s value)))
    (expect (cl-cc/vm::vm-heap-get s addr) :to-equal value))))
