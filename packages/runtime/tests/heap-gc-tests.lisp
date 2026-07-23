;;;; tests/vm-heap-gc-tests.lisp — VM Heap Instruction Serialization and Integration Tests
;;;
;;; Covers:
;;; - instruction->sexp / sexp->instruction for heap instructions
;;; - Round-trip: instruction->sexp->sexp->instruction fidelity
;;; - Integration: building and traversing lists via cons/car/cdr sequences
;;;
;;; Helper functions (eval-cps-ast etc.) defined in heap-tests.lisp are
;;; available here because ASDF serial load order ensures heap-tests loads first.

(in-package :cl-cc/test)


;;; Instruction <-> S-exp Conversion Tests

(it-sequential "instruction->sexp-heap-instructions vm-cons"
  (destructuring-bind (inst expected) (list (make-vm-cons :dst 0 :car-src 1 :cdr-src 2) '(:cons 0 1 2))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-car"
  (destructuring-bind (inst expected) (list (make-vm-car :dst 0 :src 1) '(:car 0 1))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-cdr"
  (destructuring-bind (inst expected) (list (make-vm-cdr :dst 0 :src 1) '(:cdr 0 1))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-rplaca"
  (destructuring-bind (inst expected) (list (make-vm-rplaca :cons 0 :val 1) '(:rplaca 0 1))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-rplacd"
  (destructuring-bind (inst expected) (list (make-vm-rplacd :cons 0 :val 1) '(:rplacd 0 1))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-make-closure"
  (destructuring-bind (inst expected) (list (make-vm-make-closure :dst 0 :label :func :params '(:x :y) :env-regs '(1 2)) '(:make-closure 0 :func (:x :y) 1 2))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-func-ref"
  (destructuring-bind (inst expected) (list (make-vm-func-ref :dst 0 :label :func :params '(:x :y)
                                                 :optional-params nil :rest-param nil
                                                 :key-params nil :rest-stack-alloc-p nil
                                                 :inline-policy nil :dispatch-tag '(:anonymous)) '(:func-ref 0 :func (:x :y) nil nil nil nil nil (:anonymous)))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "instruction->sexp-heap-instructions vm-closure-ref"
  (destructuring-bind (inst expected) (list (make-vm-closure-ref-idx :dst 0 :closure 1 :index 2) '(:closure-ref-idx 0 1 2))
    (expect (instruction->sexp inst) :to-equal expected)))

(it-sequential "sexp->instruction-vm-cons"
  (let ((inst (sexp->instruction '(:cons 0 1 2))))
    (expect (typep inst 'vm-cons) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect 1 :to-be (vm-car-reg inst))
    (expect 2 :to-be (vm-cdr-reg inst))))

(it-sequential "sexp->instruction-dst-src-ops car"
  (destructuring-bind (sexp expected-type) (list '(:car 0 1) 'vm-car)
    (let ((inst (sexp->instruction sexp)))
    (expect (typep inst expected-type) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect 1 :to-be (vm-src inst)))))

(it-sequential "sexp->instruction-dst-src-ops cdr"
  (destructuring-bind (sexp expected-type) (list '(:cdr 0 1) 'vm-cdr)
    (let ((inst (sexp->instruction sexp)))
    (expect (typep inst expected-type) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect 1 :to-be (vm-src inst)))))

(it-sequential "sexp->instruction-cons-val-ops rplaca"
  (destructuring-bind (sexp expected-type) (list '(:rplaca 0 1) 'vm-rplaca)
    (let ((inst (sexp->instruction sexp)))
    (expect (typep inst expected-type) :to-be-truthy)
    (expect 0 :to-be (vm-cons-reg inst))
    (expect 1 :to-be (vm-val-reg inst)))))

(it-sequential "sexp->instruction-cons-val-ops rplacd"
  (destructuring-bind (sexp expected-type) (list '(:rplacd 0 1) 'vm-rplacd)
    (let ((inst (sexp->instruction sexp)))
    (expect (typep inst expected-type) :to-be-truthy)
    (expect 0 :to-be (vm-cons-reg inst))
    (expect 1 :to-be (vm-val-reg inst)))))

(it-sequential "sexp->instruction-vm-make-closure"
  (let ((inst (sexp->instruction '(:make-closure 0 :func (:x) 1))))
    (expect (typep inst 'vm-make-closure) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect :func :to-be (vm-label-name inst))
    (expect '(:x) :to-equal (vm-make-closure-params inst))
    (expect '(1) :to-equal (vm-env-regs inst))))

(it-sequential "sexp->instruction-vm-func-ref"
  (let ((inst (sexp->instruction '(:func-ref 0 :func (:x) nil nil nil nil nil (:anonymous)))))
    (expect (typep inst 'vm-func-ref) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect :func :to-be (vm-label-name inst))
    (expect '(:x) :to-equal (vm-closure-params inst))
    (expect '(:anonymous) :to-equal (vm-func-ref-dispatch-tag inst))))

(it-sequential "sexp->instruction-vm-closure-ref-idx"
  (let ((inst (sexp->instruction '(:closure-ref-idx 0 1 3))))
    (expect (typep inst 'vm-closure-ref-idx) :to-be-truthy)
    (expect 0 :to-be (vm-dst inst))
    (expect 1 :to-be (vm-closure-reg inst))
    (expect (= (vm-closure-index inst) 3) :to-be-truthy)))

;;; Round-trip Tests

(it-sequential "roundtrip-heap-instructions vm-cons"
  (destructuring-bind (original) (list (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-car"
  (destructuring-bind (original) (list (make-vm-car :dst 0 :src 1))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-cdr"
  (destructuring-bind (original) (list (make-vm-cdr :dst 0 :src 1))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-rplaca"
  (destructuring-bind (original) (list (make-vm-rplaca :cons 0 :val 1))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-rplacd"
  (destructuring-bind (original) (list (make-vm-rplacd :cons 0 :val 1))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-make-closure"
  (destructuring-bind (original) (list (make-vm-make-closure :dst 0 :label :func :params '(:x :y) :env-regs '(1 2)))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-func-ref"
  (destructuring-bind (original) (list (make-vm-func-ref :dst 0 :label :func :params '(:x :y)
                                                 :optional-params nil :rest-param nil
                                                 :key-params nil :rest-stack-alloc-p nil
                                                 :inline-policy nil :dispatch-tag '(:anonymous)))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

(it-sequential "roundtrip-heap-instructions vm-closure-ref"
  (destructuring-bind (original) (list (make-vm-closure-ref-idx :dst 0 :closure 1 :index 5))
    (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (expect (instruction->sexp restored) :to-equal sexp))))

;;; Integration: Building Lists

(it-sequential "build-list-of-three-elements"
  (let* ((state (make-instance 'vm-io-state))
         ;; Create (3 . nil)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         ;; Create (2 . <addr1>)
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0))
         ;; Create (1 . <addr2>)
         (inst3 (make-vm-cons :dst 5 :car-src 6 :cdr-src 3)))
    (vm-reg-set state 1 3)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 2)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state 6 1)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Verify list structure: (1 2 3)
    (let* ((cell1 (vm-reg-get state 5))
           (cell2 (cdr cell1))
           (cell3 (cdr cell2)))
      (expect (= (car cell1) 1) :to-be-truthy)
      (expect (= (car cell2) 2) :to-be-truthy)
      (expect (= (car cell3) 3) :to-be-truthy)
      (expect (cdr cell3) :to-be-null))))

(it-sequential "traverse-list-with-car-cdr"
  (let* ((state (make-instance 'vm-io-state))
         ;; Build list (10 20 30)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0))
         (inst3 (make-vm-cons :dst 5 :car-src 6 :cdr-src 3)))
    (vm-reg-set state 1 30)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 20)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state 6 10)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Traverse list using vm-car/vm-cdr instructions
    (let ((list-cell (vm-reg-get state 5)))
      ;; Get first element (car list)
      (vm-reg-set state 10 list-cell)
      (execute-instruction (make-vm-car :dst 11 :src 10) state 3 (make-hash-table))
      (expect (= (vm-reg-get state 11) 10) :to-be-truthy)
      ;; Move to second element (car (cdr list))
      (execute-instruction (make-vm-cdr :dst 12 :src 10) state 4 (make-hash-table))
      (execute-instruction (make-vm-car :dst 13 :src 12) state 5 (make-hash-table))
      (expect (= (vm-reg-get state 13) 20) :to-be-truthy)
      ;; Move to third element (car (cdr (cdr list)))
      (execute-instruction (make-vm-cdr :dst 14 :src 12) state 6 (make-hash-table))
      (execute-instruction (make-vm-car :dst 15 :src 14) state 7 (make-hash-table))
      (expect (= (vm-reg-get state 15) 30) :to-be-truthy))))
