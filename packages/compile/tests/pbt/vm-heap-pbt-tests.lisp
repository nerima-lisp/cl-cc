;;;; tests/pbt/vm-heap-pbt-tests.lisp - Property-Based Tests for VM Heap Operations
;;;
;;; Property-based tests for VM heap operations — cons cells, car/cdr,
;;; rplaca/rplacd, and closures — expressed with cl-weave's NATIVE property
;;; API (cl-weave:describe + it-property + gen-integer/gen-member) directly,
;;; rather than the home-grown cl-cc/pbt defproperty DSL. An it-property body
;;; is simply a boolean-valued property.
;;;
;;; Note: cl-weave:gen-member picks one VALUE from a list (what cl-cc/pbt's
;;; gen-one-of did); cl-weave:gen-one-of is a different combinator that picks
;;; among GENERATORS.

(in-package :cl-cc/pbt)

(cl-weave:describe "VM heap operation properties"

  ;; --- Cons cells (the VM uses native Lisp cons cells) ---

  (cl-weave:it-property "vm-cons creates a pair with the given car and cdr"
      ((car-val (cl-weave:gen-integer :min -1000 :max 1000))
       (cdr-val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((state (make-instance 'vm-io-state))
           (inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
      (vm-reg-set state :r1 car-val)
      (vm-reg-set state :r2 cdr-val)
      (execute-instruction inst state 0 (make-hash-table))
      (let ((result (vm-reg-get state :r0)))
        (and (consp result)
             (= (car result) car-val)
             (= (cdr result) cdr-val)))))

  (cl-weave:it-property "vm-car extracts the car of a constructed pair"
      ((car-val (cl-weave:gen-integer :min -1000 :max 1000))
       (cdr-val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((state (make-instance 'vm-io-state))
           (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           (car-inst (make-vm-car :dst :r3 :src :r0)))
      (vm-reg-set state :r1 car-val)
      (vm-reg-set state :r2 cdr-val)
      (execute-instruction cons-inst state 0 (make-hash-table))
      (execute-instruction car-inst state 1 (make-hash-table))
      (= (vm-reg-get state :r3) car-val)))

  (cl-weave:it-property "vm-cdr extracts the cdr of a constructed pair"
      ((car-val (cl-weave:gen-integer :min -1000 :max 1000))
       (cdr-val (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((state (make-instance 'vm-io-state))
           (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           (cdr-inst (make-vm-cdr :dst :r3 :src :r0)))
      (vm-reg-set state :r1 car-val)
      (vm-reg-set state :r2 cdr-val)
      (execute-instruction cons-inst state 0 (make-hash-table))
      (execute-instruction cdr-inst state 1 (make-hash-table))
      (= (vm-reg-get state :r3) cdr-val)))

  ;; --- rplaca / rplacd ---

  (cl-weave:it-property "vm-rplaca replaces the car, leaving the cdr intact"
      ((initial-car (cl-weave:gen-integer :min -1000 :max 1000))
       (initial-cdr (cl-weave:gen-integer :min -1000 :max 1000))
       (new-car (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((state (make-instance 'vm-io-state))
           (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           (rplaca-inst (make-vm-rplaca :cons :r0 :val :r3)))
      (vm-reg-set state :r1 initial-car)
      (vm-reg-set state :r2 initial-cdr)
      (execute-instruction cons-inst state 0 (make-hash-table))
      (vm-reg-set state :r3 new-car)
      (execute-instruction rplaca-inst state 1 (make-hash-table))
      (let ((result (vm-reg-get state :r0)))
        (and (= (car result) new-car)
             (= (cdr result) initial-cdr)))))

  (cl-weave:it-property "vm-rplacd replaces the cdr, leaving the car intact"
      ((initial-car (cl-weave:gen-integer :min -1000 :max 1000))
       (initial-cdr (cl-weave:gen-integer :min -1000 :max 1000))
       (new-cdr (cl-weave:gen-integer :min -1000 :max 1000)))
    (let* ((state (make-instance 'vm-io-state))
           (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           (rplacd-inst (make-vm-rplacd :cons :r0 :val :r3)))
      (vm-reg-set state :r1 initial-car)
      (vm-reg-set state :r2 initial-cdr)
      (execute-instruction cons-inst state 0 (make-hash-table))
      (vm-reg-set state :r3 new-cdr)
      (execute-instruction rplacd-inst state 1 (make-hash-table))
      (let ((result (vm-reg-get state :r0)))
        (and (= (car result) initial-car)
             (= (cdr result) new-cdr)))))

  (cl-weave:it-property "vm-rplaca and vm-rplacd act independently on a pair"
      ((val1 (cl-weave:gen-integer :min 0 :max 100))
       (val2 (cl-weave:gen-integer :min 0 :max 100))
       (val3 (cl-weave:gen-integer :min 0 :max 100))
       (val4 (cl-weave:gen-integer :min 0 :max 100)))
    (let* ((state (make-instance 'vm-io-state))
           (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           (rplaca-inst (make-vm-rplaca :cons :r0 :val :r3))
           (rplacd-inst (make-vm-rplacd :cons :r0 :val :r4)))
      (vm-reg-set state :r1 val1)
      (vm-reg-set state :r2 val2)
      (execute-instruction cons-inst state 0 (make-hash-table))
      (vm-reg-set state :r3 val3)
      (execute-instruction rplaca-inst state 1 (make-hash-table))
      (vm-reg-set state :r4 val4)
      (execute-instruction rplacd-inst state 2 (make-hash-table))
      (let ((result (vm-reg-get state :r0)))
        (and (= (car result) val3)
             (= (cdr result) val4)))))

  ;; --- Nested cons ---

  (cl-weave:it-property "nested vm-cons builds a proper 3-element list"
      ((val1 (cl-weave:gen-integer :min 0 :max 100))
       (val2 (cl-weave:gen-integer :min 0 :max 100))
       (val3 (cl-weave:gen-integer :min 0 :max 100)))
    (let* ((state (make-instance 'vm-io-state))
           ;; Create inner cons (val3 . nil)
           (inst1 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
           ;; Create middle cons (val2 . inner)
           (inst2 (make-vm-cons :dst :r3 :car-src :r4 :cdr-src :r0))
           ;; Create outer cons (val1 . middle)
           (inst3 (make-vm-cons :dst :r5 :car-src :r6 :cdr-src :r3)))
      (vm-reg-set state :r1 val3)
      (vm-reg-set state :r2 nil)
      (execute-instruction inst1 state 0 (make-hash-table))
      (vm-reg-set state :r4 val2)
      (execute-instruction inst2 state 1 (make-hash-table))
      (vm-reg-set state :r6 val1)
      (execute-instruction inst3 state 2 (make-hash-table))
      ;; Verify structure: (val1 val2 val3)
      (let ((result (vm-reg-get state :r5)))
        (and (= (car result) val1)
             (= (car (cdr result)) val2)
             (= (car (cdr (cdr result))) val3)
             (null (cdr (cdr (cdr result))))))))

  ;; --- Closures ---

  (cl-weave:it-property "vm-make-closure allocates a closure with its entry label"
      ((env-val1 (cl-weave:gen-integer :min 0 :max 100))
       (env-val2 (cl-weave:gen-integer :min 0 :max 100)))
    (let* ((state (make-instance 'vm-io-state))
           (labels (make-hash-table))
           (inst (make-vm-make-closure
                  :dst :r0
                  :label :func
                  :params nil
                  :env-regs '(:r1 :r2))))
      (setf (gethash :func labels) 10)
      (vm-reg-set state :r1 env-val1)
      (vm-reg-set state :r2 env-val2)
      (execute-instruction inst state 0 labels)
      (let* ((addr (vm-reg-get state :r0))
             (closure (vm-heap-get state addr)))
        (and (typep closure 'vm-closure-object)
             (eq (vm-closure-entry-label closure) :func)))))

  (cl-weave:it-property "vm-closure-ref-idx reads each captured environment value"
      ((env-val1 (cl-weave:gen-integer :min 0 :max 100))
       (env-val2 (cl-weave:gen-integer :min 0 :max 100))
       (env-val3 (cl-weave:gen-integer :min 0 :max 100)))
    (let* ((state (make-instance 'vm-io-state))
           (labels (make-hash-table))
           (make-inst (make-vm-make-closure
                       :dst :r0
                       :label :func
                       :params nil
                       :env-regs '(:r1 :r2 :r3)))
           (ref-inst-0 (make-vm-closure-ref-idx :dst :r4 :closure :r0 :index 0))
           (ref-inst-1 (make-vm-closure-ref-idx :dst :r5 :closure :r0 :index 1))
           (ref-inst-2 (make-vm-closure-ref-idx :dst :r6 :closure :r0 :index 2)))
      (setf (gethash :func labels) 10)
      (vm-reg-set state :r1 env-val1)
      (vm-reg-set state :r2 env-val2)
      (vm-reg-set state :r3 env-val3)
      (execute-instruction make-inst state 0 labels)
      (execute-instruction ref-inst-0 state 1 labels)
      (execute-instruction ref-inst-1 state 2 labels)
      (execute-instruction ref-inst-2 state 3 labels)
      (and (= (vm-reg-get state :r4) env-val1)
           (= (vm-reg-get state :r5) env-val2)
           (= (vm-reg-get state :r6) env-val3))))

  ;; --- instruction->sexp round-trips ---

  (cl-weave:it-property "vm-cons survives instruction->sexp->instruction round-trip"
      ((car-reg (cl-weave:gen-member '(:r0 :r1 :r2 :r3)))
       (cdr-reg (cl-weave:gen-member '(:r4 :r5 :r6 :r7))))
    (let* ((original (make-vm-cons :dst :r0 :car-src car-reg :cdr-src cdr-reg))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-cons)
           (eq (vm-dst restored) :r0)
           (eq (vm-car-reg restored) car-reg)
           (eq (vm-cdr-reg restored) cdr-reg))))

  (cl-weave:it-property "vm-car survives instruction->sexp->instruction round-trip"
      ((src (cl-weave:gen-member '(:r0 :r1 :r2 :r3))))
    (let* ((original (make-vm-car :dst :r4 :src src))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-car)
           (eq (vm-dst restored) :r4)
           (eq (vm-src restored) src))))

  (cl-weave:it-property "vm-cdr survives instruction->sexp->instruction round-trip"
      ((src (cl-weave:gen-member '(:r0 :r1 :r2 :r3))))
    (let* ((original (make-vm-cdr :dst :r4 :src src))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-cdr)
           (eq (vm-dst restored) :r4)
           (eq (vm-src restored) src))))

  (cl-weave:it-property "vm-rplaca survives instruction->sexp->instruction round-trip"
      ((cons-reg (cl-weave:gen-member '(:r0 :r1 :r2)))
       (val-reg (cl-weave:gen-member '(:r3 :r4 :r5))))
    (let* ((original (make-vm-rplaca :cons cons-reg :val val-reg))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-rplaca)
           (eq (vm-cons-reg restored) cons-reg)
           (eq (vm-val-reg restored) val-reg))))

  (cl-weave:it-property "vm-rplacd survives instruction->sexp->instruction round-trip"
      ((cons-reg (cl-weave:gen-member '(:r0 :r1 :r2)))
       (val-reg (cl-weave:gen-member '(:r3 :r4 :r5))))
    (let* ((original (make-vm-rplacd :cons cons-reg :val val-reg))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-rplacd)
           (eq (vm-cons-reg restored) cons-reg)
           (eq (vm-val-reg restored) val-reg))))

  (cl-weave:it-property "vm-closure-ref-idx survives instruction->sexp->instruction round-trip"
      ((closure-reg (cl-weave:gen-member '(:r0 :r1 :r2)))
       (index (cl-weave:gen-integer :min 0 :max 10)))
    (let* ((original (make-vm-closure-ref-idx :dst :r5 :closure closure-reg :index index))
           (sexp (instruction->sexp original))
           (restored (sexp->instruction sexp)))
      (and (typep restored 'vm-closure-ref-idx)
           (eq (vm-dst restored) :r5)
           (eq (vm-closure-reg restored) closure-reg)
           (= (vm-closure-index restored) index)))))
