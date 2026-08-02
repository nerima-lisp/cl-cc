;;;; tests/unit/emit/x86-64-codegen-tests.lisp — x86-64 Code Generation Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp:
;;;; *vm-reg-map*, *phys-reg-to-x86-code*, vm-reg-to-x86,
;;;; vm-const-to-integer, instruction-size, build-label-offsets,
;;;; *x86-64-emitter-entries*, *x86-64-emitter-table*

(in-package :cl-cc/test)


;;; Tests that call with-replaced-function on global emit functions must run serially.

;;; ─── *vm-reg-map* ───────────────────────────────────────────────────────────

(it-sequential "x86-64-reg-map-lengths"
  (expect (= 8 (length cl-cc/codegen::*vm-reg-map*)) :to-be-truthy)
  (expect (= 15 (length cl-cc/codegen::*phys-reg-to-x86-code*)) :to-be-truthy))

(it-sequential "x86-64-fpe-codegen-target-frees-rbp"
  (let ((fpe-target (let ((cl-cc/codegen::*x86-64-omit-frame-pointer* t))
                      (cl-cc/codegen::x86-64-codegen-target)))
        (debug-target (let ((cl-cc/codegen::*x86-64-omit-frame-pointer* nil))
                        (cl-cc/codegen::x86-64-codegen-target))))
    (expect (member :rbp (cl-cc/target:target-allocatable-regs fpe-target)) :to-be-truthy)
    (expect (member :rbp (cl-cc/target:target-callee-saved fpe-target)) :to-be-truthy)
    (expect (member :rbp (cl-cc/target:target-allocatable-regs debug-target)) :to-be-falsy)))

(it-sequential "x86-64-vm-reg-map-entries R0→rax"
  (destructuring-bind (vm-reg expected) (list :R0 cl-cc/codegen::+rax+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R1→rcx"
  (destructuring-bind (vm-reg expected) (list :R1 cl-cc/codegen::+rcx+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R2→rdx"
  (destructuring-bind (vm-reg expected) (list :R2 cl-cc/codegen::+rdx+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R3→rbx"
  (destructuring-bind (vm-reg expected) (list :R3 cl-cc/codegen::+rbx+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R4→rsi"
  (destructuring-bind (vm-reg expected) (list :R4 cl-cc/codegen::+rsi+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R5→rdi"
  (destructuring-bind (vm-reg expected) (list :R5 cl-cc/codegen::+rdi+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R6→r8"
  (destructuring-bind (vm-reg expected) (list :R6 cl-cc/codegen::+r8+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

(it-sequential "x86-64-vm-reg-map-entries R7→r9"
  (destructuring-bind (vm-reg expected) (list :R7 cl-cc/codegen::+r9+)
    (expect (= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))) :to-be-truthy)))

;;; ─── *phys-reg-to-x86-code* ─────────────────────────────────────────────────


(it-sequential "x86-64-phys-reg-map-entries rax"
  (destructuring-bind (phys-reg expected) (list :rax cl-cc/codegen::+rax+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rcx"
  (destructuring-bind (phys-reg expected) (list :rcx cl-cc/codegen::+rcx+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rdx"
  (destructuring-bind (phys-reg expected) (list :rdx cl-cc/codegen::+rdx+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rbx"
  (destructuring-bind (phys-reg expected) (list :rbx cl-cc/codegen::+rbx+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rbp"
  (destructuring-bind (phys-reg expected) (list :rbp cl-cc/codegen::+rbp+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rsi"
  (destructuring-bind (phys-reg expected) (list :rsi cl-cc/codegen::+rsi+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries rdi"
  (destructuring-bind (phys-reg expected) (list :rdi cl-cc/codegen::+rdi+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r8"
  (destructuring-bind (phys-reg expected) (list :r8 cl-cc/codegen::+r8+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r9"
  (destructuring-bind (phys-reg expected) (list :r9 cl-cc/codegen::+r9+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r10"
  (destructuring-bind (phys-reg expected) (list :r10 cl-cc/codegen::+r10+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r11"
  (destructuring-bind (phys-reg expected) (list :r11 cl-cc/codegen::+r11+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r12"
  (destructuring-bind (phys-reg expected) (list :r12 cl-cc/codegen::+r12+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r13"
  (destructuring-bind (phys-reg expected) (list :r13 cl-cc/codegen::+r13+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r14"
  (destructuring-bind (phys-reg expected) (list :r14 cl-cc/codegen::+r14+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

(it-sequential "x86-64-phys-reg-map-entries r15"
  (destructuring-bind (phys-reg expected) (list :r15 cl-cc/codegen::+r15+)
    (expect (= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))) :to-be-truthy)))

;;; ─── vm-reg-to-x86 ──────────────────────────────────────────────────────────

(it-sequential "x86-64-vm-reg-to-x86-naive R0"
  (destructuring-bind (vm-reg expected) (list :R0 cl-cc/codegen::+rax+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R1"
  (destructuring-bind (vm-reg expected) (list :R1 cl-cc/codegen::+rcx+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R2"
  (destructuring-bind (vm-reg expected) (list :R2 cl-cc/codegen::+rdx+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R3"
  (destructuring-bind (vm-reg expected) (list :R3 cl-cc/codegen::+rbx+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R4"
  (destructuring-bind (vm-reg expected) (list :R4 cl-cc/codegen::+rsi+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R5"
  (destructuring-bind (vm-reg expected) (list :R5 cl-cc/codegen::+rdi+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R6"
  (destructuring-bind (vm-reg expected) (list :R6 cl-cc/codegen::+r8+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-naive R7"
  (destructuring-bind (vm-reg expected) (list :R7 cl-cc/codegen::+r9+)
    (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg)) :to-be-truthy))))

(it-sequential "x86-64-vm-reg-to-x86-unknown-signals"
  (let ((cl-cc/codegen::*current-regalloc* nil))
    (signals error (cl-cc/codegen::vm-reg-to-x86 :R99))))

;;; ─── vm-const-to-integer ────────────────────────────────────────────────────

(it-sequential "x86-64-vm-const-to-integer nil→0"
  (destructuring-bind (input expected) (list nil 0)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer input)) :to-be-truthy)))

(it-sequential "x86-64-vm-const-to-integer t→1"
  (destructuring-bind (input expected) (list t 1)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer input)) :to-be-truthy)))

(it-sequential "x86-64-vm-const-to-integer 42→42"
  (destructuring-bind (input expected) (list 42 42)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer input)) :to-be-truthy)))

(it-sequential "x86-64-vm-const-to-integer -7→-7"
  (destructuring-bind (input expected) (list -7 -7)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer input)) :to-be-truthy)))

(it-sequential "x86-64-vm-const-to-integer other→0"
  (destructuring-bind (input expected) (list :foo 0)
    (expect (= expected (cl-cc/codegen::vm-const-to-integer input)) :to-be-truthy)))

;;; ─── *x86-64-instruction-sizes* ─────────────────────────────────────────────
;;; Note: hash keys are symbols in the cl-cc package; use cl-cc: prefix.

(it-sequential "x86-64-instruction-sizes-spot-checks vm-const"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-const 10)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-move"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-move 3)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-add"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-add 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-integer-add"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-integer-add 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-sub"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-sub 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-integer-sub"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-integer-sub 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-mul"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-mul 7)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-integer-mul"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-integer-mul 7)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-integer-mul-high-u"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-integer-mul-high-u 19)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-integer-mul-high-s"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-integer-mul-high-s 19)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-sqrt"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-sqrt 8)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-sin-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-sin-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-cos-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-cos-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-exp-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-exp-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-log-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-log-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-tan-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-tan-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-asin-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-asin-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-acos-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-acos-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-atan-inst"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-atan-inst 21)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-bswap"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-bswap 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-halt"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-halt 3)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-call"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-call 6)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-tail-call"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-tail-call 3)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-label"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-label 0)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-jump"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-jump 5)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-jump-zero"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-jump-zero 9)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-ret"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-ret 1)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-ash"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-ash 24)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-div"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-div 34)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-mod"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-mod 37)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-abs"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-abs 15)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-min"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-min 10)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-sizes-spot-checks vm-max"
  (destructuring-bind (sym expected) (list 'cl-cc/vm::vm-max 10)
    (expect (= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "x86-64-instruction-size-checks"
  (dolist (tp '(cl-cc/vm::vm-lt cl-cc/vm::vm-gt cl-cc/vm::vm-le
                cl-cc/vm::vm-ge cl-cc/vm::vm-num-eq cl-cc/vm::vm-eq))
    (expect (= 12 (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy))
  (expect (= 11 (gethash 'cl-cc/vm::vm-null-p cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)
  (dolist (tp '(cl-cc/vm::vm-number-p cl-cc/vm::vm-integer-p cl-cc/vm::vm-cons-p
                cl-cc/vm::vm-symbol-p cl-cc/vm::vm-function-p))
    (expect (= 10 (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy))
  (let ((cl-cc/codegen::*current-regalloc* nil))
    (expect (= 0 (cl-cc/codegen::instruction-size (cl-cc:make-vm-move :dst :R0 :src :R0))) :to-be-truthy)))

;;; ─── *x86-64-emitter-entries* / *x86-64-emitter-table* ─────────────────────

(it-sequential "x86-64-emitter-table-integrity"
  (expect (>= (length cl-cc/codegen::*x86-64-emitter-entries*) 84) :to-be-truthy)
  (dolist (entry cl-cc/codegen::*x86-64-emitter-entries*)
    (expect (gethash (car entry) cl-cc/codegen::*x86-64-emitter-table*) :to-be-truthy)))

(it-sequential "x86-mul-high-size-and-dispatch-registered"
  (dolist (tp '(cl-cc/vm::vm-integer-mul-high-u cl-cc/vm::vm-integer-mul-high-s))
    (expect (= 19 (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*)) :to-be-truthy)
    (expect (functionp (gethash tp cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-empty-program-minimal-return-byte"
  (let* ((prog (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (bytes (cl-cc/codegen::compile-to-x86-64-bytes prog)))
    (expect (= 1 (length bytes)) :to-be-truthy)
    (expect (= #xC3 (aref bytes 0)) :to-be-truthy)))


(it-sequential "x86-64-sanitizer-flags-enable-stack-protector-policy"
  (let* ((prog (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (saw-stack-protector nil))
    (with-replaced-function (cl-cc/codegen::emit-vm-program
                             (lambda (program stream)
                               (declare (ignore program stream))
                               (setf saw-stack-protector cl-cc/codegen::*x86-64-stack-protector-enabled*)
                               nil))
      (cl-cc/codegen::compile-to-x86-64-bytes prog :asan t)
      (expect saw-stack-protector :to-be-truthy))))


(it-sequential "x86-64-leaf-and-nonleaf-without-spills-share-fpe-layout"
  (let* ((result (compile-string "(+ 1 2)" :target :x86_64))
         (prog (compilation-result-program result))
         (base (cl-cc/vm::make-vm-program
                :instructions (cl-cc/vm::vm-program-instructions prog)
                :result-register (cl-cc/vm::vm-program-result-register prog)
                :leaf-p nil))
         (leaf-bytes    (cl-cc/codegen::compile-to-x86-64-bytes prog))
         (nonleaf-bytes (cl-cc/codegen::compile-to-x86-64-bytes base)))
    (expect (cl-cc/vm::vm-program-leaf-p prog) :to-be-truthy)
    (expect (= (length leaf-bytes) (length nonleaf-bytes)) :to-be-truthy)
    (expect (equalp leaf-bytes nonleaf-bytes) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-const"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-const)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-add"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-add)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-integer-add"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-integer-add)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-float-add"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-float-add)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-float-div"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-float-div)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-sqrt"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-sqrt)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-sin-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-sin-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-cos-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-cos-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-exp-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-exp-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-log-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-log-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-tan-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-tan-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-asin-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-asin-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-acos-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-acos-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-atan-inst"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-atan-inst)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-integer-mul-high-u"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-integer-mul-high-u)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-integer-mul-high-s"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-integer-mul-high-s)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-call"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-call)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-tail-call"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-tail-call)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-lt"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-lt)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-neg"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-neg)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-bswap"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-bswap)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-and"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-and)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-logand"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-logand)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-emitter-table-spot-checks vm-null-p"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-null-p)
    (expect (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*)) :to-be-truthy)))

(it-sequential "x86-64-float-const-add-program-uses-xmm-path"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-const :dst :R0 :value 1.0d0)
                                    (cl-cc:make-vm-const :dst :R1 :value 2.0d0)
                                    (cl-cc:make-vm-float-add :dst :R2 :lhs :R0 :rhs :R1)
                                    (cl-cc:make-vm-halt :reg :R2))
                :result-register :R2))
         (bytes (coerce (cl-cc/codegen::compile-to-x86-64-bytes prog) 'list)))
    (expect (search '(#x66 #x49 #x0F #x6E) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#xF2 #x0F #x10) bytes :test #'eql) :to-be-truthy)
    (expect (search '(#xF2 #x0F #x58) bytes :test #'eql) :to-be-truthy)))

(it-sequential "x86-64-float-div-program-emits-divsd"
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-const :dst :R0 :value 10.0d0)
                                    (cl-cc:make-vm-const :dst :R1 :value 4.0d0)
                                    (cl-cc/vm::make-vm-float-div :dst :R2 :lhs :R0 :rhs :R1)
                                    (cl-cc:make-vm-halt :reg :R2))
                :result-register :R2))
         (bytes (coerce (cl-cc/codegen::compile-to-x86-64-bytes prog) 'list)))
    (expect (search '(#xF2 #x0F #x5E) bytes :test #'eql) :to-be-truthy)))

(it-sequential "x86-64-tls-base-register-uses-fsbase-plan"
  (expect (cl-cc/codegen::x86-64-tls-base-register) :to-be :fs))

(it-sequential "x86-64-atomic-lowering-plan-adds-seq-cst-fences"
  (let ((plan (cl-cc/codegen::x86-64-atomic-lowering-plan :incf :seq-cst)))
    (expect (getf plan :opcode) :to-be :lock-xadd)
    (expect (getf plan :pre-fence) :to-equal '(:mfence))
    (expect (getf plan :post-fence) :to-equal '(:mfence))))

(it-sequential "x86-64-stack-canary-plan-materializes-prologue-and-epilogue"
  (let ((enabled (cl-cc/codegen::x86-64-stack-canary-plan
                  :has-stack-buffer-p t
                  :guard-slot -16
                  :failure-target 'panic))
        (disabled (cl-cc/codegen::x86-64-stack-canary-plan
                   :has-stack-buffer-p nil
                   :guard-slot -16
                   :failure-target 'panic)))
    (expect (getf enabled :enabled-p) :to-be-truthy)
    (expect (getf enabled :guard-slot) :to-equal -16)
    (expect (getf enabled :failure-target) :to-be 'panic)
    (expect (getf enabled :prologue) :to-equal '((:op :load-canary :source :tls-canary :dst :rax)
                    (:op :store-canary :src :rax :slot -16)))
    (expect (getf enabled :epilogue) :to-equal '((:op :load-canary :source -16 :dst :rax)
                    (:op :compare-canary :left :rax :right :tls-canary)
                    (:op :branch-if-canary-mismatch :target panic)))
    (expect (getf disabled :enabled-p) :to-be-falsy)
    (expect (getf disabled :prologue) :to-be-null)
    (expect (getf disabled :epilogue) :to-be-null)))

(it-sequential "x86-64-stack-protector-emitter-signature-bytes"
  (let* ((plan (cl-cc/codegen::x86-64-stack-canary-plan
                :has-stack-buffer-p t
                :guard-slot -16
                :failure-target 'panic))
         (bytes (%x86-collect-bytes
                 (lambda (stream)
                   (cl-cc/codegen::emit-x86-64-stack-canary-prologue stream plan t)
                   (cl-cc/codegen::emit-x86-64-stack-canary-epilogue stream plan t)))))
    ;; mov rax, qword ptr fs:[0x28]  => 64 48 8B 04 25 28 00 00 00
    (expect (search '(#x64 #x48 #x8B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql) :to-be-truthy)
    ;; cmp rax, qword ptr fs:[0x28]  => 64 48 3B 04 25 28 00 00 00
    (expect (search '(#x64 #x48 #x3B #x04 #x25 #x28 #x00 #x00 #x00) bytes :test #'eql) :to-be-truthy)
    ;; mismatch trap path includes UD2 bytes.
    (expect (search '(#x0F #x0B) bytes :test #'eql) :to-be-truthy)))

(it-sequential "x86-64-cfi-plan-enables-endbr64-for-indirect-calls"
  (let ((enabled (cl-cc/codegen::x86-64-cfi-plan :has-indirect-calls-p t))
        (disabled (cl-cc/codegen::x86-64-cfi-plan :has-indirect-calls-p nil)))
    (expect (getf enabled :enabled-p) :to-be-truthy)
    (expect (getf enabled :entry-opcode) :to-be :endbr64)
    (expect (getf disabled :enabled-p) :to-be-falsy)
    (expect (getf disabled :entry-opcode) :to-be :none)))

(it-sequential "x86-64-cfi-entry-emits-endbr64-bytes"
  (let ((bytes (%x86-collect-bytes
                (lambda (stream)
                  (cl-cc/codegen::emit-x86-64-cfi-entry
                   stream
                   (cl-cc/codegen::x86-64-cfi-plan :has-indirect-calls-p t))))))
    (expect bytes :to-equal '(#xF3 #x0F #x1E #xFA))))

(it-sequential "x86-64-program-with-indirect-call-starts-with-endbr64"
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil)
                                       (cl-cc:make-vm-halt :reg :R0))
                   :result-register :R0
                   :leaf-p nil))
         (bytes (coerce (cl-cc/codegen::compile-to-x86-64-bytes program) 'list)))
    (expect (subseq bytes 0 4) :to-equal '(#xF3 #x0F #x1E #xFA))))

(it-sequential "x86-64-cfi-call-tail-call-size-accounting-matches-guard-bytes"
  (let ((cl-cc/codegen::*x86-64-cfi-enabled* t)
        (cl-cc/codegen::*x86-64-use-retpoline* nil))
    (expect (= 34 (cl-cc/codegen::instruction-size
              (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil))) :to-be-truthy)
    (expect (= 31 (cl-cc/codegen::instruction-size
              (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil))) :to-be-truthy)))

(it-sequential "x86-64-program-has-stack-buffer-p-detects-array-vector-ops"
  (expect (cl-cc/codegen::x86-64-program-has-stack-buffer-p
    (list (cl-cc:make-vm-make-array :dst :R0 :size-reg :R1 :initial-element nil))) :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-program-has-stack-buffer-p
    (list (cl-cc:make-vm-aset :array-reg :R0 :index-reg :R1 :val-reg :R2))) :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-program-has-stack-buffer-p
    (list (cl-cc:make-vm-set-fill-pointer :dst :R0 :array-reg :R1 :val-reg :R2))) :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-program-has-stack-buffer-p
    (list (cl-cc:make-vm-const :dst :R0 :value 1))) :to-be-falsy))

(it-sequential "x86-64-array-bce-metadata-skips-explicit-guard"
  (let* ((target (make-instance 'cl-cc/codegen::x86-64-target))
         (inst (cl-cc:make-vm-aref :dst :r0 :array-reg :r1 :index-reg :r2))
         (checked (with-output-to-string (s)
                    (cl-cc/codegen::emit-instruction target inst s))))
    (cl-cc/optimize:opt-mark-bounds-check-eliminable inst)
    (let ((unchecked (with-output-to-string (s)
                       (cl-cc/codegen::emit-instruction target inst s))))
      (assert-output-contains checked "cmp")
      (assert-output-contains checked "jae clcc_array_bounds_trap")
      (expect (search "cmp" unchecked) :to-be-falsy)
      (expect (search "jae clcc_array_bounds_trap" unchecked) :to-be-falsy)
      (assert-output-contains unchecked "mov"))))

(it-sequential "x86-64-program-has-nonlocal-control-p-detects-condition-handlers"
  (expect (cl-cc/codegen::x86-64-program-has-nonlocal-control-p
    (list (cl-cc:make-vm-signal :condition-reg :R0))) :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-program-has-nonlocal-control-p
    (list (cl-cc:make-vm-const :dst :R0 :value 1))) :to-be-falsy))

(it-sequential "x86-64-ibrs-token-present-p-detects-common-host-feature-formats"
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "flags\t: fpu vme ibrs avx2") :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "machdep.cpu.features: SSE4.2,IBRS,AVX2") :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "machdep.cpu.features: SSE4.2,eIBRS,AVX2") :to-be-truthy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "flags\t: fpu vme avx2") :to-be-falsy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "flags\t: fpu vme noibrs avx2") :to-be-falsy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "flags\t: fpu vme ibrs2 avx2") :to-be-falsy)
  (expect (cl-cc/codegen::x86-64-ibrs-token-present-p
    "machdep.cpu.features: SSE4.2,eibrs_disabled,AVX2") :to-be-falsy))

;;; ─── Byte-collection helper ─────────────────────────────────────────────────
;;; Used by x86-64-codegen-emitter-tests and x86-64-codegen-insn-tests,
;;; which are loaded after this file via :serial t ASDF.

(defun %x86-collect-bytes (emit-fn)
  "Call EMIT-FN with a stream that collects bytes. Returns byte list."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))
