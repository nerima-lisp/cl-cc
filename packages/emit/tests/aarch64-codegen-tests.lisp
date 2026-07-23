;;;; tests/unit/emit/aarch64-codegen-tests.lisp
;;;; Unit tests for compile-to-aarch64-bytes (src/emit/aarch64-codegen.lisp)
;;;;
;;;; Covers:
;;;;   - Return type: must be (array (unsigned-byte 8) (*))
;;;;   - Non-empty output for any valid program
;;;;   - AArch64 ISA alignment invariant (instruction width = 4 bytes)
;;;;   - Regression guard: arm64 and x86-64 backends produce different bytes

(in-package :cl-cc/test)


;;; ─────────────────────────────────────────────────────────────────────────
;;; Helper
;;; ─────────────────────────────────────────────────────────────────────────

(defun %a64-compile (source)
  "Compile SOURCE string to AArch64 machine-code bytes.
Returns the byte vector, or NIL on error."
  (ignore-errors
    (compile-to-aarch64-bytes
     (compilation-result-program
      (compile-string source :target :vm)))))

(defun %a64-collect-bytes (emit-fn)
  "Collect bytes emitted by a native AArch64 emitter."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

(defun %a64-assert-emitted-bytes (expected bytes)
  "Assert that BYTES match EXPECTED and preserve 4-byte instruction width."
  (assert-= (length expected) (length bytes))
  (assert-= 0 (mod (length bytes) 4))
  (assert-equal expected bytes))

(defun %fr072-a64-assignment (&rest pairs)
  (let ((ht (make-hash-table :test #'eq)))
    (loop for (vreg phys) on pairs by #'cddr
          do (setf (gethash vreg ht) phys))
    ht))

(it-sequential "aarch64-shrink-wrap-delays-save-to-cold-block"
  (let* ((insts (list (cl-cc:make-vm-jump-zero :reg :R0 :label "cold")
                      (cl-cc:make-vm-halt :reg :R0)
                      (cl-cc:make-vm-label :name "cold")
                      (cl-cc:make-vm-add :dst :R1 :lhs :R2 :rhs :R3)
                      (cl-cc:make-vm-halt :reg :R0)))
         (assignment (%fr072-a64-assignment :R1 :x19)))
    (multiple-value-bind (annotated entry-pairs final-pairs)
        (cl-cc/codegen::a64-shrink-wrap-instructions insts '((19 20)) assignment)
      (expect entry-pairs :to-equal nil)
      (expect final-pairs :to-equal nil)
      (expect (= 1 (count-if (lambda (x) (typep x 'cl-cc/codegen::a64-shrink-save)) annotated)) :to-be-truthy)
      (expect (= 1 (count-if (lambda (x) (typep x 'cl-cc/codegen::a64-shrink-restore)) annotated)) :to-be-truthy))))

(it-sequential "aarch64-shrink-wrap-degenerate-entry-use-stays-monolithic"
  (let* ((insts (list (cl-cc:make-vm-add :dst :R1 :lhs :R2 :rhs :R3)
                      (cl-cc:make-vm-halt :reg :R1)))
         (assignment (%fr072-a64-assignment :R1 :x19)))
    (multiple-value-bind (annotated entry-pairs final-pairs)
        (cl-cc/codegen::a64-shrink-wrap-instructions insts '((19 20)) assignment)
      (expect entry-pairs :to-equal '((19 20)))
      (expect final-pairs :to-equal '((19 20)))
      (expect (find-if (lambda (x) (typep x 'cl-cc/codegen::a64-shrink-save))
                             annotated) :to-be-falsy))))

(it-sequential "aarch64-fpe-codegen-target-frees-x29"
  (let ((fpe-target (let ((cl-cc/codegen::*a64-omit-frame-pointer* t))
                      (cl-cc/codegen::a64-codegen-target)))
        (debug-target (let ((cl-cc/codegen::*a64-omit-frame-pointer* nil))
                        (cl-cc/codegen::a64-codegen-target))))
    (expect (member :x29 (cl-cc/target:target-allocatable-regs fpe-target)) :to-be-truthy)
    (expect (member :x29 (cl-cc/target:target-callee-saved fpe-target)) :to-be-truthy)
    (expect (member :x29 (cl-cc/target:target-allocatable-regs debug-target)) :to-be-falsy)))

(it-sequential "aarch64-tls-base-register-uses-tpidr-el0-plan"
  (expect (cl-cc/codegen::aarch64-tls-base-register) :to-be :tpidr_el0))

(it-sequential "aarch64-atomic-lowering-plan-adds-acq-rel-fences"
  (let ((plan (cl-cc/codegen::aarch64-atomic-lowering-plan :cas :acq-rel)))
    (expect (getf plan :opcode) :to-be :ldxr-stxr)
    (expect (getf plan :pre-fence) :to-equal '(:dmb-ish))
    (expect (getf plan :post-fence) :to-equal '(:dmb-ish))))

(it-sequential "aarch64-cfi-plan-enables-bti-c-for-indirect-calls"
  (let ((enabled (cl-cc/codegen::aarch64-cfi-plan :has-indirect-calls-p t))
        (disabled (cl-cc/codegen::aarch64-cfi-plan :has-indirect-calls-p nil)))
    (expect (getf enabled :enabled-p) :to-be-truthy)
    (expect (getf enabled :entry-opcode) :to-be :bti-c)
    (expect (getf disabled :enabled-p) :to-be-falsy)
    (expect (getf disabled :entry-opcode) :to-be :none)))

(it-sequential "aarch64-cfi-entry-emits-bti-c-bytes"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-aarch64-cfi-entry
                   s
                   (cl-cc/codegen::aarch64-cfi-plan :has-indirect-calls-p t))))))
    ;; BTI C = 0xD503245F (little-endian)
    (expect bytes :to-equal '(#x5F #x24 #x03 #xD5))))

(it-sequential "aarch64-program-with-indirect-call-starts-with-bti-c"
  (let* ((program (cl-cc/vm::make-vm-program
                   :instructions (list (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil)
                                       (cl-cc:make-vm-halt :reg :R0))
                   :result-register :R0
                   :leaf-p nil))
         (bytes (coerce (cl-cc/codegen::compile-to-aarch64-bytes program) 'list)))
    (expect (subseq bytes 0 4) :to-equal '(#x5F #x24 #x03 #xD5))))

(it-sequential "aarch64-used-callee-saved-pairs-maps-keyword-registers"
  (let ((assignment (make-hash-table :test #'eq)))
    (setf (gethash :R0 assignment) :x19
          (gethash :R1 assignment) :x29)
    (let ((ra (cl-cc/regalloc::make-regalloc-result :assignment assignment
                                                    :spill-count 0
                                                    :instructions nil)))
      (expect (cl-cc/codegen::a64-used-callee-saved-pairs ra) :to-equal '((29 30) (19 20))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Return-type contract
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "aarch64-bytes-output-contract"
  (let ((bytes (%a64-compile "(+ 1 2)")))
    (expect bytes :to-be-truthy)
    (expect (typep bytes '(array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AArch64 ISA alignment: all instructions are 4 bytes
;;; ─────────────────────────────────────────────────────────────────────────

;;; ─────────────────────────────────────────────────────────────────────────
;;; Regression guard: arm64 != x86-64
;;; This test would have FAILED before the fix because compile-file-to-native
;;; called compile-to-x86-64-bytes for both architectures.
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "aarch64-bytes-distinct-from-x86-64"
  (let* ((a64-program (compilation-result-program
                       (compile-string "(+ 1 2)" :target :aarch64)))
         (x64-program (compilation-result-program
                       (compile-string "(+ 1 2)" :target :x86_64)))
         (a64 (ignore-errors (compile-to-aarch64-bytes a64-program)))
         (x64 (ignore-errors (compile-to-x86-64-bytes x64-program))))
    (expect a64 :to-be-truthy)
    (expect x64 :to-be-truthy)
    (expect (equalp a64 x64) :to-be-falsy)))


(it-sequential "aarch64-bswap-emitter-encoding"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-bswap (cl-cc:make-vm-bswap :dst :R0 :src :R1) s)))))
    (%a64-assert-emitted-bytes '(#x20 #x08 #xC0 #x5A) bytes)))

(it-sequential "aarch64-rotate-emitter-encoding"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-rotate
                   (cl-cc:make-vm-rotate :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#xE0 #x03 #x01 #xAA
       #x00 #x2C #xC2 #x9A)
     bytes)))

(it-sequential "aarch64-mul-high-emitter-encodings"
  (let ((umulh-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-vm-integer-mul-high-u
                         (cl-cc:make-vm-integer-mul-high-u :dst :R0 :lhs :R1 :rhs :R2) s))))
        (smulh-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-vm-integer-mul-high-s
                         (cl-cc:make-vm-integer-mul-high-s :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (expect umulh-bytes :to-equal '(#x20 #x7C #xC2 #x9B))
    (expect smulh-bytes :to-equal '(#x20 #x7C #x42 #x9B))))

(it-sequential "aarch64-mul-high-size-and-dispatch-registered"
  (dolist (tp '(cl-cc/vm::vm-integer-mul-high-u cl-cc/vm::vm-integer-mul-high-s))
    (expect (= 4 (gethash tp cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)
    (expect (functionp (gethash tp cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-sqrt-emitter-encoding"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-sqrt
                   (cl-cc:make-vm-sqrt :dst :R0 :src :R1) s)))))
    (expect bytes :to-equal '(#x20 #xC0 #x61 #x1E))))

(it-sequential "aarch64-sqrt-size-and-dispatch-registered"
  (expect (= 4 (gethash 'cl-cc/vm::vm-sqrt cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)
  (expect (functionp (gethash 'cl-cc/vm::vm-sqrt cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy))

;;; ─── Libm call emitters (sin/cos/exp/log/tan/asin/acos/atan — FR-286) ─────

(it-sequential "aarch64-libm-unary-emitter-size sin"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-sin (cl-cc/vm::make-vm-sin-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size cos"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-cos (cl-cc/vm::make-vm-cos-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size exp"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-exp (cl-cc/vm::make-vm-exp-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size log"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-log (cl-cc/vm::make-vm-log-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size tan"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-tan (cl-cc/vm::make-vm-tan-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size asin"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-asin (cl-cc/vm::make-vm-asin-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size acos"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-acos (cl-cc/vm::make-vm-acos-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-libm-unary-emitter-size atan"
  (destructuring-bind (emit-fn inst) (list #'cl-cc/codegen::emit-a64-vm-atan (cl-cc/vm::make-vm-atan-inst :dst :R0 :src :R1))
    (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (expect (>= (length bytes) 24) :to-be-truthy)       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))      ; AArch64 = 4-byte aligned

(it-sequential "aarch64-libm-sin-starts-with-fmov"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-sin
                   (cl-cc/vm::make-vm-sin-inst :dst :R0 :src :R1) s)))))
    ;; FMOV D0, D1 = encode-fmov-dd(0, 1) = 0x1E604000 | (1<<5) | 0 = 0x1E604020
    (expect (subseq bytes 0 4) :to-equal '(#x20 #x40 #x60 #x1E))))

(it-sequential "aarch64-instruction-size-table-entries bswap"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-bswap)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries tail-call"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-tail-call)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries min"
  (destructuring-bind (expected instr-type) (list 8 'cl-cc/vm::vm-min)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries max"
  (destructuring-bind (expected instr-type) (list 8 'cl-cc/vm::vm-max)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries select"
  (destructuring-bind (expected instr-type) (list 12 'cl-cc/vm::vm-select)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries integer-add"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-integer-add)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries integer-sub"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-integer-sub)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries integer-mul"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-integer-mul)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries integer-mul-high-u"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-integer-mul-high-u)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries integer-mul-high-s"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-integer-mul-high-s)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries sqrt"
  (destructuring-bind (expected instr-type) (list 4 'cl-cc/vm::vm-sqrt)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries sin-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-sin-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries cos-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-cos-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries exp-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-exp-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries log-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-log-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries tan-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-tan-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries asin-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-asin-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries acos-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-acos-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-instruction-size-table-entries atan-inst"
  (destructuring-bind (expected instr-type) (list 36 'cl-cc/vm::vm-atan-inst)
    (expect (= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)) :to-be-truthy)))

(it-sequential "aarch64-vm-move-self-elision-emits-no-bytes"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-move
                   (cl-cc:make-vm-move :dst :R0 :src :R0) s)))))
    (expect (= 0 (length bytes)) :to-be-truthy))
  (expect (= 0 (cl-cc/codegen::a64-instruction-size (cl-cc:make-vm-move :dst :R0 :src :R0))) :to-be-truthy))

(it-sequential "aarch64-scs-single-register-encodings"
  (let ((store (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-str-post 30 18 8) s))))
        (load  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-ldr-pre 17 18 -8) s))))
        (beq   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-b-cond 2 0) s))))
        (brk   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-brk 0) s)))))
    (expect store :to-equal '(94 134 0 248))
    (expect load :to-equal '(81 142 95 248))
    (expect beq :to-equal '(64 0 0 84))
    (expect brk :to-equal '(0 0 32 212))))

(it-sequential "aarch64-emitter-table-entries min"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-min)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries max"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-max)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries integer-add"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-integer-add)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries integer-sub"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-integer-sub)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries integer-mul"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-integer-mul)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries integer-mul-high-u"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-integer-mul-high-u)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries integer-mul-high-s"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-integer-mul-high-s)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries sqrt"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-sqrt)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries sin-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-sin-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries cos-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-cos-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries exp-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-exp-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries log-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-log-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries tan-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-tan-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries asin-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-asin-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries acos-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-acos-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-emitter-table-entries atan-inst"
  (destructuring-bind (instr-type) (list 'cl-cc/vm::vm-atan-inst)
    (expect (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*)) :to-be-truthy)))

(it-sequential "aarch64-tail-call-emitter-encoding"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-instruction
                   (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil)
                   s 0 (make-hash-table :test #'eq))))))
    (%a64-assert-emitted-bytes '(#x20 #x00 #x1F #xD6) bytes)))

(it-sequential "aarch64-build-label-offsets-account-for-elided-self-move"
  (let* ((insts (list (cl-cc:make-vm-move :dst :R0 :src :R0)
                      (cl-cc:make-vm-label :name "after-self-move")
                      (cl-cc:make-vm-halt :reg :R0)))
         (offsets (cl-cc/codegen::build-a64-label-offsets insts 0)))
    (expect (= 0 (gethash "after-self-move" offsets)) :to-be-truthy)))

(it-sequential "aarch64-min-max-emitter-encoding"
  (let ((min-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-a64-vm-min
                       (cl-cc:make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))))
        (max-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-a64-vm-max
                       (cl-cc:make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB
       #x20 #xB0 #x82 #x9A)
     min-bytes)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB
       #x20 #xC0 #x82 #x9A)
     max-bytes)))

(it-sequential "aarch64-null-p-emitter-byte-patterns"
  (let ((zero-src (%a64-collect-bytes
                   (lambda (s)
                     (cl-cc/codegen::emit-a64-vm-null-p
                      (cl-cc:make-vm-null-p :dst :R0 :src :R0) s))))
        (non-zero-src (%a64-collect-bytes
                       (lambda (s)
                         (cl-cc/codegen::emit-a64-vm-null-p
                          (cl-cc:make-vm-null-p :dst :R0 :src :R1) s)))))
    (%a64-assert-emitted-bytes
     '(#x1F #x00 #x1F #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     zero-src)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x1F #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     non-zero-src)))

(it-sequential "aarch64-eq-emitter-byte-patterns"
  (let ((equal-operands (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-eq
                            (cl-cc:make-vm-eq :dst :R0 :lhs :R1 :rhs :R1) s))))
        (unequal-operands (%a64-collect-bytes
                           (lambda (s)
                             (cl-cc/codegen::emit-a64-vm-eq
                              (cl-cc:make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     equal-operands)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     unequal-operands)))

(it-sequential "aarch64-num-eq-emitter-byte-patterns"
  (let ((equal-operands (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-num-eq
                            (cl-cc:make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R1) s))))
        (unequal-operands (%a64-collect-bytes
                           (lambda (s)
                             (cl-cc/codegen::emit-a64-vm-num-eq
                              (cl-cc:make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     equal-operands)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #x02 #x80 #x9A)
     unequal-operands)))

(it-sequential "aarch64-lt-gt-emitter-byte-patterns"
  (let ((lt-true-shape (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-lt
                           (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))))
        (lt-false-shape (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-lt
                            (cl-cc:make-vm-lt :dst :R0 :lhs :R2 :rhs :R1) s))))
        (gt-true-shape (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-gt
                           (cl-cc:make-vm-gt :dst :R0 :lhs :R2 :rhs :R1) s))))
        (gt-false-shape (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-gt
                            (cl-cc:make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xB2 #x80 #x9A)
     lt-true-shape)
    (%a64-assert-emitted-bytes
     '(#x5F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xB2 #x80 #x9A)
     lt-false-shape)
    (%a64-assert-emitted-bytes
     '(#x5F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xC2 #x80 #x9A)
     gt-true-shape)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xC2 #x80 #x9A)
     gt-false-shape)))

(it-sequential "aarch64-le-ge-emitter-byte-patterns"
  (let ((le-true-shape (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-le
                           (cl-cc:make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))))
        (le-false-shape (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-le
                            (cl-cc:make-vm-le :dst :R0 :lhs :R2 :rhs :R1) s))))
        (ge-true-shape (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-ge
                           (cl-cc:make-vm-ge :dst :R0 :lhs :R2 :rhs :R1) s))))
        (ge-false-shape (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-ge
                             (cl-cc:make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xD2 #x80 #x9A)
     le-true-shape)
    (%a64-assert-emitted-bytes
     '(#x5F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xD2 #x80 #x9A)
     le-false-shape)
    (%a64-assert-emitted-bytes
     '(#x5F #x00 #x01 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xA2 #x80 #x9A)
     ge-true-shape)
    (%a64-assert-emitted-bytes
     '(#x3F #x00 #x02 #xEB #x00 #x00 #x80 #xD2
       #x30 #x00 #x80 #xD2 #x00 #xA2 #x80 #x9A)
     ge-false-shape)))

(it-sequential "aarch64-fixnum-only-predicate-emitter-byte-patterns"
  (let ((numberp-bytes (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-true-pred
                           (cl-cc:make-vm-number-p :dst :R0 :src :R1) s))))
        (integerp-bytes (%a64-collect-bytes
                         (lambda (s)
                           (cl-cc/codegen::emit-a64-vm-true-pred
                            (cl-cc:make-vm-integer-p :dst :R0 :src :R1) s))))
        (consp-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-vm-false-pred
                         (cl-cc:make-vm-cons-p :dst :R0 :src :R1) s))))
        (symbolp-bytes (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-vm-false-pred
                           (cl-cc:make-vm-symbol-p :dst :R0 :src :R1) s))))
        (functionp-bytes (%a64-collect-bytes
                          (lambda (s)
                            (cl-cc/codegen::emit-a64-vm-false-pred
                             (cl-cc:make-vm-function-p :dst :R0 :src :R1) s)))))
    (dolist (bytes (list numberp-bytes integerp-bytes))
      (%a64-assert-emitted-bytes '(#x20 #x00 #x80 #xD2) bytes))
    (dolist (bytes (list consp-bytes symbolp-bytes functionp-bytes))
      (%a64-assert-emitted-bytes '(#x00 #x00 #x80 #xD2) bytes))))

(it-sequential "aarch64-select-emitter-encoding"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-select
                   (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (expect (gethash 'cl-cc/vm::vm-select cl-cc/codegen::*a64-emitter-table*) :to-be #'cl-cc/codegen::emit-a64-vm-select)
    (expect (= 12 (length bytes)) :to-be-truthy)
    ;; First instruction is MOV X0, X3; second is CMP X1, XZR; third is CSEL
    (expect (= #xE0 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x03 (nth 1 bytes)) :to-be-truthy)
    (expect (= #x1F (nth 6 bytes)) :to-be-truthy)
    (expect (= #x9A (nth 11 bytes)) :to-be-truthy)
    ;; The high opcode byte of each 32-bit word is neither B/B.cond nor CBZ.
    (dolist (high-op (list (nth 3 bytes) (nth 7 bytes) (nth 11 bytes)))
      (expect (member high-op '(#x14 #x54 #xB4 #xB5) :test #'=) :to-be-falsy))))

(it-sequential "aarch64-jump-zero-uses-cbz"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-jump-zero
                   (cl-cc:make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 4)
                         ht))))))
    (expect (= 4 (length bytes)) :to-be-truthy)
    ;; low byte and high opcode nibble are enough to prove CBZ path, not CMP+B.EQ
    (expect (= #x01 (logand (nth 0 bytes) #x1F)) :to-be-truthy)
    (expect (= #xB4 (nth 3 bytes)) :to-be-truthy)))

(it-sequential "aarch64-leaf-and-nonleaf-without-spills-share-fpe-layout"
  (let* ((result (compile-string "(+ 1 2)" :target :aarch64))
         (program (compilation-result-program result))
         (base (cl-cc/vm::make-vm-program :instructions (cl-cc/vm::vm-program-instructions program)
                                        :result-register (cl-cc/vm::vm-program-result-register program)
                                        :leaf-p nil))
         (leaf-bytes (compile-to-aarch64-bytes program))
         (nonleaf-bytes (compile-to-aarch64-bytes base)))
    (expect (cl-cc/vm::vm-program-leaf-p program) :to-be-truthy)
    (expect (= (length leaf-bytes) (length nonleaf-bytes)) :to-be-truthy)
    (expect (equalp leaf-bytes nonleaf-bytes) :to-be-truthy)))

(it-sequential "aarch64-empty-program-default-fpe-emits-24-bytes-with-shadow-call-stack"
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (bytes (compile-to-aarch64-bytes program)))
    (expect (= 24 (length bytes)) :to-be-truthy)
    ;; Prologue begins with STR LR, [X18], #8
    (expect (= 94 (elt bytes 0)) :to-be-truthy)
    (expect (= 134 (elt bytes 1)) :to-be-truthy)
    (expect (= 0 (elt bytes 2)) :to-be-truthy)
    (expect (= 248 (elt bytes 3)) :to-be-truthy)
    ;; With default FPE, the epilogue starts immediately with the SCS verification load.
    (expect (= 81 (elt bytes 4)) :to-be-truthy)
    (expect (= 142 (elt bytes 5)) :to-be-truthy)
    (expect (= 95 (elt bytes 6)) :to-be-truthy)
    (expect (= 248 (elt bytes 7)) :to-be-truthy)
    ;; Epilogue contains BRK #0 just before final RET.
    (expect (= 0 (elt bytes 16)) :to-be-truthy)
    (expect (= 0 (elt bytes 17)) :to-be-truthy)
    (expect (= 32 (elt bytes 18)) :to-be-truthy)
    (expect (= 212 (elt bytes 19)) :to-be-truthy)
    (expect (= #xC0 (elt bytes 20)) :to-be-truthy)
    (expect (= #x03 (elt bytes 21)) :to-be-truthy)
    (expect (= #x5F (elt bytes 22)) :to-be-truthy)
    (expect (= #xD6 (elt bytes 23)) :to-be-truthy)))

(it-sequential "aarch64-default-fpe-uses-sp-relative-spill-frame"
  (let* ((store-inst (cl-cc:make-vm-spill-store :src-reg :x19 :slot 1))
         (load-inst (cl-cc:make-vm-spill-load :dst-reg :x20 :slot 1))
         (program (cl-cc/vm::make-vm-program
                   :instructions (list store-inst load-inst)
                   :result-register :R0
                   :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions program)))
         (alloc-bytes (%a64-collect-bytes
                       (lambda (s)
                         (cl-cc/codegen::emit-a64-instr
                          (cl-cc/codegen::encode-sub-imm cl-cc/codegen::+a64-sp+
                                                         cl-cc/codegen::+a64-sp+
                                                         8 0)
                          s))))
         (free-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-instr
                         (cl-cc/codegen::encode-add-imm cl-cc/codegen::+a64-sp+
                                                        cl-cc/codegen::+a64-sp+
                                                        8 0)
                         s))))
         (store-bytes (let ((cl-cc/codegen::*current-a64-spill-base-reg* cl-cc/codegen::+a64-sp+)
                            (cl-cc/codegen::*current-a64-spill-offset-bias* 8))
                        (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-store store-inst s)))))
         (load-bytes (let ((cl-cc/codegen::*current-a64-spill-base-reg* cl-cc/codegen::+a64-sp+)
                           (cl-cc/codegen::*current-a64-spill-offset-bias* 8))
                       (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-load load-inst s)))))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra))
                  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (expect (subseq bytes 0 4) :to-equal '(#x5E #x86 #x00 #xF8))
    (expect (subseq bytes 4 8) :to-equal alloc-bytes)
    (expect (subseq bytes 8 12) :to-equal store-bytes)
    (expect (subseq bytes 12 16) :to-equal load-bytes)
    (expect (subseq bytes 16 20) :to-equal free-bytes)
    (expect (subseq bytes (- (length bytes) 4) (length bytes)) :to-equal '(#xC0 #x03 #x5F #xD6))))

(it-sequential "aarch64-debug-opt-out-keeps-fp-lr-pair-and-fp-spills"
  (let* ((store-inst (cl-cc:make-vm-spill-store :src-reg :x19 :slot 1))
         (load-inst (cl-cc:make-vm-spill-load :dst-reg :x20 :slot 1))
         (program (cl-cc/vm::make-vm-program
                   :instructions (list store-inst load-inst)
                   :result-register :R0
                   :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions program)))
         (save-pair (%a64-collect-bytes
                     (lambda (s)
                       (cl-cc/codegen::emit-a64-instr
                        (cl-cc/codegen::encode-stp-pre cl-cc/codegen::+a64-fp+
                                                       cl-cc/codegen::+a64-lr+
                                                       cl-cc/codegen::+a64-sp+
                                                       -2)
                        s))))
         (restore-pair (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-instr
                           (cl-cc/codegen::encode-ldp-post cl-cc/codegen::+a64-fp+
                                                           cl-cc/codegen::+a64-lr+
                                                           cl-cc/codegen::+a64-sp+
                                                           2)
                           s))))
         (store-bytes (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-store store-inst s))))
         (load-bytes (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-load load-inst s))))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra)
                      (cl-cc/codegen::*a64-omit-frame-pointer* nil))
                  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (expect (subseq bytes 0 4) :to-equal '(#x5E #x86 #x00 #xF8))
    (expect (subseq bytes 4 8) :to-equal save-pair)
    (expect (subseq bytes 8 12) :to-equal store-bytes)
    (expect (subseq bytes 12 16) :to-equal load-bytes)
    (expect (subseq bytes 16 20) :to-equal restore-pair)))

(it-sequential "aarch64-stack-probe-emits-page-touch-sequence"
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-stack-probes s 1)))))
    (expect bytes :to-equal '(#xF0 #x07 #x40 #xD1 #x1F #x02 #x40 #xF8))))

(it-sequential "aarch64-default-fpe-large-spill-frame-signals-unsupported-adjust"
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0 :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 512
                                                   :instructions nil)))
    (signals error (let ((cl-cc/codegen::*current-a64-regalloc* ra))
        (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s)))))))

(it-sequential "aarch64-large-spill-frame-inserts-stack-probe-before-prologue-when-fpe-disabled"
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0 :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                    :spill-count 512
                                                    :instructions nil))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra)
                      (cl-cc/codegen::*a64-omit-frame-pointer* nil))
                   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (expect (subseq bytes 0 8) :to-equal '(#xF0 #x07 #x40 #xD1 #x1F #x02 #x40 #xF8))
    (expect (subseq bytes 8 12) :to-equal '(#x5E #x86 #x00 #xF8))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Variety of programs
;;; ─────────────────────────────────────────────────────────────────────────

(it-sequential "aarch64-bytes-various-programs constant"
  (destructuring-bind (source) (list "42")
    (let ((bytes (%a64-compile source)))
    (expect bytes :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-bytes-various-programs arithmetic"
  (destructuring-bind (source) (list "(+ 3 4)")
    (let ((bytes (%a64-compile source)))
    (expect bytes :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-bytes-various-programs let"
  (destructuring-bind (source) (list "(let ((x 10)) (* x 2))")
    (let ((bytes (%a64-compile source)))
    (expect bytes :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-bytes-various-programs if"
  (destructuring-bind (source) (list "(if 1 100 200)")
    (let ((bytes (%a64-compile source)))
    (expect bytes :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))

(it-sequential "aarch64-bytes-various-programs nested"
  (destructuring-bind (source) (list "(+ (* 2 3) (- 10 4))")
    (let ((bytes (%a64-compile source)))
    (expect bytes :to-be-truthy)
    (expect (> (length bytes) 0) :to-be-truthy)
    (expect (= 0 (mod (length bytes) 4)) :to-be-truthy))))
