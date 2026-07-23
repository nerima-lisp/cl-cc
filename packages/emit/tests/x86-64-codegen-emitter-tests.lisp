;;;; tests/unit/emit/x86-64-codegen-emitter-tests.lisp — x86-64 Comparison/Unary Emitter Tests
;;;;
;;;; Tests for comparison emitter byte content, unary emitter byte content,
;;;; and build-label-offsets from src/emit/x86-64-codegen.lisp.

(in-package :cl-cc/test)


;;; ─── Comparison emitter byte content ────────────────────────────────────────
;;;
;;; Each comparison emitter emits: CMP(3) + SETcc(3) + MOVZX(4) = 10 bytes
;;; (when all three registers are low regs R0/R1/R2 = rax/rcx/rdx, no REX on SETcc).
;;; The SETcc sub-sequence is at offset [3]: 0F <opcode2> ModRM.
;;; So byte index 4 is the condition opcode distinguishing each comparison.

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-lt"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-lt
                          (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)) #x9C)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-gt"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-gt
                          (cl-cc:make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)) #x9F)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-le"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-le
                          (cl-cc:make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)) #x9E)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-ge"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-ge
                          (cl-cc:make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)) #x9D)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-num-eq"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-num-eq
                          (cl-cc:make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)) #x94)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-setcc-opcode vm-eq"
  (destructuring-bind (emit-fn expected-opcode2) (list (lambda (s) (cl-cc/codegen::emit-vm-eq
                          (cl-cc:make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)) #x94)
    (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= expected-opcode2 setcc-opcode2) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-lt"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-lt
                          (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-gt"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-gt
                          (cl-cc:make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-le"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-le
                          (cl-cc:make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-ge"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-ge
                          (cl-cc:make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-num-eq"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-num-eq
                          (cl-cc:make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-comparison-emitter-cmp-opcode vm-eq"
  (destructuring-bind (emit-fn) (list (lambda (s) (cl-cc/codegen::emit-vm-eq
                          (cl-cc:make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)))
    (let ((bytes (%x86-collect-bytes emit-fn)))
    (expect (= #x39 (nth 1 bytes)) :to-be-truthy))))

(it-sequential "x86-64-num-eq-and-eq-share-encoding"
  (let ((num-eq-bytes (%x86-collect-bytes
                       (lambda (s) (cl-cc/codegen::emit-vm-num-eq
                                    (cl-cc:make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
        (eq-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc/codegen::emit-vm-eq
                                (cl-cc:make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (expect eq-bytes :to-equal num-eq-bytes)))

;;; ─── Unary emitter byte content ──────────────────────────────────────────────
;;;
;;; vm-neg:    MOV dst←src (3) + NEG dst (3) = 6 bytes
;;; vm-lognot: MOV dst←src (3) + NOT dst (3) = 6 bytes
;;; vm-not:    TEST src,src (3) + SETE dst (3) + MOVZX dst,dst8 (4) = 10 bytes
;;; vm-inc:    MOV dst←src (3) + ADD dst,1 imm8 (4) = 7 bytes
;;; vm-dec:    MOV dst←src (3) + SUB dst,1 imm8 (4) = 7 bytes

(it-sequential "x86-64-unary-emitter-byte-count vm-neg"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-neg
                          (cl-cc:make-vm-neg :dst :R0 :src :R1) s)) 6)
    (expect (= expected-size (length (%x86-collect-bytes emit-fn))) :to-be-truthy)))

(it-sequential "x86-64-unary-emitter-byte-count vm-lognot"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-lognot
                          (cl-cc:make-vm-lognot :dst :R0 :src :R1) s)) 6)
    (expect (= expected-size (length (%x86-collect-bytes emit-fn))) :to-be-truthy)))

(it-sequential "x86-64-unary-emitter-byte-count vm-not"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-not
                          (cl-cc:make-vm-not :dst :R0 :src :R1) s)) 10)
    (expect (= expected-size (length (%x86-collect-bytes emit-fn))) :to-be-truthy)))

(it-sequential "x86-64-unary-emitter-byte-count vm-inc"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-inc
                          (cl-cc:make-vm-inc :dst :R0 :src :R1) s)) 7)
    (expect (= expected-size (length (%x86-collect-bytes emit-fn))) :to-be-truthy)))

(it-sequential "x86-64-unary-emitter-byte-count vm-dec"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-dec
                          (cl-cc:make-vm-dec :dst :R0 :src :R1) s)) 7)
    (expect (= expected-size (length (%x86-collect-bytes emit-fn))) :to-be-truthy)))

(it-sequential "x86-64-unary-encoding-details"
  (let ((neg-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/codegen::emit-vm-neg
                                 (cl-cc:make-vm-neg :dst :R0 :src :R1) s)))))
    (expect (= #x48 (nth 0 neg-bytes)) :to-be-truthy)
    (expect (= #x89 (nth 1 neg-bytes)) :to-be-truthy))
  (let ((not-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/codegen::emit-vm-not
                                 (cl-cc:make-vm-not :dst :R0 :src :R1) s)))))
    (expect (= #x48 (nth 0 not-bytes)) :to-be-truthy)
    (expect (= #x0F (nth 3 not-bytes)) :to-be-truthy)
    (expect (= #x94 (nth 4 not-bytes)) :to-be-truthy))
  (let ((inc-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/codegen::emit-vm-inc
                                 (cl-cc:make-vm-inc :dst :R0 :src :R1) s))))
        (dec-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/codegen::emit-vm-dec
                                 (cl-cc:make-vm-dec :dst :R0 :src :R1) s)))))
    (expect (= #x83 (nth 4 inc-bytes)) :to-be-truthy)
    (expect (= #x83 (nth 4 dec-bytes)) :to-be-truthy)
    (expect (= 1 (car (last inc-bytes))) :to-be-truthy)
    (expect (= 1 (car (last dec-bytes))) :to-be-truthy)))

;;; ─── build-label-offsets ────────────────────────────────────────────────────

(it-sequential "x86-64-build-label-offsets-empty"
  (expect (= 0 (hash-table-count (cl-cc/codegen::build-label-offsets '() 0))) :to-be-truthy))

(it-sequential "x86-64-build-label-offsets-first-label-at-zero"
  (let* ((lbl (cl-cc:make-vm-label :name "entry"))
         (offsets (cl-cc/codegen::build-label-offsets (list lbl) 0)))
    (expect (= 0 (gethash "entry" offsets)) :to-be-truthy)))

(it-sequential "x86-64-build-label-offsets-prologue-offset"
  (let* ((lbl (cl-cc:make-vm-label :name "start"))
         (offsets (cl-cc/codegen::build-label-offsets (list lbl) 6)))
    (expect (= 6 (gethash "start" offsets)) :to-be-truthy)))

(it-sequential "x86-64-build-label-offsets-after-const-is-10"
  (let* ((const-inst (cl-cc:make-vm-const :dst :R0 :value 42))
         (lbl (cl-cc:make-vm-label :name "after-const"))
         (offsets (cl-cc/codegen::build-label-offsets (list const-inst lbl) 0)))
    (expect (= 10 (gethash "after-const" offsets)) :to-be-truthy)))

(it-sequential "x86-64-build-label-offsets-elided-self-move"
  (let* ((insts (list (cl-cc:make-vm-move :dst :R0 :src :R0)
                      (cl-cc:make-vm-label :name "after-self-move")
                      (cl-cc:make-vm-halt :reg :R0)))
         (offsets (let ((cl-cc/codegen::*current-regalloc* nil))
                    (cl-cc/codegen::build-label-offsets insts 0))))
    (expect (= 0 (gethash "after-self-move" offsets)) :to-be-truthy)))

;;; ─── FR-072 shrink-wrapping ────────────────────────────────────────────────

(defun %fr072-assignment (&rest pairs)
  (let ((ht (make-hash-table :test #'eq)))
    (loop for (vreg phys) on pairs by #'cddr
          do (setf (gethash vreg ht) phys))
    ht))

(it-sequential "x86-64-shrink-wrap-delays-save-to-cold-block"
  (let* ((insts (list (cl-cc:make-vm-jump-zero :reg :R0 :label "cold")
                      (cl-cc:make-vm-halt :reg :R0)
                      (cl-cc:make-vm-label :name "cold")
                      (cl-cc:make-vm-add :dst :R1 :lhs :R2 :rhs :R3)
                      (cl-cc:make-vm-halt :reg :R0)))
         (assignment (%fr072-assignment :R1 :rbx))
         (rbx cl-cc/codegen::+rbx+))
    (multiple-value-bind (annotated entry-regs final-regs)
        (cl-cc/codegen::x86-64-shrink-wrap-instructions insts (list rbx) assignment)
      (expect entry-regs :to-equal nil)
      (expect final-regs :to-equal nil)
      (expect (= 1 (count-if (lambda (x) (typep x 'cl-cc/codegen::x86-64-shrink-save)) annotated)) :to-be-truthy)
      (expect (= 1 (count-if (lambda (x) (typep x 'cl-cc/codegen::x86-64-shrink-restore)) annotated)) :to-be-truthy))))

(it-sequential "x86-64-shrink-wrap-early-return-path-skips-save"
  (let* ((insts (list (cl-cc:make-vm-jump-zero :reg :R0 :label "cold")
                      (cl-cc:make-vm-halt :reg :R0)
                      (cl-cc:make-vm-label :name "cold")
                      (cl-cc:make-vm-add :dst :R1 :lhs :R2 :rhs :R3)
                      (cl-cc:make-vm-halt :reg :R0)))
         (assignment (%fr072-assignment :R1 :rbx)))
    (multiple-value-bind (annotated entry-regs final-regs)
        (cl-cc/codegen::x86-64-shrink-wrap-instructions
         insts (list cl-cc/codegen::+rbx+) assignment)
      (declare (ignore entry-regs final-regs))
      (let ((early-halt (position-if (lambda (x) (typep x 'cl-cc/vm::vm-halt)) annotated))
            (save (position-if (lambda (x) (typep x 'cl-cc/codegen::x86-64-shrink-save))
                               annotated)))
        (expect (< early-halt save) :to-be-truthy)))))

(it-sequential "x86-64-shrink-wrap-degenerate-entry-use-stays-monolithic"
  (let* ((insts (list (cl-cc:make-vm-add :dst :R1 :lhs :R2 :rhs :R3)
                      (cl-cc:make-vm-halt :reg :R1)))
         (assignment (%fr072-assignment :R1 :rbx))
         (rbx cl-cc/codegen::+rbx+))
    (multiple-value-bind (annotated entry-regs final-regs)
        (cl-cc/codegen::x86-64-shrink-wrap-instructions insts (list rbx) assignment)
      (expect entry-regs :to-equal (list rbx))
      (expect final-regs :to-equal (list rbx))
      (expect (find-if (lambda (x) (typep x 'cl-cc/codegen::x86-64-shrink-save))
                             annotated) :to-be-falsy))))
