;;;; tests/unit/emit/x86-64-sequences-tests.lisp
;;;; Coverage for src/emit/x86-64-sequences.lisp:
;;;;   emit-idiv-r11, emit-cqo, emit-idiv-sequence, emit-mul-high-sequence,
;;;;   emit-sal-r64-cl, emit-sar-r64-cl, emit-ror-r64-cl,
;;;;   emit-add-ri8, emit-sub-ri8, emit-and-ri8, emit-jge-short,
;;;;   emit-cmovl-rr64, emit-cmovg-rr64, emit-cmovne-rr64,
;;;;   emit-and-rr64, emit-or-rr64, emit-xor-rr64,
;;;;   emit-not-r64, emit-bswap-r32, emit-neg-r64, emit-dec-r64,
;;;;   emit-setcc, emit-movzx-r64-r8.

(in-package :cl-cc/test)



;;; ─── Helper ──────────────────────────────────────────────────────────────

(defun %collect-seq-bytes (emit-fn)
  "Call EMIT-FN with a byte-collector stream. Returns list of emitted bytes."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-idiv-r11 ───────────────────────────────────────────────────────

(it-sequential "x86-seq-idiv-r11-emits-3-bytes"
  (let ((bs (%collect-seq-bytes #'cl-cc/codegen::emit-idiv-r11)))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #x49 (first bs)) :to-be-truthy)   ; REX.W=1, REX.B=1
    (expect (= #xF7 (second bs)) :to-be-truthy)  ; IDIV opcode
    (expect (= (cl-cc/codegen::modrm 3 7 3) (third bs)) :to-be-truthy))) ; mod=11, reg=7, rm=3

;;; ─── emit-cqo ────────────────────────────────────────────────────────────

(it-sequential "x86-seq-cqo-emits-2-bytes"
  (let ((bs (%collect-seq-bytes #'cl-cc/codegen::emit-cqo)))
    (expect (= 2 (length bs)) :to-be-truthy)
    (expect (= #x48 (first bs)) :to-be-truthy)
    (expect (= #x99 (second bs)) :to-be-truthy)))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(it-sequential "x86-seq-idiv-sequence-18-bytes quotient"
  (destructuring-bind (remainder-p) (list nil)
    (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc/codegen::emit-idiv-sequence
                          cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ remainder-p s)))))
    (expect (= 18 (length bs)) :to-be-truthy))))

(it-sequential "x86-seq-idiv-sequence-18-bytes remainder"
  (destructuring-bind (remainder-p) (list t)
    (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc/codegen::emit-idiv-sequence
                          cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ remainder-p s)))))
    (expect (= 18 (length bs)) :to-be-truthy))))

(it-sequential "x86-seq-idiv-sequence-contains-cqo"
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc/codegen::emit-idiv-sequence
                          cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ nil s)))))
    ;; CQO = 48 99 somewhere in the sequence
    (let ((cqo-pos (loop for i from 0 below (1- (length bs))
                         when (and (= (nth i bs) #x48)
                                   (= (nth (1+ i) bs) #x99))
                           return i)))
      (expect cqo-pos :to-be-truthy))))

(it-sequential "x86-seq-mul-high-sequence-encodings"
  (let ((umulh-bytes (%collect-seq-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mul-high-sequence
                         cl-cc/codegen::+rax+
                         cl-cc/codegen::+rcx+
                         nil
                         s))))
        (smulh-bytes (%collect-seq-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mul-high-sequence
                         cl-cc/codegen::+rax+
                         cl-cc/codegen::+rcx+
                         t
                         s)))))
    (expect umulh-bytes :to-equal '(#x49 #x89 #xCB #x50 #x52 #x48 #x89 #xC0 #x49 #xF7 #xE3 #x49 #x89 #xD3 #x5A #x58))
    (expect smulh-bytes :to-equal '(#x49 #x89 #xCB #x50 #x52 #x48 #x89 #xC0 #x49 #xF7 #xEB #x49 #x89 #xD3 #x5A #x58))))

;;; ─── emit-sal-r64-cl / emit-sar-r64-cl / emit-ror-r64-cl ────────────────

(it-sequential "x86-seq-shift-r64-cl-cases sal"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-sal-r64-cl)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xD3 (second bs)) :to-be-truthy))))

(it-sequential "x86-seq-shift-r64-cl-cases sar"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-sar-r64-cl)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xD3 (second bs)) :to-be-truthy))))

(it-sequential "x86-seq-shift-r64-cl-cases ror"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-ror-r64-cl)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xD3 (second bs)) :to-be-truthy))))

;;; ─── emit-add/sub/and-ri8 ────────────────────────────────────────────────

(it-sequential "x86-seq-alu-ri8-cases add"
  (destructuring-bind (emitter imm8) (list #'cl-cc/codegen::emit-add-ri8 5)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ imm8 s)))))
    (expect (= 4 (length bs)) :to-be-truthy)
    (expect (= #x83 (second bs)) :to-be-truthy)
    (expect (= imm8 (fourth bs)) :to-be-truthy))))

(it-sequential "x86-seq-alu-ri8-cases sub"
  (destructuring-bind (emitter imm8) (list #'cl-cc/codegen::emit-sub-ri8 8)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ imm8 s)))))
    (expect (= 4 (length bs)) :to-be-truthy)
    (expect (= #x83 (second bs)) :to-be-truthy)
    (expect (= imm8 (fourth bs)) :to-be-truthy))))

(it-sequential "x86-seq-alu-ri8-cases and"
  (destructuring-bind (emitter imm8) (list #'cl-cc/codegen::emit-and-ri8 15)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ imm8 s)))))
    (expect (= 4 (length bs)) :to-be-truthy)
    (expect (= #x83 (second bs)) :to-be-truthy)
    (expect (= imm8 (fourth bs)) :to-be-truthy))))

;;; ─── emit-jge-short ──────────────────────────────────────────────────────

(it-sequential "x86-seq-jge-short-emits-2-bytes"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-jge-short 10 s)))))
    (expect (= 2 (length bs)) :to-be-truthy)
    (expect (= #x7D (first bs)) :to-be-truthy)
    (expect (= 10 (second bs)) :to-be-truthy)))

;;; ─── CMOV variants ───────────────────────────────────────────────────────

(it-sequential "x86-seq-cmovl-rr64-emits-correct-prefix"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-cmovl-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (> (length bs) 2) :to-be-truthy)
    (expect (member #x0F bs) :to-be-truthy)
    (expect (member #x4C bs) :to-be-truthy)))

(it-sequential "x86-seq-cmovg-rr64-emits-correct-opcode"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-cmovg-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (member #x0F bs) :to-be-truthy)
    (expect (member #x4F bs) :to-be-truthy)))

(it-sequential "x86-seq-cmovne-rr64-emits-correct-opcode"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-cmovne-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (member #x0F bs) :to-be-truthy)
    (expect (member #x45 bs) :to-be-truthy)))

;;; ─── Boolean ops on registers ────────────────────────────────────────────

(it-sequential "x86-seq-boolean-rr64-cases and"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-and-rr64)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (>= (length bs) 3) :to-be-truthy))))

(it-sequential "x86-seq-boolean-rr64-cases or"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-or-rr64)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (>= (length bs) 3) :to-be-truthy))))

(it-sequential "x86-seq-boolean-rr64-cases xor"
  (destructuring-bind (emitter) (list #'cl-cc/codegen::emit-xor-rr64)
    (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)))))
    (expect (>= (length bs) 3) :to-be-truthy))))

;;; ─── Unary ops ───────────────────────────────────────────────────────────

(it-sequential "x86-seq-not-neg-r64-3-bytes"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-not-r64 cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xF7 (second bs)) :to-be-truthy))
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-neg-r64 cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xF7 (second bs)) :to-be-truthy)))

(it-sequential "x86-seq-dec-r64-emits-3-bytes"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-dec-r64 cl-cc/codegen::+rax+ s)))))
    (expect (= 3 (length bs)) :to-be-truthy)
    (expect (= #xFF (second bs)) :to-be-truthy)))

(it-sequential "x86-seq-bswap-r32-emits-correct-bytes"
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/codegen::emit-bswap-r32 cl-cc/codegen::+rax+ s)))))
    (expect (>= (length bs) 2) :to-be-truthy)
    (expect (member #x0F bs) :to-be-truthy)))
