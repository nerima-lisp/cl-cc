(in-package :cl-cc/test)



(it-sequential "ebpf-plan-rejects-heap-ops"
  (signals error (cl-cc/emit:plan-ebpf-program 'bad '((alloc 16) (:exit)))))

(it-sequential "ebpf-emit-bytecode-encodes-basic-program"
  (multiple-value-bind (plan bytes)
      (cl-cc/emit:compile-ebpf-program 'ok
                                       '((:mov64-imm 0 42)
                                         (:add64-imm 0 1)
                                         (:exit)))
    (expect (cl-cc/emit::ebpf-program-plan-program-name plan) :to-equal 'ok)
    (expect (cl-cc/emit::ebpf-program-plan-verifier-safe-p plan) :to-be-truthy)
    (expect (length bytes) :to-equal 24)
    ;; mov64 imm r0, 42 => opcode B7, dst/src byte 00, imm low byte 2A
    (expect (aref bytes 0) :to-equal #xB7)
    (expect (aref bytes 1) :to-equal #x00)
    (expect (aref bytes 4) :to-equal #x2A)
    ;; add64 imm r0, 1 => opcode 07 at second insn
    (expect (aref bytes 8) :to-equal #x07)
    (expect (aref bytes 12) :to-equal #x01)
    ;; exit opcode at third insn
    (expect (aref bytes 16) :to-equal #x95)))
