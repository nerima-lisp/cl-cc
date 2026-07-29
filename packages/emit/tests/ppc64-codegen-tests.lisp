(in-package :cl-cc/test)



(it-sequential "power10-basic-encoders-and-big-endian-emission"
  (expect (cl-cc/codegen:encode-ppc64-nop) :to-equal #x60000000)
  (expect (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r3 cl-cc/codegen:r1 16) :to-equal #x38610010)
  (expect (cl-cc/codegen:encode-power10-add cl-cc/codegen:r3 cl-cc/codegen:r3 cl-cc/codegen:r4) :to-equal #x7C632214)
  (expect (cl-cc/codegen:encode-power10-ld cl-cc/codegen:r3 cl-cc/codegen:r1 16) :to-equal #xE8610010)
  (let ((bytes nil))
    (cl-cc/codegen:emit-ppc64-instr #x38610010 (lambda (b) (push b bytes)))
    (expect (nreverse bytes) :to-equal '(#x38 #x61 #x00 #x10))))

(it-sequential "power10-elfv2-frame-and-backend-gate"
  (let ((prologue (cl-cc/codegen:ppc64-elfv2-prologue :frame-size 32))
        (epilogue (cl-cc/codegen:ppc64-elfv2-epilogue :frame-size 32)))
    (expect (length prologue) :to-equal 3)
    (expect (length epilogue) :to-equal 3)
    (expect (third prologue) :to-equal (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r1 cl-cc/codegen:r1 -32))
    (expect (first epilogue) :to-equal (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r1 cl-cc/codegen:r1 32)))
  (signals error (cl-cc/codegen:ppc64-elfv2-prologue :frame-size 24))
  (let ((cl-cc/codegen:*ppc64-enabled* nil))
    (expect (cl-cc/codegen:ppc64-backend-available-p) :to-be-falsy))
  (let ((cl-cc/codegen:*ppc64-enabled* t))
    (expect (cl-cc/codegen:ppc64-backend-available-p) :to-be-truthy)))
