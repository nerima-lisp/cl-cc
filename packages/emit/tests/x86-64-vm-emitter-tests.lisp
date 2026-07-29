;;;; tests/unit/emit/x86-64-vm-emitter-tests.lisp — VM Instruction Emitter Byte-Size Tests
;;;;
;;;; Continuation of x86-64-encoding-tests.lisp.
;;;; Tests for VM instruction emitters: exact byte counts and instruction-size
;;;; dispatch table completeness.

(in-package :cl-cc/test)


;;; ─── VM instruction emitter byte sizes ──────────────────────────────────────

(it-sequential "x86-vm-emitter-byte-size vm-neg"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-neg
                           (cl-cc:make-vm-neg :dst :R0 :src :R1) s)) 6)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-not"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-not
                           (cl-cc:make-vm-not :dst :R0 :src :R1) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-lognot"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-lognot
                           (cl-cc:make-vm-lognot :dst :R0 :src :R1) s)) 6)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-inc"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-inc
                           (cl-cc:make-vm-inc :dst :R0 :src :R1) s)) 7)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-dec"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-dec
                           (cl-cc:make-vm-dec :dst :R0 :src :R1) s)) 7)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-abs"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-abs
                           (make-vm-abs :dst :R0 :src :R1) s)) 15)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-min"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-min
                           (make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-max"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-max
                            (make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-select"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-select
                            (make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logand"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logand
                            (make-vm-logand :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logior"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logior
                           (make-vm-logior :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logxor"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logxor
                           (make-vm-logxor :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logeqv"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logeqv
                           (cl-cc:make-vm-logeqv :dst :R0 :lhs :R1 :rhs :R2) s)) 9)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-and"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-and
                           (cl-cc:make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s)) 17)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-or"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-or
                           (cl-cc:make-vm-or  :dst :R0 :lhs :R1 :rhs :R2) s)) 17)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-true-pred"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-true-pred
                             (cl-cc:make-vm-number-p :dst :R0 :src :R1) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-false-pred"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-false-pred
                             (cl-cc:make-vm-cons-p :dst :R0 :src :R1) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-truncate"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-truncate
                           (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s)) 21)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-rem"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-rem
                           (cl-cc:make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s)) 21)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-ash"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-ash
                            (make-vm-ash :dst :R0 :lhs :R1 :rhs :R2) s)) 24)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-mul"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-mul
                            (cl-cc:make-vm-mul :dst :R0 :lhs :R1 :rhs :R2) s)) 7)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-integer-mul-high-u"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-integer-mul-high-u
                             (cl-cc:make-vm-integer-mul-high-u :dst :R0 :lhs :R1 :rhs :R2) s)) 19)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-integer-mul-high-s"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-integer-mul-high-s
                             (cl-cc:make-vm-integer-mul-high-s :dst :R0 :lhs :R1 :rhs :R2) s)) 19)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-div"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-div
                            (cl-cc:make-vm-div :dst :R0 :lhs :R1 :rhs :R2) s)) 34)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-mod"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-mod
                           (cl-cc:make-vm-mod :dst :R0 :lhs :R1 :rhs :R2) s)) 37)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-lt"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-lt
                           (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-gt"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-gt
                           (cl-cc:make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-le"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-le
                           (cl-cc:make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-ge"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-ge
                           (cl-cc:make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-num-eq"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-num-eq
                           (make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-eq"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-eq
                           (make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-null-p"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-null-p
                           (cl-cc:make-vm-null-p :dst :R0 :src :R1) s)) 10)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logtest"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logtest
                           (cl-cc:make-vm-logtest :dst :R0 :lhs :R1 :rhs :R2) s)) 13)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

(it-sequential "x86-vm-emitter-byte-size vm-logbitp"
  (destructuring-bind (emit-fn expected-size) (list (lambda (s) (cl-cc/codegen::emit-vm-logbitp
                           (cl-cc:make-vm-logbitp :dst :R0 :lhs :R1 :rhs :R2) s)) 15)
    (expect (length (%x86-encoding-collect-bytes emit-fn)) :to-equal expected-size)))

;;; ─── instruction-size table ──────────────────────────────────────────────────

(it-sequential "x86-instruction-size-table-coverage"
  (dolist (tp '(cl-cc/vm::vm-const cl-cc/vm::vm-move cl-cc/vm::vm-add cl-cc/vm::vm-sub cl-cc/vm::vm-mul
                cl-cc/vm::vm-integer-mul-high-u cl-cc/vm::vm-integer-mul-high-s
                cl-cc/vm::vm-halt cl-cc/vm::vm-label cl-cc/vm::vm-jump cl-cc/vm::vm-jump-zero cl-cc/vm::vm-ret
                cl-cc/vm::vm-lt cl-cc/vm::vm-gt cl-cc/vm::vm-le cl-cc/vm::vm-ge cl-cc/vm::vm-num-eq cl-cc/vm::vm-eq
                cl-cc/vm::vm-neg cl-cc/vm::vm-not cl-cc/vm::vm-lognot cl-cc/vm::vm-inc cl-cc/vm::vm-dec
                cl-cc/vm::vm-abs cl-cc/vm::vm-min cl-cc/vm::vm-max cl-cc/vm::vm-select cl-cc/vm::vm-ash
                cl-cc/vm::vm-truncate cl-cc/vm::vm-rem cl-cc/vm::vm-div cl-cc/vm::vm-mod
                cl-cc/vm::vm-and cl-cc/vm::vm-or cl-cc/vm::vm-logand cl-cc/vm::vm-logior cl-cc/vm::vm-logxor
                cl-cc/vm::vm-logeqv cl-cc/vm::vm-logtest cl-cc/vm::vm-logbitp
                cl-cc/vm::vm-null-p cl-cc/vm::vm-number-p cl-cc/vm::vm-integer-p
                cl-cc/vm::vm-cons-p cl-cc/vm::vm-symbol-p cl-cc/vm::vm-function-p
                cl-cc/codegen::vm-spill-store cl-cc/codegen::vm-spill-load))
    (expect (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*) :to-be-truthy)))
