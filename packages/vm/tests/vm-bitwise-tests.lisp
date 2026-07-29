;;;; tests/unit/vm/vm-bitwise-tests.lisp — VM bitwise integer instruction tests

(in-package :cl-cc/test)



(defun %make-unary (ctor src)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-binary (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-pred2 (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(it-sequential "vm-bitwise-binary logand"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logand #xFF #x0F #x0F)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary logior"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logior #xF0 #x0F #xFF)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary logxor"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logxor #xFF #x0F #xF0)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary logeqv"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logeqv #xFF #xFF -1)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary ash-left"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-ash 1 4 16)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary ash-right"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-ash 16 -2 4)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-binary expt-2^10"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-expt 2 10 1024)
    (expect (= expected (%make-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary lognot-0"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-lognot 0 -1)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary bswap-1234"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-bswap #x12345678 #x78563412)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary lognot-ff"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-lognot #xFF (lognot #xFF))
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary logcount-7"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-logcount 7 3)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary logcount-0"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-logcount 0 0)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary intlen-0"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-integer-length 0 0)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-unary intlen-255"
  (destructuring-bind (ctor src expected) (list #'cl-cc:make-vm-integer-length 255 8)
    (expect (= expected (%make-unary ctor src)) :to-be-truthy)))

(it-sequential "vm-bitwise-pred logtest-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logtest #xFF #x01 1)
    (expect (= expected (%make-pred2 ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-pred logtest-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logtest #xF0 #x0F 0)
    (expect (= expected (%make-pred2 ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-pred logbitp-set"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logbitp 0 1 1)
    (expect (= expected (%make-pred2 ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-bitwise-pred logbitp-clear"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-logbitp 1 1 0)
    (expect (= expected (%make-pred2 ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-mul-high-64-semantics"
  (expect (%make-binary #'cl-cc:make-vm-integer-mul-high-u
                              #xFFFFFFFFFFFFFFFF
                              2) :to-equal 1)
  (expect (%make-binary #'cl-cc:make-vm-integer-mul-high-u
                              (ash 1 64)
                              5) :to-equal 0)
  (expect (%make-binary #'cl-cc:make-vm-integer-mul-high-s
                              -1
                              2) :to-equal -1)
  (expect (%make-binary #'cl-cc:make-vm-integer-mul-high-s
                              #xFFFFFFFFFFFFFFFF
                              2) :to-equal -1)
  (expect (%make-binary #'cl-cc:make-vm-integer-mul-high-s
                              (- (ash 1 63))
                              2) :to-equal -1))
