;;;; tests/unit/vm/list-coerce-tests.lisp — VM List Coercion Instruction Tests

(in-package :cl-cc/test)

;;; ─── Coercion instructions ─────────────────────────────────────────────────

(it-sequential "vm-list-coerce-simple chars-to-string"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-coerce-to-string '(#\h #\i) "hi")
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-coerce-simple vector-to-list"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-coerce-to-list #(1 2 3) '(1 2 3))
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-coerce-simple symbol-to-name"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-coerce 'hello "HELLO")
    (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal expected))))

(it-sequential "vm-list-coerce-to-vector"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc:make-vm-coerce-to-vector :dst 0 :src 1) s)
    (let ((v (cl-cc:vm-reg-get s 0)))
      (expect (vectorp v) :to-be-truthy)
      (expect (= 3 (length v)) :to-be-truthy)
      (expect (aref v 0) :to-be 'a))))
