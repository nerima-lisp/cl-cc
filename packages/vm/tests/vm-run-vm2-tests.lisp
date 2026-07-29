;;;; tests/unit/vm/vm-run-vm2-tests.lisp — VM2 run-vm execution tests

(in-package :cl-cc/test)


(it-sequential "run-vm-basic-ops nop"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-nop+ 0 nil nil
                          cl-cc:+op2-const+ 0 42 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 42)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-const"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-const+ 0 '(1 2 3) nil
                          cl-cc:+op2-halt2+ 0 nil nil) '(1 2 3))
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops const-load"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 0 42 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 42)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-nil"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-nil+ 0 nil nil
                          cl-cc:+op2-halt2+ 0 nil nil) nil)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-true+ 0 nil nil
                          cl-cc:+op2-halt2+ 0 nil nil) t)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops load-fixnum"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 0 123 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 123)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops move"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7   nil
                          cl-cc:+op2-move+  0 1   nil
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops neg"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7   nil
                          cl-cc:+op2-neg+   0 1   nil
                          cl-cc:+op2-halt2+ 0 nil nil) -7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops inc"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7   nil
                          cl-cc:+op2-inc+   0 1   nil
                          cl-cc:+op2-halt2+ 0 nil nil) 8)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops dec"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7   nil
                          cl-cc:+op2-dec+   0 1   nil
                          cl-cc:+op2-halt2+ 0 nil nil) 6)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops add"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 3   nil
                          cl-cc:+op2-const+ 2 4   nil
                          cl-cc:+op2-add2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops sub"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 10  nil
                          cl-cc:+op2-const+ 2 3   nil
                          cl-cc:+op2-sub2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops mul"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 6   nil
                          cl-cc:+op2-const+ 2 7   nil
                          cl-cc:+op2-mul2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 42)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops div"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 21  nil
                          cl-cc:+op2-const+ 2 3   nil
                          cl-cc:+op2-div+   0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops mod"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 22  nil
                          cl-cc:+op2-const+ 2 5   nil
                          cl-cc:+op2-mod+   0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil) 2)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-jump+ 8 nil nil
                          cl-cc:+op2-const+ 0 1 nil
                          cl-cc:+op2-const+ 0 2 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 2)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump-if-nil"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-nil+ 1 nil nil
                          cl-cc:+op2-jump-if-nil+ 1 8 nil
                          cl-cc:+op2-const+ 0 1 nil
                          cl-cc:+op2-const+ 0 2 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 2)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops jump-if-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-true+ 1 nil nil
                          cl-cc:+op2-jump-if-true+ 1 8 nil
                          cl-cc:+op2-const+ 0 1 nil
                          cl-cc:+op2-const+ 0 2 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 2)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops values/recv-values"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 0 10 nil
                          cl-cc:+op2-load-fixnum+ 1 20 nil
                          cl-cc:+op2-values+ 2 nil nil
                          cl-cc:+op2-load-nil+ 0 nil nil
                          cl-cc:+op2-load-nil+ 1 nil nil
                          cl-cc:+op2-recv-values+ 2 nil nil
                          cl-cc:+op2-halt2+ 1 nil nil) 20)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops return"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 1 33 nil
                          cl-cc:+op2-return+ 1 nil nil) 33)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops return-nil"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-return-nil+ 0 nil nil) nil)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops fixnump"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 1 123 nil
                          cl-cc:+op2-fixnump+    0 1   nil
                          cl-cc:+op2-halt2+      0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops consp"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 '(a . b) nil
                          cl-cc:+op2-consp+ 0 1 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops symbolp"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 'foo nil
                          cl-cc:+op2-symbolp+ 0 1 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops functionp"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 #'car nil
                          cl-cc:+op2-functionp+ 0 1 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops stringp"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 "hi" nil
                          cl-cc:+op2-stringp+ 0 1 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops cons/car"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 'a nil
                          cl-cc:+op2-const+ 2 'b nil
                          cl-cc:+op2-cons+  3 1 2
                          cl-cc:+op2-car+   0 3 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 'a)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops cdr"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 'a nil
                          cl-cc:+op2-const+ 2 'b nil
                          cl-cc:+op2-cons+  3 1 2
                          cl-cc:+op2-cdr+   0 3 nil
                          cl-cc:+op2-halt2+ 0 nil nil) 'b)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops vector-ref/set"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 1 3 nil
                          cl-cc:+op2-load-fixnum+ 2 0 nil
                          cl-cc:+op2-load-fixnum+ 3 7 nil
                          cl-cc:+op2-make-vector+ 4 1 2
                          cl-cc:+op2-vector-set+ 4 2 3
                          cl-cc:+op2-vector-ref+ 0 4 2
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops hash-ref/set"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 1 8 nil
                          cl-cc:+op2-make-hash+  4 1 nil
                          cl-cc:+op2-const+      2 'foo nil
                          cl-cc:+op2-load-fixnum+ 3 77 nil
                          cl-cc:+op2-hash-set+   4 2 3
                          cl-cc:+op2-hash-ref+   0 4 2
                          cl-cc:+op2-halt2+      0 nil nil) 77)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-basic-ops global-ref/set"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-load-fixnum+ 1 55 nil
                          cl-cc:+op2-set-global+ 'foo 1 nil
                          cl-cc:+op2-get-global+ 0 'foo nil
                          cl-cc:+op2-halt2+      0 nil nil) 55)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (cl-cc/vm::run-vm bytecode s) :to-equal expected))))

(it-sequential "run-vm-immediate-ops add-imm"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 3   nil
                          cl-cc:+op2-add-imm2+ 0 1   4
                          cl-cc:+op2-halt2+ 0 nil nil) 7)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-immediate-ops cmp-imm"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7   nil
                          cl-cc:+op2-num-gt-imm2+ 0 1   5
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-immediate-ops cmp-imm-false"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 2   nil
                          cl-cc:+op2-num-eq-imm2+ 0 1   5
                          cl-cc:+op2-halt2+ 0 nil nil) 0)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-eq-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 5 nil
                          cl-cc:+op2-const+ 2 5 nil
                          cl-cc:+op2-num-eq2+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-lt-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 3 nil
                          cl-cc:+op2-const+ 2 9 nil
                          cl-cc:+op2-num-lt2+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-gt-false"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 3 nil
                          cl-cc:+op2-const+ 2 9 nil
                          cl-cc:+op2-num-gt2+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 0)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-le-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 9 nil
                          cl-cc:+op2-const+ 2 9 nil
                          cl-cc:+op2-num-le2+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops num-ge-false"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 3 nil
                          cl-cc:+op2-const+ 2 9 nil
                          cl-cc:+op2-num-ge2+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 0)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops eq-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7 nil
                          cl-cc:+op2-move+  2 1 nil
                          cl-cc:+op2-eq+    0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops eql-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 7 nil
                          cl-cc:+op2-const+ 2 7 nil
                          cl-cc:+op2-eql+   0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))

(it-sequential "run-vm-generic-compare-ops equal-true"
  (destructuring-bind (bytecode expected) (list (make-bytecode cl-cc:+op2-const+ 1 '(1 2) nil
                          cl-cc:+op2-const+ 2 '(1 2) nil
                          cl-cc:+op2-equal+ 0 1 2
                          cl-cc:+op2-halt2+ 0 nil nil) 1)
    (let ((s (cl-cc:make-vm2-state)))
    (expect (= expected (cl-cc/vm::run-vm bytecode s)) :to-be-truthy))))
