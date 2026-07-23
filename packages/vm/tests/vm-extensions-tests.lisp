;;;; tests/unit/vm/vm-extensions-tests.lisp — VM language extension instruction tests

(in-package :cl-cc/test)



(defun %vm-ext-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-ext-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(it-sequential "vm-symbol-get-default"
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-GET-DEFAULT-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 :none)
    (exec1 (cl-cc:make-vm-symbol-get :dst 0 :sym 1 :indicator 2 :default 3) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal :none)))

(it-sequential "vm-symbol-set-and-get-roundtrip-with-host-sync"
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-ROUNDTRIP-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (cl-cc:vm-reg-set s 4 :shape)
    (cl-cc:vm-reg-set s 5 'circle)
    (cl-cc:vm-reg-set s 6 nil)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 4 :value 5) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 7 :sym 1 :indicator 2 :default 6) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 8 :sym 1 :indicator 4 :default 6) s)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 9 :src 1) s)
    (expect (cl-cc:vm-reg-get s 7) :to-equal 'red)
    (expect (cl-cc:vm-reg-get s 8) :to-equal 'circle)
    (expect (getf (cl-cc:vm-reg-get s 9) :color) :to-equal 'red)
    (expect (getf (cl-cc:vm-reg-get s 9) :shape) :to-equal 'circle)
    (expect (consp (cl-cc:vm-reg-get s 9)) :to-be-truthy)
    (expect (getf (symbol-plist sym) :color) :to-equal 'red)
    (expect (getf (symbol-plist sym) :shape) :to-equal 'circle)))

(it-sequential "vm-set-symbol-value"
  (let ((s (make-test-vm)))
    (setf (symbol-value 'prim-test-global) 0)
    (cl-cc:vm-reg-set s 1 'prim-test-global)
    (cl-cc:vm-reg-set s 2 77)
    (exec1 (cl-cc:make-vm-set-symbol-value :dst 0 :lhs 1 :rhs 2) s)
    (expect (= 77 (cl-cc:vm-reg-get s 0)) :to-be-truthy)
    (expect (= 77 (symbol-value 'prim-test-global)) :to-be-truthy)))

(it-sequential "vm-remprop"
  (let ((s (make-test-vm))
        (sym (gensym "VM-REMPROP-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-remprop :dst 0 :sym 1 :indicator 2) s)
    (expect (cl-cc:vm-reg-get s 0) :to-equal t)
    (expect (getf (symbol-plist sym) :color) :to-be-null)))

(it-sequential "vm-symbol-plist-empty"
  (let ((s (make-test-vm))
        (sym (gensym "VM-SYMBOL-EMPTY-")))
    (cl-cc:vm-reg-set s 1 sym)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 0 :src 1) s)
    (expect (cl-cc:vm-reg-get s 0) :to-be-null)))

(it-sequential "vm-set-symbol-plist-overwrites-and-promotes-long-plist"
  (let* ((s (make-test-vm))
         (sym (gensym "VM-LONG-PLIST-"))
         (long-plist '(:a 1 :b 2 :c 3 :d 4 :e 5)))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :stale)
    (cl-cc:vm-reg-set s 3 :old)
    (cl-cc:vm-reg-set s 4 long-plist)
    (cl-cc:vm-reg-set s 5 nil)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (exec1 (cl-cc:make-vm-set-symbol-plist :dst 6 :sym 1 :plist-reg 4) s)
    (exec1 (cl-cc:make-vm-symbol-get :dst 7 :sym 1 :indicator 2 :default 5) s)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 9 :src 1) s)
    (expect (cl-cc:vm-reg-get s 7) :to-be-null)
    (expect (getf (cl-cc:vm-reg-get s 9) :a) :to-equal 1)
    (expect (getf (cl-cc:vm-reg-get s 9) :e) :to-equal 5)
    (expect (getf (symbol-plist sym) :e) :to-equal 5)
    (expect (cl-cc/vm::%vm-symbol-property-entry-p
      (gethash sym (cl-cc/vm::vm-symbol-plists s))) :to-be-truthy)))

(it-sequential "vm-system-property-storage-is-separate"
  (let* ((s (make-test-vm))
         (sym (gensym "VM-SYSTEM-PROPS-")))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :user)
    (cl-cc:vm-reg-set s 3 :value)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (cl-cc/vm::vm-system-property-set s sym :function 'compiled-fn)
    (cl-cc/vm::vm-system-property-set s sym :type 'function)
    (exec1 (cl-cc:make-vm-symbol-plist :dst 4 :src 1) s)
    (expect (getf (cl-cc:vm-reg-get s 4) :user) :to-equal :value)
    (expect (getf (cl-cc:vm-reg-get s 4) :function) :to-be-null)
    (expect (cl-cc/vm::vm-system-property-get s sym :function) :to-equal 'compiled-fn)
    (expect (getf (cl-cc/vm::vm-system-property-plist s sym) :type) :to-equal 'function)
    (expect (getf (symbol-plist sym) :function) :to-be-null)))

(it-sequential "vm-symbol-plist-lock-and-read-barrier-are-usable"
  (let* ((s (make-test-vm))
         (sym (gensym "VM-BARRIER-"))
         (lock (cl-cc/vm::vm-symbol-plist-lock s))
         (barrier-before (cl-cc/vm::vm-symbol-plist-read-barrier s)))
    (expect lock :to-be-truthy)
    (cl-cc/runtime:rt-with-lock (lock)
      (expect (cl-cc/vm::vm-symbol-plist-read-barrier s) :to-equal barrier-before))
    (cl-cc:vm-reg-set s 1 sym)
    (cl-cc:vm-reg-set s 2 :flag)
    (cl-cc:vm-reg-set s 3 t)
    (exec1 (cl-cc:make-vm-symbol-set :dst 0 :sym 1 :indicator 2 :value 3) s)
    (multiple-value-bind (plist barrier-after)
        (cl-cc/vm::vm-symbol-plist-read-snapshot s sym)
      (expect (> barrier-after barrier-before) :to-be-truthy)
      (expect (getf plist :flag) :to-equal t))))

(it-sequential "vm-progv-enter-exit"
  (let ((s (make-test-vm)))
    (setf (gethash 'prim-pv-x (cl-cc:vm-global-vars s)) 10)
    (cl-cc:vm-reg-set s 1 '(prim-pv-x prim-pv-y))
    (cl-cc:vm-reg-set s 2 '(99 100))
    (exec1 (cl-cc:make-vm-progv-enter :dst 0 :syms 1 :vals 2) s)
    (expect (= 99 (gethash 'prim-pv-x (cl-cc:vm-global-vars s))) :to-be-truthy)
    (expect (= 100 (gethash 'prim-pv-y (cl-cc:vm-global-vars s))) :to-be-truthy)
    (exec1 (cl-cc:make-vm-progv-exit :saved 0) s)
    (expect (= 10 (gethash 'prim-pv-x (cl-cc:vm-global-vars s))) :to-be-truthy)
    (expect (nth-value 1 (gethash 'prim-pv-y (cl-cc:vm-global-vars s))) :to-be-falsy)))

(it-sequential "vm-generic-arith add"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-add 10 3 13)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-generic-arith sub"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-sub 10 3 7)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-generic-arith mul"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-mul 10 3 30)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-generic-arith div"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-div 10 3 3)
    (expect (= expected (%vm-ext-binary ctor lhs rhs)) :to-be-truthy)))

(it-sequential "vm-generic-comparison eq-equal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-eq "hello" "hello" t)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))

(it-sequential "vm-generic-comparison eq-unequal"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-eq "hello" "world" nil)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))

(it-sequential "vm-generic-comparison lt-true"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-lt 3 5 t)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))

(it-sequential "vm-generic-comparison gt-false"
  (destructuring-bind (ctor lhs rhs expected) (list #'cl-cc:make-vm-generic-gt 3 5 nil)
    (if expected
      (expect (%vm-ext-binary ctor lhs rhs) :to-equal t)
      (expect (%vm-ext-binary ctor lhs rhs) :to-be-null))))

(it-sequential "vm-generic-div-by-zero"
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (signals error (exec1 (cl-cc:make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
