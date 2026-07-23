(in-package :cl-cc/test)



(it-sequential "fr-670-flat-closure-uses-fixed-vector-access"
  (let* ((env (cl-cc/vm::vm-make-flat-environment #(:x :y) #(10 20)))
         (closure (cl-cc/vm::vm-make-flat-closure "L" '(:arg) #(:x :y) #(0 0)
                                                  :environment env)))
    (expect (typep closure 'cl-cc/vm::vm-flat-closure) :to-be-truthy)
    (expect (cl-cc/vm::vm-flat-closure-ref closure 0) :to-equal 10)
    (expect (cl-cc/vm::vm-flat-closure-ref closure 1) :to-equal 20)
    (setf (cl-cc/vm::vm-flat-closure-ref closure 1) 99)
    (expect (aref (cl-cc/vm::vm-flat-environment-values env) 1) :to-equal 99)))

(it-sequential "fr-670-lambda-liftable-small-non-recursive-closures"
  (expect (cl-cc/vm::vm-lambda-liftable-p '(:a :b) :recursive-p nil :escaping-p nil) :to-be-truthy)
  (expect (cl-cc/vm::vm-lambda-liftable-p '(:a :b) :recursive-p t :escaping-p nil) :to-be-falsy)
  (expect (cl-cc/vm::vm-lambda-lift-params '(:x) '(:a :b)) :to-equal '(:a :b :x)))

(it-sequential "fr-671-slot-vector-layout-and-direct-ref"
  (let* ((class (make-hash-table :test #'eq))
         (object (make-array 3 :initial-element cl-cc/vm::*unbound-slot-marker*)))
    (setf (gethash :__name__ class) 'point
          (gethash :__slots__ class) '(x y)
          (aref object 0) class)
    (cl-cc/vm::vm-install-slot-vector-layout class '(x y))
    (expect (cl-cc/vm::vm-slot-offset class 'x) :to-equal 0)
    (expect (cl-cc/vm::vm-slot-offset class 'y) :to-equal 1)
    (setf (cl-cc/vm::vm-slot-ref object 0) 7)
    (setf (cl-cc/vm::vm-slot-ref object 1) 9)
    (expect (cl-cc/vm::vm-slot-ref object 0) :to-equal 7)
    (expect (cl-cc/vm::vm-slot-ref object 1) :to-equal 9)))

(it-sequential "fr-671-change-class-remaps-shared-slots"
  (let* ((old-class (make-hash-table :test #'eq))
         (new-class (make-hash-table :test #'eq))
         (object (make-array 3 :initial-element cl-cc/vm::*unbound-slot-marker*)))
    (setf (gethash :__slots__ old-class) '(x y)
          (gethash :__slots__ new-class) '(y z x)
          (aref object 0) old-class)
    (cl-cc/vm::vm-install-slot-vector-layout old-class '(x y))
    (cl-cc/vm::vm-install-slot-vector-layout new-class '(y z x))
    (setf (cl-cc/vm::vm-slot-ref object 0) :x-value
          (cl-cc/vm::vm-slot-ref object 1) :y-value)
    (let ((remapped (cl-cc/vm::vm-change-class-remap-slot-vector object new-class)))
      (expect (cl-cc/vm::vm-slot-ref remapped 0) :to-equal :y-value)
      (expect (cl-cc/vm::vm-slot-ref remapped 2) :to-equal :x-value))))

(it-sequential "fr-672-vtable-single-dispatch-rebuilds-lazily"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (class (make-hash-table :test #'eq))
         (gf (make-hash-table :test #'equal))
         (methods (make-hash-table :test #'equal))
         (method-a (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "A" :params nil))
         (method-b (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "B" :params nil)))
    (setf (gethash :__name__ class) 'thing
          (gethash :__cpl__ class) '(thing t)
          (gethash :__methods__ gf) methods
          (gethash 'thing methods) method-a)
    (setf (gethash 'thing (cl-cc/vm::vm-class-registry state)) class)
    (cl-cc/vm::vm-mark-vtables-dirty gf)
    (expect (cl-cc/vm::vm-vtable-method class gf state) :to-be method-a)
    (setf (gethash 'thing methods) method-b)
    (cl-cc/vm::vm-mark-vtables-dirty gf)
    (expect (cl-cc/vm::vm-vtable-method class gf state) :to-be method-b)))

(it-sequential "fr-672-two-dimensional-dispatch-table"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (gf (make-hash-table :test #'equal))
         (methods (make-hash-table :test #'equal))
         (method :method-ab))
    (setf (gethash :__methods__ gf) methods
          (gethash '(a b) methods) method)
    (let ((table (cl-cc/vm::vm-build-2d-dispatch-table gf state '(a) '(b))))
      (expect (aref table 0 0) :to-equal method))))
