(in-package :cl-cc/test)



(it-sequential "vm-mop-loads-with-vm"
  :timeout
  5
  (expect (asdf:find-system :cl-cc-vm nil) :to-be-truthy))

(defun make-mop-test-class (name &key superclasses direct-slots slots initargs initforms
                                   slot-types class-slots cpl direct-subclasses)
  "Build a VM class descriptor for MOP tests."
  (let ((class (make-hash-table :test #'eq)))
    (setf (gethash :__name__ class) name
          (gethash :__superclasses__ class) superclasses
          (gethash :__direct-slots__ class) direct-slots
          (gethash :__slots__ class) (or slots direct-slots)
          (gethash :__initargs__ class) initargs
          (gethash :__initforms__ class) initforms
          (gethash :__slot-types__ class) slot-types
          (gethash :__class-slots__ class) class-slots
          (gethash :__slot-locations__ class)
          (loop for slot in (or slots direct-slots)
                for index from 0
                collect (cons slot index))
          (gethash :__cpl__ class) (or cpl (list name t))
          (gethash :__direct-subclasses__ class) direct-subclasses)
    class))

(defun make-mop-test-method (specializers &key qualifiers function)
  "Build a VM method descriptor for MOP tests."
  (let ((method (make-hash-table :test #'eq)))
    (setf (gethash :specializers method) specializers
          (gethash :specializer method)
          (if (= (length specializers) 1) (first specializers) specializers)
          (gethash :qualifiers method) qualifiers
          (gethash :function method) (or function #'identity))
    method))

(it-sequential "fr-930-class-slot-and-class-introspection"
  :timeout
  10
  (let* ((parent (make-mop-test-class 'mop-parent :direct-slots '(a) :slots '(a)
                                      :direct-subclasses '(mop-child)))
         (child (make-mop-test-class 'mop-child
                                     :superclasses '(mop-parent)
                                     :direct-slots '(b)
                                     :slots '(a b)
                                     :initargs '((:a . a) (:b . b))
                                     :initforms '((a . 1) (b . 2))
                                     :slot-types '((a . integer) (b . string))
                                     :class-slots '(b)
                                     :cpl '(mop-child mop-parent t))))
    (declare (ignore parent))
    (let ((slots (cl-cc/vm::class-slots child)))
      (expect (mapcar #'cl-cc/vm::slot-definition-name slots) :to-equal '(a b))
      (expect (cl-cc/vm::slot-definition-type (first slots)) :to-be 'integer)
      (expect (= 1 (cl-cc/vm::slot-definition-initform (first slots))) :to-be-truthy)
      (expect (cl-cc/vm::slot-definition-allocation (second slots)) :to-be :class)
      (expect (cl-cc/vm::class-direct-superclasses child) :to-equal '(mop-parent))
      (expect (cl-cc/vm::class-direct-subclasses parent) :to-equal '(mop-child))
      (expect (cl-cc/vm::class-precedence-list child) :to-equal '(mop-child mop-parent t)))))

(it-sequential "fr-930-generic-function-method-introspection"
  :timeout
  10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-gf :lambda-list '(x)))
         (primary (make-mop-test-method '(integer)))
         (before (make-mop-test-method '(integer) :qualifiers '(:before))))
    (cl-cc/vm::add-method gf primary)
    (cl-cc/vm::add-method gf before)
    (expect (member primary (cl-cc/vm::generic-function-methods gf)) :to-be-truthy)
    (expect (member before (cl-cc/vm::generic-function-methods gf)) :to-be-truthy)
    (expect (cl-cc/vm::method-specializers primary) :to-equal '(integer))
    (expect (cl-cc/vm::method-qualifiers before) :to-equal '(:before))
    (expect (cl-cc/vm::method-combination-type
                          (cl-cc/vm::generic-function-method-combination gf)) :to-be 'standard)))

(it-sequential "fr-930-satiating-gfs-p-introspection"
  :timeout
  10
  (let ((gf (cl-cc/vm::ensure-generic-function 'mop-satiated :lambda-list '(x))))
    (expect (cl-cc/vm::satiating-gfs-p) :to-be-falsy)
    (expect (cl-cc/vm::satiating-gfs-p gf) :to-be-falsy)
    (setf (gethash :__satiated__ gf) t)
    (expect (cl-cc/vm::satiating-gfs-p gf) :to-be-truthy)
    (setf (gethash :__satiated__ gf) nil)
    (expect (cl-cc/vm::satiating-gfs-p gf) :to-be-falsy)))

(it-sequential "fr-930-compute-applicable-methods"
  :timeout
  10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-applicable :lambda-list '(x)))
         (fallback (make-mop-test-method '(t)))
         (integer-method (make-mop-test-method '(integer)))
         (string-method (make-mop-test-method '(string))))
    (cl-cc/vm::add-method gf fallback)
    (cl-cc/vm::add-method gf integer-method)
    (cl-cc/vm::add-method gf string-method)
    (let ((methods (cl-cc/vm::compute-applicable-methods gf (list 42))))
      (expect (member integer-method methods) :to-be-truthy)
      (expect (member fallback methods) :to-be-truthy)
      (expect (member string-method methods) :to-be-falsy))))

(it-sequential "fr-931-compute-applicable-methods-using-classes"
  :timeout
  10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-camuc :lambda-list '(x)))
         (fallback (make-mop-test-method '(t)))
         (integer-method (make-mop-test-method '(integer))))
    (cl-cc/vm::add-method gf fallback)
    (cl-cc/vm::add-method gf integer-method)
    (multiple-value-bind (methods definitivep)
        (cl-cc/vm::compute-applicable-methods-using-classes gf '(integer))
      (expect definitivep :to-be-truthy)
      (expect (member integer-method methods) :to-be-truthy)
      (expect (member fallback methods) :to-be-truthy))))

(it-sequential "fr-931-find-add-remove-method"
  :timeout
  10
  (let* ((gf (cl-cc/vm::ensure-generic-function 'mop-dynamic :lambda-list '(x)))
         (method (make-mop-test-method '(integer))))
    (cl-cc/vm::add-method gf method)
    (expect (cl-cc/vm::find-method gf nil '(integer) nil) :to-be method)
    (cl-cc/vm::remove-method gf method)
    (expect (cl-cc/vm::find-method gf nil '(integer) nil) :to-be-null)))

(it-sequential "fr-931-slot-value-using-class-hooks"
  :timeout
  10
  (let* ((class (make-mop-test-class 'mop-slots :direct-slots '(x) :slots '(x)))
         (object (make-array 2)))
    (setf (aref object 0) class)
    (setf (cl-cc/vm::slot-value-using-class class object 'x) 7)
    (expect (= 7 (cl-cc/vm::slot-value-using-class class object 'x)) :to-be-truthy)
    (expect (cl-cc/vm::slot-bound-using-class-p class object 'x) :to-be-truthy)
    (cl-cc/vm::slot-makunbound-using-class class object 'x)
    (expect (cl-cc/vm::slot-bound-using-class-p class object 'x) :to-be-falsy)))
