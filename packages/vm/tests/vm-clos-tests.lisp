;;;; tests/unit/vm/vm-clos-tests.lisp — CLOS infrastructure + error dispatch unit tests

(in-package :cl-cc/test)



;;; Helpers — build a minimal class-registry hash table

(defun make-test-registry ()
  "Return a fresh empty registry (eq-keyed hash table)."
  (make-hash-table :test #'eq))

(defun registry-add-class (registry name &key (superclasses '()) (slots '()) (initargs '()))
  "Insert a minimal class entry into REGISTRY and return the class hash table."
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash :__name__          ht) name
          (gethash :__superclasses__  ht) superclasses
          (gethash :__slots__         ht) slots
          (gethash :__initargs__      ht) initargs)
    (setf (gethash name registry) ht)
    ht))

;;; 1. collect-inherited-slots

;;; 2. collect-inherited-initargs

;;; 3. compute-class-precedence-list

;;; 4. EQL specializer dispatch index

;;; 5. %cis-walk (extracted DFS helper for collect-inherited-slots)

;;; 6. %cia-walk (extracted DFS helper for collect-inherited-initargs)

;;; 6b. %vm-allow-other-keys-p / %vm-validate-initargs

;;; 7. %cpl-linearize (extracted C3 linearization step)

;;; FR-821: copy operations

(defclass vm-copy-host-object ()
  ((items :initarg :items :accessor vm-copy-host-object-items)
   (flag :initarg :flag :accessor vm-copy-host-object-flag)))

;;; FR-838: extensible sequence protocol

(defclass vm-test-deque (cl-cc/vm::sequence)
  ((storage :initarg :storage :accessor vm-test-deque-storage)))

(defmethod cl-cc/vm::length ((sequence vm-test-deque))
  (length (vm-test-deque-storage sequence)))

(defmethod cl-cc/vm::elt ((sequence vm-test-deque) index)
  (aref (vm-test-deque-storage sequence) index))

(defmethod (setf cl-cc/vm::elt) (value (sequence vm-test-deque) index)
  (setf (aref (vm-test-deque-storage sequence) index) value))

(defmethod cl-cc/vm::make-sequence-like ((sequence vm-test-deque) size &key initial-element)
  (declare (ignore sequence))
  (make-instance 'vm-test-deque
                 :storage (make-array size :initial-element initial-element)))

;;; 8. FR-888 allocate-instance fast path

(it-sequential "fr-888-finalize-class-builds-slot-vector-index"
  (let ((class (registry-add-class (make-test-registry) 'point :slots '(x y))))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (expect (= 1 (cl-cc/vm::class-slot-vector-index class 'x)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::class-slot-vector-index class 'y)) :to-be-truthy)
    (expect (cl-cc/vm::class-slot-vector-index class 'z) :to-be-null)))

(it-sequential "fr-888-allocate-instance-vector-uses-class-header-and-class-id"
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x y)))
         (instance (progn
                     (cl-cc/vm::finalize-class-allocation-cache class)
                     (cl-cc/vm::allocate-instance-vector class))))
    (expect (vectorp instance) :to-be-truthy)
    (expect (aref instance 0) :to-be class)
    (expect (= 3 (length instance)) :to-be-truthy)
    (expect (integerp (gethash :__class-id__ class)) :to-be-truthy)
    (expect (= (gethash :__class-id__ class) (cl-cc/vm::%vm-vector-instance-class-id instance)) :to-be-truthy)))

(it-sequential "fr-888-slot-value-by-index-reads-and-writes-in-constant-time"
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x y)))
         (instance (progn
                     (cl-cc/vm::finalize-class-allocation-cache class)
                     (cl-cc/vm::allocate-instance-vector class)))
         (x-index (cl-cc/vm::class-slot-vector-index class 'x)))
    (setf (cl-cc/vm::slot-value-by-index instance x-index) 42)
    (expect (= 42 (cl-cc/vm::slot-value-by-index instance x-index)) :to-be-truthy)))

;;; 9. FR-889 default-initargs / make-instance caching

(it-sequential "fr-889-finalize-caches-evaluated-default-initargs"
  (let ((class (registry-add-class (make-test-registry)
                                   'shape
                                   :slots '(color radius)
                                   :initargs '((:color . color) (:radius . radius)))))
    (setf (gethash :__default-initargs__ class) '((:color . :red) (:radius . 5)))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (expect (gethash :__cached-default-initargs__ class) :to-equal '((:color . :red) (:radius . 5)))))

(it-sequential "fr-889-merge-cached-defaults-preserves-explicit-initargs"
  (let ((merged (cl-cc/vm::merge-cached-default-initargs
                 '((:color . :red) (:radius . 5))
                 '((:radius . 9) (:name . circle)))))
    (expect merged :to-equal '((:radius . 9) (:name . circle) (:color . :red)))))

(it-sequential "fr-889-zero-arg-template-copy-produces-fresh-vector"
  (let* ((class (registry-add-class (make-test-registry) 'point :slots '(x)))
         (first nil)
         (second nil))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (setf first (cl-cc/vm::%vm-copy-instance-template class)
          second (cl-cc/vm::%vm-copy-instance-template class))
    (expect (eq first second) :to-be-falsy)
    (setf (cl-cc/vm::slot-value-by-index first 1) 10)
    (expect (cl-cc/vm::slot-value-by-index second 1) :to-be-null)))

(it-sequential "fr-889-make-instance-cache-specializes-by-initarg-signature"
  (let ((class (registry-add-class (make-test-registry) 'point :slots '(x y))))
    (cl-cc/vm::finalize-class-allocation-cache class)
    (expect (cl-cc/vm::%vm-cached-constructor-path class nil) :to-be :zero-arg-template)
    (expect (cl-cc/vm::%vm-cached-constructor-path class '((:x . :r0))) :to-be :standard-vector)
    (expect (cl-cc/vm::%vm-cached-constructor-path class '((:x . :r1))) :to-be :standard-vector)
    (expect (= 2 (hash-table-count (gethash :__make-instance-cache__ class))) :to-be-truthy)))
