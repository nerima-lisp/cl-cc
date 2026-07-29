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

(it-sequential "collect-inherited-slots"
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (expect (cl-cc/vm::collect-inherited-slots '() reg) :to-be-null))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'animal :slots '(name age))
    (expect (cl-cc/vm::collect-inherited-slots '(animal) reg) :to-equal '(name age)))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses
    (registry-add-class reg 'flyable  :slots '(wingspan))
    (registry-add-class reg 'swimable :slots '(fin-count))
    (let ((result (cl-cc/vm::collect-inherited-slots '(flyable swimable) reg)))
      (expect (member 'wingspan  result) :to-be-truthy)
      (expect (member 'fin-count result) :to-be-truthy)))
  (let ((reg (make-test-registry)))
    ;; no duplicates
    (registry-add-class reg 'base  :slots '(id))
    (registry-add-class reg 'mixin :slots '(id extra))
    (let ((result (cl-cc/vm::collect-inherited-slots '(base mixin) reg)))
      (expect (= 2 (length result)) :to-be-truthy)
      (expect (member 'id    result) :to-be-truthy)
      (expect (member 'extra result) :to-be-truthy))))

;;; 2. collect-inherited-initargs

(it-sequential "collect-inherited-initargs"
  (let ((reg (make-test-registry)))
    ;; no superclasses
    (expect (cl-cc/vm::collect-inherited-initargs '() reg) :to-be-null))
  (let ((reg (make-test-registry)))
    ;; single superclass
    (registry-add-class reg 'person :initargs '((:name . name) (:age . age)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(person) reg)))
      (expect (assoc :name result) :to-be-truthy)
      (expect (assoc :age  result) :to-be-truthy)))
  (let ((reg (make-test-registry)))
    ;; multiple superclasses; first occurrence wins
    (registry-add-class reg 'named  :initargs '((:name . name)))
    (registry-add-class reg 'tagged :initargs '((:tag . tag)))
    (let ((result (cl-cc/vm::collect-inherited-initargs '(named tagged) reg)))
      (expect (assoc :name result) :to-be-truthy)
      (expect (assoc :tag  result) :to-be-truthy))))

;;; 3. compute-class-precedence-list

(it-sequential "compute-class-precedence-list"
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'root)
    (expect (cl-cc/vm::compute-class-precedence-list 'root reg) :to-equal '(root)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'animal)
    (registry-add-class reg 'dog :superclasses '(animal))
    (expect (cl-cc/vm::compute-class-precedence-list 'dog reg) :to-equal '(dog animal)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (expect (cl-cc/vm::compute-class-precedence-list 'a reg) :to-equal '(a b c)))
  (let ((reg (make-test-registry)))
    ;; diamond: A -> B, A -> C, B -> D, C -> D
    (registry-add-class reg 'd)
    (registry-add-class reg 'b :superclasses '(d))
    (registry-add-class reg 'c :superclasses '(d))
    (registry-add-class reg 'a :superclasses '(b c))
    (let ((cpl (cl-cc/vm::compute-class-precedence-list 'a reg)))
      (expect (member 'a cpl) :to-be-truthy)
      (expect (member 'b cpl) :to-be-truthy)
      (expect (member 'c cpl) :to-be-truthy)
      (expect (member 'd cpl) :to-be-truthy)
      (expect (= (length cpl) (length (remove-duplicates cpl))) :to-be-truthy)
      (expect cpl :to-equal '(a b c d))))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b :superclasses '(a))
    (registry-add-class reg 'c :superclasses '(a))
    (registry-add-class reg 'd :superclasses '(b c))
    (expect (cl-cc/vm::compute-class-precedence-list 'd reg) :to-equal '(d b c a)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'a)
    (registry-add-class reg 'b)
    (registry-add-class reg 'x :superclasses '(a b))
    (registry-add-class reg 'y :superclasses '(b a))
    (expect (cl-cc/vm::compute-class-precedence-list 'x reg) :to-equal '(x a b))
    (expect (cl-cc/vm::compute-class-precedence-list 'y reg) :to-equal '(y b a)))
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'z)
    (registry-add-class reg 'y :superclasses '(z))
    (registry-add-class reg 'x :superclasses '(z))
    (registry-add-class reg 'w :superclasses '(y x))
    (expect (cl-cc/vm::compute-class-precedence-list 'w reg) :to-equal '(w y x z))))

;;; 4. EQL specializer dispatch index

(it-sequential "eql-specializer-dispatch-index"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (gf (make-hash-table :test #'equal))
         (method 'read-method)
         (inst (cl-cc:make-vm-register-method
                :gf-reg :r0
                :specializer '(eql :read)
                :qualifier nil
                :method-reg :r1)))
    (setf (gethash :__methods__ gf) (make-hash-table :test #'equal)
          (gethash :__eql-index__ gf) (make-hash-table :test #'equal))
    (cl-cc:vm-reg-set state :r0 gf)
    (cl-cc:vm-reg-set state :r1 method)
    (cl-cc/vm::execute-instruction inst state 0 nil)
    (expect (cl-cc/vm::%vm-gf-eql-methods gf :read) :to-equal (list method))
    (expect (gethash :read (gethash :__eql-index__ gf)) :to-equal (list method))))

;;; 5. %cis-walk (extracted DFS helper for collect-inherited-slots)

(it-sequential "cis-walk-empty-class-list-leaves-result-unchanged"
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (cl-cc/vm::%cis-walk '() reg seen cell)
    (expect (car cell) :to-be-null)))

(it-sequential "cis-walk-single-class-accumulates-slots"
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (registry-add-class reg 'thing :slots '(x y))
    (cl-cc/vm::%cis-walk '(thing) reg seen cell)
    (expect (member 'x (car cell)) :to-be-truthy)
    (expect (member 'y (car cell)) :to-be-truthy)))

(it-sequential "cis-walk-deduplicates-slots-via-seen"
  (let ((reg (make-test-registry))
        (seen (make-hash-table :test #'eq))
        (cell (list nil)))
    (setf (gethash 'id seen) t)
    (registry-add-class reg 'base :slots '(id extra))
    (cl-cc/vm::%cis-walk '(base) reg seen cell)
    (expect (member 'id    (car cell)) :to-be-falsy)
    (expect (member 'extra (car cell)) :to-be-truthy)))

;;; 6. %cia-walk (extracted DFS helper for collect-inherited-initargs)

(it-sequential "cia-walk-empty-class-list-leaves-result-unchanged"
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (cl-cc/vm::%cia-walk '() reg cell)
    (expect (car cell) :to-be-null)))

(it-sequential "cia-walk-single-class-accumulates-initargs"
  (let ((reg (make-test-registry))
        (cell (list nil)))
    (registry-add-class reg 'named :initargs '((:name . name)))
    (cl-cc/vm::%cia-walk '(named) reg cell)
    (expect (assoc :name (car cell)) :to-be-truthy)))

(it-sequential "cia-walk-does-not-duplicate-existing-keys"
  (let ((reg (make-test-registry))
        (cell (list (list '(:name . name-old)))))
    (registry-add-class reg 'alt :initargs '((:name . name-new) (:extra . extra)))
    (cl-cc/vm::%cia-walk '(alt) reg cell)
    (expect (= 1 (length (remove-if-not (lambda (e) (eq (car e) :name)) (car cell)))) :to-be-truthy)
    (expect (assoc :extra (car cell)) :to-be-truthy)))

;;; 6b. %vm-allow-other-keys-p / %vm-validate-initargs

(it-sequential "vm-allow-other-keys-p-cases truthy"
  (destructuring-bind (reg-val alist) (list t '((:allow-other-keys . :r0)))
    (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 reg-val)
    (if reg-val
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-truthy)
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

(it-sequential "vm-allow-other-keys-p-cases nil-val"
  (destructuring-bind (reg-val alist) (list nil '((:allow-other-keys . :r0)))
    (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 reg-val)
    (if reg-val
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-truthy)
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

(it-sequential "vm-allow-other-keys-p-cases absent"
  (destructuring-bind (reg-val alist) (list nil '((:x . :r0)))
    (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 reg-val)
    (if reg-val
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-truthy)
        (expect (cl-cc/vm::%vm-allow-other-keys-p alist s) :to-be-falsy)))))

(it-sequential "vm-validate-initargs-known-key-accepts-without-signal"
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (expect (progn
       (cl-cc/vm::%vm-validate-initargs '((:width . :r0)) '((:width . width)) s)
       t) :to-be-truthy)))

(it-sequential "vm-validate-initargs-unknown-key-signals-error"
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (signals error (cl-cc/vm::%vm-validate-initargs '((:unknown-key . :r0)) '((:width . width)) s))))

(it-sequential "vm-validate-initargs-allow-other-keys-bypasses-check"
  (let ((s (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc:vm-reg-set s :r0 42)
    (cl-cc:vm-reg-set s :r1 t)
    (expect (progn
       (cl-cc/vm::%vm-validate-initargs
        '((:unknown-key . :r0) (:allow-other-keys . :r1))
        '((:width . width))
        s)
       t) :to-be-truthy)))

;;; 7. %cpl-linearize (extracted C3 linearization step)

(it-sequential "cpl-linearize-unknown-class-returns-singleton"
  (let ((reg (make-test-registry)))
    (expect (cl-cc/vm::%cpl-linearize 'unknown reg) :to-equal '(unknown))))

(it-sequential "cpl-linearize-leaf-class-returns-singleton"
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'leaf)
    (expect (cl-cc/vm::%cpl-linearize 'leaf reg) :to-equal '(leaf))))

(it-sequential "cpl-linearize-linear-inheritance-chain"
  (let ((reg (make-test-registry)))
    (registry-add-class reg 'c)
    (registry-add-class reg 'b :superclasses '(c))
    (registry-add-class reg 'a :superclasses '(b))
    (expect (cl-cc/vm::%cpl-linearize 'a reg) :to-equal '(a b c))))

;;; FR-821: copy operations

(defclass vm-copy-host-object ()
  ((items :initarg :items :accessor vm-copy-host-object-items)
   (flag :initarg :flag :accessor vm-copy-host-object-flag)))

(it-sequential "fr-821-copy-instance-host-standard-object"
  (let* ((items (list 1 2 3))
         (object (make-instance 'vm-copy-host-object :items items :flag :original))
         (copy (cl-cc/vm::copy-instance object)))
    (setf (vm-copy-host-object-flag copy) :changed)
    (expect (eq object copy) :to-be-falsy)
    (expect (vm-copy-host-object-items copy) :to-be items)
    (expect (vm-copy-host-object-flag object) :to-be :original)
    (expect (vm-copy-host-object-flag copy) :to-be :changed)))

(it-sequential "fr-821-deep-copy-recursive-conses-and-vectors"
  (let* ((nested (vector (list :a :b)))
         (copy (cl-cc/vm::deep-copy nested)))
    (setf (car (aref nested 0)) :changed)
    (expect (eq nested copy) :to-be-falsy)
    (expect (eq (aref nested 0) (aref copy 0)) :to-be-falsy)
    (expect (car (aref copy 0)) :to-be :a)))

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

(it-sequential "fr-838-extensible-sequence-subseq-and-setf-elt"
  (let* ((deque (make-instance 'vm-test-deque :storage (vector 10 20 30 40)))
         (slice (cl-cc/vm::subseq deque 1 3)))
    (setf (cl-cc/vm::elt deque 2) 99)
    (expect (cl-cc/vm::sequence-protocol-p deque) :to-be-truthy)
    (expect (= 4 (cl-cc/vm::length deque)) :to-be-truthy)
    (expect (= 99 (cl-cc/vm::elt deque 2)) :to-be-truthy)
    (expect (= 2 (cl-cc/vm::length slice)) :to-be-truthy)
    (expect (= 20 (cl-cc/vm::elt slice 0)) :to-be-truthy)
    (expect (= 30 (cl-cc/vm::elt slice 1)) :to-be-truthy)))

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

(it-sequential "fr-888-dynamic-layout-falls-back-from-vector-allocation"
  (let ((class (registry-add-class (make-test-registry) 'dynamic :slots '(x))))
    (setf (gethash :__dynamic-slot-layout__ class) t)
    (signals error (cl-cc/vm::allocate-instance-vector class))))

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
