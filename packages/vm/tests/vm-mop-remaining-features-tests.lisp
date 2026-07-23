;;;; tests/unit/vm/vm-mop-remaining-features-tests.lisp
;;;; TDD RED-phase baseline tests for the remaining MOP/SBCL compatibility
;;;; surface. These tests intentionally define the expected behavior before the
;;;; VM features are implemented; they should fail until the corresponding
;;;; buckets are completed.

(in-package :cl-cc/test)



(defmacro assert-mop-red-ok (source)
  "Run SOURCE in the CL-CC VM with stdlib support and expect :OK.
Wraps execution in a 8-second timeout so undefined-function hangs in the VM
turn into clean test failures rather than blocking the parallel worker thread."
  `(handler-case
       (sb-ext:with-timeout 8
         (assert-evaluates-to ,source :ok :stdlib t))
     (sb-ext:timeout ()
       (%fail-test (format nil "assert-mop-red-ok: 8s timeout evaluating ~S" ,source)
                   :expected :ok
                   :actual :timeout
                   :form (list 'assert-mop-red-ok ,source)))))


;;; ─── Effective slots ─────────────────────────────────────────────────────

(it-sequential "mop-effective-slots-compute-effective-slot-definition"
  (assert-mop-red-ok
   "(progn
      (defclass mop-eff-base () ((x :initarg :x :initform 10)))
      (let* ((class (class-of (make-instance 'mop-eff-base)))
             (direct-slots (class-direct-slots class))
             (slot (compute-effective-slot-definition class 'x direct-slots)))
        (if (and slot (eq (slot-definition-name slot) 'x)) :ok :bad)))"))

(it-sequential "mop-effective-slots-class-slots-includes-inherited-slots"
  (assert-mop-red-ok
   "(progn
      (defclass mop-eff-parent () ((a :initarg :a)))
      (defclass mop-eff-child (mop-eff-parent) ((b :initarg :b)))
      (let ((names (mapcar #'slot-definition-name
                           (class-slots (class-of (make-instance 'mop-eff-child))))))
        (if (and (member 'a names) (member 'b names)) :ok :bad)))"))

(it-sequential "mop-effective-slots-class-direct-slots-excludes-inherited-slots"
  (assert-mop-red-ok
   "(progn
      (defclass mop-direct-parent () ((a :initarg :a)))
      (defclass mop-direct-child (mop-direct-parent) ((b :initarg :b)))
      (let ((names (mapcar #'slot-definition-name
                           (class-direct-slots (class-of (make-instance 'mop-direct-child))))))
        (if (and (not (member 'a names)) (member 'b names)) :ok :bad)))"))

(it-sequential "mop-effective-slots-same-name-inherited-slot-merge"
  (assert-mop-red-ok
   "(progn
      (defclass mop-merge-parent () ((x :initarg :parent-x :initform :parent)))
      (defclass mop-merge-child (mop-merge-parent) ((x :initarg :child-x :initform :child)))
      (let* ((slots (class-slots (class-of (make-instance 'mop-merge-child))))
             (names (mapcar #'slot-definition-name slots)))
        (if (= 1 (length (remove-if-not (lambda (name) (eq name 'x)) names)))
            :ok
            :bad)))"))

;;; ─── Initargs/initforms ──────────────────────────────────────────────────

(it-sequential "mop-initargs-union-across-superclass-slots"
  (assert-mop-red-ok
   "(progn
      (defclass mop-init-parent () ((x :initarg :x)))
      (defclass mop-init-child (mop-init-parent) ((y :initarg :y)))
      (let ((obj (make-instance 'mop-init-child :x 7 :y 9)))
        (if (and (= (slot-value obj 'x) 7)
                 (= (slot-value obj 'y) 9))
            :ok
            :bad)))"))

(it-sequential "mop-initforms-own-slot-precedence-over-inherited-slot"
  (assert-mop-red-ok
   "(progn
      (defclass mop-own-init-parent () ((x :initform :parent)))
      (defclass mop-own-init-child (mop-own-init-parent) ((x :initform :child)))
      (let ((obj (make-instance 'mop-own-init-child)))
        (if (eq (slot-value obj 'x) :child) :ok :bad)))"))

(it-sequential "mop-initforms-explicit-initarg-suppresses-initform"
  (assert-mop-red-ok
   "(progn
       (defclass mop-initarg-wins () ((x :initarg :x :initform :default)))
       (let ((obj (make-instance 'mop-initarg-wins :x :explicit)))
         (if (eq (slot-value obj 'x) :explicit) :ok :bad)))"))

(it-sequential "mop-initargs-inherited-overridden-slot-initarg-suppresses-initform"
  (assert-mop-red-ok
   "(progn
       (defclass mop-inherited-initarg-a () ((x :initarg :a-x :initform 1)))
       (defclass mop-inherited-initarg-b (mop-inherited-initarg-a) ((x :initarg :b-x)))
       (let ((obj (make-instance 'mop-inherited-initarg-b :a-x 5)))
         (if (= (slot-value obj 'x) 5) :ok :bad)))"))

(it-sequential "mop-initforms-leftmost-superclass-priority"
  (assert-mop-red-ok
   "(progn
       (defclass mop-initform-left () ((x :initform :left)))
       (defclass mop-initform-right () ((x :initform :right)))
       (defclass mop-initform-child (mop-initform-left mop-initform-right) ())
       (let ((obj (make-instance 'mop-initform-child)))
         (if (eq (slot-value obj 'x) :left) :ok :bad)))"))

;;; ─── Metaclass hooks ─────────────────────────────────────────────────────

(it-sequential "mop-metaclass-hooks-slot-access-protocol"
  (assert-mop-red-ok
   "(progn
      (defclass mop-hook-meta (standard-class) ())
      (defclass mop-hook-target () ((x :initarg :x)) (:metaclass mop-hook-meta))
      (defmethod slot-value-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore object slot-name))
        :hook-read)
      (defmethod (setf slot-value-using-class) (new-value (class mop-hook-meta) object slot-name)
        (declare (ignore class slot-name))
        (setf (gethash :hook-write object) new-value))
      (defmethod slot-boundp-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore class object slot-name))
        :hook-boundp)
      (defmethod slot-makunbound-using-class ((class mop-hook-meta) object slot-name)
        (declare (ignore class slot-name))
        (setf (gethash :hook-makunbound object) t)
        object)
      (let ((obj (make-instance 'mop-hook-target :x :initial)))
        (setf (slot-value obj 'x) :written)
        (let ((read-result (slot-value obj 'x))
              (boundp-result (slot-boundp obj 'x)))
          (slot-makunbound obj 'x)
          (if (and (eq read-result :hook-read)
                   (eq boundp-result :hook-boundp)
                   (eq (gethash :hook-write obj) :written)
                   (gethash :hook-makunbound obj))
              :ok
              :bad))))"))

(it-sequential "mop-metaclass-hooks-allocate-instance"
  (assert-mop-red-ok
   "(progn
      (defclass mop-alloc-meta (standard-class) ())
      (defclass mop-alloc-target () ((x :initarg :x)) (:metaclass mop-alloc-meta))
      (defmethod allocate-instance ((class mop-alloc-meta) &rest initargs)
        (declare (ignore initargs))
        (let ((obj (call-next-method)))
          (setf (slot-value obj 'x) :allocated)
          obj))
      (let ((obj (make-instance 'mop-alloc-target)))
        (if (eq (slot-value obj 'x) :allocated) :ok :bad)))"))

(it-sequential "mop-metaclass-hooks-initialize-instance-fallback"
  (assert-mop-red-ok
   "(progn
      (defclass mop-init-fallback () ((x :initarg :x :initform :default)))
      (let ((obj (make-instance 'mop-init-fallback :x :from-initarg)))
        (if (eq (slot-value obj 'x) :from-initarg) :ok :bad)))"))

;;; ─── Dispatch memoization ────────────────────────────────────────────────

(it-sequential "mop-dispatch-memoization-multi-dispatch-cache-hit"
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-md (a b))
      (defmethod mop-md ((a integer) (b string)) :integer-string)
      (if (and (eq (mop-md 1 \"x\") :integer-string)
               (eq (mop-md 2 \"y\") :integer-string))
          :ok :bad))"))

(it-sequential "mop-dispatch-memoization-invalidates-after-register-method"
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-register-method (x))
      (defmethod mop-register-method ((x t)) :fallback)
      (mop-register-method 1)
      (defmethod mop-register-method ((x integer)) :integer)
      (if (eq (mop-register-method 1) :integer) :ok :bad))"))

(it-sequential "mop-dispatch-memoization-eql-specializer-safety"
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-eql-safe (x))
      (defmethod mop-eql-safe ((x symbol)) :symbol)
      (defmethod mop-eql-safe ((x (eql :exact))) :exact)
      (mop-eql-safe :other)
      (if (and (eq (mop-eql-safe :exact) :exact)
               (eq (mop-eql-safe :other) :symbol))
          :ok
          :bad))"))

(it-sequential "mop-dispatch-memoization-qualified-method-safety"
  (assert-mop-red-ok
   "(progn
      (defvar *mop-qualified-log* nil)
      (defgeneric mop-qualified (x))
      (defmethod mop-qualified :before ((x integer)) (push :before *mop-qualified-log*))
      (defmethod mop-qualified ((x integer)) :primary)
      (mop-qualified 1)
      (setq *mop-qualified-log* nil)
      (let ((result (mop-qualified 2)))
        (if (and (eq result :primary) (equal *mop-qualified-log* '(:before))) :ok :bad)))"))

;;; ─── Shape/layout ────────────────────────────────────────────────────────

(it-sequential "mop-shape-layout-slot-definition-location-valid-indices"
  (assert-mop-red-ok
   "(progn
      (defclass mop-layout-index () ((a :initarg :a) (b :initarg :b)))
      (let ((locations (mapcar #'slot-definition-location
                               (class-slots (class-of (make-instance 'mop-layout-index))))))
        (if (every (lambda (x) (and (integerp x) (>= x 0))) locations) :ok :bad)))"))

(it-sequential "mop-shape-layout-class-redefinition-shape-change"
  (assert-mop-red-ok
   "(progn
      (defclass mop-shape-change () ((x :initarg :x)))
      (defparameter *mop-shape-object* (make-instance 'mop-shape-change :x 1))
      (defclass mop-shape-change () ((x :initarg :x) (y :initform 2)))
      (if (and (= (slot-value *mop-shape-object* 'x) 1)
               (= (slot-value *mop-shape-object* 'y) 2))
          :ok
          :bad))"))

(it-sequential "mop-shape-layout-slot-indexes-consistent"
  (assert-mop-red-ok
   "(progn
      (defclass mop-layout-stable () ((a :initarg :a) (b :initarg :b)))
      (let* ((o1 (make-instance 'mop-layout-stable :a 1 :b 2))
             (o2 (make-instance 'mop-layout-stable :a 3 :b 4))
             (slots1 (class-slots (class-of o1)))
             (slots2 (class-slots (class-of o2)))
             (locs1 (mapcar #'slot-definition-location slots1))
             (locs2 (mapcar #'slot-definition-location slots2)))
        (if (and (equal locs1 locs2)
                 (= (length locs1) (length (remove-duplicates locs1))))
            :ok
            :bad)))"))

;;; ─── Standard-instance abstraction ───────────────────────────────────────

(it-sequential "mop-standard-instance-slot-operations-through-abstraction"
  (assert-mop-red-ok
   "(progn
      (defclass mop-standard-instance () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-standard-instance :x 1)))
        (setf (slot-value obj 'x) 2)
        (let ((read (= (slot-value obj 'x) 2))
              (bound-before (slot-boundp obj 'x)))
          (slot-makunbound obj 'x)
          (if (and read bound-before (not (slot-boundp obj 'x))) :ok :bad))))"))

(it-sequential "mop-standard-instance-no-direct-hash-table-assumptions"
  (assert-mop-red-ok
   "(progn
      (defclass mop-not-raw-hash () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-not-raw-hash :x 1)))
        (if (and (not (hash-table-p obj)) (= (slot-value obj 'x) 1)) :ok :bad)))"))

(it-sequential "mop-standard-instance-vector-backed-instance-equality"
  (assert-mop-red-ok
   "(progn
      (defclass mop-vector-identity () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-vector-identity :x 1)))
        (let ((same obj))
          (setf (slot-value obj 'x) 2)
          (if (and (eq obj same) (= (slot-value same 'x) 2)) :ok :bad))))"))

;;; ─── Sealed classes ──────────────────────────────────────────────────────

(it-sequential "mop-sealed-classes-subclass-rejection"
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-root () ())
      (handler-case
          (progn (defclass mop-sealed-child (mop-sealed-root) ()) :bad)
        (error () :ok)))"))

(it-sequential "mop-sealed-classes-define-sealed-type-macro"
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-leaf () ((x :initarg :x)))
      (let ((obj (make-instance 'mop-sealed-leaf :x 5)))
        (if (and (= (slot-value obj 'x) 5)
                 (sealed-class-p (class-of obj)))
            :ok
            :bad)))"))

(it-sequential "mop-sealed-classes-safe-static-devirtualization"
  (assert-mop-red-ok
   "(progn
      (define-sealed-type mop-sealed-point () ((x :initarg :x)))
      (defgeneric mop-sealed-value (x))
      (defmethod mop-sealed-value ((x mop-sealed-point)) (slot-value x 'x))
      (let ((obj (make-instance 'mop-sealed-point :x 42)))
        (if (= (mop-sealed-value obj) 42) :ok :bad)))"))

(it-sequential "mop-sealed-classes-dynamic-fallback"
  (assert-mop-red-ok
   "(progn
      (defclass mop-open-base () ())
      (defclass mop-open-child (mop-open-base) ())
      (defgeneric mop-open-dispatch (x))
      (defmethod mop-open-dispatch ((x mop-open-base)) :base)
      (defmethod mop-open-dispatch ((x mop-open-child)) :child)
      (if (eq (mop-open-dispatch (make-instance 'mop-open-child)) :child) :ok :bad))"))

;;; ─── Satiated generic functions ──────────────────────────────────────────

(it-sequential "mop-satiated-gfs-dispatch-after-satiation-same-method"
  :timeout
  5
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-satiated (x))
      (defmethod mop-satiated ((x integer)) :integer)
      (if (eq (mop-satiated 1) :integer) :ok :bad))"))

(it-sequential "mop-satiated-gfs-method-addition-after-satiation-behavior"
  :timeout
  5
  (assert-mop-red-ok
   "(progn
      (defgeneric mop-satiated-add (x))
      (defmethod mop-satiated-add ((x t)) :fallback)
      (defmethod mop-satiated-add ((x integer)) :integer)
      (if (eq (mop-satiated-add 1) :integer) :ok :bad))"))

(it-sequential "mop-satiated-gfs-predicate"
  :timeout
  5
  (assert-mop-red-ok
   "(if (fboundp 'satiating-gfs-p) :ok :bad)"))

;;; ─── MOP functions directly accessible ──────────────────────────────────

(it-sequential "mop-compute-effective-slot-definition-accessible"
  (assert-mop-red-ok
   "(progn
      (multiple-value-bind (sym status)
          (find-symbol \"COMPUTE-EFFECTIVE-SLOT-DEFINITION\" (find-package \"CL-CC\"))
        (if (and sym (eq status :external)) :ok :bad))))"))

(it-sequential "mop-class-slots-accessible"
  (assert-mop-red-ok
   "(progn
      (multiple-value-bind (sym status)
          (find-symbol \"CLASS-SLOTS\" (find-package \"CL-CC\"))
        (if (and sym (eq status :external)) :ok :bad))))"))

(it-sequential "mop-satiating-gfs-p-accessible"
  (assert-mop-red-ok
   "(if (fboundp 'satiating-gfs-p) :ok :bad)"))

;;; ─── copy-instance ───────────────────────────────────────────────────────

(it-sequential "mop-copy-instance-distinct-object"
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-distinct () ((x :initarg :x)))
      (let* ((obj (make-instance 'mop-copy-distinct :x 1))
             (copy (copy-instance obj)))
        (if (not (eq obj copy)) :ok :bad)))"))

(it-sequential "mop-copy-instance-same-class"
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-class () ((x :initarg :x)))
      (let* ((obj (make-instance 'mop-copy-class :x 1))
             (copy (copy-instance obj)))
        (if (eq (class-of obj) (class-of copy)) :ok :bad)))"))

(it-sequential "mop-copy-instance-shallow-slot-copy"
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-shallow () ((items :initarg :items)))
      (let* ((items (list 1 2 3))
             (obj (make-instance 'mop-copy-shallow :items items))
             (copy (copy-instance obj)))
        (if (eq (slot-value copy 'items) items) :ok :bad)))"))

(it-sequential "mop-copy-instance-compatible-with-vector-layout"
  (assert-mop-red-ok
   "(progn
      (defclass mop-copy-vector () ((x :initarg :x) (y :initarg :y)))
      (let* ((obj (make-instance 'mop-copy-vector :x 10 :y 20))
             (copy (copy-instance obj)))
        (if (and (not (hash-table-p copy))
                 (= (slot-value copy 'x) 10)
                 (= (slot-value copy 'y) 20))
            :ok
            :bad)))"))

;;; Apply a 10-second timeout to all mop-* tests AFTER they are all registered.
;;; TDD RED-phase tests may call undefined VM functions that can hang the VM
;;; evaluator; the timeout ensures they fail cleanly instead of blocking workers.
(set-test-timeouts-by-prefix! "MOP-" 10)
