;;;; tests/unit/vm/vm-clos-execute-tests.lisp
;;;; Coverage for src/vm/vm-clos-execute.lisp:
;;;;   execute-instruction for vm-class-def, vm-make-obj,
;;;;   vm-slot-read, vm-slot-write, vm-slot-boundp,
;;;;   vm-slot-makunbound, vm-slot-exists-p.

(in-package :cl-cc/test)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-clos-vm ()
  "Fresh vm-state for CLOS instruction tests."
  (make-instance 'cl-cc/vm:vm-io-state))
(defun class-def-inst (dst name &key (supers '()) (slots '()) (initargs '()) (class-slots '()))
  "Build a vm-class-def instruction for simple class registration."
  (cl-cc:make-vm-class-def
   :dst dst
   :class-name name
   :superclasses supers
   :slot-names slots
   :slot-initargs initargs
   :slot-initform-regs nil
   :default-initarg-regs nil
   :class-slots class-slots))

(defun exec-class-def (s dst name &key (supers '()) (slots '()) (initargs '()) (class-slots '()))
  "Execute a vm-class-def and return the new PC."
  (values (cl-cc/vm:execute-instruction
           (class-def-inst dst name :supers supers :slots slots :initargs initargs :class-slots class-slots)
           s 0 nil)))
(defun make-cdef-for-test (name &key (supers '()) (default-initarg-regs nil) (class-slots nil))
  "Build a vm-class-def with empty slot/initarg lists and explicit defaults for testing."
  (cl-cc:make-vm-class-def
   :dst :R0 :class-name name :superclasses supers
   :slot-names '() :slot-initargs '() :slot-initform-regs nil
   :default-initarg-regs default-initarg-regs :class-slots class-slots))

(defun test-instance-class (instance)
  "Return INSTANCE's class descriptor for either supported instance layout."
  (cond
    ((hash-table-p instance) (gethash :__class__ instance))
    ((and (vectorp instance) (> (length instance) 0)) (aref instance 0))))

(defun test-slot-index (class slot-name)
  "Return SLOT-NAME's vector index in CLASS."
  (let ((location (cdr (assoc slot-name (gethash :__slot-locations__ class) :test #'eq))))
    (and location (1+ location))))

(defun test-instance-slot-value (instance slot-name)
  "Return SLOT-NAME from INSTANCE using the active raw layout."
  (let ((class (test-instance-class instance)))
    (if (hash-table-p instance)
        (gethash slot-name instance)
        (cl-cc/vm::slot-value-by-index instance (test-slot-index class slot-name)))))

(defun test-instance-slot-bound-p (instance slot-name)
  "Return whether SLOT-NAME is bound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class class-slots) (cl-cc/vm::%vm-class-slots-of instance)
    (cl-cc/vm::%vm-raw-slot-boundp class class-slots instance slot-name)))

(defun test-instance-makunbound (instance slot-name)
  "Make SLOT-NAME unbound in INSTANCE using VM raw slot logic."
  (multiple-value-bind (class class-slots) (cl-cc/vm::%vm-class-slots-of instance)
    (cl-cc/vm::%vm-raw-slot-makunbound class class-slots instance slot-name)))

;;; ─── vm-class-def / vm-make-obj ──────────────────────────────────────────

;;; ─── vm-slot-read / vm-slot-write / vm-slot-boundp / vm-slot-makunbound ─

(defun make-test-instance (s class-reg obj-reg class-name slots)
  "Register CLASS-NAME with SLOTS and create an instance, storing in OBJ-REG."
  (exec-class-def s class-reg class-name :slots slots)
  (cl-cc/vm:execute-instruction
   (cl-cc:make-vm-make-obj :dst obj-reg :class-reg class-reg :initarg-regs nil)
   s 0 nil))

;;; ─── vm-class-def helper functions ──────────────────────────────────────

(defun %make-reg-with-base (slots &key (initargs nil))
  "Build a class registry HT containing a single 'base class with given SLOTS."
  (let ((reg (make-hash-table :test #'eq))
        (base-ht (make-hash-table :test #'eq)))
    (setf (gethash :__slots__ base-ht) slots
          (gethash :__superclasses__ base-ht) nil)
    (when initargs (setf (gethash :__initargs__ base-ht) initargs))
    (setf (gethash 'base reg) base-ht)
    reg))

;;; ─── copy-instance layout coverage ───────────────────────────────────────

(it-sequential "vm-copy-instance-vector-backed-standard-instance"
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-vector () ((items :initarg :items) (flag :initarg :flag)))
      (let* ((items (list 1 2 3))
             (obj (make-instance 'vm-copy-vector :items items :flag :original))
             (copy (copy-instance obj)))
        (setf (slot-value copy 'flag) :changed)
        (if (and (vectorp obj)
                 (vectorp copy)
                 (not (eq obj copy))
                 (eq (class-of obj) (class-of copy))
                 (eq (slot-value copy 'items) items)
                 (eq (slot-value obj 'flag) :original)
                 (eq (slot-value copy 'flag) :changed))
            :ok
            :bad)))"
   :ok
   :stdlib t))

(it-sequential "vm-copy-instance-vector-backed-preserves-unbound-slot"
  (assert-evaluates-to
   "(progn
      (defclass vm-copy-vector-unbound () ((x :initarg :x) (y :initarg :y)))
      (let* ((obj (make-instance 'vm-copy-vector-unbound :x 10 :y 20)))
        (slot-makunbound obj 'y)
        (let ((copy (copy-instance obj)))
          (if (and (slot-boundp copy 'x)
                   (= (slot-value copy 'x) 10)
                   (not (slot-boundp copy 'y)))
              :ok
              :bad))))"
   :ok
   :stdlib t))

(it-sequential "vm-copy-instance-hash-table-backed-standard-instance"
  :timeout
  10
  (handler-case
      (sb-ext:with-timeout 8
        (assert-evaluates-to
         "(progn
      (defclass vm-copy-hash-meta (standard-class) ())
      (defclass vm-copy-hash ()
        ((items :initarg :items) (flag :initarg :flag))
        (:metaclass vm-copy-hash-meta))
      (let* ((items (list :a :b))
             (obj (make-instance 'vm-copy-hash :items items :flag :original))
             (copy (copy-instance obj)))
        (setf (slot-value copy 'flag) :changed)
        (if (and (hash-table-p obj)
                 (hash-table-p copy)
                 (not (eq obj copy))
                 (eq (gethash :__class__ obj) (gethash :__class__ copy))
                 (eq (slot-value copy 'items) items)
                 (eq (slot-value obj 'flag) :original)
                 (eq (slot-value copy 'flag) :changed))
            :ok
            :bad)))"
         :ok
         :stdlib t))
    (sb-ext:timeout ()
      (%fail-test "vm-copy-instance-hash-table-backed-standard-instance: 8s timeout (custom metaclass may not be implemented)"
                  :expected :ok :actual :timeout
                  :form 'vm-copy-instance-hash-table-backed-standard-instance))))

(it-sequential "vm-copy-instance-hash-table-backed-preserves-unbound-slot"
  :timeout
  10
  (handler-case
      (sb-ext:with-timeout 8
        (assert-evaluates-to
         "(progn
      (defclass vm-copy-hash-unbound-meta (standard-class) ())
      (defclass vm-copy-hash-unbound ()
        ((x :initarg :x) (y :initarg :y))
        (:metaclass vm-copy-hash-unbound-meta))
      (let ((obj (make-instance 'vm-copy-hash-unbound :x 10 :y 20)))
        (slot-makunbound obj 'y)
        (let ((copy (copy-instance obj)))
          (if (and (slot-boundp copy 'x)
                   (= (slot-value copy 'x) 10)
                   (not (slot-boundp copy 'y)))
              :ok
              :bad))))"
         :ok
         :stdlib t))
    (sb-ext:timeout ()
      (%fail-test "vm-copy-instance-hash-table-backed-preserves-unbound-slot: 8s timeout (custom metaclass may not be implemented)"
                  :expected :ok :actual :timeout
                  :form 'vm-copy-instance-hash-table-backed-preserves-unbound-slot))))

;;; ─── vm-slot-exists-p / vm-class-name-fn / vm-class-of-fn / vm-find-class

;;; ─── vm-generic-call inline cache ─────────────────────────────────────────

(it-sequential "stdlib-slot-exists-p-funcall-present-and-missing"
  (assert-evaluates-to
   "(progn
      (defclass vm-slot-exists-wrapper () ((present :initarg :present)))
      (let ((instance (make-instance (quote vm-slot-exists-wrapper) :present 1)))
        (list (funcall (function slot-exists-p) instance (quote present))
              (funcall (function slot-exists-p) instance (quote missing)))))"
   (quote (t nil))
   :stdlib t))

(it-sequential "stdlib-make-instances-obsolete-symbol-and-class-return-class"
  (assert-evaluates-to
   "(progn
      (defclass vm-obsolete-return () ())
      (let ((class (find-class (quote vm-obsolete-return))))
        (list (eq (make-instances-obsolete (quote vm-obsolete-return)) class)
              (eq (make-instances-obsolete class) class))))"
   (quote (t t))
   :stdlib t))
