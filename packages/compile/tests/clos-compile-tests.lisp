;;;; clos-compile-tests.lisp — CLOS compilation, inheritance, generic functions, setf slot-value
(in-package :cl-cc/test)

(it-sequential "clos-compile-slot-access slot-x"
  (destructuring-bind (expected form) (list 10 "(defclass point () ((x :initarg :x) (y :initarg :y)))
     (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'x))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-compile-slot-access slot-y"
  (destructuring-bind (expected form) (list 20 "(defclass point () ((x :initarg :x) (y :initarg :y)))
     (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'y))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-compile-slot-access slot-sum"
  (destructuring-bind (expected form) (list 8 "(defclass rect () ((w :initarg :w) (h :initarg :h)))
     (let ((r (make-instance 'rect :w 5 :h 3))) (+ (slot-value r 'w) (slot-value r 'h)))")
    (expect (= expected (run-string form)) :to-be-truthy)))


(it-sequential "clos-compile-reader-methods first-field"
  (destructuring-bind (expected form) (list 3 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dx v))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-compile-reader-methods second-field"
  (destructuring-bind (expected form) (list 4 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dy v))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-compile-generic-methods constant-method"
  (destructuring-bind (expected source) (list 42 "(defclass animal () ((name :initarg :name)))
            (defgeneric speak (a))
            (defmethod speak ((a animal)) 42)
            (let ((a (make-instance 'animal :name 'dog))) (speak a))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "clos-compile-generic-methods slot-sum-method"
  (destructuring-bind (expected source) (list 20 "(defclass pair () ((a :initarg :a) (b :initarg :b)))
            (defgeneric pair-sum (p))
            (defmethod pair-sum ((p pair)) (+ (slot-value p 'a) (slot-value p 'b)))
            (let ((p (make-instance 'pair :a 7 :b 13))) (pair-sum p))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "clos-compile-generic-methods formula-method"
  (destructuring-bind (expected source) (list 15 "(defclass rect () ((w :initarg :w) (h :initarg :h)))
            (defgeneric area (shape))
            (defmethod area ((r rect)) (* (slot-value r 'w) (slot-value r 'h)))
            (let ((r (make-instance 'rect :w 3 :h 5))) (area r))")
    (expect (= expected (run-string source)) :to-be-truthy)))

(it-sequential "clos-compile-instance-variations multi-instance"
  (destructuring-bind (expected form) (list 30 "(defclass counter () ((val :initarg :val)))
     (let ((c1 (make-instance 'counter :val 10))
           (c2 (make-instance 'counter :val 20)))
       (+ (slot-value c1 'val) (slot-value c2 'val)))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-compile-instance-variations uninitialized-nil"
  (destructuring-bind (expected form) (list nil "(defclass box () ((content :initarg :content)))
     (let ((b (make-instance 'box))) (slot-value b 'content))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-compile-instance-variations conditional-slot"
  (destructuring-bind (expected form) (list 1 "(defclass flag () ((active :initarg :active)))
     (let ((f (make-instance 'flag :active 1))) (if (slot-value f 'active) 1 0))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-compile-instance-variations three-slots"
  (destructuring-bind (expected form) (list 60 "(defclass color () ((r :initarg :r) (g :initarg :g) (b :initarg :b)))
     (let ((c (make-instance 'color :r 10 :g 20 :b 30)))
       (+ (slot-value c 'r) (+ (slot-value c 'g) (slot-value c 'b))))")
    (expect (run-string form) :to-equal expected)))

;;; Slot Specification Parsing Tests

(it-sequential "clos-parse-slot-spec-bare"
  (let ((slot (parse-slot-spec 'x)))
    (expect (typep slot 'ast-slot-def) :to-be-truthy)
    (expect (ast-slot-name slot) :to-be 'x)
    (expect (ast-slot-initarg slot) :to-be-null)
    (expect (ast-slot-reader slot) :to-be-null)))

(it-sequential "clos-parse-slot-spec-full"
  (let ((slot (parse-slot-spec '(x :initarg :x :reader get-x :writer set-x :accessor x-accessor))))
    (expect (ast-slot-name slot) :to-be 'x)
    (expect (ast-slot-initarg slot) :to-be :x)
    (expect (ast-slot-reader slot) :to-be 'get-x)
    (expect (ast-slot-writer slot) :to-be 'set-x)
    (expect (ast-slot-accessor slot) :to-be 'x-accessor)))

(it-sequential "clos-parse-slot-spec-to-sexp"
  (let* ((slot (parse-slot-spec '(x :initarg :x :reader get-x)))
         (sexp (slot-def-to-sexp slot)))
    (expect (first sexp) :to-be 'x)
    (expect (member :initarg sexp) :to-be-truthy)
    (expect (member :reader sexp) :to-be-truthy)))

;;; CLOS Inheritance Tests

(it-sequential "clos-inherit-slot-access inherited-slot"
  (destructuring-bind (expected accessor-expr) (list 10 "(slot-value c 'x)")
    (expect (= expected (run-string
             (concatenate 'string
              "(defclass base () ((x :initarg :x)))
               (defclass child (base) ((y :initarg :y)))
               (let ((c (make-instance 'child :x 10 :y 20)))
                 " accessor-expr ")"))) :to-be-truthy)))

(it-sequential "clos-inherit-slot-access own-slot"
  (destructuring-bind (expected accessor-expr) (list 20 "(slot-value c 'y)")
    (expect (= expected (run-string
             (concatenate 'string
              "(defclass base () ((x :initarg :x)))
               (defclass child (base) ((y :initarg :y)))
               (let ((c (make-instance 'child :x 10 :y 20)))
                 " accessor-expr ")"))) :to-be-truthy)))

(it-sequential "clos-inherit-slot-access slot-arithmetic"
  (destructuring-bind (expected accessor-expr) (list 30 "(+ (slot-value c 'x) (slot-value c 'y))")
    (expect (= expected (run-string
             (concatenate 'string
              "(defclass base () ((x :initarg :x)))
               (defclass child (base) ((y :initarg :y)))
               (let ((c (make-instance 'child :x 10 :y 20)))
                 " accessor-expr ")"))) :to-be-truthy)))

;;; Inheritance + Generic Function Dispatch Tests

(it-sequential "clos-inherit-and-gf-numeric inherit-method-from-superclass"
  (destructuring-bind (expected form) (list 4 "(defclass animal () ((legs :initarg :legs)))
     (defgeneric leg-count (a))
     (defmethod leg-count ((a animal)) (slot-value a 'legs))
     (defclass dog (animal) ((breed :initarg :breed)))
     (let ((d (make-instance 'dog :legs 4 :breed 'lab))) (leg-count d))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric inherit-method-override"
  (destructuring-bind (expected form) (list 99 "(defclass shape () ((n :initarg :n)))
     (defgeneric info (s))
     (defmethod info ((s shape)) (slot-value s 'n))
     (defclass circle (shape) ((r :initarg :r)))
     (defmethod info ((s circle)) 99)
     (let ((c (make-instance 'circle :n 1 :r 5))) (info c))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric allow-other-keys-bypasses-check"
  (destructuring-bind (expected form) (list 10 "(defclass foo () ((x :initarg :x)))
     (let ((obj (make-instance 'foo :x 10 :y 1 :allow-other-keys t)))
       (slot-value obj 'x))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric gf-method-count"
  (destructuring-bind (expected form) (list 2 "(defgeneric describe-it (x))
     (defmethod describe-it ((x integer)) x)
     (defmethod describe-it ((x string)) x)
     (length (generic-function-methods #'describe-it))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric superclass-method-still-works"
  (destructuring-bind (expected form) (list 7 "(defclass shape2 () ((n :initarg :n)))
     (defgeneric info2 (s))
     (defmethod info2 ((s shape2)) (slot-value s 'n))
     (defclass circle2 (shape2) ((r :initarg :r)))
     (defmethod info2 ((s circle2)) 99)
     (let ((s (make-instance 'shape2 :n 7))) (info2 s))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric inherit-two-levels"
  (destructuring-bind (expected form) (list 100 "(defclass ga () ((x :initarg :x)))
     (defclass gb (ga) ((y :initarg :y)))
     (defclass gc (gb) ((z :initarg :z)))
     (let ((obj (make-instance 'gc :x 100 :y 200 :z 300))) (slot-value obj 'x))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric inherit-method-two-levels"
  (destructuring-bind (expected form) (list 600 "(defclass ha () ((x :initarg :x)))
     (defgeneric get-x (obj))
     (defmethod get-x ((obj ha)) (slot-value obj 'x))
     (defclass hb (ha) ((y :initarg :y)))
     (defclass hc (hb) ((z :initarg :z)))
     (let ((obj (make-instance 'hc :x 600 :y 0 :z 0))) (get-x obj))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-inherit-and-gf-numeric inherit-initargs-from-super"
  (destructuring-bind (expected form) (list 5 "(defclass base2 () ((val :initarg :val)))
     (defclass ext2 (base2) ((extra :initarg :extra)))
     (let ((e (make-instance 'ext2 :val 5 :extra 10))) (slot-value e 'val))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-make-instance-invalid-initarg-signals-error"
  (let ((%%signaled1 nil)) (handler-case (progn (run-string "(defclass foo () ((x :initarg :x)))
                 (make-instance 'foo :y 1)")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "clos-generic-function-method-combination-defaults-to-standard"
  (expect (run-string "(defgeneric describe-combo (x))
                           (generic-function-method-combination #'describe-combo)") :to-be 'standard))

(defun %clos-vm-instruction-types (source)
  "Return the VM instruction type names SOURCE compiles to.

Compiles for :VM explicitly rather than going through ASSERT-COMPILES-TO, which
uses the default :X86_64 target — its assembly backend raises on instructions it
has no emitter for, and the helper's IGNORE-ERRORS then reports every instruction
as absent."
  (mapcar #'type-of
          (cl-cc/vm::vm-program-instructions
           (cl-cc/compile:compilation-result-program
            (cl-cc/compile:compile-string source :target :vm)))))

(it-sequential "clos-custom-metaclass-instance-is-not-scalarized"
  (let ((types (%clos-vm-instruction-types
                "(defclass scalarize-meta () ())
                 (defclass scalarize-obj () ((x :initarg :x))
                   (:metaclass scalarize-meta))
                 (let ((o (make-instance 'scalarize-obj :x 1))) (slot-value o 'x))")))
    (expect (member 'cl-cc/vm:vm-make-obj types :test #'eq) :to-be-truthy)
    (expect (member 'cl-cc/vm:vm-slot-read types :test #'eq) :to-be-truthy))
  (let ((types (%clos-vm-instruction-types
                "(defclass scalarize-plain () ((x :initarg :x)))
                 (let ((o (make-instance 'scalarize-plain :x 1))) (slot-value o 'x))")))
    (expect (member 'cl-cc/vm:vm-slot-read types :test #'eq) :to-be-falsy)))

(it-sequential "clos-custom-metaclass-overrides class-of-instance-returns-metaclass"
  (destructuring-bind (expected form) (list "META-A" "(defclass meta-a () ())
     (defclass object-a () () (:metaclass meta-a))
     (symbol-name (class-name (class-of (make-instance 'object-a))))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-custom-metaclass-overrides slot-value-using-class-before-method-runs"
  (destructuring-bind (expected form) (list '(42 1) "(defclass meta-b () ())
     (defclass object-b () ((x :initarg :x)) (:metaclass meta-b))
     (defvar *slot-hook-count* 0)
     (defmethod slot-value-using-class :before ((class meta-b) object slot-name)
       (declare (ignore class object slot-name))
       (setq *slot-hook-count* (+ *slot-hook-count* 1)))
     (let* ((obj (make-instance 'object-b :x 42))
            (value (slot-value obj 'x)))
       (list value *slot-hook-count*))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-custom-metaclass-overrides initialize-instance-after-method-runs"
  (destructuring-bind (expected form) (list '(1 9) "(defclass meta-c () ())
     (defclass object-c () ((x :initarg :x)) (:metaclass meta-c))
     (defvar *init-hook-count* 0)
     (defmethod initialize-instance :after ((object meta-c))
       (setq *init-hook-count* (+ *init-hook-count* 1)))
     (let ((obj (make-instance 'object-c :x 9)))
       (list *init-hook-count* (gethash 'x obj)))")
    (expect (run-string form) :to-equal expected)))

;;; Setf Slot-Value Tests

(it-sequential "clos-setf-slot-value-mutations basic-set"
  (destructuring-bind (expected form) (list 99 "(defclass box () ((content :initarg :content)))
            (let ((b (make-instance 'box :content 0)))
              (setf (slot-value b 'content) 99)
              (slot-value b 'content))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-setf-slot-value-mutations returns-new-value"
  (destructuring-bind (expected form) (list 42 "(defclass box2 () ((content :initarg :content)))
            (let ((b (make-instance 'box2 :content 0)))
              (setf (slot-value b 'content) 42))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-setf-slot-value-mutations multiple-mutations"
  (destructuring-bind (expected form) (list 30 "(defclass counter () ((val :initarg :val)))
            (let ((c (make-instance 'counter :val 10)))
              (setf (slot-value c 'val) 20)
              (setf (slot-value c 'val) 30)
              (slot-value c 'val))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-setf-slot-value-mutations computed-value"
  (destructuring-bind (expected form) (list 15 "(defclass pair () ((a :initarg :a) (b :initarg :b)))
            (let ((p (make-instance 'pair :a 5 :b 10)))
              (setf (slot-value p 'a)
                    (+ (slot-value p 'a) (slot-value p 'b)))
              (slot-value p 'a))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-builtin-class-precedence-dispatch number-matches-integer"
  (destructuring-bind (expected source) (list "NUM" "(defmethod cpl-a ((x number)) 'num) (symbol-name (cpl-a 42))")
    (expect (run-string source) :to-equal expected)))

(it-sequential "clos-builtin-class-precedence-dispatch real-matches-integer"
  (destructuring-bind (expected source) (list "REAL" "(defmethod cpl-b ((x real)) 'real) (symbol-name (cpl-b 42))")
    (expect (run-string source) :to-equal expected)))

(it-sequential "clos-builtin-class-precedence-dispatch number-matches-float"
  (destructuring-bind (expected source) (list "NUM" "(defmethod cpl-c ((x number)) 'num) (symbol-name (cpl-c 1.5))")
    (expect (run-string source) :to-equal expected)))

(it-sequential "clos-builtin-class-precedence-dispatch float-is-named"
  (destructuring-bind (expected source) (list "FLT" "(defmethod cpl-d ((x float)) 'flt) (symbol-name (cpl-d 1.5))")
    (expect (run-string source) :to-equal expected)))

(it-sequential "clos-builtin-class-precedence-dispatch list-matches-cons"
  (destructuring-bind (expected source) (list "LST" "(defmethod cpl-e ((x list)) 'lst) (symbol-name (cpl-e (cons 1 2)))")
    (expect (run-string source) :to-equal expected)))

(it-sequential "clos-builtin-class-precedence-dispatch exact-class-still-wins-over-ancestor"
  (destructuring-bind (expected source) (list "INT" "(defmethod cpl-f ((x number)) 'num)
                   (defmethod cpl-f ((x integer)) 'int)
                   (symbol-name (cpl-f 42))")
    (expect (run-string source) :to-equal expected)))
