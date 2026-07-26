;;;; clos-compile-tests.lisp — CLOS compilation, inheritance, generic functions, setf slot-value
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(deftest-each clos-compile-slot-access
  "Slot access compiles and evaluates correctly."
  :cases
  (("slot-x"  10
    "(defclass point () ((x :initarg :x) (y :initarg :y)))
     (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'x))")
   ("slot-y"  20
    "(defclass point () ((x :initarg :x) (y :initarg :y)))
     (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'y))")
   ("slot-sum" 8
    "(defclass rect () ((w :initarg :w) (h :initarg :h)))
     (let ((r (make-instance 'rect :w 5 :h 3))) (+ (slot-value r 'w) (slot-value r 'h)))"))
  (expected form)
  (assert-= expected (run-string form)))


(deftest-each clos-compile-reader-methods
  "Reader accessor methods work on first and second class slots."
  :cases (("first-field"  3 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dx v))")
          ("second-field" 4 "(defclass vec ()
               ((dx :initarg :dx :reader vec-dx)
                (dy :initarg :dy :reader vec-dy)))
             (let ((v (make-instance 'vec :dx 3 :dy 4)))
               (vec-dy v))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each clos-compile-generic-methods
  "Generic function dispatch, slot-accessing methods, and formula methods compile correctly."
  :cases (("constant-method"
           42
           "(defclass animal () ((name :initarg :name)))
            (defgeneric speak (a))
            (defmethod speak ((a animal)) 42)
            (let ((a (make-instance 'animal :name 'dog))) (speak a))")
          ("slot-sum-method"
           20
           "(defclass pair () ((a :initarg :a) (b :initarg :b)))
            (defgeneric pair-sum (p))
            (defmethod pair-sum ((p pair)) (+ (slot-value p 'a) (slot-value p 'b)))
            (let ((p (make-instance 'pair :a 7 :b 13))) (pair-sum p))")
          ("formula-method"
           15
           "(defclass rect () ((w :initarg :w) (h :initarg :h)))
            (defgeneric area (shape))
            (defmethod area ((r rect)) (* (slot-value r 'w) (slot-value r 'h)))
            (let ((r (make-instance 'rect :w 3 :h 5))) (area r))"))
  (expected source)
  (assert-= expected (run-string source)))

(deftest-each clos-compile-instance-variations
  "Instance creation edge cases: multi-instance, uninitialized slot, conditional, three slots."
  :cases
  (("multi-instance"    30
    "(defclass counter () ((val :initarg :val)))
     (let ((c1 (make-instance 'counter :val 10))
           (c2 (make-instance 'counter :val 20)))
       (+ (slot-value c1 'val) (slot-value c2 'val)))")
   ("uninitialized-nil" nil
    "(defclass box () ((content :initarg :content)))
     (let ((b (make-instance 'box))) (slot-value b 'content))")
   ("conditional-slot"   1
    "(defclass flag () ((active :initarg :active)))
     (let ((f (make-instance 'flag :active 1))) (if (slot-value f 'active) 1 0))")
   ("three-slots"       60
    "(defclass color () ((r :initarg :r) (g :initarg :g) (b :initarg :b)))
     (let ((c (make-instance 'color :r 10 :g 20 :b 30)))
       (+ (slot-value c 'r) (+ (slot-value c 'g) (slot-value c 'b))))"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Slot Specification Parsing Tests

(deftest clos-parse-slot-spec-bare
  "parse-slot-spec: a bare symbol produces a minimal slot-def with nil options."
  (let ((slot (parse-slot-spec 'x)))
    (assert-type ast-slot-def slot)
    (assert-eq 'x (ast-slot-name slot))
    (assert-null (ast-slot-initarg slot))
    (assert-null (ast-slot-reader slot))))

(deftest clos-parse-slot-spec-full
  "parse-slot-spec: a full spec with :initarg/:reader/:writer/:accessor is preserved."
  (let ((slot (parse-slot-spec '(x :initarg :x :reader get-x :writer set-x :accessor x-accessor))))
    (assert-eq 'x (ast-slot-name slot))
    (assert-eq :x (ast-slot-initarg slot))
    (assert-eq 'get-x (ast-slot-reader slot))
    (assert-eq 'set-x (ast-slot-writer slot))
    (assert-eq 'x-accessor (ast-slot-accessor slot))))

(deftest clos-parse-slot-spec-to-sexp
  "slot-def-to-sexp preserves name, :initarg, and :reader in the output sexp."
  (let* ((slot (parse-slot-spec '(x :initarg :x :reader get-x)))
         (sexp (slot-def-to-sexp slot)))
    (assert-eq 'x (first sexp))
    (assert-true (member :initarg sexp))
    (assert-true (member :reader sexp))))

;;; CLOS Inheritance Tests

(deftest-each clos-inherit-slot-access
  "Subclass inherits and provides access to both inherited and own slots."
  :cases (("inherited-slot"  10 "(slot-value c 'x)")
          ("own-slot"        20 "(slot-value c 'y)")
          ("slot-arithmetic" 30 "(+ (slot-value c 'x) (slot-value c 'y))"))
  (expected accessor-expr)
  (assert-= expected
            (run-string
             (concatenate 'string
              "(defclass base () ((x :initarg :x)))
               (defclass child (base) ((y :initarg :y)))
               (let ((c (make-instance 'child :x 10 :y 20)))
                 " accessor-expr ")"))))

;;; Inheritance + Generic Function Dispatch Tests

(deftest-each clos-inherit-and-gf-numeric
  "Inheritance and generic function dispatch all produce the expected numeric result."
  :cases
  (("inherit-method-from-superclass"   4
    "(defclass animal () ((legs :initarg :legs)))
     (defgeneric leg-count (a))
     (defmethod leg-count ((a animal)) (slot-value a 'legs))
     (defclass dog (animal) ((breed :initarg :breed)))
     (let ((d (make-instance 'dog :legs 4 :breed 'lab))) (leg-count d))")
   ("inherit-method-override"         99
    "(defclass shape () ((n :initarg :n)))
     (defgeneric info (s))
     (defmethod info ((s shape)) (slot-value s 'n))
     (defclass circle (shape) ((r :initarg :r)))
     (defmethod info ((s circle)) 99)
     (let ((c (make-instance 'circle :n 1 :r 5))) (info c))")
   ("allow-other-keys-bypasses-check" 10
    "(defclass foo () ((x :initarg :x)))
     (let ((obj (make-instance 'foo :x 10 :y 1 :allow-other-keys t)))
       (slot-value obj 'x))")
   ("gf-method-count"                  2
    "(defgeneric describe-it (x))
     (defmethod describe-it ((x integer)) x)
     (defmethod describe-it ((x string)) x)
     (length (generic-function-methods #'describe-it))")
   ("superclass-method-still-works"    7
    "(defclass shape2 () ((n :initarg :n)))
     (defgeneric info2 (s))
     (defmethod info2 ((s shape2)) (slot-value s 'n))
     (defclass circle2 (shape2) ((r :initarg :r)))
     (defmethod info2 ((s circle2)) 99)
     (let ((s (make-instance 'shape2 :n 7))) (info2 s))")
   ("inherit-two-levels"             100
    "(defclass ga () ((x :initarg :x)))
     (defclass gb (ga) ((y :initarg :y)))
     (defclass gc (gb) ((z :initarg :z)))
     (let ((obj (make-instance 'gc :x 100 :y 200 :z 300))) (slot-value obj 'x))")
   ("inherit-method-two-levels"      600
    "(defclass ha () ((x :initarg :x)))
     (defgeneric get-x (obj))
     (defmethod get-x ((obj ha)) (slot-value obj 'x))
     (defclass hb (ha) ((y :initarg :y)))
     (defclass hc (hb) ((z :initarg :z)))
     (let ((obj (make-instance 'hc :x 600 :y 0 :z 0))) (get-x obj))")
   ("inherit-initargs-from-super"      5
    "(defclass base2 () ((val :initarg :val)))
     (defclass ext2 (base2) ((extra :initarg :extra)))
     (let ((e (make-instance 'ext2 :val 5 :extra 10))) (slot-value e 'val))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest clos-make-instance-invalid-initarg-signals-error
  "make-instance rejects unknown initargs unless :allow-other-keys is true."
  (assert-signals error
    (run-string "(defclass foo () ((x :initarg :x)))
                 (make-instance 'foo :y 1)")))

(deftest clos-generic-function-method-combination-defaults-to-standard
  "generic-function-method-combination reports STANDARD when no custom combination is set."
  (assert-eq 'standard
             (run-string "(defgeneric describe-combo (x))
                           (generic-function-method-combination #'describe-combo)")))

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

(deftest clos-custom-metaclass-instance-is-not-scalarized
  "An instance of a class with a custom metaclass keeps its allocation and slot reads.

A non-escaping LET-bound instance is normally scalarized into registers, which
replaces VM-MAKE-OBJ and VM-SLOT-READ with plain moves. That is sound for a
standard metaclass — there are no protocol hooks to run — but a custom metaclass
can carry INITIALIZE-INSTANCE and SLOT-VALUE-USING-CLASS methods, and those
instructions are what dispatch them. Scalarizing dropped their side effects."
  (let ((types (%clos-vm-instruction-types
                "(defclass scalarize-meta () ())
                 (defclass scalarize-obj () ((x :initarg :x))
                   (:metaclass scalarize-meta))
                 (let ((o (make-instance 'scalarize-obj :x 1))) (slot-value o 'x))")))
    (assert-true (member 'cl-cc/vm:vm-make-obj types :test #'eq))
    (assert-true (member 'cl-cc/vm:vm-slot-read types :test #'eq)))
  ;; A standard metaclass still gets scalarized: no hooks can observe the reads.
  (let ((types (%clos-vm-instruction-types
                "(defclass scalarize-plain () ((x :initarg :x)))
                 (let ((o (make-instance 'scalarize-plain :x 1))) (slot-value o 'x))")))
    (assert-false (member 'cl-cc/vm:vm-slot-read types :test #'eq))))

(deftest-each clos-custom-metaclass-overrides
  "Custom metaclasses affect class-of, slot-value-using-class, and make-instance initialization hooks."
  :cases
  ;; Compared by name. RUN-STRING reads its source into :CL-CC, so the symbol it
  ;; returns is CL-CC::META-A while a bare 'META-A here is CL-CC/TEST::META-A;
  ;; ASSERT-EQUAL on the symbols compared packages, not class names.
  (("class-of-instance-returns-metaclass"
    "META-A"
    "(defclass meta-a () ())
     (defclass object-a () () (:metaclass meta-a))
     (symbol-name (class-name (class-of (make-instance 'object-a))))")
   ;; The counter has to be read *after* the slot access: the hook fires on the
   ;; read, and (list *count* (slot-value ...)) evaluates left to right, so the
   ;; original form sampled the counter before anything could increment it.
   ("slot-value-using-class-before-method-runs"
    '(42 1)
    "(defclass meta-b () ())
     (defclass object-b () ((x :initarg :x)) (:metaclass meta-b))
     (defvar *slot-hook-count* 0)
     (defmethod slot-value-using-class :before ((class meta-b) object slot-name)
       (declare (ignore class object slot-name))
       (setq *slot-hook-count* (+ *slot-hook-count* 1)))
     (let* ((obj (make-instance 'object-b :x 42))
            (value (slot-value obj 'x)))
       (list value *slot-hook-count*))")
   ("initialize-instance-after-method-runs"
    '(1 9)
    "(defclass meta-c () ())
     (defclass object-c () ((x :initarg :x)) (:metaclass meta-c))
     (defvar *init-hook-count* 0)
     (defmethod initialize-instance :after ((object meta-c))
       (setq *init-hook-count* (+ *init-hook-count* 1)))
     (let ((obj (make-instance 'object-c :x 9)))
       (list *init-hook-count* (gethash 'x obj)))"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Setf Slot-Value Tests

(deftest-each clos-setf-slot-value-mutations
  "setf slot-value: basic set, return value, multiple mutations, and computed value."
  :cases (("basic-set"          99
           "(defclass box () ((content :initarg :content)))
            (let ((b (make-instance 'box :content 0)))
              (setf (slot-value b 'content) 99)
              (slot-value b 'content))")
          ("returns-new-value"  42
           "(defclass box2 () ((content :initarg :content)))
            (let ((b (make-instance 'box2 :content 0)))
              (setf (slot-value b 'content) 42))")
          ("multiple-mutations" 30
           "(defclass counter () ((val :initarg :val)))
            (let ((c (make-instance 'counter :val 10)))
              (setf (slot-value c 'val) 20)
              (setf (slot-value c 'val) 30)
              (slot-value c 'val))")
          ("computed-value"     15
           "(defclass pair () ((a :initarg :a) (b :initarg :b)))
            (let ((p (make-instance 'pair :a 5 :b 10)))
              (setf (slot-value p 'a)
                    (+ (slot-value p 'a) (slot-value p 'b)))
              (slot-value p 'a))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each clos-builtin-class-precedence-dispatch
  "A method on a built-in superclass applies to its subclasses.

Dispatch compared the specializer against VM-CLASSIFY-ARG's answer with EQ, so
only an exact class name matched and there was no ancestry at all:
(defmethod f ((x number))) reported no applicable method for 42. A float was
worse off still -- the classifier named only INTEGER, STRING and SYMBOL, so
nothing could specialise on one."
  :cases (("number-matches-integer"
           "NUM"  "(defmethod cpl-a ((x number)) 'num) (symbol-name (cpl-a 42))")
          ("real-matches-integer"
           "REAL" "(defmethod cpl-b ((x real)) 'real) (symbol-name (cpl-b 42))")
          ("number-matches-float"
           "NUM"  "(defmethod cpl-c ((x number)) 'num) (symbol-name (cpl-c 1.5))")
          ("float-is-named"
           "FLT"  "(defmethod cpl-d ((x float)) 'flt) (symbol-name (cpl-d 1.5))")
          ("list-matches-cons"
           "LST"  "(defmethod cpl-e ((x list)) 'lst) (symbol-name (cpl-e (cons 1 2)))")
          ("exact-class-still-wins-over-ancestor"
           "INT"  "(defmethod cpl-f ((x number)) 'num)
                   (defmethod cpl-f ((x integer)) 'int)
                   (symbol-name (cpl-f 42))"))
  (expected source)
  (assert-string= expected (run-string source)))
