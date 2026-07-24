(in-package :cl-cc/test)


(it-sequential "self-host-clos-full-pipeline"
  (expect (= 100 (run-string *self-host-clos-full-pipeline-program* :stdlib t)) :to-be-truthy))

;;; Generic Function as First-Class Value Tests

(it-sequential "generic-function-numeric funcall"
  (destructuring-bind (expected form) (list 11 "(progn (defgeneric my-fn (x)) (defmethod my-fn ((x t)) (+ x 1)) (funcall #'my-fn 10))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "generic-function-numeric apply"
  (destructuring-bind (expected form) (list 42 "(progn (defgeneric add1 (x)) (defmethod add1 ((x t)) (+ x 1)) (apply #'add1 (list 41)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "generic-function-numeric in-let"
  (destructuring-bind (expected form) (list 5 "(progn (defgeneric double (x)) (defmethod double ((x t)) (* x 2)) (let ((f #'double)) (funcall f 2) (+ (funcall f 2) 1)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "funcall-generic-function-clos"
  (expect (run-string "(progn (defclass animal () ()) (defclass dog (animal) ()) (defgeneric speak (x)) (defmethod speak ((x dog)) \"dog-speak\") (defmethod speak ((x t)) \"default\") (funcall #'speak (make-instance 'dog)))") :to-equal "dog-speak"))

(it-sequential "mapcar-generic-function"
  (expect (run-string "(progn (defgeneric inc (x)) (defmethod inc ((x t)) (+ x 1)) (mapcar #'inc (list 1 2 3)))" :stdlib t) :to-equal '(2 3 4)))

(it-sequential "mapcar-generic-function-reader"
  (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass item () ((name :initarg :name :reader item-name))) (let ((items (list (make-instance 'item :name 'a) (make-instance 'item :name 'b) (make-instance 'item :name 'c)))) (mapcar #'item-name items)))" :stdlib t)))) :to-equal "(a b c)"))

(it-sequential "self-host-mapcar-inst-sexp"
  (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
      (string-downcase (format nil "~S"
                               (run-string *self-host-mapcar-inst-sexp-program*
                                           :stdlib t)))) :to-equal "((:const r0 42) (:const r1 7) (:add r2 r0 r1))"))

;;; Run Tests Function

;;; Global Variable (defvar) Persistence Tests

(it-sequential "defvar-persistence counter"
  (destructuring-bind (expected form) (list 3 "(progn (defvar *counter* 0) (defun inc-counter () (setq *counter* (+ *counter* 1)) *counter*) (inc-counter) (inc-counter) (inc-counter))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "defvar-persistence sequence"
  (destructuring-bind (expected form) (list '(0 1 2) "(progn (defvar *n* 0) (defun next-n () (let ((val *n*)) (setq *n* (+ *n* 1)) val)) (list (next-n) (next-n) (next-n)))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "defvar-label-generation"
  (expect (run-string "(progn (defvar *lbl* 0) (defun make-label (prefix) (let ((n *lbl*)) (setq *lbl* (+ n 1)) (concatenate 'string prefix \"_\" (write-to-string n)))) (list (make-label \"L\") (make-label \"L\") (make-label \"L\")))" :stdlib t) :to-equal '("L_0" "L_1" "L_2")))

;;; Defmacro in progn Tests

(it-sequential "defmacro-in-progn simple"
  (destructuring-bind (expected form) (list 10 "(progn (defmacro my-dbl (x) (list '+ x x)) (my-dbl 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "defmacro-in-progn rest"
  (destructuring-bind (expected form) (list 42 "(progn (defmacro my-when (test &rest body) (list 'if test (cons 'progn body) nil)) (my-when (= 1 1) 42))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "defmacro-in-progn used-twice"
  (destructuring-bind (expected form) (list 12 "(progn (defmacro my-add1 (x) (list '+ x 1)) (+ (my-add1 5) (my-add1 5)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Self-Hosting Compiler Pattern Tests

(it-sequential "self-host-compiler-context-full"
  (let ((result (run-string *self-host-compiler-context-program* :stdlib t)))
    (expect result :to-equal '((:CONST :R0 42) (:CONST :R1 7) (:ADD :R2 :R0 :R1)))))

(it-sequential "self-host-ast-compile-dispatch"
  (let ((result (run-string *self-host-ast-compile-dispatch-program* :stdlib t)))
    (expect result :to-equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3)))))

(it-sequential "self-host-macro-expander"
  (let ((result (run-string *self-host-simple-macro-expander-program* :stdlib t)))
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" result))) :to-equal "(if x (progn (+ 1 2)) nil)")))

;;; Multiple-Value-List Tests

(it-sequential "multiple-value-list floor"
  (destructuring-bind (expected form) (list '(3 2) "(multiple-value-list (floor 17 5))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "multiple-value-list values"
  (destructuring-bind (expected form) (list '(1 2 3) "(multiple-value-list (values 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "multiple-value-list single"
  (destructuring-bind (expected form) (list '(42) "(multiple-value-list (values 42))")
    (expect (run-string form) :to-equal expected)))

;;; Apply with Spread Arguments Tests

(it-sequential "apply-spread-args-numeric plus-spread"
  (destructuring-bind (expected form) (list 10 "(apply #'+ 1 2 (list 3 4))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "apply-spread-args-numeric plus-quoted-nil"
  (destructuring-bind (expected form) (list 3 "(apply #'+ 1 2 nil)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "apply-spread-args-numeric minus-list"
  (destructuring-bind (expected form) (list 5 "(apply #'- (list 10 3 2))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "apply-spread-args-numeric multiply-list"
  (destructuring-bind (expected form) (list 24 "(apply #'* (list 2 3 4))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "apply-spread-args-numeric plus-five"
  (destructuring-bind (expected form) (list 15 "(apply #'+ (list 1 2 3 4 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "apply-spread-quoted-nil-preserves-evaluation-order"
  (expect (run-string
           "(let ((n 0))
                (flet ((next () (setq n (+ n 1)) n))
                  (list (apply #'+ (next) (next) nil) n)))") :to-equal '(3 2)))

(it-sequential "apply-improper-quoted-list-signals-error"
  (signals error (run-string "(apply #'+ '(1 . 2))")))

(it-sequential "apply-spread-args-append"
  (expect (run-string "(apply #'append (list (list 1 2) (list 3 4)))") :to-equal '(1 2 3 4)))


;;; Typed Defun/Lambda Tests

(it-sequential "typed-defun-runtime basic-add"
  (destructuring-bind (expected form) (list 7 "(progn (defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y)) (typed-add 3 4))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typed-defun-runtime no-return-type"
  (destructuring-bind (expected form) (list 12 "(progn (defun typed-mul ((x fixnum) (y fixnum)) (* x y)) (typed-mul 3 4))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typed-defun-runtime mixed-params"
  (destructuring-bind (expected form) (list 7 "(progn (defun typed-mixed ((x fixnum) y) (+ x y)) (typed-mixed 3 4))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typed-defun-runtime string-return"
  (destructuring-bind (expected form) (list "Hello World" "(progn (defun typed-greet ((name string)) string (concatenate 'string \"Hello \" name)) (typed-greet \"World\"))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typed-lambda with-return"
  (destructuring-bind (expected form) (list 30 "(funcall (lambda ((x fixnum) (y fixnum)) fixnum (+ x y)) 10 20)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "typed-lambda no-return"
  (destructuring-bind (expected form) (list 6 "(funcall (lambda ((a fixnum) (b fixnum)) (* a b)) 2 3)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "typed-defun-type-registry"
  (let ((old-count (hash-table-count cl-cc/compile::*function-type-registry*)))
    (run-string "(defun typed-reg-test ((x fixnum)) fixnum x)")
    (expect (> (hash-table-count cl-cc/compile::*function-type-registry*) old-count) :to-be-truthy)))

(it-sequential "typed-multi-form-top-level"
  (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (result type)
        (run-string-typed "(defvar *typed-top-level* 1)
                           42")
      (declare (ignore type))
      (expect (= 42 result) :to-be-truthy))))

;;; CLOS Type Inference Tests

(it-sequential "clos-type-inference-slot-types make-instance"
  (destructuring-bind (form check-result expected-type-name) (list "(progn (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
              (make-instance 'point :x 1 :y 2))" (lambda (r) (declare (ignore r))) "POINT")
    (multiple-value-bind (result type) (run-string-typed form)
    (funcall check-result result)
    (expect (typep type 'cl-cc/type:type-primitive) :to-be-truthy)
    (expect (symbol-name (cl-cc/type:type-primitive-name type)) :to-equal expected-type-name))))

(it-sequential "clos-type-inference-slot-types slot-fixnum"
  (destructuring-bind (form check-result expected-type-name) (list "(progn (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
              (let ((p (make-instance 'point :x 10 :y 20))) (slot-value p 'x)))" (lambda (r) (expect (= 10 r) :to-be-truthy)) "FIXNUM")
    (multiple-value-bind (result type) (run-string-typed form)
    (funcall check-result result)
    (expect (typep type 'cl-cc/type:type-primitive) :to-be-truthy)
    (expect (symbol-name (cl-cc/type:type-primitive-name type)) :to-equal expected-type-name))))

(it-sequential "clos-type-inference-slot-types slot-string"
  (destructuring-bind (form check-result expected-type-name) (list "(progn (defclass person () ((name :initarg :name :type string)))
              (let ((p (make-instance 'person :name \"Alice\"))) (slot-value p 'name)))" (lambda (r) (expect r :to-equal "Alice")) "STRING")
    (multiple-value-bind (result type) (run-string-typed form)
    (funcall check-result result)
    (expect (typep type 'cl-cc/type:type-primitive) :to-be-truthy)
    (expect (symbol-name (cl-cc/type:type-primitive-name type)) :to-equal expected-type-name))))

;;; Type Alias (deftype) Tests

(it-sequential "deftype-numeric basic"
  (destructuring-bind (expected form) (list 42 "(progn (deftype my-int fixnum) (defun typed-id ((x my-int)) my-int x) (typed-id 42))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "deftype-numeric in-slot"
  (destructuring-bind (expected form) (list 10 "(progn (deftype coordinate fixnum) (defclass point2 () ((x :initarg :x :type coordinate))) (let ((p (make-instance 'point2 :x 10))) (slot-value p 'x)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "deftype-union"
  (let ((old-count (hash-table-count cl-cc/type:*type-alias-registry*)))
    (run-string "(deftype int-or-str (or fixnum string))")
    (expect (> (hash-table-count cl-cc/type:*type-alias-registry*) old-count) :to-be-truthy)))

;;; Type Narrowing Tests

(it-sequential "type-narrowing-predicate numberp-to-fixnum"
  (destructuring-bind (form check-result expected-type-name) (list "(let ((x 42)) (if (numberp x) (+ x 1) 0))" (lambda (r) (expect (= 43 r) :to-be-truthy)) "FIXNUM")
    (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (result type) (run-string-typed form)
      (funcall check-result result)
      (expect (typep type 'cl-cc/type:type-primitive) :to-be-truthy)
      (when expected-type-name
        (expect (symbol-name (cl-cc/type:type-primitive-name type)) :to-equal expected-type-name))))))

(it-sequential "type-narrowing-predicate stringp-to-string"
  (destructuring-bind (form check-result expected-type-name) (list "(let ((x \"hello\")) (if (stringp x) x \"default\"))" (lambda (r) (expect r :to-equal "hello")) nil)
    (handler-bind ((warning #'muffle-warning))
    (multiple-value-bind (result type) (run-string-typed form)
      (funcall check-result result)
      (expect (typep type 'cl-cc/type:type-primitive) :to-be-truthy)
      (when expected-type-name
        (expect (symbol-name (cl-cc/type:type-primitive-name type)) :to-equal expected-type-name))))))

;;; Higher-Order Function Macro Expansions (Self-Hosting)

(it-sequential "hof-list-result mapcar-basic"
  (destructuring-bind (expected form) (list '(2 4 6) "(mapcar (lambda (x) (* x 2)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result mapcar-empty"
  (destructuring-bind (expected form) (list nil "(mapcar (lambda (x) x) nil)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result mapc-original"
  (destructuring-bind (expected form) (list '(1 2 3) "(mapc (lambda (x) (+ x 1)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result mapcan-flatten"
  (destructuring-bind (expected form) (list '(1 1 2 2 3 3) "(mapcan (lambda (x) (list x x)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result every-true"
  (destructuring-bind (expected form) (list t "(every (lambda (x) (> x 0)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result every-false"
  (destructuring-bind (expected form) (list nil "(every (lambda (x) (> x 2)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result every-empty"
  (destructuring-bind (expected form) (list t "(every (lambda (x) x) nil)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result some-not-found"
  (destructuring-bind (expected form) (list nil "(some (lambda (x) (if (> x 10) x nil)) (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result find-not-found"
  (destructuring-bind (expected form) (list nil "(find 99 (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result position-not-found"
  (destructuring-bind (expected form) (list nil "(position 99 (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result remove-if"
  (destructuring-bind (expected form) (list '(1 3 5) "(remove-if (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result remove-if-not"
  (destructuring-bind (expected form) (list '(2 4) "(remove-if-not (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result remove-basic"
  (destructuring-bind (expected form) (list '(1 3 5) "(remove 2 (list 1 2 3 2 5))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-list-result remove-duplicates"
  (destructuring-bind (expected form) (list '(1 2 3) "(remove-duplicates (list 1 2 3 2 1))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "hof-numeric-result some-found"
  (destructuring-bind (expected form) (list 3 "(some (lambda (x) (if (> x 2) x nil)) (list 1 2 3 4))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "hof-numeric-result find-basic"
  (destructuring-bind (expected form) (list 3 "(find 3 (list 1 2 3 4 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "hof-numeric-result find-if"
  (destructuring-bind (expected form) (list 4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "hof-numeric-result position"
  (destructuring-bind (expected form) (list 2 "(position 3 (list 1 2 3 4 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "hof-numeric-result count"
  (destructuring-bind (expected form) (list 3 "(count 2 (list 1 2 2 3 2))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "hof-numeric-result count-if"
  (destructuring-bind (expected form) (list 2 "(count-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Parametric types, defparameter, equal, numeric, warn, format tests → compiler-tests-selfhost-types.lisp.
