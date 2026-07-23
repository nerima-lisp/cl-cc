(in-package :cl-cc/test)

;;; Self-Hosting Integration Tests

(it-sequential "self-host-patterns eval-loop"
  (destructuring-bind (expected form) (list 7 " (defun mini-eval (form env) (cond ((integerp form) form) ((symbolp form) (cdr (assoc form env))) ((and (consp form) (eq (car form) 'quote)) (cadr form)) ((and (consp form) (eq (car form) 'if)) (if (not (= 0 (mini-eval (cadr form) env))) (mini-eval (caddr form) env) (mini-eval (cadddr form) env))) ((and (consp form) (eq (car form) '+)) (+ (mini-eval (cadr form) env) (mini-eval (caddr form) env))) (t 0))) (mini-eval '(if 1 (+ 3 4) 0) nil) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "self-host-patterns defstruct-pipe"
  (destructuring-bind (expected form) (list 42 " (defstruct node type value children) (let ((n (make-node :type 'add :value nil :children (list (make-node :type 'lit :value 42 :children nil))))) (node-value (car (node-children n)))) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "self-host-patterns ht-registry"
  (destructuring-bind (expected form) (list 30 " (let ((registry (make-hash-table))) (setf (gethash 'add registry) (lambda (a b) (+ a b))) (setf (gethash 'mul registry) (lambda (a b) (* a b))) (let ((op (gethash 'add registry))) (funcall op 10 20))) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "self-host-patterns tree-walk"
  (destructuring-bind (expected form) (list 10 " (defun tree-sum (tree) (if (consp tree) (+ (tree-sum (car tree)) (tree-sum (cdr tree))) (if (integerp tree) tree 0))) (tree-sum '((1 . 2) . (3 . (4 . nil)))) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "self-host-patterns closure-counter"
  (destructuring-bind (expected form) (list 3 " (let ((counter 0)) (defun next-id () (setq counter (+ counter 1)) counter)) (next-id) (next-id) (next-id) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "self-host-patterns macro-code-gen"
  (destructuring-bind (expected form) (list 15 " (defun make-add-expr (a b) (list '+ a b)) (defun make-let-expr (var val body) (list 'let (list (list var val)) body)) (eval (make-let-expr 'x 10 (make-add-expr 'x 5))) ")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

;;; Non-Constant Default Parameter Tests

(it-sequential "non-constant-default-params key-default"
  (destructuring-bind (expected form) (list '(1 2 3) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "non-constant-default-params key-supplied"
  (destructuring-bind (expected form) (list '(4 5 6) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn :data (list 4 5 6)))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "non-constant-default-params optional-default"
  (destructuring-bind (expected form) (list '(10 20) "(progn (defun test-fn (&optional (data (list 10 20))) data) (test-fn))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "defstruct-non-constant-default"
  (expect (run-string "(progn (defstruct registry (entries (make-hash-table :test 'eq))) (let ((r (make-registry))) (setf (gethash 'foo (registry-entries r)) :bar) (gethash 'foo (registry-entries r))))" :stdlib t) :to-be :bar))

;;; Multiple Dispatch Tests

(it-sequential "multi-dispatch-numeric dog+bone"
  (destructuring-bind (expected form) (list 1 "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'dog) (make-instance 'bone)))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "multi-dispatch-numeric cat+fish"
  (destructuring-bind (expected form) (list 2 "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'cat) (make-instance 'fish)))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "multi-dispatch-numeric circle-mixed"
  (destructuring-bind (expected form) (list 10 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'circle) 99))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "multi-dispatch-numeric rect-mixed"
  (destructuring-bind (expected form) (list 20 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'rect) 99))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "multi-dispatch-numeric clos-2nd-arg"
  (destructuring-bind (expected form) (list 42 "(progn (defclass ctx () ()) (defclass nd () ()) (defclass nd-int (nd) ((v :initarg :v :reader nd-v))) (defgeneric cmp (n c)) (defmethod cmp ((n nd-int) c) 42) (cmp (make-instance 'nd-int :v 1) (make-instance 'ctx)))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "multi-dispatch-inheritance-fallback"
  (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass a () ()) (defclass b (a) ()) (defclass x () ()) (defclass y (x) ()) (defgeneric op (p q)) (defmethod op ((p a) (q x)) 'base) (op (make-instance 'b) (make-instance 'y)))" :stdlib t)))) :to-equal "base"))

(it-sequential "multi-dispatch-type-equality same-type"
  (destructuring-bind (expected form) (list t "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-int)))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "multi-dispatch-type-equality diff-type"
  (destructuring-bind (expected form) (list nil "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-str)))")
    (expect (run-string form :stdlib t) :to-equal expected)))

;;; CLOS Initform and Accessor Setf Tests

(it-sequential "clos-initform integer"
  (destructuring-bind (expected form) (list 0 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (counter-n (make-instance 'counter)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-initform setf"
  (destructuring-bind (expected form) (list 99 "(progn (defclass box () ((val :initarg :val :initform 0 :accessor box-val))) (let ((b (make-instance 'box))) (setf (box-val b) 99) (box-val b)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-initform increment"
  (destructuring-bind (expected form) (list 3 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (let ((c (make-instance 'counter))) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (counter-n c)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Self-Hosting Bootstrap Tests

(it-sequential "self-host-make-register"
  (expect (run-string *self-host-make-register-program* :stdlib t) :to-equal '(:R0 :R1 :R2)))

(it-sequential "self-host-mini-compiler"
  (expect (= 35 (run-string *self-host-mini-compiler-program* :stdlib t)) :to-be-truthy))

(it-sequential "self-host-clos-compiler-full"
  (expect (run-string *self-host-clos-compiler-full-program* :stdlib t) :to-equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3))))
