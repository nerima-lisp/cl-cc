(in-package :cl-cc/test)

;;; Standard Library Set Operations Tests


(deftest-compile stdlib-list-ops
  "set-difference, union, append-lists, and last-cons work on lists."
  :cases (("set-diff"       '(1 3 5)     "(set-difference (list 1 2 3 4 5) (list 2 4))")
          ("set-diff-empty" '(1 2 3)     "(set-difference (list 1 2 3) (list))")
          ("union"          '(1 2 3 4 5) "(sort (union (list 1 2 3) (list 3 4 5)) #'<)")
          ("append-lists"   '(1 2 3 4)   "(append (list 1 2) (list 3 4))")
          ("last-cons"      3            "(car (last (list 1 2 3)))"))
  :stdlib t)

(deftest-compile stdlib-reduce
  "reduce folds a list with a function and optional initial value."
  :cases (("basic"       10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
          ("single"      42 "(reduce (lambda (a b) (+ a b)) (list 42))")
          ("with-init"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4) :initial-value 0)")
          ("empty-init"   0 "(reduce (lambda (a b) (+ a b)) nil :initial-value 0)"))
  :stdlib t)

(deftest-compile stdlib-reduce-edge
  "reduce edge cases: nil initial value, reduce-init accumulation."
  :cases (("init-nil"     nil      "(reduce (lambda (a b) (cons b a)) nil :initial-value nil)")
          ("init-accum"   '(3 2 1) "(reduce (lambda (acc x) (cons x acc)) (list 1 2 3) :initial-value nil)"))
  :stdlib t)


(it-sequential "compile-hash-table-keys"
  (expect (= 2 (run-string " (let ((ht (make-hash-table))) (setf (gethash :x ht) 10) (setf (gethash :y ht) 20) (length (hash-table-keys ht)))")) :to-be-truthy))

(it-sequential "compile-clos-mop-introspection direct-vs-effective-slots"
  (destructuring-bind (expected form) (list '((b) (a b)) "(progn
              (defclass mop-base () ((a :initarg :a)))
              (defclass mop-child (mop-base) ((b :initarg :b)))
              (list (mapcar #'slot-definition-name (class-direct-slots (find-class 'mop-child)))
                    (mapcar #'slot-definition-name (class-slots (find-class 'mop-child)))))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-clos-mop-introspection slot-type-initfunction-metaclass"
  (destructuring-bind (expected form) (list '(x integer 7 standard-class) "(progn
              (defclass typed-mop () ((x :initarg :x :initform 7 :type integer)) (:metaclass standard-class))
              (let ((slot (car (class-slots (find-class 'typed-mop)))))
                (list (slot-definition-name slot)
                      (slot-definition-type slot)
                      (funcall (slot-definition-initfunction slot))
                      (class-metaclass (find-class 'typed-mop)))))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-clos-mop-introspection compute-effective-slot-definition"
  (destructuring-bind (expected form) (list 'integer "(progn
              (defclass effective-mop () ((x :initarg :x :type integer)))
              (slot-definition-type
               (compute-effective-slot-definition
                (find-class 'effective-mop)
                'x
                (class-direct-slots (find-class 'effective-mop)))))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-clos-mop-introspection redefined-class-lazy-migration"
  (destructuring-bind (expected form) (list nil "(let ((obj nil))
              (defclass redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'redef-mop :x 1))
              (defclass redef-mop () ((y :initarg :y)))
              (slot-value obj 'y))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-clos-mop-introspection redefined-class-slot-boundp-migration"
  (destructuring-bind (expected form) (list t "(let ((obj nil))
              (defclass boundp-redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'boundp-redef-mop :x 1))
              (defclass boundp-redef-mop () ((y :initarg :y)))
              (slot-boundp obj 'y))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-clos-mop-introspection redefined-class-slot-makunbound-migration"
  (destructuring-bind (expected form) (list nil "(let ((obj nil))
              (defclass makun-redef-mop () ((x :initarg :x)))
              (setq obj (make-instance 'makun-redef-mop :x 1))
              (defclass makun-redef-mop () ((y :initarg :y)))
              (slot-makunbound obj 'y)
              (slot-boundp obj 'y))")
    (expect (run-string form :stdlib t) :to-equal expected)))

;;; Defstruct Tests

(it-sequential "compile-defstruct basic"
  (destructuring-bind (expected form) (list 10 " (progn (defstruct point x y) (let ((p (make-point :x 10 :y 20))) (point-x p)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-defstruct default"
  (destructuring-bind (expected form) (list 0 " (progn (defstruct counter (count 0)) (let ((c (make-counter))) (counter-count c)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-defstruct predicate"
  (destructuring-bind (expected form) (list 1 " (progn (defstruct my-box value) (let ((b (make-my-box :value 42))) (if (my-box-p b) 1 0)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-defstruct typep"
  (destructuring-bind (expected form) (list 1 " (progn (defstruct my-pair first second) (let ((p (make-my-pair :first 1 :second 2))) (if (typep p 'my-pair) 1 0)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-defstruct boa"
  (destructuring-bind (expected form) (list 3 " (progn (defstruct (my-vec (:constructor make-my-vec (x y))) x y) (let ((v (make-my-vec 1 3))) (my-vec-y v)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-defstruct conc-name"
  (destructuring-bind (expected form) (list 42 " (progn (defstruct (my-item (:conc-name item-)) value) (let ((i (make-my-item :value 42))) (item-value i)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Car/Cdr Composition Tests

(it-sequential "compile-cxr-basic caar"
  (destructuring-bind (expected form) (list 1 "(caar (list (list 1 2) (list 3 4)))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-cxr-basic cadr"
  (destructuring-bind (expected form) (list 2 "(cadr (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-cxr-basic cddr"
  (destructuring-bind (expected form) (list '(3) "(cddr (list 1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-cxr-basic caddr"
  (destructuring-bind (expected form) (list 3 "(caddr (list 1 2 3 4))")
    (expect (run-string form) :to-equal expected)))

;;; Stdlib Find/Position Tests

(deftest-compile stdlib-find-position
  "find and position return element/index, or nil when not found."
  :cases (("find"          3   "(find 3 (list 1 2 3 4 5))")
          ("find-miss"     nil "(find 9 (list 1 2 3))")
          ("position"      2   "(position 3 (list 1 2 3 4 5))")
          ("position-miss" nil "(position 9 (list 1 2 3))"))
  :stdlib t)

(it-sequential "stdlib-cons-printing-forms find-with-key"
  (destructuring-bind (expected form) (list "(2 . b)" "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key (lambda (x) (car x)))")
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))) :to-equal expected)))

(it-sequential "stdlib-cons-printing-forms pairlis"
  (destructuring-bind (expected form) (list "((b . 2) (a . 1))" "(pairlis (list 'a 'b) (list 1 2))")
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))) :to-equal expected)))

(it-sequential "stdlib-cons-printing-forms assoc-if"
  (destructuring-bind (expected form) (list "(2 . b)" "(assoc-if (lambda (k) (= k 2)) (list (cons 1 'a) (cons 2 'b)))")
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))) :to-equal expected)))

(it-sequential "stdlib-cons-printing-forms rassoc"
  (destructuring-bind (expected form) (list "(2 . b)" "(rassoc 'b (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))")
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))) :to-equal expected)))

(it-sequential "stdlib-cons-printing-forms find-sharpsign-key"
  (destructuring-bind (expected form) (list "(2 . b)" "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key #'car)")
    (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))) :to-equal expected)))

(it-sequential "stdlib-identity"
  (assert-run= 42 "(identity 42)"))

;;; Setf Places Tests

(it-sequential "compile-setf-places car"
  (destructuring-bind (expected form) (list 99 "(let ((pair (cons 1 2))) (setf (car pair) 99) (car pair))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places cdr"
  (destructuring-bind (expected form) (list 99 "(let ((pair (cons 1 2))) (setf (cdr pair) 99) (cdr pair))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places first"
  (destructuring-bind (expected form) (list 42 "(let ((lst (list 1 2 3))) (setf (first lst) 42) (first lst))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places rest"
  (destructuring-bind (expected form) (list 42 "(let ((lst (list 1 2 3))) (setf (rest lst) (list 42)) (car (cdr lst)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places second"
  (destructuring-bind (expected form) (list 99 "(let ((lst (list 10 20 30))) (setf (second lst) 99) (car (cdr lst)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places cadr"
  (destructuring-bind (expected form) (list 88 "(let ((lst (list 10 20 30))) (setf (cadr lst) 88) (car (cdr lst)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places cddr"
  (destructuring-bind (expected form) (list 77 "(let ((lst (list 10 20 30))) (setf (cddr lst) (list 77)) (car (cdr (cdr lst))))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places caddr"
  (destructuring-bind (expected form) (list 66 "(let ((lst (list 10 20 30))) (setf (caddr lst) 66) (car (cdr (cdr lst))))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places nth"
  (destructuring-bind (expected form) (list 99 "(let ((lst (list 10 20 30))) (setf (nth 1 lst) 99) (nth 1 lst))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-setf-places returns-val"
  (destructuring-bind (expected form) (list 42 "(let ((pair (cons 1 2))) (setf (car pair) 42))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Package System Tests

(it-sequential "compile-package-forms in-package"
  (destructuring-bind (expected form) (list :cl-cc "(in-package :cl-cc)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-package-forms defpackage"
  (destructuring-bind (expected form) (list :test-pkg "(defpackage :test-pkg (:use :cl))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-package-forms in-package-then-code"
  (destructuring-bind (expected form) (list 42 "(progn (in-package :cl-cc) 42)")
    (expect (run-string form) :to-equal expected)))

;;; Macrolet Tests

;;; Macrolet and Function Reference Tests (#'builtin)

(it-sequential "compile-macrolet-and-funcall macrolet-basic"
  (destructuring-bind (expected form) (list 6 "(macrolet ((double (x) `(+ ,x ,x))) (double 3))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-macrolet-and-funcall macrolet-multiple"
  (destructuring-bind (expected form) (list 10 "(macrolet ((add1 (x) `(+ ,x 1)) (add2 (x) `(+ ,x 2))) (+ (add1 3) (add2 4)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-macrolet-and-funcall macrolet-scoped"
  (destructuring-bind (expected form) (list 42 "(let ((x 42)) (macrolet ((get-x () 'x)) (get-x)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-macrolet-and-funcall macrolet-nested"
  (destructuring-bind (expected form) (list 8 "(macrolet ((square (x) `(* ,x ,x))) (macrolet ((sq-plus-sq (a b) `(+ (square ,a) (square ,b)))) (sq-plus-sq 2 2)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-macrolet-and-funcall funcall-car"
  (destructuring-bind (expected form) (list 1 "(funcall #'car (cons 1 2))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-macrolet-and-funcall funcall-plus"
  (destructuring-bind (expected form) (list 7 "(funcall #'+ 3 4)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-function-sharpsign cons-pair"
  (destructuring-bind (expected form) (list '(1 . 2) "(funcall #'cons 1 2)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-function-sharpsign car-mapcar"
  (destructuring-bind (expected form) (list '(1 2 3) "(mapcar #'car (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))")
    (expect (run-string form :stdlib t) :to-equal expected)))

;;; Warn Test

;;; String Concatenation Tests

(it-sequential "compile-string-concat two-strings"
  (destructuring-bind (expected form) (list "hello world" "(string-concat \"hello \" \"world\")")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-concat concat-abc"
  (destructuring-bind (expected form) (list "abc" "(concatenate 'string \"a\" \"b\" \"c\")")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-concat concat-two"
  (destructuring-bind (expected form) (list "foobar" "(concatenate 'string \"foo\" \"bar\")")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-concat concat-one"
  (destructuring-bind (expected form) (list "hello" "(concatenate 'string \"hello\")")
    (expect (run-string form) :to-equal expected)))

;;; Check-Type Tests

(it-sequential "compile-check-type passes"
  (destructuring-bind (form verify) (list "(let ((x 42)) (check-type x integer))" (lambda (form)
             (expect (run-string form) :to-be nil)))
    (funcall verify form)))

(it-sequential "compile-check-type errors"
  (destructuring-bind (form verify) (list "(let ((x \"hello\")) (check-type x integer))" (lambda (form)
             (signals error (run-string form))))
    (funcall verify form)))

(it-sequential "compile-correctable-type-restarts check-type-store-value"
  (destructuring-bind (expected form) (list 7 "(let ((x \"bad\")) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 7)))) (check-type x integer) x))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-correctable-type-restarts ccase-store-value"
  (destructuring-bind (expected form) (list 11 "(let ((x 'bad)) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 'ok)))) (ccase x (ok 11))))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-correctable-type-restarts ctypecase-store-value"
  (destructuring-bind (expected form) (list 42 "(let ((x \"bad\")) (handler-bind ((type-error (lambda (c) (declare (ignore c)) (store-value 42)))) (ctypecase x (integer x))))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-assert-place-restarts"
  (let ((result
          (run-string
           "(let ((x 1) (y 2))
  (handler-bind ((error (lambda (c) (declare (ignore c)) (store-value '(5 6)))))
    (assert (= (+ x y) 11) (x y))
    (list x y)))"
           :stdlib t)))
    (expect result :to-equal '(5 6))))

;;; Eval-When Tests

(it-sequential "compile-eval-when-numeric execute"
  (destructuring-bind (expected form) (list 42 "(eval-when (:execute) 42)")
    (handler-bind ((warning #'muffle-warning))
    (expect (= expected (run-string form)) :to-be-truthy))))

(it-sequential "compile-eval-when-numeric load-toplevel"
  (destructuring-bind (expected form) (list 10 "(eval-when (:load-toplevel :execute) (+ 3 7))")
    (handler-bind ((warning #'muffle-warning))
    (expect (= expected (run-string form)) :to-be-truthy))))

(it-sequential "compile-eval-when-numeric all"
  (destructuring-bind (expected form) (list 5 "(eval-when (:compile-toplevel :load-toplevel :execute) 5)")
    (handler-bind ((warning #'muffle-warning))
    (expect (= expected (run-string form)) :to-be-truthy))))

(it-sequential "compile-eval-when-skip"
  (handler-bind ((warning #'muffle-warning))
    (expect (run-string "(eval-when (:compile-toplevel) 42)") :to-be nil)))

;;; Property List and Set Operations Tests

(deftest-compile stdlib-getf-and-set-ops
  "getf returns the correct value; intersection and remove filter list elements."
  :cases (("getf-found"          2         "(getf (list :a 1 :b 2 :c 3) :b)")
          ("getf-default"        99        "(getf (list :a 1) :z 99)")
          ("getf-first"          1         "(getf (list :a 1 :b 2) :a)")
          ("getf-not-found"      nil       "(getf (list :a 1 :b 2) :z)")
          ("set-intersection"    '(2 3)    "(intersection (list 1 2 3) (list 2 3 4))")
          ("set-intersection-empty" nil    "(intersection (list 1 2) (list 3 4))")
          ("set-remove"          '(1 3 5)  "(remove 2 (list 1 2 3 2 5))"))
  :stdlib t)

;;; Eval Tests

(it-sequential "our-eval-numeric basic"
  (destructuring-bind (expected form) (list 42 '(+ 20 22))
    (expect (= expected (our-eval form)) :to-be-truthy)))

(it-sequential "our-eval-numeric lambda"
  (destructuring-bind (expected form) (list 10 '(funcall (lambda (x) (+ x 3)) 7))
    (expect (= expected (our-eval form)) :to-be-truthy)))

(it-sequential "our-eval-numeric let"
  (destructuring-bind (expected form) (list 15 '(let ((a 5) (b 10)) (+ a b)))
    (expect (= expected (our-eval form)) :to-be-truthy)))
