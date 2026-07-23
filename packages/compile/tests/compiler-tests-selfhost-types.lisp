;;;; compiler-tests-selfhost-types.lisp — Parametric types, defparameter, equal, numeric, warn, format tests
(in-package :cl-cc/test)


;;; Parametric Types (type-constructor)

(it-sequential "parametric-type-parse-single-arg list-fixnum"
  (destructuring-bind (spec expected-name expected-arg-type) (list '(list fixnum) 'list cl-cc/type:type-int)
    (let ((ty (cl-cc/type:parse-type-specifier spec)))
    (expect (typep ty 'cl-cc/type:type-constructor) :to-be-truthy)
    (expect (cl-cc/type:type-constructor-name ty) :to-be expected-name)
    (expect (= 1 (length (cl-cc/type:type-constructor-args ty))) :to-be-truthy)
    (expect (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                          expected-arg-type) :to-be-truthy))))

(it-sequential "parametric-type-parse-single-arg option-string"
  (destructuring-bind (spec expected-name expected-arg-type) (list '(Option string) 'Option cl-cc/type:type-string)
    (let ((ty (cl-cc/type:parse-type-specifier spec)))
    (expect (typep ty 'cl-cc/type:type-constructor) :to-be-truthy)
    (expect (cl-cc/type:type-constructor-name ty) :to-be expected-name)
    (expect (= 1 (length (cl-cc/type:type-constructor-args ty))) :to-be-truthy)
    (expect (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                          expected-arg-type) :to-be-truthy))))

(it-sequential "parametric-type-parse-pair"
  (let ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string))))
    (expect (typep ty 'cl-cc/type:type-constructor) :to-be-truthy)
    (expect (cl-cc/type:type-constructor-name ty) :to-be 'Pair)
    (expect (= 2 (length (cl-cc/type:type-constructor-args ty))) :to-be-truthy)
    (expect (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int) :to-be-truthy)
    (expect (cl-cc/type:type-equal-p (second (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string) :to-be-truthy)))

(it-sequential "parametric-type-unify-same"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (expect ok :to-be-truthy)
      ;; No bindings needed — subst may be empty struct or nil
      (expect (or (null subst)
                       (zerop (hash-table-count
                                (cl-cc/type:substitution-bindings subst)))) :to-be-truthy))))

(it-sequential "parametric-type-unify-with-var"
  (let* ((tv (cl-cc/type:fresh-type-var 'a))
         (t1 (cl-cc/type:make-type-constructor 'list (list tv)))
         (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (expect ok :to-be-truthy)
      (expect (null subst) :to-be-falsy)
      (let ((resolved (cl-cc/type:zonk tv subst)))
        (expect (cl-cc/type:type-equal-p resolved cl-cc/type:type-int) :to-be-truthy)))))

(it-sequential "parametric-type-unify-different-constructors"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(Option fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (declare (ignore subst))
      (expect ok :to-be-falsy))))

(it-sequential "parametric-type-utilities unparse-roundtrip"
  (destructuring-bind (check) (list (lambda ()
             (let* ((ty   (cl-cc/type:parse-type-specifier '(Pair fixnum string)))
                    (spec (cl-cc/type:unparse-type ty)))
               (expect (first spec) :to-equal 'Pair)
               (expect (= 3 (length spec)) :to-be-truthy))))
    (funcall check)))

(it-sequential "parametric-type-utilities to-string"
  (destructuring-bind (check) (list (lambda ()
             (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
               (expect (cl-cc/type:type-to-string ty) :to-equal "(LIST FIXNUM)"))))
    (funcall check)))

(it-sequential "parametric-type-utilities equal-p"
  (destructuring-bind (check) (list (lambda ()
             (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
                   (t2 (cl-cc/type:parse-type-specifier '(list fixnum)))
                   (t3 (cl-cc/type:parse-type-specifier '(list string))))
               (expect (cl-cc/type:type-equal-p t1 t2) :to-be-truthy)
               (expect (cl-cc/type:type-equal-p t1 t3) :to-be-falsy))))
    (funcall check)))

(it-sequential "parametric-type-utilities free-vars"
  (destructuring-bind (check) (list (lambda ()
             (let* ((tv (cl-cc/type:fresh-type-var 'x))
                    (ty (cl-cc/type:make-type-constructor 'list (list tv))))
               (expect (= 1 (length (cl-cc/type:type-free-vars ty))) :to-be-truthy))))
    (funcall check)))

(it-sequential "parametric-type-nested"
  (let ((ty (cl-cc/type:parse-type-specifier '(list (Option fixnum)))))
    (expect (typep ty 'cl-cc/type:type-constructor) :to-be-truthy)
    (expect (cl-cc/type:type-constructor-name ty) :to-be 'list)
    (let ((inner (first (cl-cc/type:type-constructor-args ty))))
      (expect (typep inner 'cl-cc/type:type-constructor) :to-be-truthy)
      (expect (cl-cc/type:type-constructor-name inner) :to-be 'Option))))

(it-sequential "parametric-type-in-typed-defun"
  (let ((result (run-string "(progn
    (deftype int-list (list fixnum))
    (defun make-nums () (list 1 2 3))
    (length (make-nums)))")))
    (expect (= 3 result) :to-be-truthy)))

;;; Defparameter Tests

(it-sequential "defparameter-persistence basic"
  (destructuring-bind (expected form) (list 42 "(progn (defparameter *val* 42) *val*)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "defparameter-persistence with-function"
  (destructuring-bind (expected form) (list 10 "(progn (defparameter *base* 10) (defun get-base () *base*) (get-base))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "defparameter-persistence setq-mutation"
  (destructuring-bind (expected form) (list 5 "(progn (defparameter *x* 0) (setq *x* 5) *x*)")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; String= and Equal Tests

(it-sequential "compile-equal-truthy string=-match"
  (destructuring-bind (form) (list "(string= \"hello\" \"hello\")")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-equal-truthy equal-numbers"
  (destructuring-bind (form) (list "(equal 42 42)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-equal-truthy equal-strings"
  (destructuring-bind (form) (list "(equal \"abc\" \"abc\")")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-equal-truthy equal-lists"
  (destructuring-bind (form) (list "(equal '(1 2 3) (list 1 2 3))")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-equal-false string=-diff"
  (destructuring-bind (form) (list "(string= \"hello\" \"world\")")
    (expect (null (run-string form)) :to-be-truthy)))

(it-sequential "compile-equal-false equal-diff"
  (destructuring-bind (form) (list "(equal 1 2)")
    (expect (null (run-string form)) :to-be-truthy)))

;;; Numeric Builtins Tests (max, min, mod, zerop, plusp, minusp)

(it-sequential "numeric-arithmetic max"
  (destructuring-bind (expected form) (list 5 "(max 3 5)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "numeric-arithmetic min"
  (destructuring-bind (expected form) (list 3 "(min 3 5)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "numeric-arithmetic mod-basic"
  (destructuring-bind (expected form) (list 1 "(mod 7 3)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "numeric-arithmetic mod-even"
  (destructuring-bind (expected form) (list 0 "(mod 6 3)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "numeric-arithmetic abs"
  (destructuring-bind (expected form) (list 5 "(abs -5)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "numeric-predicates-truthy zerop-zero"
  (destructuring-bind (form) (list "(zerop 0)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "numeric-predicates-truthy plusp-pos"
  (destructuring-bind (form) (list "(plusp 5)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "numeric-predicates-truthy minusp-neg"
  (destructuring-bind (form) (list "(minusp -3)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "numeric-predicates-truthy evenp-even"
  (destructuring-bind (form) (list "(evenp 4)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "numeric-predicates-truthy oddp-odd"
  (destructuring-bind (form) (list "(oddp 3)")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "numeric-predicates-false zerop-nonzero"
  (destructuring-bind (form) (list "(zerop 5)")
    (expect (run-string form) :to-be-falsy)))

(it-sequential "numeric-predicates-false plusp-neg"
  (destructuring-bind (form) (list "(plusp -3)")
    (expect (run-string form) :to-be-falsy)))

(it-sequential "numeric-predicates-false minusp-pos"
  (destructuring-bind (form) (list "(minusp 5)")
    (expect (run-string form) :to-be-falsy)))

;;; Warn Compilation Tests

(it-sequential "compile-warn"
  (expect (null (run-string "(warn \"test warning\")")) :to-be-truthy)
  (expect (= 42 (run-string "(progn (warn \"warning\") 42)")) :to-be-truthy))

;;; Format Compilation Tests

(it-sequential "compile-format-nil simple"
  (destructuring-bind (expected form) (list "hello" "(format nil \"~A\" \"hello\")")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-format-nil number"
  (destructuring-bind (expected form) (list "42" "(format nil \"~D\" 42)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-format-nil concat"
  (destructuring-bind (expected form) (list "hello world" "(format nil \"~A ~A\" \"hello\" \"world\")")
    (expect (run-string form) :to-equal expected)))
