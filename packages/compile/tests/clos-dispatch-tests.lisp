;;;; clos-dispatch-tests.lisp — CLOS :default-initargs, :allocation :class, EQL specializers, and method qualifiers
(in-package :cl-cc/test)

;;; ── :default-initargs ──────────────────────────────────────────────────────

(it-sequential "clos-parse-default-initargs"
  (let ((ast (lower-sexp-to-ast '(defclass point ()
                                    ((x :initarg :x :initform 0))
                                    (:default-initargs :x 42)))))
    (expect (typep ast 'ast-defclass) :to-be-truthy)
    (let ((di (cl-cc/ast:ast-defclass-default-initargs ast)))
      (expect (= 1 (length di)) :to-be-truthy)
      (expect (car (first di)) :to-be :x))))

(it-sequential "clos-default-initargs-runtime default-applies"
  (destructuring-bind (expected form) (list 42 "(progn (defclass da-point () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point) 'x))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-default-initargs-runtime explicit-overrides"
  (destructuring-bind (expected form) (list 99 "(progn (defclass da-point2 () ((x :initarg :x :initform 0)) (:default-initargs :x 42)) (slot-value (make-instance 'da-point2 :x 99) 'x))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; ── :allocation :class Tests ────────────────────────────────────────────────

(it-sequential "clos-allocation-class class-slot-shared-read"
  (destructuring-bind (expected form) (list 42 "(defclass counter ()
       ((count :initarg :count :allocation :class)))
     (let ((c1 (make-instance 'counter :count 42))
           (c2 (make-instance 'counter)))
       (slot-value c2 'count))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-allocation-class class-slot-mutation-reads"
  (destructuring-bind (expected form) (list 99 "(defclass shared-box ()
       ((val :initarg :val :allocation :class)))
     (let ((a (make-instance 'shared-box :val 10))
           (b (make-instance 'shared-box)))
       (setf (slot-value a 'val) 99)
       (slot-value b 'val))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-allocation-class mixed-shared-from-second"
  (destructuring-bind (expected form) (list 100 "(defclass mixed ()
       ((shared :initarg :shared :allocation :class)
        (own    :initarg :own)))
     (let ((a (make-instance 'mixed :shared 100 :own 1))
           (b (make-instance 'mixed :own 2)))
       (slot-value b 'shared))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-allocation-class mixed-instance-independent"
  (destructuring-bind (expected form) (list 1 "(defclass mixed2 ()
       ((shared :initarg :shared :allocation :class)
        (own    :initarg :own)))
     (let ((a (make-instance 'mixed2 :shared 100 :own 1))
           (b (make-instance 'mixed2 :own 2)))
       (slot-value a 'own))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; ── EQL Specializer Tests ──────────────────────────────────────────────────

(it-sequential "clos-eql-specializer-value-shared-with-method-body"
  (expect (= 42 (run-string "(defgeneric coin-same (x))
                            (defmethod coin-same ((x (eql 42))) 42)
                            (coin-same 42)"
                           :stdlib t)) :to-be-truthy)
  (expect (= 7 (run-string "(defgeneric coin-diff (x))
                           (defmethod coin-diff ((x (eql 42))) 7)
                           (coin-diff 42)"
                          :stdlib t)) :to-be-truthy))

(it-sequential "clos-eql-specializer penny"
  (destructuring-bind (expected form) (list 1 "(defgeneric coin-value (c))
                               (defmethod coin-value ((c (eql :penny))) 1)
                               (defmethod coin-value ((c (eql :nickel))) 5)
                               (coin-value :penny)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-eql-specializer nickel"
  (destructuring-bind (expected form) (list 5 "(defgeneric coin-val2 (c))
                               (defmethod coin-val2 ((c (eql :penny))) 1)
                               (defmethod coin-val2 ((c (eql :nickel))) 5)
                               (coin-val2 :nickel)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-eql-specializer eql-match"
  (destructuring-bind (expected form) (list 42 "(defgeneric describe-it (x))
                               (defmethod describe-it ((x (eql 42))) 42)
                               (defmethod describe-it ((x integer)) 0)
                               (describe-it 42)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-eql-specializer class-fallbk"
  (destructuring-bind (expected form) (list 0 "(defgeneric describe-it2 (x))
                               (defmethod describe-it2 ((x (eql 42))) 42)
                               (defmethod describe-it2 ((x integer)) 0)
                               (describe-it2 99)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-eql-specializer sym-match"
  (destructuring-bind (expected form) (list 100 "(defgeneric sym-val (s))
                               (defmethod sym-val ((s (eql 'foo))) 100)
                               (defmethod sym-val ((s symbol)) 0)
                               (sym-val 'foo)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "clos-eql-specializer sym-fallbk"
  (destructuring-bind (expected form) (list 0 "(defgeneric sym-val2 (s))
                               (defmethod sym-val2 ((s (eql 'foo))) 100)
                               (defmethod sym-val2 ((s symbol)) 0)
                               (sym-val2 'bar)")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; ── Method Qualifier Tests (:before/:after/:around) ──────────────────────

(it-sequential "clos-defmethod-qualifier-run before"
  :timeout
  15
  (destructuring-bind (expected form) (list "before:primary" "(defvar *bq-log* \"\")
            (defgeneric greet-q (x))
            (defmethod greet-q ((x integer))
              (setf *bq-log* (concatenate 'string *bq-log* \"primary\"))
              *bq-log*)
            (defmethod greet-q :before ((x integer))
              (setf *bq-log* (concatenate 'string *bq-log* \"before:\")))
            (greet-q 1)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-defmethod-qualifier-run before-and-after"
  :timeout
  15
  (destructuring-bind (expected form) (list "B:P:A" "(defvar *ba-log* \"\")
            (defgeneric ba-test (x))
            (defmethod ba-test ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \"P\"))
              *ba-log*)
            (defmethod ba-test :before ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \"B:\")))
            (defmethod ba-test :after ((x integer))
              (setf *ba-log* (concatenate 'string *ba-log* \":A\")))
            (ba-test 1)
            *ba-log*")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-defmethod-qualifier-run around"
  :timeout
  15
  (destructuring-bind (expected form) (list "WRAPPED:42" "(defgeneric around-test (x))
            (defmethod around-test ((x integer))
              42)
            (defmethod around-test :around ((x integer))
              (let ((result (call-next-method)))
                (concatenate 'string \"WRAPPED:\" (write-to-string result))))
            (around-test 1)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-defmethod-qualifier-run around-with-before-after"
  :timeout
  15
  (destructuring-bind (expected form) (list "B:P:A" "(defvar *aba-log* \"\")
            (defgeneric aba-test (x))
            (defmethod aba-test ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \"P\"))
              *aba-log*)
            (defmethod aba-test :before ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \"B:\")))
            (defmethod aba-test :after ((x integer))
              (setf *aba-log* (concatenate 'string *aba-log* \":A\")))
            (defmethod aba-test :around ((x integer))
              (call-next-method)
              *aba-log*)
            (aba-test 1)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "clos-defmethod-after-qualifier"
  :timeout
  15
  (expect (= 42 (run-string
     "(defgeneric aft-test (x))
      (defmethod aft-test ((x integer))
        42)
       (defmethod aft-test :after ((x integer))
         99)
       (aft-test 1)")) :to-be-truthy))

(it-sequential "clos-custom-method-combination-multi-dispatch"
  :timeout
  15
  (let ((result (run-string
                 "(defgeneric combo-test (x y) (:method-combination list))
       (defmethod combo-test list ((x integer) (y integer)) 'ii)
       (defmethod combo-test list ((x integer) (y t)) 'it)
       (defmethod combo-test list ((x t) (y integer)) 'ti)
       (defmethod combo-test list ((x t) (y t)) 'tt)
       (combo-test 1 2)")))
    (expect (mapcar (lambda (s) (string-upcase (symbol-name s))) result) :to-equal '("II" "IT" "TI" "TT"))))

(it-sequential "clos-defmethod-qualifier-parse before"
  (destructuring-bind (form expected-qualifier) (list '(defmethod foo :before ((x integer)) (print x)) :before)
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-defmethod) :to-be-truthy)
    (expect (cl-cc/ast:ast-defmethod-qualifier ast) :to-be expected-qualifier))))

(it-sequential "clos-defmethod-qualifier-parse around"
  (destructuring-bind (form expected-qualifier) (list '(defmethod foo :around ((x integer)) (print x)) :around)
    (let ((ast (lower-sexp-to-ast form)))
    (expect (typep ast 'ast-defmethod) :to-be-truthy)
    (expect (cl-cc/ast:ast-defmethod-qualifier ast) :to-be expected-qualifier))))

(it-sequential "clos-defmethod-around-without-cnm"
  (expect (run-string
     "(defgeneric around-only-test (x))
      (defmethod around-only-test ((x integer))
        \"PRIMARY\")
      (defmethod around-only-test :around ((x integer))
        \"AROUND-ONLY\")
      (around-only-test 1)") :to-equal "AROUND-ONLY"))
