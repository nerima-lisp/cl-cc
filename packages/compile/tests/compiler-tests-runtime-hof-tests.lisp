;;;; tests/integration/compiler-tests-runtime-hof-tests.lisp — HOF and Control Tests
;;;;
;;;; Continuation of compiler-tests-runtime-string-tests.lisp.
;;;; Tests for higher-order functions, ignore-errors, unwind-protect,
;;;; self-host CLOS, copy-hash-table, and cxr compositions.

(in-package :cl-cc/test)

(it-sequential "stdlib-hof-basic mapcar"
  (destructuring-bind (expected form) (list '(2 4 6) "(mapcar (lambda (x) (* x 2)) '(1 2 3))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "stdlib-hof-basic reduce"
  (destructuring-bind (expected form) (list 15 "(reduce #'+ '(1 2 3 4 5) :initial-value 0)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "stdlib-hof-basic remove-if"
  (destructuring-bind (expected form) (list '(2 4) "(remove-if #'oddp '(1 2 3 4 5))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "remove-if-macro-no-stdlib remove-if"
  (destructuring-bind (expected form) (list '(1 3) "(remove-if #'evenp '(1 2 3 4))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "remove-if-macro-no-stdlib remove-if-not"
  (destructuring-bind (expected form) (list '(2 4) "(remove-if-not #'evenp '(1 2 3 4))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "remove-if-macro-no-stdlib remove-if-not-key"
  (destructuring-bind (expected form) (list '(1 3) "(remove-if-not #'evenp '(1 2 3 4) :key #'1+)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "remove-if-macro-no-stdlib remove-if-empty"
  (destructuring-bind (expected form) (list nil "(remove-if #'plusp '(-1 -2))")
    (expect (run-string form) :to-equal expected)))

;;; Let Alias Fix and Prog1/Prog2 Tests

(it-sequential "let-no-alias-and-prog1-prog2 let-simple"
  (destructuring-bind (expected form) (list 0 "(let ((x 0)) (let ((y x)) (setq x 10) y))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "let-no-alias-and-prog1-prog2 let-nested"
  (destructuring-bind (expected form) (list 5 "(let ((a 5)) (let ((b a)) (let ((c b)) (setq a 99) c)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "let-no-alias-and-prog1-prog2 prog1-basic"
  (destructuring-bind (expected form) (list 42 "(prog1 42 (+ 1 2))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "let-no-alias-and-prog1-prog2 prog1-side-eff"
  (destructuring-bind (expected form) (list 0 "(let ((x 0)) (prog1 x (setq x 10)))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "let-no-alias-and-prog1-prog2 prog2"
  (destructuring-bind (expected form) (list 42 "(prog2 1 42 3)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-ignore-errors success"
  (destructuring-bind (expected form) (list 3 "(ignore-errors (+ 1 2))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-ignore-errors failure"
  (destructuring-bind (expected form) (list nil "(ignore-errors (error \"boom\"))")
    (expect (run-string form) :to-equal expected)))

;;; Unwind-Protect Integration Tests

(it-sequential "unwind-protect-cleanup-visible"
  (expect (run-string "
(let ((cleaned nil)) (handler-case (unwind-protect (error \"boom\") (setf cleaned t)) (error (e) cleaned)))") :to-be t))

(it-sequential "handler-case-as-sole-top-level-form"
  (expect (run-string "(handler-case (error \"x\") (error (e) :caught))") :to-equal :caught)
  (expect (run-string "(handler-case (error \"x\") (error (e) (progn :caught)))") :to-equal :caught)
  (expect (run-string "(+ 2 (handler-case (error \"x\") (error (e) 3)))") :to-equal 5)
  (expect (run-string "(ignore-errors (error \"y\"))") :to-be nil)
  (expect (= 7 (run-string "(handler-case (+ 3 4) (error (e) 0))")) :to-be-truthy))

;;; Self-Hosting CLOS Compiler Test

(it-sequential "self-host-clos-compiler"
  (expect (run-string *self-host-clos-compiler-program* :stdlib t) :to-equal '(:R2 3)))

;;; Hash Table Extended Builtins Tests

(it-sequential "compile-hash-table-values"
  (let ((result (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 1)
  (setf (gethash :b ht) 2)
  (hash-table-values ht))")))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (null (set-difference result '(1 2))) :to-be-truthy)))

(it-sequential "compile-hash-table-test default-eql"
  (destructuring-bind (expected form) (list 'eql "(hash-table-test (make-hash-table))")
    (expect (run-string form) :to-be expected)))

(it-sequential "compile-hash-table-test explicit-equal"
  (destructuring-bind (expected form) (list 'equal "(hash-table-test (make-hash-table :test 'equal))")
    (expect (run-string form) :to-be expected)))

(it-sequential "compile-copy-hash-table"
  (expect (= 42 (run-string "
(let ((ht (make-hash-table))) (setf (gethash :a ht) 42) (let ((ht2 (copy-hash-table ht))) (setf (gethash :a ht) 99) (gethash :a ht2)))")) :to-be-truthy))

;;; Car/Cdr Composition and List Accessor Tests

(it-sequential "cxr-compositions cadddr"
  (destructuring-bind (expected form) (list "d" "(string-downcase (symbol-name (cadddr '(a b c d e))))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "cxr-compositions caadr"
  (destructuring-bind (expected form) (list "x" "(string-downcase (symbol-name (caadr '(a (x y) c))))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "cxr-compositions caddar"
  (destructuring-bind (expected form) (list 3 "(caddar '((1 2 3) b c))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "subseq-sequences list"
  (destructuring-bind (expected form) (list '(1 2) "(subseq '(1 2 3 4) 0 2)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "subseq-sequences list-rest"
  (destructuring-bind (expected form) (list '(2 3 4) "(subseq '(1 2 3 4) 1)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "subseq-sequences string"
  (destructuring-bind (expected form) (list "he" "(subseq \"hello\" 0 2)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "subseq-sequences butlast"
  (destructuring-bind (expected form) (list '(1 2) "(butlast '(1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "subseq-sequences butlast-n"
  (destructuring-bind (expected form) (list '(1 2) "(butlast '(1 2 3 4) 2)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "subseq-vector-runtime"
  (expect (= 3 (run-string "(length (subseq (vector 1 2 3 4 5) 1 4))")) :to-be-truthy))

;;; COPY-SEQ / COW-Wrapper Print Tests

(it-sequential "copy-seq-sequences list"
  (destructuring-bind (expected form) (list '(1 2 3) "(copy-seq '(1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "copy-seq-sequences symbols"
  (destructuring-bind (expected form) (list '(a b c) "(copy-seq '(a b c))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "copy-seq-sequences string"
  (destructuring-bind (expected form) (list "hello" "(copy-seq \"hello\")")
    (expect (run-string form) :to-equal expected)))

(it-sequential "copy-seq-sequences copy-list"
  (destructuring-bind (expected form) (list '(1 2 3) "(copy-list '(1 2 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "copy-seq-vector-independent"
  (expect (= 1 (run-string
               "(let* ((o (vector 1 2 3)) (c (copy-seq o))) (setf (aref c 0) 99) (aref o 0))")) :to-be-truthy))

(it-sequential "copy-seq-nested-wrapper-prints"
  (expect (run-string "(list (copy-seq (vector 1 2)) 9)") :to-equal '(#(1 2) 9)))

;;; TYPECASE Dispatch Tests

(it-sequential "typecase-dispatch integer"
  (destructuring-bind (expected form) (list "i" "(typecase 5 (string \"s\") (integer \"i\") (cons \"c\") (t \"o\"))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typecase-dispatch string"
  (destructuring-bind (expected form) (list "s" "(typecase \"hi\" (string \"s\") (integer \"i\") (cons \"c\") (t \"o\"))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typecase-dispatch cons"
  (destructuring-bind (expected form) (list "c" "(typecase '(1 2) (string \"s\") (integer \"i\") (cons \"c\") (t \"o\"))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "typecase-dispatch default"
  (destructuring-bind (expected form) (list "o" "(typecase 3.5 (string \"s\") (integer \"i\") (cons \"c\") (t \"o\"))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "etypecase-dispatch-runtime"
  (expect (run-string "(etypecase 42 (string \"S\") (integer \"I\"))") :to-equal "I"))
