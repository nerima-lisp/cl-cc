(in-package :cl-cc/test)


;;; Nested Destructuring-Bind Tests

(it-sequential "destructuring-bind-nested nested"
  (destructuring-bind (expected form) (list 10 "(destructuring-bind (a (b c) d) (list 1 (list 2 3) 4) (+ a b c d))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "destructuring-bind-nested deep-nested"
  (destructuring-bind (expected form) (list 15 "(destructuring-bind (a (b (c d)) e) (list 1 (list 2 (list 3 4)) 5) (+ a b c d e))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

;;; Variadic Append/Nconc Tests

(it-sequential "append-variadic three-args"
  (destructuring-bind (expected form) (list '(1 2 3 4 5) "(append (list 1 2) (list 3 4) (list 5))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "append-variadic zero-args"
  (destructuring-bind (expected form) (list nil "(append)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "append-variadic one-arg"
  (destructuring-bind (expected form) (list '(1 2 3) "(append (list 1 2 3))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "append-variadic nconc"
  (destructuring-bind (expected form) (list '(1 2 3 4 5 6) "(nconc (list 1 2) (list 3 4) (list 5 6))")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "self-host-stack-compiler"
  (expect (= 17 (run-string *self-host-stack-compiler-program* :stdlib t)) :to-be-truthy))

;;; Consp Fix / Type Predicates Tests

(it-sequential "consp-and consp-list"
  (destructuring-bind (form verify) (list "(consp (list 1 2))" (lambda (result)
             (expect result :to-be-truthy)))
    (funcall verify (run-string form))))

(it-sequential "consp-and consp-int"
  (destructuring-bind (form verify) (list "(consp 42)" (lambda (result)
             (expect result :to-equal nil)))
    (funcall verify (run-string form))))

(it-sequential "consp-and and-consp"
  (destructuring-bind (form verify) (list "(and (consp (list 1)) 42)" (lambda (result)
             (expect result :to-equal 42)))
    (funcall verify (run-string form))))

(it-sequential "mini-compiler-self-host"
  (expect (run-string "(defun my-compile (expr) (cond ((integerp expr) (list :const expr)) ((and (consp expr) (eq (car expr) (quote +))) (list :add (my-compile (second expr)) (my-compile (third expr)))) (t (list :unknown expr)))) (my-compile (quote (+ 1 2)))" :stdlib t) :to-equal '(:ADD (:CONST 1) (:CONST 2))))

;;; Funcall/Apply with Quoted Symbols Tests

(it-sequential "funcall-apply-quoted funcall-builtin"
  (destructuring-bind (expected form) (list 7 "(funcall (quote +) 3 4)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted funcall-user"
  (destructuring-bind (expected form) (list 7 "(defun my-add2 (a b) (+ a b)) (funcall (quote my-add2) 3 4)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted apply-builtin"
  (destructuring-bind (expected form) (list 6 "(apply (quote +) (list 1 2 3))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted apply-user"
  (destructuring-bind (expected form) (list 7 "(defun my-add3 (a b) (+ a b)) (apply (quote my-add3) (list 3 4))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted apply-lambda"
  (destructuring-bind (expected form) (list 6 "(apply (lambda (a b c) (+ a b c)) (list 1 2 3))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted funcall-function-lambda"
  (destructuring-bind (expected form) (list 42 "(funcall #'(lambda (x) (+ x 1)) 41)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "funcall-apply-quoted funcall-hash-ref"
  (destructuring-bind (expected form) (list 7 "(funcall #'+ 3 4)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

;;; Maphash Tests

(it-sequential "maphash-collect-values"
  (let ((result (run-string "(let ((result nil))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (maphash (lambda (k v) (setq result (cons v result))) ht)
    result))")))
    (expect (listp result) :to-be-truthy)
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (null (set-difference result (list 1 2))) :to-be-truthy)))

(it-sequential "maphash-behavior"
  (expect (null (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :x ht) 10)
  (maphash (lambda (k v) v) ht))")) :to-be-truthy)
  (expect (null (run-string "(let ((ht (make-hash-table)))
  (maphash (lambda (k v) k) ht))")) :to-be-truthy)
  (expect (= 3 (run-string "(let ((count 0))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (maphash (lambda (k v) (setq count (+ count 1))) ht)
    count))")) :to-be-truthy))

;;; Capture-by-Reference Tests

(it-sequential "capture-by-ref counter"
  (destructuring-bind (expected form) (list 3 "(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1)) count)))
    (funcall inc) (funcall inc) (funcall inc)))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "capture-by-ref shared-state"
  (destructuring-bind (expected form) (list 42 "(let ((x 0))
  (defun get-x4 () x)
  (defun set-x4 (v) (setq x v))
  (set-x4 42)
  (get-x4))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "capture-by-ref accumulator"
  (destructuring-bind (expected form) (list 10 "(let ((sum 0))
  (let ((add (lambda (n) (setq sum (+ sum n)) sum)))
    (funcall add 1) (funcall add 2) (funcall add 3) (funcall add 4)))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

;;; File I/O Tests

(it-sequential "file-io"
  (let* ((tmp-dir  (uiop:ensure-directory-pathname (uiop:temporary-directory)))
         (wr-path  (namestring (merge-pathnames "cl-cc-test-wr.txt" tmp-dir)))
         (wof-path (namestring (merge-pathnames "cl-cc-test-wof2.txt" tmp-dir)))
         (rd-path  (namestring (merge-pathnames "cl-cc-test-rd.txt" tmp-dir))))
    ;; write-char + read-char via open/close
    (let ((result (run-string
                   (format nil "(let ((h (open ~S :direction :output)))
  (write-char #\\H h)
  (write-char #\\i h)
  (close h)
  (let ((h2 (open ~S :direction :input)))
    (let ((c1 (read-char h2)))
      (let ((c2 (read-char h2)))
        (close h2)
        (list c1 c2)))))" wr-path wr-path))))
      (expect '(#\H #\i) :to-equal result))
    ;; explicit open/close again (stable backend path)
    (let ((result (run-string
                   (format nil "(let ((out (open ~S :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (write-char #\\X out)
  (close out)
  (let ((in (open ~S :direction :input)))
    (let ((ch (read-char in)))
      (close in)
      ch)))" wof-path wof-path))))
      (expect #\X :to-be result))
    ;; read-from-string
    (let ((result (run-string "(read-from-string \"(+ 1 2)\")")))
      (expect (listp result) :to-be-truthy)
      (expect (= 3 (length result)) :to-be-truthy))
    (expect (stringp rd-path) :to-be-truthy)))

;;; Setf Accessor Tests

(it-sequential "setf-defstruct accessor"
  (destructuring-bind (expected form) (list 42 "(defstruct my-cell (value 0))
(let ((c (make-my-cell)))
  (setf (my-cell-value c) 42)
  (my-cell-value c))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "setf-defstruct counter"
  (destructuring-bind (expected form) (list 3 "(defstruct my-counter2 (n 0))
(let ((c (make-my-counter2)))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (my-counter2-n c))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; Self-Hosting Pattern Tests

(it-sequential "self-host-compiler-context"
  (expect (run-string "
(defstruct compiler-ctx2 (counter 0) (instructions nil))
(defun make-reg2 (ctx) (let ((n (compiler-ctx2-counter ctx))) (setf (compiler-ctx2-counter ctx) (+ n 1)) n))
(defun emit-inst2 (ctx inst) (setf (compiler-ctx2-instructions ctx) (cons inst (compiler-ctx2-instructions ctx))))
(let ((ctx (make-compiler-ctx2))) (let ((r1 (make-reg2 ctx)) (r2 (make-reg2 ctx)) (r3 (make-reg2 ctx))) (emit-inst2 ctx (list :const r1 42)) (emit-inst2 ctx (list :const r2 10)) (emit-inst2 ctx (list :add r3 r1 r2)) (list (compiler-ctx2-counter ctx) (length (compiler-ctx2-instructions ctx)) r1 r2 r3)))" :stdlib t) :to-equal '(3 3 0 1 2)))

(it-sequential "self-host-clos-ast-eval"
  (expect (= 42 (run-string "
(defclass eval-int () ((value :initarg :value :reader eval-value)))
(defclass eval-binop () ((op :initarg :op :reader eval-op) (lhs :initarg :lhs :reader eval-lhs) (rhs :initarg :rhs :reader eval-rhs)))
(defgeneric eval-node (node))
(defmethod eval-node ((node eval-int)) (eval-value node))
(defmethod eval-node ((node eval-binop)) (let ((l (eval-node (eval-lhs node))) (r (eval-node (eval-rhs node)))) (if (eq (eval-op node) :add) (+ l r) (* l r))))
(let ((tree (make-instance 'eval-binop :op :add :lhs (make-instance 'eval-int :value 30) :rhs (make-instance 'eval-binop :op :mul :lhs (make-instance 'eval-int :value 4) :rhs (make-instance 'eval-int :value 3))))) (eval-node tree))")) :to-be-truthy))

;;; Labels Mutual Recursion Tests

(it-sequential "labels-mutual-recursion odd-3"
  (destructuring-bind (expected form) (list nil "(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1)))) (odd-p (n) (if (= n 0) nil (even-p (- n 1))))) (even-p 3))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "labels-mutual-recursion three-fns"
  (destructuring-bind (expected form) (list 6 "(labels ((a (n) (if (= n 0) 0 (+ 1 (b (- n 1))))) (b (n) (if (= n 0) 0 (+ 1 (c (- n 1))))) (c (n) (if (= n 0) 0 (+ 1 (a (- n 1)))))) (a 6))")
    (expect (run-string form) :to-equal expected)))

;;; Hash Table :test Parameter Tests

(it-sequential "ht-test-parameter equal-quote"
  (destructuring-bind (expected form) (list 42 "(let ((ht (make-hash-table :test 'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "ht-test-parameter equal-sharp-quote"
  (destructuring-bind (expected form) (list 42 "(let ((ht (make-hash-table :test #'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "ht-test-parameter eql-int-keys"
  (destructuring-bind (expected form) (list 99 "(let ((ht (make-hash-table))) (setf (gethash 1 ht) 99) (gethash 1 ht))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; New Builtin Tests (type-of, make-list, alphanumericp, prin1-to-string)

(it-sequential "builtin-type-of integer"
  (destructuring-bind (expected form) (list 'fixnum "(type-of 42)")
    (expect (run-string form) :to-be expected)))

(it-sequential "builtin-type-of string"
  (destructuring-bind (expected form) (list 'string "(type-of \"hello\")")
    (expect (run-string form) :to-be expected)))

(it-sequential "builtin-type-of cons"
  (destructuring-bind (expected form) (list 'cons "(type-of '(1 2))")
    (expect (run-string form) :to-be expected)))

(it-sequential "builtin-make-list three"
  (destructuring-bind (expected form) (list '(nil nil nil) "(make-list 3)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "builtin-make-list zero"
  (destructuring-bind (expected form) (list nil "(make-list 0)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "builtin-alphanumericp alpha"
  (destructuring-bind (form expected) (list "(alphanumericp #\\a)" t)
    (expect (not (zerop (run-string form))) :to-equal expected)))

(it-sequential "builtin-alphanumericp digit"
  (destructuring-bind (form expected) (list "(alphanumericp #\\5)" t)
    (expect (not (zerop (run-string form))) :to-equal expected)))

(it-sequential "builtin-alphanumericp punct"
  (destructuring-bind (form expected) (list "(alphanumericp #\\!)" nil)
    (expect (not (zerop (run-string form))) :to-equal expected)))

(it-sequential "builtin-print-to-string prin1"
  (destructuring-bind (form) (list "(prin1-to-string 42)")
    (expect (stringp (run-string form)) :to-be-truthy)))

(it-sequential "builtin-print-to-string princ"
  (destructuring-bind (form) (list "(princ-to-string 42)")
    (expect (stringp (run-string form)) :to-be-truthy)))
