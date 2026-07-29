;;;; tests/e2e/selfhost-tests.lisp — Self-Hosting End-to-End Tests
;;;; Demonstrates that cl-cc can compile and run significant pieces of its own
;;;; compiler infrastructure: CPS transformer, optimizer, macro expander, etc.

(in-package :cl-cc/test)

(defmacro with-selfhost-warning-tolerance (&body body)
  `(handler-bind ((warning #'muffle-warning))
     ,@body))


(defbefore :each (selfhost-suite)
  (setf cl-cc:*macro-eval-fn* #'cl-cc::our-eval))


;;; ─── REPL State Tests ──────────────────────────────────────────────────────

(it-sequential "selfhost-defvar-persists"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
       "(defvar *sh-counter* 100)"
       "(setq *sh-counter* 200)"
       "*sh-counter*") :to-be 200)))

(it-sequential "selfhost-label-isolation"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
       "(defun sh-pred (x) (or (numberp x) (symbolp x)))"
       "(defun sh-check (x y) (if (sh-pred x) :yes :no))"
       "(sh-check 42 'ignored)") :to-be :yes)))

;;; ─── Self-Hosting: CPS Transformer ─────────────────────────────────────────


(it-sequential "selfhost-quasiquote defun-builds-form"
  (destructuring-bind (expected setup-form eval-form) (list 9 "(defun make-mul (a b) `(* ,a ,b))" "(let ((form (make-mul 3 3))) (eval form))")
    (with-selfhost-warning-tolerance
    (expect (equal expected
             (run-repl-forms setup-form eval-form)) :to-be-truthy))))

(it-sequential "selfhost-quasiquote defun-builds-binding-form"
  (destructuring-bind (expected setup-form eval-form) (list t "(defun wrap-in-let (var val body) `(let ((,var ,val)) ,body))" "(equal (wrap-in-let 'x 5 '(+ x 1)) '(let ((x 5)) (+ x 1)))")
    (with-selfhost-warning-tolerance
    (expect (equal expected
             (run-repl-forms setup-form eval-form)) :to-be-truthy))))

(it-sequential "selfhost-cps-transformer"
  (with-selfhost-warning-tolerance
    (let ((r (run-repl-forms
              "(defun sh-cps-atom-p (x)
              (or (numberp x) (symbolp x) (stringp x)))"
              "(defun sh-cps (expr k)
               (cond
                 ((sh-cps-atom-p expr) `(funcall ,k ,expr))
                  ((eq (car expr) 'if)
                  (let ((tv (gensym \"T\"))
                        (then-r (sh-cps (caddr expr) k))
                        (else-r (sh-cps (cadddr expr) k)))
                    (sh-cps (cadr expr)
                             `(lambda (,tv) (if ,tv ,then-r ,else-r)))))
                 (t `(funcall ,k ,expr))))"
              "(sh-cps '(if x 1 2) '(lambda (v) v))")))
      (expect (and (consp r) (eq (car r) 'funcall)) :to-be-truthy))))

(it-sequential "selfhost-cps-transformer-arithmetic add-1-2"
  (destructuring-bind (expected expr) (list 3 '(+ 1 2))
    (with-selfhost-warning-tolerance
    (expect (= expected (run-repl-forms
               "(defun sh-cps-run (expr)
                 (cond
                   ((integerp expr) expr)
                   ((symbolp expr) expr)
                  ((consp expr)
                   (case (car expr)
                     (+ (+ (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (- (- (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (* (* (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (otherwise (error \"Unsupported\"))))
                  (t (error \"Unsupported\"))))"
               (format nil "(sh-cps-run '~S)" expr))) :to-be-truthy))))

(it-sequential "selfhost-cps-transformer-arithmetic mul-6-7"
  (destructuring-bind (expected expr) (list 42 '(* 6 7))
    (with-selfhost-warning-tolerance
    (expect (= expected (run-repl-forms
               "(defun sh-cps-run (expr)
                 (cond
                   ((integerp expr) expr)
                   ((symbolp expr) expr)
                  ((consp expr)
                   (case (car expr)
                     (+ (+ (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (- (- (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (* (* (sh-cps-run (second expr)) (sh-cps-run (third expr))))
                     (otherwise (error \"Unsupported\"))))
                  (t (error \"Unsupported\"))))"
               (format nil "(sh-cps-run '~S)" expr))) :to-be-truthy))))

;;; ─── Self-Hosting: Optimizer Pattern Matcher ───────────────────────────────

(it-sequential "selfhost-optimizer-fold"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
        "(defun sh-fold (op a b)
         (cond
           ((and (eq op '+) (numberp a) (numberp b)) (+ a b))
           ((and (eq op '+) (eql a 0)) b)
           ((and (eq op '+) (eql b 0)) a)
           (t (list op a b))))"
        "(sh-fold '+ 3 4)") :to-be 7)))

;;; ─── Self-Hosting: Macro Code Generation ───────────────────────────────────

(it-sequential "selfhost-macro-codegen"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
        "(defmacro sh-def-record (name &rest fields)
         `(progn
            (defun ,(intern (format nil \"MAKE-~A\" name)) (&rest args)
              args)
           (defun ,(intern (format nil \"~A-REF\" name)) (obj field)
             (getf obj field))))"
      "(sh-def-record sh-person :name :age)"
        "(let ((p (make-sh-person :name \"Alice\" :age 30)))
           (sh-person-ref p :age))") :to-be 30)))

;;; ─── Self-Hosting: Recursive Data Processing ──────────────────────────────


(it-sequential "selfhost-tree-walk"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
       "(defun sh-tree-sum (tree)
         (if (numberp tree)
             tree
             (+ (sh-tree-sum (car tree))
                (sh-tree-sum (cdr tree)))))"
       "(sh-tree-sum '(1 . (2 . (3 . 4))))") :to-be 10)))

;;; ─── Self-Hosting: Load File ───────────────────────────────────────────────

(it-sequential "selfhost-load-multi-form"
  (let ((tmpfile (format nil "/tmp/cl-cc-selfhost-~A.lisp" (get-universal-time))))
    (unwind-protect
         (progn
           (with-open-file (s tmpfile :direction :output :if-exists :supersede)
             (write-string "(defvar *sh-base* 100)
(defun sh-offset (n) (+ *sh-base* n))" s))
            (with-selfhost-warning-tolerance
              (expect (run-repl-forms
                 (format nil "(load ~S)" tmpfile)
                 "(sh-offset 42)") :to-be 142)))
      (ignore-errors (delete-file tmpfile)))))

;;; ─── Self-Hosting: Higher-Order Functions ──────────────────────────────────

(it-sequential "selfhost-hof-pipeline"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
       "(defun sh-compose (f g) (lambda (x) (funcall f (funcall g x))))"
       "(defun sh-add1 (x) (+ x 1))"
       "(defun sh-double (x) (* x 2))"
       "(funcall (sh-compose (lambda (x) (sh-add1 x)) (lambda (x) (sh-double x))) 10)") :to-be 21)))

;;; ─── Self-Hosting: Handler-Case with Recovery ─────────────────────────────

(it-sequential "selfhost-error-recovery"
  (expect (run-string "(handler-case
                   (progn (error \"oops\") 0)
                   (error (e) 42))") :to-be 42))

;;; ─── Self-Hosting: defstruct roundtrip ─────────────────────────────────────

(it-sequential "selfhost-defstruct-roundtrip"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
        "(defstruct sh-point x y)"
        "(let ((p (make-sh-point :x 3 :y 4)))
           (sh-point-y p))") :to-be 4)))

;;; ─── Self-Hosting: Mutual Recursion via labels ─────────────────────────────

(it-sequential "selfhost-mutual-recursion"
  (with-selfhost-warning-tolerance
    (expect (run-repl-forms
       "(labels ((is-even (n) (if (= n 0) t (is-odd (- n 1))))
                (is-odd (n) (if (= n 0) nil (is-even (- n 1)))))
         (is-even 10))") :to-be-truthy)))

;;; ─── Self-Hosting: Reader Macros ─────────────────────────────────────────

(it-sequential "selfhost-reader-macros"
  (expect (run-string "(symbolp (quote #:foo))") :to-be-truthy)
  (%with-selfhost-features
    (expect (run-string "#+cl-cc-self-hosting :yes") :to-be :yes))
  (expect (run-string "#-nonexistent-feature :yes") :to-be :yes)
  (expect (run-string "(progn #+nonexistent-feature :no :fallback)") :to-be :fallback)
  (expect (run-string "(+ 1 #.(+ 2 3))") :to-be 6))

(it-sequential "selfhost-read-eval-respects-special"
  (expect (null
    (ignore-errors
      (run-string "(let ((*read-eval* nil)) (read-from-string \"#.(+ 2 3)\"))"))) :to-be-truthy))
