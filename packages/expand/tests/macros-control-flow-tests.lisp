;;;; tests/unit/expand/macros-control-flow-tests.lisp — Macro control-flow tests

(in-package :cl-cc/test)



(it-sequential "when-macro-expansions multi-body"
  (destructuring-bind (form expected) (list '(when test body1 body2) '(if test (progn body1 body2) nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "when-macro-expansions single-body"
  (destructuring-bind (form expected) (list '(when test body) '(if test (progn body) nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "when-macro-expansions no-body"
  (destructuring-bind (form expected) (list '(when test) '(if test (progn) nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "when-idempotent-expansion"
  :timeout
  5
  (dolist (form '((when t body)
                  (when flag body1 body2)
                  (when (= x 0) (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (expect exp2 :to-equal exp1))))

(it-sequential "unless-idempotent-expansion"
  :timeout
  5
  (dolist (form '((unless t body)
                  (unless flag body1 body2)
                  (unless (= x 0) (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (expect exp2 :to-equal exp1))))

(it-sequential "unless-macro-expansions multi-body"
  (destructuring-bind (form expected) (list '(unless test body1 body2) '(if test nil (progn body1 body2)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "unless-macro-expansions single-body"
  (destructuring-bind (form expected) (list '(unless test body) '(if test nil (progn body)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "unless-macro-expansions no-body"
  (destructuring-bind (form expected) (list '(unless test) '(if test nil (progn)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "cond-macro-simple-expansions empty"
  (destructuring-bind (form expected) (list '(cond) nil)
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "cond-macro-simple-expansions single-expr"
  (destructuring-bind (form expected) (list '(cond (x)) '(or x (cond)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "cond-macro-simple-expansions single-full"
  (destructuring-bind (form expected) (list '(cond (test body)) '(if test (progn body) (cond)))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "cond-macro-multiple-clauses"
  (let ((result (our-macroexpand-1 '(cond (test1 body1) (test2 body2) (t body3)))))
    (expect 'if :to-be (car result))
    (expect 'test1 :to-equal (cadr result))
    (expect 'progn :to-be (caaddr result))
    (expect 'body1 :to-equal (car (cdaddr result)))))

(it-sequential "and-macro-simple-expansions empty"
  (destructuring-bind (form expected) (list '(and) t)
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "and-macro-simple-expansions single-arg"
  (destructuring-bind (form expected) (list '(and x) 'x)
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "and-macro-simple-expansions two-args"
  (destructuring-bind (form expected) (list '(and a b) '(if a (and b) nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "and-macro-simple-expansions multiple-args"
  (destructuring-bind (form expected) (list '(and a b c) '(if a (and b c) nil))
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "and-full-expansion-creates-nested-ifs"
  (let ((result (our-macroexpand-all '(and a b c))))
    (expect (car result) :to-be 'if)
    (expect (cadr result) :to-equal 'a)
    (expect (caaddr result) :to-be 'if)))

(it-sequential "and-idempotent-expansion"
  :timeout
  5
  (dolist (form '((and a b)
                  (and a b c)
                  (and (= x 0) flag (print 1))))
    (let* ((exp1 (our-macroexpand form))
           (exp2 (our-macroexpand exp1)))
      (expect exp2 :to-equal exp1))))

(it-sequential "or-macro-simple-expansions empty"
  (destructuring-bind (form expected) (list '(or) nil)
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "or-macro-simple-expansions single-arg"
  (destructuring-bind (form expected) (list '(or x) 'x)
    (expect expected :to-equal (our-macroexpand-1 form))))

(it-sequential "or-macro-multi-arg-expansion"
  (let ((result (our-macroexpand-1 '(or a b))))
    (expect 'let :to-be (car result))
    (expect (= (length result) 3) :to-be-truthy)
    (expect 'if :to-be (car (caddr result))))
  (let ((result (our-macroexpand-all '(or a b c))))
    (expect 'let :to-be (car result))
    (let ((inner (cadddr (caddr result))))
      (expect 'let :to-be (car inner)))))

(it-sequential "let*-macro-base-cases empty"
  (destructuring-bind (form expected) (list '(let* () body1 body2) '(progn body1 body2))
    (expect (our-macroexpand-1 form) :to-equal expected)))

(it-sequential "let*-macro-base-cases single"
  (destructuring-bind (form expected) (list '(let* ((a 1)) body) '(let ((a 1)) (let* nil body)))
    (expect (our-macroexpand-1 form) :to-equal expected)))

(it-sequential "let*-one-step-nests-remainder-in-let*"
  (let ((result (our-macroexpand-1 '(let* ((a 1) (b a)) body))))
    (expect 'let :to-be (car result))
    (expect '((a 1)) :to-equal (cadr result))
    (expect 'let* :to-be (car (caddr result)))))

(it-sequential "let*-full-expansion-produces-nested-lets"
  (let ((result (our-macroexpand-all '(let* ((a 1) (b a)) body))))
    (expect 'let :to-be (car result))
    (expect '((a 1)) :to-equal (cadr result))
    (expect 'let :to-be (caaddr result))))

(it-sequential "let*-dependency-chain-nests-correctly"
  (let ((result (our-macroexpand-all '(let* ((x 1) (y (+ x 1)) (z (* y 2))) body))))
    (expect 'let :to-be (car result))
    (let ((y-binding (caddr result)))
      (expect 'let :to-be (car y-binding)))))

(it-sequential "prog1-expansion-binds-result-and-returns-it"
  (let ((result (our-macroexpand-1 '(prog1 first-form body1 body2))))
    (expect 'let :to-be (car result))
    (expect (symbolp (caaadr result)) :to-be-truthy)
    (expect 'first-form :to-be (cadr (caadr result)))
    (expect (caaadr result) :to-be (car (last result))))
  (let ((result (our-macroexpand-1 '(prog1 first-form))))
    (expect 'let :to-be (car result))
    (expect (= (length result) 3) :to-be-truthy)))

(it-sequential "prog2-expansion-evaluates-first-then-returns-second"
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body1 body2))))
    (expect 'progn :to-be (car result))
    (expect 'first-form :to-be (cadr result))
    (expect 'let :to-be (car (caddr result)))
    (let ((let-body (caddr result)))
      (expect (caaadr let-body) :to-be (car (last let-body)))))
  (let ((result (our-macroexpand-1 '(prog2 first-form second-form body))))
    (expect 'progn :to-be (car result))
    (expect 'first-form :to-be (cadr result))
    (expect 'let :to-be (caaddr result))))

(it-sequential "defun-c-runtime-contracts"
  :timeout
  10
  (let ((expanded-1
          (our-macroexpand-1
           '(defun/c add1-positive-mcf (x)
              :requires (> x 0)
              :ensures (= result (+ x 1))
              (+ x 1)))))
    (expect (car expanded-1) :to-be 'defun)
    (expect (cadr expanded-1) :to-be 'add1-positive-mcf)
    (expect (caddr expanded-1) :to-equal '(x))
    (expect (some (lambda (form) (and (consp form) (eq (car form) 'unless)))
           (cdddr expanded-1)) :to-be-truthy)
    (expect (some (lambda (form) (and (consp form) (eq (car form) 'let)))
           (cdddr expanded-1)) :to-be-truthy)))

(it-sequential "macroexpansion-memoization-reuses-cached-result"
  (let ((count 0)
        (name (gensym "CACHE-TEST-")))
    (cl-cc/expand:register-macro name
                           (lambda (form env)
                             (declare (ignore form env))
                             (incf count)
                             '(+ 1 2)))
    (let ((form (list name 'x)))
      (expect (our-macroexpand-all form nil) :to-equal '(+ 1 2))
      (expect (our-macroexpand-all form nil) :to-equal '(+ 1 2))
      (expect (<= count 2) :to-be-truthy))))
