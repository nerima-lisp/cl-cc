;;;; tests/unit/expand/macro-definition-tests.lisp — Macro definition tests

(in-package :cl-cc/test)



(describe-sequential "macro-definition-suite"
  (before-each
    (clrhash cl-cc/expand:*compiler-macro-table*)
    (setf cl-cc/expand:*macro-eval-fn* #'eval))


(it-sequential "defun-macro-structure basic"
  (destructuring-bind (form expected-block) (list '(defun foo (x y) body1 body2) '(block foo body1 body2))
    (let* ((result (our-macroexpand-1 form))
         (lambda-form (third result)))
    (expect 'setf :to-be (car result))
    (expect '(fdefinition 'foo) :to-equal (cadr result))
    (expect 'lambda :to-be (car lambda-form))
    (expect (member expected-block (cddr lambda-form) :test #'equal) :to-be-truthy))))

(it-sequential "defun-macro-structure with-docstring"
  (destructuring-bind (form expected-block) (list '(defun foo (x y) "Docstring" body) '(block foo body))
    (let* ((result (our-macroexpand-1 form))
         (lambda-form (third result)))
    (expect 'setf :to-be (car result))
    (expect '(fdefinition 'foo) :to-equal (cadr result))
    (expect 'lambda :to-be (car lambda-form))
    (expect (member expected-block (cddr lambda-form) :test #'equal) :to-be-truthy))))

(it-sequential "defun-macro-block-follows-docstring-and-declarations"
  (let* ((result (our-macroexpand-1 '(defun foo (x) "Doc" (declare (ignore x)) (return-from foo 1))))
         (lambda-form (third result)))
    (expect (third lambda-form) :to-equal "Doc")
    (expect (fourth lambda-form) :to-equal '(declare (ignore x)))
    (expect (fifth lambda-form) :to-equal '(block foo (return-from foo 1)))))

(it-sequential "define-compiler-macro-returns-name"
  (let ((result (our-macroexpand-1 '(define-compiler-macro foo (x) (+ x 1)))))
    (expect result :to-equal '(quote foo))))

(it-sequential "define-compiler-macro-expands-call"
  (let ((name (gensym "CM-FOO-")))
    (our-macroexpand-1 `(define-compiler-macro ,name (x) (+ x 1)))
    (let ((result (cl-cc/expand:compiler-macroexpand-all `(,name 2))))
      (expect result :to-equal 3))))

(it-sequential "our-defmacro-binds-whole-form"
  (let ((name (gensym "M-WHOLE-")))
    (our-macroexpand-1
     `(our-defmacro ,name (&whole whole x)
        (if (equal whole (list ,name 2))
            (+ x 10)
            whole)))
    (expect (our-macroexpand-1 `(,name 2)) :to-equal '(+ 2 10))))

(it-sequential "invoke-registered-expander-supports-descriptor-backed-compiler-macros"
  (let ((cl-cc/expand:*macro-eval-fn* #'eval)
        (expander (cl-cc/expand::make-compiler-macro-expander '(x) '((+ x 1)))))
    (expect (cl-cc/expand::invoke-registered-expander expander '(foo 2) nil) :to-equal 3)))

(it-sequential "invoke-registered-expander-ignores-descriptor-leading-declarations"
  (let ((cl-cc/expand:*macro-eval-fn* #'eval)
        (expander '(:kind :macro-expander
                    :lambda-list (x)
                    :body ((declare (ignore x))
                           (quote ok)))))
    (expect (cl-cc/expand::invoke-registered-expander expander '(declared 1) nil) :to-equal 'ok)))

(it-sequential "compiler-macro-function-accesses-registered-expander"
  (let ((name (gensym "CMF-"))
        (expander (lambda (form env)
                    (declare (ignore form env))
                    42)))
    (setf (cl-cc/expand::compiler-macro-function name) expander)
    (expect (cl-cc/expand::compiler-macro-function name) :to-be expander)
    (setf (cl-cc/expand::compiler-macro-function name) nil)
    (expect (cl-cc/expand::compiler-macro-function name) :to-be-null)))

(it-sequential "define-compiler-macro-expands-funcall-function-designator"
  (let ((name (gensym "CM-FUNCALL-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&whole form x)
        (if (eq (car form) 'funcall)
            (+ x 10)
            form)))
    (expect (cl-cc/expand:compiler-macroexpand-all
                   `(funcall #',name 2)) :to-equal 12)))

(it-sequential "define-compiler-macro-can-decline-with-whole-form"
  (let ((name (gensym "CM-DECLINE-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&whole form x)
        (if (integerp x)
            (+ x 1)
            form)))
    (expect (cl-cc/expand:compiler-macroexpand-all `(,name 3)) :to-equal 4)
    (expect (cl-cc/expand:compiler-macroexpand-all `(,name a)) :to-equal `(,name a))))

(it-sequential "define-compiler-macro-binds-environment"
  (let ((name (gensym "CM-ENV-")))
    (our-macroexpand-1
     `(define-compiler-macro ,name (&environment env x)
        (if (and (null env) (eql x 1)) :null-env :non-null-env)))
    (expect (cl-cc/expand:compiler-macroexpand-all `(,name 1)) :to-be :null-env)))

(it-sequential "define-compiler-macro-reregisters-after-repeat-expansion"
  (let ((form '(define-compiler-macro cached-cm (x) (+ x 2))))
    (let ((cl-cc/expand:*compiler-macro-table* (make-hash-table :test #'eq)))
      (expect (our-macroexpand-1 form) :to-equal '(quote cached-cm))
      (expect (cl-cc/expand::compiler-macro-function 'cached-cm) :to-be-truthy))
    (let ((cl-cc/expand:*compiler-macro-table* (make-hash-table :test #'eq)))
      (expect (our-macroexpand-1 form) :to-equal '(quote cached-cm))
      (expect (cl-cc/expand::compiler-macro-function 'cached-cm) :to-be-truthy)
      (expect (cl-cc/expand:compiler-macroexpand-all '(cached-cm 3)) :to-equal 5))))

;;; ─── %contains-uninterned-symbol-p ──────────────────────────────────────

(it-sequential "contains-uninterned-symbol-p-cases gensym"
  (destructuring-bind (expected form) (list t (list (gensym "G")))
    (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-falsy)))))

(it-sequential "contains-uninterned-symbol-p-cases normal-symbol"
  (destructuring-bind (expected form) (list nil '(foo bar))
    (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-falsy)))))

(it-sequential "contains-uninterned-symbol-p-cases integer"
  (destructuring-bind (expected form) (list nil 42)
    (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-falsy)))))

(it-sequential "contains-uninterned-symbol-p-cases string"
  (destructuring-bind (expected form) (list nil "hello")
    (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-falsy)))))

(it-sequential "contains-uninterned-symbol-p-cases nested-gensym"
  (destructuring-bind (expected form) (list t nil)
    (let ((test-form (if (eq form nil)
                       (list 'quote (list (gensym "NESTED")))
                       form)))
    (if expected
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%contains-uninterned-symbol-p test-form) :to-be-falsy)))))

;;; ─── %cacheable-macroexpansion-p ─────────────────────────────────────────

(it-sequential "cacheable-macroexpansion-p-cases interned-form"
  (destructuring-bind (expected form) (list t '(+ 1 2))
    (let ((test-form (if (null form)
                       (list (gensym "G") 1 2)
                       form)))
    (if expected
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-falsy)))))

(it-sequential "cacheable-macroexpansion-p-cases keyword-form"
  (destructuring-bind (expected form) (list t '(:x :y))
    (let ((test-form (if (null form)
                       (list (gensym "G") 1 2)
                       form)))
    (if expected
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-falsy)))))

(it-sequential "cacheable-macroexpansion-p-cases gensym-form"
  (destructuring-bind (expected form) (list nil nil)
    (let ((test-form (if (null form)
                       (list (gensym "G") 1 2)
                       form)))
    (if expected
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-truthy)
        (expect (cl-cc/expand::%cacheable-macroexpansion-p test-form) :to-be-falsy)))))

;;; ─── %expander-descriptor-p ──────────────────────────────────────────────

(it-sequential "expander-descriptor-p-cases macro-kind"
  (destructuring-bind (expected object) (list t (list :kind :macro-expander :lambda-list '() :body '()))
    (if expected
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-truthy)
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-falsy))))

(it-sequential "expander-descriptor-p-cases compiler-kind"
  (destructuring-bind (expected object) (list t (list :kind :compiler-macro-expander :lambda-list '() :body '()))
    (if expected
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-truthy)
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-falsy))))

(it-sequential "expander-descriptor-p-cases function"
  (destructuring-bind (expected object) (list nil #'identity)
    (if expected
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-truthy)
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-falsy))))

(it-sequential "expander-descriptor-p-cases bare-list"
  (destructuring-bind (expected object) (list nil '(foo bar))
    (if expected
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-truthy)
      (expect (cl-cc/expand::%expander-descriptor-p object) :to-be-falsy))))

;;; ─── %compiler-macro-lambda-list-parts ──────────────────────────────────

(it-sequential "compiler-macro-lambda-list-plain"
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(x y z))
    (expect ll :to-equal '(x y z))
    (expect whole :to-be-null)
    (expect env :to-be-null)))

(it-sequential "compiler-macro-lambda-list-whole"
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(&whole w x y))
    (expect ll :to-equal '(x y))
    (expect whole :to-be 'w)
    (expect env :to-be-null)))

(it-sequential "compiler-macro-lambda-list-environment"
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(x &environment e))
    (expect ll :to-equal '(x))
    (expect whole :to-be-null)
    (expect env :to-be 'e)))

(it-sequential "compiler-macro-lambda-list-whole-and-environment"
  (multiple-value-bind (ll whole env)
      (cl-cc/expand::%compiler-macro-lambda-list-parts '(&whole w x &environment e y))
    (expect ll :to-equal '(x y))
    (expect whole :to-be 'w)
    (expect env :to-be 'e)))

)
