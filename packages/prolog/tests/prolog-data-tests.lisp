(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest prolog-data-built-in-handler-specs
  "The built-in predicate table stays data-only and exposes the expected handlers."
  (assert-prolog-specs= `((cl-cc/prolog::! cl-cc/prolog::prolog-cut-handler)
                          (,(find-symbol "AND" :cl) cl-cc/prolog::solve-conjunction)
                          (,(find-symbol "OR" :cl) cl-cc/prolog::prolog-or-handler)
                          (,(find-symbol "=" :cl) cl-cc/prolog::prolog-unify-handler)
                          (,(find-symbol "/=" :cl) cl-cc/prolog::prolog-not-unify-handler)
                          (:when cl-cc/prolog::prolog-when-handler))
                        cl-cc/prolog::*builtin-predicate-specs*))

(deftest prolog-data-dcg-specs
  "The DCG data tables stay centralized in the shared data file."
  (assert-prolog-specs= '(:T-RPAREN :T-SEMI :T-EOF)
                        cl-cc/prolog::*dcg-sync-tokens*)
  (assert-prolog-specs= '((cl-cc/prolog::dcg-alt cl-cc/prolog::%dcg-alt)
                          (cl-cc/prolog::dcg-opt cl-cc/prolog::%dcg-opt)
                          (cl-cc/prolog::dcg-star cl-cc/prolog::%dcg-star)
                          (cl-cc/prolog::dcg-plus cl-cc/prolog::%dcg-plus)
                          (cl-cc/prolog::dcg-error-recovery cl-cc/prolog::%dcg-error-recovery)
                          (cl-cc/prolog::dcg-token-match cl-cc/prolog::%dcg-token-match)
                          (cl-cc/prolog::dcg-token-match-value cl-cc/prolog::%dcg-token-match-value))
                        cl-cc/prolog::*dcg-builtin-specs*))

(deftest prolog-data-rule-specs
  "Declarative and type-rule specs are assembled from dedicated data groups."
  (assert-prolog-specs= (append cl-cc/prolog::*prolog-list-rule-specs*
                                cl-cc/prolog::*prolog-environment-rule-specs*)
                        cl-cc/prolog::*prolog-declarative-rule-specs*)
  (assert-prolog-specs= '((integer-type binop (+ - * / mod))
                          (boolean-type cmp   (< > <= >= = /=)))
                        cl-cc/prolog::*prolog-type-rule-specs*))

(deftest prolog-builtins-builds-dispatch-table-from-specs
  "The built-in table constructor resolves handler symbols into callable functions."
  (let ((specs '((foo cl-cc/prolog::prolog-cut-handler)))
        (table (make-hash-table :test 'eq)))
    (cl-cc/prolog::register-builtin-specs table specs)
    (let ((handler (gethash 'foo table)))
      (assert-true handler)
      (assert-eq (symbol-function 'cl-cc/prolog::prolog-cut-handler) handler))))

(deftest prolog-register-prolog-rule-uses-active-table
  "register-prolog-rule stores rules in the dynamically bound registry."
  (let ((cl-cc/prolog::*prolog-rules* (make-hash-table :test 'eq)))
    (let ((rule1 (cl-cc/prolog::register-prolog-rule '(foo ?x) '((bar ?x))))
          (rule2 (cl-cc/prolog::register-prolog-rule '(foo ?y) '((baz ?y)))))
      (assert-prolog-rule= rule1 '(foo ?x) '((bar ?x)))
      (assert-prolog-rule= rule2 '(foo ?y) '((baz ?y)))
      (let ((rules (gethash 'foo cl-cc/prolog::*prolog-rules*)))
        (assert-prolog-rule-store= rules
                                   ((foo ?y) ((baz ?y)))
                                   ((foo ?x) ((bar ?x))))))))

(deftest prolog-subst-for-eval-quotes-bound-symbols
  "subst-for-eval preserves quoted forms and quotes substituted symbols for CL eval."
  (let ((env (list (cons '?x 'foo)
                   (cons '?y 7))))
    (assert-equal '(and 'foo (quote bar) 7)
                  (cl-cc/prolog::subst-for-eval '(and ?x (quote bar) ?y) env))))

(deftest prolog-eval-lisp-condition-handles-success-and-error
  "eval-lisp-condition evaluates truthy forms and returns NIL on evaluation errors."
  (let ((env (list (cons '?x 'foo))))
    (assert-true (cl-cc/prolog::eval-lisp-condition '(eq ?x 'foo) env)))
  (assert-false (cl-cc/prolog::eval-lisp-condition '(/ 1 0) nil)))

(deftest prolog-unify-handler-behavior
  "prolog-unify-handler succeeds only when terms can be unified."
  (let ((results nil))
    (funcall #'cl-cc/prolog::prolog-unify-handler
             '(?x 1)
             nil
             (lambda (env)
               (push env results)))
    (assert-equal '(((?x . 1))) results))
  (let ((results nil))
    (funcall #'cl-cc/prolog::prolog-unify-handler
             '(?x (?x))
             nil
             (lambda (env)
               (push env results)))
    (assert-null results)))

(deftest prolog-when-unify-succeeds-gates-body-on-success
  "when-unify-succeeds only runs its body when unification succeeds."
  (let ((ran nil))
    (cl-cc/prolog::when-unify-succeeds (result '?x 1 nil)
      (setf ran t)
      (assert-equal '((?x . 1)) result))
    (assert-true ran))
  (let ((ran nil))
    (cl-cc/prolog::when-unify-succeeds (result '?x '(?x) nil)
      (setf ran t))
    (assert-false ran)))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data remains available after the data/logic split."
  (assert-true (listp cl-cc/prolog:*peephole-rules*))
  (assert-true (>= (length cl-cc/prolog:*peephole-rules*) 30))
  (assert-true (member '((:const cl-cc/prolog::?src cl-cc/prolog::?val)
                          (:move cl-cc/prolog::?dst cl-cc/prolog::?src)
                          ((:const cl-cc/prolog::?dst cl-cc/prolog::?val)))
                       cl-cc/prolog:*peephole-rules*
                         :test #'equal)))

(deftest-each prolog-logic-var-p
  "logic-var-p recognises ?-prefixed symbols as logic variables."
  :cases (("?x"   '?x   t)
          ("?foo" '?foo t)
          ("x"    'x    nil)
          ("42"   42    nil)
          ("nil"  nil   nil))
  (term expected)
  (if expected
      (assert-true (cl-cc:logic-var-p term))
      (assert-false (cl-cc:logic-var-p term))))

(deftest-each prolog-occurs-check-simple-cases
  "occurs-check: same-variable, nested structure, and absent cases with empty env."
  :cases (("self"   t   '?x '?x    nil)
          ("nested" t   '?x '(?x 1) nil)
          ("absent" nil '?x 42      nil))
  (expected var term env)
  (if expected
      (assert-true (cl-cc/prolog::occurs-check var term env))
      (assert-false (cl-cc/prolog::occurs-check var term env))))

(deftest prolog-occurs-check-via-binding
  "occurs-check: follows binding chain when ?y is bound to ?x."
  (let ((env (list (cons '?y '?x))))
    (assert-true (cl-cc/prolog::occurs-check '?x '?y env))))

(deftest-each prolog-unify-atoms
  "unify: atomic terms unify iff they are equal."
  :cases (("same int" 42 42 nil)
          ("same sym" 'a  'a  nil)
          ("diff ints" 1  2   :unify-fail)
          ("int vs sym" 42 'a :unify-fail))
  (t1 t2 expected)
  (assert-prolog-unify-result= expected t1 t2))

(deftest prolog-unify-variable-binding
  "unify: logic variable binds to atom; occurs-check prevents circular unification."
  (assert-prolog-unify-result= '((?x . 42)) '?x 42)
  (assert-prolog-unify-fails '?x '(?x)))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased."
  (assert-prolog-unify-result= '((?x . ?y)) '?x '?y)
  (with-prolog-unify-result (env '?x 99 '((?x . ?y)))
    (assert-prolog-logic-substitute= '?y 99 env)))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise."
  (assert-prolog-goal-results=
   '(= (?x 2) (1 ?y))
   '((1 2))
   (list ?x ?y)))

(deftest-each prolog-logic-substitute-cases
  "logic-substitute handles atoms, bound and unbound variables correctly."
  :cases (("atom-integer" 42       nil                   42)
          ("atom-symbol"  'hello   nil                   'hello)
          ("bound-var"    '?x      (list (cons '?x 99))  99)
          ("unbound-var"  '?unbound nil                   '?unbound))
  (input env expected)
  (assert-equal expected (cl-cc:logic-substitute input env)))

(deftest prolog-logic-substitute-traverses-structure-and-chains
  "logic-substitute traverses nested cons structure and follows variable chains."
  (with-prolog-unify-result (env '?x '(a b c))
    (assert-equal '((a b c) ?y) (cl-cc:logic-substitute '(?x ?y) env)))
  (with-prolog-unify-result (e1 '?a '?b)
    (with-prolog-unify-result (e2 '?b 7 e1)
      (assert-prolog-logic-substitute= '?a 7 e2))))

(deftest prolog-rename-variables-behavior
  "rename-variables produces fresh symbols distinct from originals and consistent across head/body."
  (let* ((rule1 (cl-cc/prolog::make-prolog-rule :head '(foo ?x ?y)
                                                 :body '((bar ?x ?z))))
         (fresh1 (cl-cc/prolog::rename-variables rule1)))
    (assert-false (eq '?x (second (cl-cc/prolog::rule-head fresh1))))
    (assert-true (cl-cc:logic-var-p (second (cl-cc/prolog::rule-head fresh1)))))
  (let* ((rule2 (cl-cc/prolog::make-prolog-rule :head '(foo ?x)
                                                :body '((bar ?x))))
         (fresh2 (cl-cc/prolog::rename-variables rule2)))
    (assert-eq (second (cl-cc/prolog::rule-head fresh2))
               (second (car (cl-cc/prolog::rule-body fresh2))))))
