(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest prolog-data-built-in-handler-specs
  "The built-in predicate table stays data-only and exposes the expected handlers."
  (assert-equal `((,(find-symbol "AND" :cl) cl-cc/prolog::solve-conjunction)
                  (,(find-symbol "OR" :cl) cl-cc/prolog::prolog-or-handler)
                  (,(find-symbol "=" :cl) cl-cc/prolog::prolog-unify-handler)
                  (,(find-symbol "/=" :cl) cl-cc/prolog::prolog-not-unify-handler)
                  (:when cl-cc/prolog::prolog-when-handler))
                cl-cc/prolog::*builtin-predicate-specs*))

(deftest prolog-data-dcg-specs
  "The DCG data tables stay centralized in the shared data file."
  (assert-equal '(:T-RPAREN :T-SEMI :T-EOF)
                cl-cc/prolog::*dcg-sync-tokens*)
  (assert-equal '((cl-cc/prolog::dcg-alt cl-cc/prolog::%dcg-alt)
                  (cl-cc/prolog::dcg-opt cl-cc/prolog::%dcg-opt)
                  (cl-cc/prolog::dcg-star cl-cc/prolog::%dcg-star)
                  (cl-cc/prolog::dcg-plus cl-cc/prolog::%dcg-plus)
                  (cl-cc/prolog::dcg-error-recovery cl-cc/prolog::%dcg-error-recovery)
                  (cl-cc/prolog::dcg-token-match cl-cc/prolog::%dcg-token-match)
                  (cl-cc/prolog::dcg-token-match-value cl-cc/prolog::%dcg-token-match-value))
                cl-cc/prolog::*dcg-builtin-specs*))

(deftest prolog-data-rule-specs
  "Declarative and type-rule specs are assembled from dedicated data groups."
  (assert-equal (append cl-cc/prolog::*prolog-list-rule-specs*
                        cl-cc/prolog::*prolog-environment-rule-specs*)
                cl-cc/prolog::*prolog-declarative-rule-specs*)
  (assert-equal '((integer-type binop (+ - * / mod))
                  (boolean-type cmp   (< > <= >= = /=)))
                cl-cc/prolog::*prolog-type-rule-specs*))

(deftest prolog-builtins-builds-dispatch-table-from-specs
  "The built-in table constructor resolves handler symbols into callable functions."
  (let ((specs '((foo cl-cc/prolog::prolog-unify-handler)))
        (table (make-hash-table :test 'eq)))
    (cl-cc/prolog::register-builtin-specs table specs)
    (let ((handler (gethash 'foo table)))
      (assert-true handler)
      (assert-true (eq (symbol-function 'cl-cc/prolog::prolog-unify-handler)
                       handler)))))

(deftest prolog-register-prolog-rule-uses-active-table
  "register-prolog-rule stores rules in the dynamically bound registry."
  (let ((cl-cc/prolog::*prolog-rules* (make-hash-table :test 'eq)))
    (let ((rule1 (cl-cc/prolog::register-prolog-rule '(foo ?x) '((bar ?x))))
          (rule2 (cl-cc/prolog::register-prolog-rule '(foo ?y) '((baz ?y)))))
      (assert-equal '(foo ?x) (cl-cc/prolog::rule-head rule1))
      (assert-equal '((bar ?x)) (cl-cc/prolog::rule-body rule1))
      (assert-equal '(foo ?y) (cl-cc/prolog::rule-head rule2))
      (assert-equal '((baz ?y)) (cl-cc/prolog::rule-body rule2))
      (let ((rules (gethash 'foo cl-cc/prolog::*prolog-rules*)))
        (assert-equal 2 (length rules))
        (assert-equal '(foo ?y) (cl-cc/prolog::rule-head (first rules)))
        (assert-equal '((baz ?y)) (cl-cc/prolog::rule-body (first rules)))
        (assert-equal '(foo ?x) (cl-cc/prolog::rule-head (second rules)))
        (assert-equal '((bar ?x)) (cl-cc/prolog::rule-body (second rules)))))))

(deftest-each prolog-invalid-predicate-rejections
  "Rules and queries reject invalid predicates instead of silently missing."
  :cases (("nil rule head"       :register nil)
          ("nil predicate head"  :register '(nil ?x))
          ("number predicate"    :register '(1 ?x))
          ("nil query predicate" :query    '(nil ?x))
          ("number query predicate" :query '(1 ?x)))
  (operation form)
  (ecase operation
    (:register
     (assert-signals error
       (cl-cc/prolog::register-prolog-rule form)))
    (:query
     (assert-signals error
       (cl-cc/prolog:query-all form)))))

(deftest-each prolog-eval-lisp-condition-handles-success-and-error
  "eval-lisp-condition evaluates truthy forms and returns NIL on evaluation errors."
  :cases (("success with quoted symbol binding" t '(eq ?x 'foo) (list (cons '?x 'foo)))
          ("success with self-evaluating binding" t '(eql ?x 42) (list (cons '?x 42)))
          ("error"   nil '(/ 1 0)      nil))
  (expected form env)
  (if expected
      (assert-true (cl-cc/prolog::eval-lisp-condition form env))
      (assert-false (cl-cc/prolog::eval-lisp-condition form env))))

(deftest-each prolog-when-handler-gates-body-on-truth
  "prolog-when-handler only runs its continuation for truthy embedded Lisp conditions."
  :cases (("success" t   '(eq ?x 'foo) (list (cons '?x 'foo)))
          ("error"   nil '(/ 1 0)      nil))
  (expected condition env)
  (let ((ran nil))
    (cl-cc/prolog::prolog-when-handler (list condition) env
                                       (lambda (new-env)
                                         (declare (ignore new-env))
                                         (setf ran t)))
    (if expected
        (assert-true ran)
        (assert-false ran))))

(deftest prolog-when-handler-rejects-extra-arguments
  "The :when built-in is unary; extra arguments are programmer errors."
  (assert-prolog-call-arguments-rejected cl-cc/prolog::prolog-when-handler
                                         '(t t)
                                         nil
                                         (lambda (next-env) next-env)))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data stays internal after the data/logic split."
  (assert-eq :internal (nth-value 1 (find-symbol "*PEEPHOLE-RULES*" :cl-cc/prolog)))
  (let ((rules cl-cc/prolog::*peephole-rules*))
    (assert-true (listp rules))
    (assert-true (>= (length rules) 30))
    (assert-true (member '((:const cl-cc/prolog::?src cl-cc/prolog::?val)
                           (:move cl-cc/prolog::?dst cl-cc/prolog::?src)
                           ((:const cl-cc/prolog::?dst cl-cc/prolog::?val)))
                         rules
                         :test #'equal))
    (assert-true (member '((:move cl-cc/prolog::?mid cl-cc/prolog::?src)
                           (:move cl-cc/prolog::?dst cl-cc/prolog::?mid)
                           ((:move cl-cc/prolog::?mid cl-cc/prolog::?src)
                            (:move cl-cc/prolog::?dst cl-cc/prolog::?src)))
                         rules
                         :test #'equal))
    (assert-true (member '((:const cl-cc/prolog::?r cl-cc/prolog::?_v1)
                           (:const cl-cc/prolog::?r cl-cc/prolog::?v2)
                           ((:const cl-cc/prolog::?r cl-cc/prolog::?v2)))
                         rules
                         :test #'equal))))

(deftest prolog-solve-goal-is-internal
  "query-all is the public solver entry point; solve-goal stays internal."
  (assert-eq :internal (nth-value 1 (find-symbol "SOLVE-GOAL" :cl-cc/prolog)))
  (with-prolog-facts ((solver-entry 1))
    (assert-prolog-query-count= '(solver-entry ?x) 1)))

(deftest prolog-symbols-are-not-reexported-by-umbrella-package
  "The Prolog API is owned by :cl-cc/prolog, not the top-level :cl-cc facade."
  (dolist (name '("LOGIC-VAR-P" "UNIFY" "LOGIC-SUBSTITUTE" "DEF-RULE"
                  "QUERY-ALL" "DEF-DCG-RULE" "PHRASE" "PHRASE-ALL"))
    (assert-eq nil (nth-value 1 (find-symbol name :cl-cc)))))
