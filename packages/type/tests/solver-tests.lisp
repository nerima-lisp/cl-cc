;;;; tests/unit/type/solver-tests.lisp — Constraint Solver Tests
;;;;
;;;; Tests for src/type/solver.lisp:
;;;; solve-constraints and collect-constraints.

(in-package :cl-cc/test)



;;; ─── solve-constraints: equality ───────────────────────────────────────────

(it-sequential "solver-equality-trivial"
  (let* ((v (fresh-type-var :name "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (expect residual :to-be-null)
      (let ((resolved (zonk v new-subst)))
        (expect (type-equal-p type-int resolved) :to-be-truthy)))))

(it-sequential "solver-equality-chain"
  (let* ((v1 (fresh-type-var :name "a"))
         (v2 (fresh-type-var :name "b"))
         (c1 (make-equal-constraint v1 v2))
         (c2 (make-equal-constraint v2 type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c1 c2) s)
      (expect residual :to-be-null)
      (expect (type-equal-p type-int (zonk v1 new-subst)) :to-be-truthy)
      (expect (type-equal-p type-int (zonk v2 new-subst)) :to-be-truthy))))

(it-sequential "solver-bounded-type-var-equality"
  (let* ((number-type (make-type-primitive :name 'number))
         (upper-ok (fresh-type-var :name "a" :upper-bound number-type))
         (upper-bad (fresh-type-var :name "b" :upper-bound number-type))
         (lower-ok (fresh-type-var :name "c" :lower-bound type-int))
         (lower-bad (fresh-type-var :name "d" :lower-bound type-int)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint upper-ok type-int))
         (make-substitution))
      (declare (ignore _subst))
      (expect residual :to-be-null))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint upper-bad type-string))
         (make-substitution))
      (declare (ignore _subst))
      (expect (length residual) :to-equal 1))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint lower-ok type-any))
         (make-substitution))
      (declare (ignore _subst))
      (expect residual :to-be-null))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint lower-bad type-string))
         (make-substitution))
      (declare (ignore _subst))
      (expect (length residual) :to-equal 1))))


(it-sequential "solver-bounded-type-var-propagates-through-var"
  (let* ((number-type (make-type-primitive :name 'number))
         (bounded (fresh-type-var :name "a" :upper-bound number-type))
         (survivor (fresh-type-var :name "b")))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify bounded survivor (make-substitution))
      (expect ok :to-be-truthy)
      (expect (type-equal-p number-type (cl-cc/type:type-var-upper-bound survivor)) :to-be-truthy)
      (multiple-value-bind (_bad bad-ok)
          (cl-cc/type:type-unify survivor type-string subst)
        (declare (ignore _bad))
        (expect bad-ok :to-be-falsy))
      (multiple-value-bind (_good good-ok)
          (cl-cc/type:type-unify survivor type-int subst)
        (declare (ignore _good))
        (expect good-ok :to-be-truthy)))))

(it-sequential "solver-instantiate-preserves-bounded-quantifier"
  (let* ((number-type (make-type-primitive :name 'number))
         (qvar (fresh-type-var :name "a"
                               :upper-bound number-type
                               :lower-bound type-int))
         (scheme (make-type-scheme (list qvar) qvar))
         (instantiated (instantiate scheme)))
    (expect (type-var-p instantiated) :to-be-truthy)
    (expect (type-var-equal-p qvar instantiated) :to-be-falsy)
    (expect (type-equal-p number-type
                               (cl-cc/type:type-var-upper-bound instantiated)) :to-be-truthy)
    (expect (type-equal-p type-int
                               (cl-cc/type:type-var-lower-bound instantiated)) :to-be-truthy)))

(it-sequential "solver-conflicting-equalities-produce-residual"
  (let* ((v  (fresh-type-var :name "a"))
         (c1 (make-equal-constraint v type-int))
         (c2 (make-equal-constraint v type-string))
         (s  (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c1 c2) s)
      (declare (ignore _subst))
      (expect (> (length residual) 0) :to-be-truthy))))

(it-sequential "solver-trivial-equality-has-no-residual"
  (let* ((c (make-equal-constraint type-int type-int))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (declare (ignore _subst))
      (expect residual :to-be-null))))

;;; ─── solve-constraints: subtyping ──────────────────────────────────────────

(it-sequential "solver-binary-constraint-kinds subtype"
  (destructuring-bind (sat-c viol-c) (list (make-subtype-constraint type-int type-any) (make-subtype-constraint type-string type-int))
    (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals sat-c) :to-be-null)
    (expect (length (residuals viol-c)) :to-equal 1))))

(it-sequential "solver-binary-constraint-kinds typeclass"
  (destructuring-bind (sat-c viol-c) (list (make-typeclass-constraint 'eq cl-cc/type:+type-unknown+) (make-typeclass-constraint 'eq (fresh-type-var :name "a")))
    (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals sat-c) :to-be-null)
    (expect (length (residuals viol-c)) :to-equal 1))))

(it-sequential "solver-binary-constraint-kinds effect-subset"
  (destructuring-bind (sat-c viol-c) (list (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+) (make-effect-subset-constraint +io-effect-row+ +pure-effect-row+))
    (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals sat-c) :to-be-null)
    (expect (length (residuals viol-c)) :to-equal 1))))

(it-sequential "solver-binary-constraint-kinds mult-leq"
  (destructuring-bind (sat-c viol-c) (list (make-mult-leq-constraint :zero :omega) (make-mult-leq-constraint :omega :zero))
    (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals sat-c) :to-be-null)
    (expect (length (residuals viol-c)) :to-equal 1))))

(it-sequential "solver-binary-constraint-kinds kind-equal"
  (destructuring-bind (sat-c viol-c) (list (make-kind-equal-constraint +kind-type+ +kind-type+) (make-kind-equal-constraint +kind-type+ +kind-effect+))
    (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals sat-c) :to-be-null)
    (expect (length (residuals viol-c)) :to-equal 1))))

;;; ─── solve-constraints: row-lacks ──────────────────────────────────────────

(it-sequential "solver-row-lacks-constraints"
  (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (expect (residuals (make-row-lacks-constraint (fresh-type-var :name "rho") 'x)) :to-be-null)
    (expect (residuals (make-row-lacks-constraint
                              (make-type-record :fields (list (cons 'y type-int)) :row-var nil) 'x)) :to-be-null)
    (expect (length (residuals (make-row-lacks-constraint
                                         (make-type-record :fields (list (cons 'x type-int)) :row-var nil) 'x))) :to-equal 1)))

;;; ─── solve-constraints: implication ────────────────────────────────────────

(it-sequential "solver-implication-solvable"
  (let* ((v (fresh-type-var :name "a"))
         (given (list (make-equal-constraint v type-int)))
         (wanted (list (make-equal-constraint v type-int)))
         (c (make-implication-constraint (list v) given wanted))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (declare (ignore _subst))
      (expect residual :to-be-null))))

(it-sequential "solver-defaults-numeric-typeclass-vars"
  (let* ((v (fresh-type-var :name "a"))
         (c (make-typeclass-constraint 'num v))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (expect residual :to-be-null)
      (expect (type-equal-p type-int (zonk v new-subst)) :to-be-truthy))))

;;; ─── solve-constraints: empty input ────────────────────────────────────────

(it-sequential "solver-empty-and-nil-subst"
  (multiple-value-bind (subst residual)
      (cl-cc/type:solve-constraints nil nil)
    (expect (substitution-p subst) :to-be-truthy)
    (expect residual :to-be-null))
  (multiple-value-bind (subst residual)
      (cl-cc/type:solve-constraints (list (make-equal-constraint type-int type-int)) nil)
    (expect (substitution-p subst) :to-be-truthy)
    (expect residual :to-be-null)))

;;; ─── solve-constraints: mixed ──────────────────────────────────────────────

(it-sequential "solver-mixed-constraints"
  (let* ((v (fresh-type-var :name "a"))
         (c1 (make-equal-constraint v type-int))            ; solvable
         (c2 (make-subtype-constraint type-string type-int)) ; violated → residual
         (c3 (make-mult-leq-constraint :zero :omega))       ; satisfied
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c1 c2 c3) s)
      (expect (type-equal-p type-int (zonk v new-subst)) :to-be-truthy)
      (expect (length residual) :to-equal 1)
      (expect (constraint-kind (first residual)) :to-be :subtype))))

(it-sequential "solve-constraints-trivial-success"
  (let* ((subst  (cl-cc/type:make-substitution))
         (result (cl-cc/type:solve-constraints nil subst)))
    (expect (cl-cc/type:substitution-p result) :to-be-truthy))
  (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints
       (list (cl-cc/type:make-equal-constraint type-int type-int))
       (cl-cc/type:make-substitution))
    (expect (cl-cc/type:substitution-p new-subst) :to-be-truthy)
    (expect residual :to-be-null)))

(it-sequential "solve-constraints-equal-binds-var"
  (let* ((tvar  (cl-cc/type:fresh-type-var "a"))
         (subst (cl-cc/type:make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint tvar type-int))
         subst)
      (expect residual :to-be-null)
      (let ((bound (cl-cc/type:zonk tvar new-subst)))
        (expect (type-equal-p type-int bound) :to-be-truthy)))))

(it-sequential "solve-constraints-produces-residual equal-mismatch"
  (destructuring-bind (c) (list (cl-cc/type:make-equal-constraint type-int type-string))
    (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints (list c) (cl-cc/type:make-substitution))
    (declare (ignore new-subst))
    (expect (= 1 (length residual)) :to-be-truthy))))

(it-sequential "solve-constraints-produces-residual subtype-violation"
  (destructuring-bind (c) (list (cl-cc/type:make-subtype-constraint type-string type-int))
    (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints (list c) (cl-cc/type:make-substitution))
    (declare (ignore new-subst))
    (expect (= 1 (length residual)) :to-be-truthy))))

(it-sequential "solve-constraints-subtype-ok fixnum<integer"
  (destructuring-bind (sub-name super-name) (list 'fixnum 'integer)
    (let ((t1 (make-type-primitive :name sub-name))
        (t2 (make-type-primitive :name super-name)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-subtype-constraint t1 t2))
         (cl-cc/type:make-substitution))
      (declare (ignore new-subst))
      (expect residual :to-be-null)))))

(it-sequential "solve-constraints-subtype-ok integer<number"
  (destructuring-bind (sub-name super-name) (list 'integer 'number)
    (let ((t1 (make-type-primitive :name sub-name))
        (t2 (make-type-primitive :name super-name)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-subtype-constraint t1 t2))
         (cl-cc/type:make-substitution))
      (declare (ignore new-subst))
      (expect residual :to-be-null)))))

(it-sequential "solve-constraints-subtype-ok float<real"
  (destructuring-bind (sub-name super-name) (list 'float 'real)
    (let ((t1 (make-type-primitive :name sub-name))
        (t2 (make-type-primitive :name super-name)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-subtype-constraint t1 t2))
         (cl-cc/type:make-substitution))
      (declare (ignore new-subst))
      (expect residual :to-be-null)))))

(it-sequential "solve-constraints-multiple-sequential"
  (let* ((ta    (cl-cc/type:fresh-type-var "a"))
         (tb    (cl-cc/type:fresh-type-var "b")))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint ta type-int)
               (cl-cc/type:make-equal-constraint tb ta))
         (cl-cc/type:make-substitution))
      (expect residual :to-be-null)
      (expect (type-equal-p type-int (cl-cc/type:zonk ta new-subst)) :to-be-truthy)
      (expect (type-equal-p type-int (cl-cc/type:zonk tb new-subst)) :to-be-truthy))))
