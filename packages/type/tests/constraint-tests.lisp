;;;; tests/unit/type/constraint-tests.lisp — Constraint Language Tests
;;;;
;;;; Tests for src/type/constraint.lisp:
;;;; Smart constructors, constraint-free-vars, constraint-substitute.

(in-package :cl-cc/test)


;;; ─── Smart constructors ────────────────────────────────────────────────────

(it-sequential "constraint-equal-creation"
  (let ((c (make-equal-constraint type-int type-string)))
    (expect (constraint-p c) :to-be-truthy)
    (expect (constraint-kind c) :to-be :equal)
    (expect (length (constraint-args c)) :to-equal 2)
    (expect (type-equal-p type-int (first (constraint-args c))) :to-be-truthy)
    (expect (type-equal-p type-string (second (constraint-args c))) :to-be-truthy)))

(it-sequential "constraint-subtype-creation"
  (let ((c (make-subtype-constraint type-int type-any)))
    (expect (constraint-kind c) :to-be :subtype)
    (expect (type-equal-p type-int (first (constraint-args c))) :to-be-truthy)))

(it-sequential "constraint-typeclass-creation"
  (let ((c (make-typeclass-constraint 'num type-int)))
    (expect (constraint-kind c) :to-be :typeclass)
    (expect (first (constraint-args c)) :to-be 'num)
    (expect (type-equal-p type-int (second (constraint-args c))) :to-be-truthy)))

(it-sequential "constraint-implication-creation"
  (let* ((v (fresh-type-var :name "a"))
         (given (list (make-equal-constraint v type-int)))
         (wanted (list (make-subtype-constraint v type-any)))
         (c (make-implication-constraint (list v) given wanted)))
    (expect (constraint-kind c) :to-be :implication)
    (expect (length (constraint-args c)) :to-equal 3)
    (expect (length (first (constraint-args c))) :to-equal 1)
    (expect (length (second (constraint-args c))) :to-equal 1)
    (expect (length (third (constraint-args c))) :to-equal 1)))

(it-sequential "constraint-ground-kinds-creation"
  (expect (constraint-kind
                              (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+)) :to-be :effect-subset)
  (expect (constraint-kind
                              (make-kind-equal-constraint +kind-type+ +kind-type+)) :to-be :kind-equal)
  (let ((c (make-mult-leq-constraint :zero :omega)))
    (expect (constraint-kind c) :to-be :mult-leq)
    (expect (first  (constraint-args c)) :to-be :zero)
    (expect (second (constraint-args c)) :to-be :omega))
  (let* ((rv (fresh-type-var :name "rho"))
         (c  (make-row-lacks-constraint rv 'x)))
    (expect (constraint-kind c) :to-be :row-lacks)
    (expect (second (constraint-args c)) :to-be 'x)))

;;; ─── constraint-free-vars ──────────────────────────────────────────────────

(it-sequential "constraint-free-vars-binary-constraints equal"
  (destructuring-bind (c) (list (let* ((v1 (fresh-type-var :name "a")) (v2 (fresh-type-var :name "b"))) (make-equal-constraint v1 v2)))
    (expect (length (cl-cc/type:constraint-free-vars c)) :to-equal 2)))

(it-sequential "constraint-free-vars-binary-constraints subtype"
  (destructuring-bind (c) (list (let* ((v1 (fresh-type-var :name "x")) (v2 (fresh-type-var :name "y"))) (make-subtype-constraint v1 v2)))
    (expect (length (cl-cc/type:constraint-free-vars c)) :to-equal 2)))

(it-sequential "constraint-free-vars-dedup-and-binding"
  (let* ((v  (fresh-type-var :name "a"))
         (c1 (make-equal-constraint v v)))
    (expect (length (cl-cc/type:constraint-free-vars c1)) :to-equal 1))
  (let* ((v  (fresh-type-var :name "a"))
         (c2 (make-typeclass-constraint 'eq v)))
    (expect (length (cl-cc/type:constraint-free-vars c2)) :to-equal 1))
  (let* ((v  (fresh-type-var :name "a"))
         (c3 (make-implication-constraint
              (list v)
              (list (make-equal-constraint v type-int))
              (list (make-subtype-constraint v type-any)))))
    (expect (length (cl-cc/type:constraint-free-vars c3)) :to-equal 0)))

(it-sequential "constraint-free-vars-ground-types-empty"
  (expect (cl-cc/type:constraint-free-vars (make-mult-leq-constraint :one :omega)) :to-be-null)
  (expect (cl-cc/type:constraint-free-vars (make-kind-equal-constraint +kind-type+ +kind-effect+)) :to-be-null))

(it-sequential "constraint-free-vars-row-lacks with-var"
  (destructuring-bind (rho-val expected-count) (list (fresh-type-var :name "rho") 1)
    (let* ((c (make-row-lacks-constraint rho-val 'x))
         (fvs (cl-cc/type:constraint-free-vars c)))
    (expect (length fvs) :to-equal expected-count))))

(it-sequential "constraint-free-vars-row-lacks without-var"
  (destructuring-bind (rho-val expected-count) (list 'not-a-type 0)
    (let* ((c (make-row-lacks-constraint rho-val 'x))
         (fvs (cl-cc/type:constraint-free-vars c)))
    (expect (length fvs) :to-equal expected-count))))

;;; ─── constraint-substitute ─────────────────────────────────────────────────

(it-sequential "constraint-substitute-equal-applies-binding"
  (let* ((v (fresh-type-var :name "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type:constraint-substitute c s)))
      (expect (constraint-kind c2) :to-be :equal)
      (expect (type-equal-p type-string (first (constraint-args c2))) :to-be-truthy)
      (expect (type-equal-p type-int (second (constraint-args c2))) :to-be-truthy))))

(it-sequential "constraint-substitute-subtype-applies-bindings"
  (let* ((v1 (fresh-type-var :name "a"))
         (v2 (fresh-type-var :name "b"))
         (c  (make-subtype-constraint v1 v2))
         (s  (make-substitution)))
    (subst-extend! v1 type-int s)
    (subst-extend! v2 type-any s)
    (let ((c2 (cl-cc/type:constraint-substitute c s)))
      (expect (type-equal-p type-int (first (constraint-args c2))) :to-be-truthy)
      (expect (type-equal-p type-any (second (constraint-args c2))) :to-be-truthy))))

(it-sequential "constraint-substitute-typeclass-applies-binding"
  (let* ((v  (fresh-type-var :name "a"))
         (c  (make-typeclass-constraint 'show v))
         (s  (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type:constraint-substitute c s)))
      (expect (first (constraint-args c2)) :to-be 'show)
      (expect (type-equal-p type-string (second (constraint-args c2))) :to-be-truthy))))

(it-sequential "constraint-substitute-ground-and-effect"
  (let ((s (make-substitution)))
    (let ((c (make-mult-leq-constraint :one :omega)))
      (expect (cl-cc/type:constraint-substitute c s) :to-be c))
    (let ((c (make-kind-equal-constraint +kind-type+ +kind-type+)))
      (expect (cl-cc/type:constraint-substitute c s) :to-be c)))
  (let* ((v  (fresh-type-var :name "ε"))
         (c  (make-effect-subset-constraint v +pure-effect-row+))
         (s  (make-substitution)))
    (subst-extend! v +io-effect-row+ s)
    (expect (constraint-kind (cl-cc/type:constraint-substitute c s)) :to-be :effect-subset)))

(it-sequential "constraint-kind-check subtype"
  (destructuring-bind (expected-kind c) (list :subtype (cl-cc/type:make-subtype-constraint type-int type-any))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check typeclass"
  (destructuring-bind (expected-kind c) (list :typeclass (cl-cc/type:make-typeclass-constraint 'num (cl-cc/type:fresh-type-var "a")))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check implication"
  (destructuring-bind (expected-kind c) (list :implication (let* ((tv (cl-cc/type:fresh-type-var "a"))
                  (eq-c (cl-cc/type:make-equal-constraint tv type-int))
                  (tc-c (cl-cc/type:make-typeclass-constraint 'num tv)))
             (cl-cc/type:make-implication-constraint (list tv) (list eq-c) (list tc-c))))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check effect-subset"
  (destructuring-bind (expected-kind c) (list :effect-subset (cl-cc/type:make-effect-subset-constraint
            cl-cc/type:+pure-effect-row+ cl-cc/type:+io-effect-row+))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check kind-equal"
  (destructuring-bind (expected-kind c) (list :kind-equal (cl-cc/type:make-kind-equal-constraint
            cl-cc/type:+kind-type+ cl-cc/type:+kind-type+))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check mult-leq"
  (destructuring-bind (expected-kind c) (list :mult-leq (cl-cc/type:make-mult-leq-constraint :one :omega))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-kind-check row-lacks"
  (destructuring-bind (expected-kind c) (list :row-lacks (cl-cc/type:make-row-lacks-constraint (cl-cc/type:fresh-type-var "r") 'x))
    (expect (cl-cc/type:constraint-kind c) :to-be expected-kind)))

(it-sequential "constraint-free-vars-count"
  (let* ((tv1   (cl-cc/type:fresh-type-var "a"))
         (tv2   (cl-cc/type:fresh-type-var "b"))
         (ceq   (cl-cc/type:make-equal-constraint tv1 tv2))
         (ctc   (cl-cc/type:make-typeclass-constraint 'num tv1)))
    (expect (length (cl-cc/type:constraint-free-vars ceq)) :to-equal 2)
    (expect (length (cl-cc/type:constraint-free-vars ctc)) :to-equal 1)))

(it-sequential "constraint-free-vars-zero-vars mult-leq"
  (destructuring-bind (c) (list (cl-cc/type:make-mult-leq-constraint :one :omega))
    (expect (cl-cc/type:constraint-free-vars c) :to-be-null)))

(it-sequential "constraint-free-vars-zero-vars kind-equal"
  (destructuring-bind (c) (list (cl-cc/type:make-kind-equal-constraint
                             cl-cc/type:+kind-type+ cl-cc/type:+kind-effect+))
    (expect (cl-cc/type:constraint-free-vars c) :to-be-null)))

(it-sequential "constraint-free-vars-zero-vars implication-quantified"
  (destructuring-bind (c) (list (let* ((tv    (cl-cc/type:fresh-type-var "a"))
                  (inner (cl-cc/type:make-equal-constraint tv type-int)))
             (cl-cc/type:make-implication-constraint (list tv) (list inner) (list inner))))
    (expect (cl-cc/type:constraint-free-vars c) :to-be-null)))

(it-sequential "constraint-substitute"
  (let* ((tv    (cl-cc/type:fresh-type-var "a"))
         (c     (cl-cc/type:make-equal-constraint tv type-string))
         (subst (cl-cc/type:subst-extend tv type-int (cl-cc/type:make-substitution)))
         (c2    (cl-cc/type:constraint-substitute c subst)))
    (expect (cl-cc/type:constraint-kind c2) :to-be :equal)
    (expect (type-equal-p type-int (first (cl-cc/type:constraint-args c2))) :to-be-truthy))
  (let* ((c     (cl-cc/type:make-mult-leq-constraint :one :omega))
         (c2    (cl-cc/type:constraint-substitute c (cl-cc/type:make-substitution))))
    (expect c2 :to-be c)))
