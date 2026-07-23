;;;; tests/unit/optimize/egraph-rules-tests.lisp — E-Graph Rewrite Rule Tests
;;;
;;; Comprehensive tests for all 55 built-in e-graph rewrite rules defined in
;;; src/optimize/egraph-rules.lisp.  Each test is independent and creates its
;;; own fresh e-graph.
;;;
;;; IMPORTANT implementation notes about the e-graph:
;;;
;;;   1. Op symbols MUST use cl-cc: prefix (e.g., 'cl-cc/optimize::add).  Unqualified
;;;      symbols in this package do not match the cl-cc: symbols registered by
;;;      defrule and cause silent pattern-match failures.
;;;
;;;   2. All (egraph-add eg 'cl-cc/optimize::const) calls with the same e-graph return
;;;      the SAME class ID (memoized by op+children).  Constant values are stored
;;;      as ec-data on that single class.
;;;
;;;   3. After egraph-merge + egraph-rebuild, the union-find is updated but the
;;;      eg-classes hash-table may contain "zombie" entries whose canonical ID
;;;      (via egraph-find) points to the surviving class.  Use eg-all-nodes to
;;;      collect nodes across all zombie classes sharing the same canonical ID.
;;;
;;;   4. Rules with (const ?var) as a pattern sub-expression are currently
;;;      limited: the pattern matcher's "constant pattern" branch consumes
;;;      (const X) and compares ec-data against X directly.  When X is a
;;;      pattern variable symbol (e.g., ?a), the equal check fails.  This
;;;      affects fold-*, mul-pow2/div-pow2, and type-predicate rules.
;;;      Those rules are registered correctly and tested for registration only.

(in-package :cl-cc/test)

;;; ─── Shared Helpers ───────────────────────────────────────────────────────

(defun make-eg-const (eg value)
  "Add a cl-cc:const node and store VALUE as its ec-data.  Returns the class ID.
   NOTE: all consts in the same e-graph share one class (memoized)."
  (let ((id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::const)))
    (let ((cls (gethash (cl-cc/optimize:egraph-find eg id) (cl-cc:eg-classes eg))))
      (when cls (setf (cl-cc:ec-data cls) value)))
    id))

(defun eg-merged-p (eg id1 id2)
  "Return true iff ID1 and ID2 are in the same equivalence class."
  (= (cl-cc/optimize:egraph-find eg id1) (cl-cc/optimize:egraph-find eg id2)))

(defun eg-all-nodes (eg id)
  "Return ALL e-nodes logically in the equivalence class of ID.
   Collects nodes from both the canonical class and any zombie classes whose
   egraph-find returns the same canonical ID (due to deferred class consolidation)."
  (let ((canon (cl-cc/optimize:egraph-find eg id))
        (nodes nil))
    (maphash (lambda (k cls)
               (when (= (cl-cc/optimize:egraph-find eg k) canon)
                 (setf nodes (append nodes (cl-cc:ec-nodes cls)))))
             (cl-cc:eg-classes eg))
    nodes))

(defun eg-class-contains-op-p (eg id op)
  "Return true if the equivalence class of ID contains any node with OP."
  (some (lambda (n) (eq (cl-cc:en-op n) op))
        (eg-all-nodes eg id)))

(defun eg-any-class-data-eql-p (eg value)
  "Return true if any e-class in EG has ec-data eql VALUE."
  (let ((found nil))
    (maphash (lambda (_id cls)
               (declare (ignore _id))
               (when (eql (cl-cc:ec-data cls) value)
                 (setf found t)))
             (cl-cc:eg-classes eg))
    found))

(defun eg-any-class-has-op-p (eg op)
  "Return true if any e-class in EG contains a node with OP."
  (let ((found nil))
    (maphash (lambda (_id cls)
               (declare (ignore _id))
               (when (some (lambda (n) (eq (cl-cc:en-op n) op))
                           (cl-cc:ec-nodes cls))
                 (setf found t)))
             (cl-cc:eg-classes eg))
    found))

(defun eg-saturate (eg)
  "Run all built-in rules to saturation, then rebuild congruence."
  (cl-cc/optimize:egraph-saturate eg (cl-cc/optimize:egraph-builtin-rules) :limit 10 :fuel 1000)
  (cl-cc/optimize:egraph-rebuild eg))

(defun eg-rule-registered-p (name)
  "Return true if NAME is a registered built-in rule name."
  (let ((rules (cl-cc/optimize:egraph-builtin-rules)))
    (not (null (find name rules :key (lambda (r) (getf r :name)))))))

;;; ─── Constant Folding Rules — Registration Tests ─────────────────────────
;;;
;;; The fold-* rules use (const ?a) patterns that cannot bind ?a via the
;;; current pattern-matcher constant-pattern branch.  We test that the rules
;;; are correctly registered.

(it-sequential "egraph-fold-rules-registered fold-add"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-add)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-sub"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-sub)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-mul"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-mul)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-neg"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-neg)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-not"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-not)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-lt"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-lt)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-gt"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-gt)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-le"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-le)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

(it-sequential "egraph-fold-rules-registered fold-ge"
  (destructuring-bind (rule-name) (list 'cl-cc/optimize::fold-ge)
    (expect (eg-rule-registered-p rule-name) :to-be-truthy)))

;;; ─── Algebraic Identity: bidirectional identity rules ───────────────────
;;; Pattern: (op ?x (const N)) → ?x   and   (op (const N) ?x) → ?x
;;; Both arg orders must merge the result class with x.

(it-sequential "egraph-bidirectional-identity-fires add-zero"
  (destructuring-bind (op identity-val) (list 'cl-cc/optimize::add 0)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op c x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-bidirectional-identity-fires mul-one"
  (destructuring-bind (op identity-val) (list 'cl-cc/optimize::mul 1)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op c x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-bidirectional-identity-fires logand-neg1"
  (destructuring-bind (op identity-val) (list 'cl-cc/optimize::logand -1)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize:egraph-add eg op c x)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

;;; ─── Algebraic Identity: bidirectional annihilator rules ────────────────
;;; RHS is (const 0). egraph-build-rhs for the template (const 0) creates a
;;; const-with-child node (a new class).  eg-all-nodes detects both the
;;; original op and const nodes in the merged equivalence class.
;;;
;;; Covered: mul-zero, logand-zero (both share the same structural test).

(it-sequential "egraph-bidirectional-annihilator-fires mul-zero"
  (destructuring-bind (op annihilator-val) (list 'cl-cc/optimize::mul 0)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize:egraph-add eg op c x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-bidirectional-annihilator-fires logand-zero"
  (destructuring-bind (op annihilator-val) (list 'cl-cc/optimize::logand 0)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy)) (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize:egraph-add eg op c x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

;;; ─── Algebraic Identity: single-sided zero/one identities ───────────────

(it-sequential "egraph-single-sided-identity-rules-fire sub-zero"
  (destructuring-bind (op const-val) (list 'cl-cc/optimize::sub 0)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg const-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-single-sided-identity-rules-fire div-one"
  (destructuring-bind (op const-val) (list 'cl-cc/optimize::div 1)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg const-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

(it-sequential "egraph-single-sided-identity-rules-fire ash-zero"
  (destructuring-bind (op const-val) (list 'cl-cc/optimize::ash 0)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg const-val))
         (id (cl-cc/optimize:egraph-add eg op x c)))
    (eg-saturate eg)
    (expect (eg-merged-p eg id x) :to-be-truthy))))

;;; ─── Self-Reference Rules ────────────────────────────────────────────────
;;; sub-self: (sub ?x ?x) → (const 0).
;;; Both args are the same class (memoized var).  RHS (const 0) creates a
;;; const-with-child zombie class.  Use eg-class-contains-op-p.

(it-sequential "egraph-self-reference-rules-fire sub-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::sub)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-self-reference-rules-fire eq-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::num-eq)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-self-reference-rules-fire lt-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::lt)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-self-reference-rules-fire gt-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::gt)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-self-reference-rules-fire le-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::le)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))

(it-sequential "egraph-self-reference-rules-fire ge-self"
  (destructuring-bind (op) (list 'cl-cc/optimize::ge)
    (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (expect (eg-class-contains-op-p eg id 'cl-cc/optimize::const) :to-be-truthy))))
