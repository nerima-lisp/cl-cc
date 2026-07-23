;;;; tests/unit/type/unification-tests.lisp — Unification Tests
;;;
;;; Tests for type-unify, type-unify-lists, and unify-effect-rows
;;; focusing on coverage gaps: product types, intersection types,
;;; type constructors, effect rows, and edge cases.

(in-package :cl-cc/test)

;;; ─── Product Type Unification ───────────────────────────────────────────

(it-sequential "unify-product-cases same-types"
  (destructuring-bind (types expected-ok) (list (let ((p (cl-cc/type:make-type-product
                     :elems (list cl-cc/type:type-int cl-cc/type:type-string))))
             (list p p)) t)
    (destructuring-bind (lhs rhs) types
    (multiple-value-bind (s ok) (type-unify lhs rhs)
      (if expected-ok
          (progn
            (expect ok :to-be-truthy)
            (expect (cl-cc/type:substitution-p s) :to-be-truthy))
          (expect ok :to-be-falsy))))))

(it-sequential "unify-product-cases length-mismatch"
  (destructuring-bind (types expected-ok) (list (list (cl-cc/type:make-type-product :elems (list cl-cc/type:type-int))
                 (cl-cc/type:make-type-product
                  :elems (list cl-cc/type:type-int cl-cc/type:type-string))) nil)
    (destructuring-bind (lhs rhs) types
    (multiple-value-bind (s ok) (type-unify lhs rhs)
      (if expected-ok
          (progn
            (expect ok :to-be-truthy)
            (expect (cl-cc/type:substitution-p s) :to-be-truthy))
          (expect ok :to-be-falsy))))))

;;; ─── Union Type Unification ─────────────────────────────────────────────

(it-sequential "unify-union-identical-succeeds"
  (let ((u (cl-cc/type:make-type-union-raw :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify u u)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

(it-sequential "unify-union-left-member-succeeds"
  (let ((u (cl-cc/type:make-type-union-raw :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify u cl-cc/type:type-int)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

(it-sequential "unify-union-right-member-succeeds"
  (let ((u (cl-cc/type:make-type-union-raw :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify cl-cc/type:type-string u)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

(it-sequential "unify-union-non-member-fails"
  (let ((u (cl-cc/type:make-type-union-raw :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (_ ok) (type-unify u cl-cc/type:type-bool)
      (declare (ignore _))
      (expect ok :to-be-falsy))))

;;; ─── Primitive Unification Edge Cases ───────────────────────────────────

(it-sequential "unify-different-primitives-fail"
  (multiple-value-bind (s ok) (type-unify cl-cc/type:type-int cl-cc/type:type-string)
    (declare (ignore s))
    (expect ok :to-be-falsy)))

(it-sequential "unify-error-type-with-anything"
  (let ((err (cl-cc/type:make-type-error)))
    (multiple-value-bind (s ok) (type-unify err cl-cc/type:type-int)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))
    (multiple-value-bind (s ok) (type-unify cl-cc/type:type-string err)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

;;; ─── Variable Binding ───────────────────────────────────────────────────

(it-sequential "unify-var-bound-in-subst"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (multiple-value-bind (s2 ok) (type-unify a cl-cc/type:type-int s)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s2) :to-be-truthy))))

(it-sequential "unify-var-bound-conflicting-fails"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (multiple-value-bind (s2 ok) (type-unify a cl-cc/type:type-string s)
      (declare (ignore s2))
      (expect ok :to-be-falsy))))

;;; ─── type-unify-lists ───────────────────────────────────────────────────

(it-sequential "unify-lists-empty"
  (multiple-value-bind (s ok) (type-unify-lists nil nil (make-substitution))
    (expect ok :to-be-truthy)
    (expect (cl-cc/type:substitution-p s) :to-be-truthy)))

(it-sequential "unify-lists-length-mismatch"
  (multiple-value-bind (s ok) (type-unify-lists
                                (list cl-cc/type:type-int)
                                nil
                                (make-substitution))
    (declare (ignore s))
    (expect ok :to-be-falsy)))

(it-sequential "unify-lists-pairwise"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b)))
    (multiple-value-bind (s ok)
        (type-unify-lists (list a b)
                          (list cl-cc/type:type-int cl-cc/type:type-string)
                          (make-substitution))
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name (zonk a s)) :to-be 'fixnum)
      (expect (cl-cc/type:type-primitive-name (zonk b s)) :to-be 'string))))

(it-sequential "unify-lists-partial-failure"
  (multiple-value-bind (s ok)
      (type-unify-lists (list cl-cc/type:type-int cl-cc/type:type-int)
                        (list cl-cc/type:type-int cl-cc/type:type-string)
                        (make-substitution))
    (declare (ignore s))
    (expect ok :to-be-falsy)))

;;; ─── Arrow Unification Edge Cases ───────────────────────────────────────

(it-sequential "unify-arrow-mismatch-cases arity-mismatch"
  (destructuring-bind (lhs rhs) (list (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                           :return cl-cc/type:type-int) (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int cl-cc/type:type-int)
                                           :return cl-cc/type:type-int))
    (multiple-value-bind (s ok) (type-unify lhs rhs)
    (declare (ignore s))
    (expect ok :to-be-falsy))))

(it-sequential "unify-arrow-mismatch-cases return-mismatch"
  (destructuring-bind (lhs rhs) (list (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                           :return cl-cc/type:type-int) (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                           :return cl-cc/type:type-string))
    (multiple-value-bind (s ok) (type-unify lhs rhs)
    (declare (ignore s))
    (expect ok :to-be-falsy))))

;;; ─── Effect Row Unification ─────────────────────────────────────────────

(it-sequential "unify-effect-row-empty-rows-succeed"
  (let* ((r1 (cl-cc/type:make-type-effect-row :effects nil :row-var nil))
         (r2 (cl-cc/type:make-type-effect-row :effects nil :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

(it-sequential "unify-effect-row-same-effects-succeed"
  (let* ((e1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e2 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e1) :row-var nil))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e2) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (expect ok :to-be-truthy)
      (expect (cl-cc/type:substitution-p s) :to-be-truthy))))

(it-sequential "unify-effect-row-open-absorbs-extra-effect"
  (let* ((rv (cl-cc/type:fresh-type-var 'r))
         (e-io (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e-exn (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e-io) :row-var rv))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e-io e-exn) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (expect ok :to-be-truthy)
      (let ((bound (zonk rv s)))
        (expect (cl-cc/type:type-effect-row-p bound) :to-be-truthy)))))

(it-sequential "unify-effect-row-closed-rejects-extra-effect"
  (let* ((e-io (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e-exn (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e-io) :row-var nil))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e-io e-exn) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (declare (ignore s))
      (expect ok :to-be-falsy))))

;;; ─── Occurs Check in Unification ────────────────────────────────────────

(it-sequential "unify-occurs-check-circular"
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (fn (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int)))
    (multiple-value-bind (s ok) (type-unify a fn)
      (declare (ignore s))
      (expect ok :to-be-falsy))))
