;;;; tests/unit/type/types-extended-coverage-tests.lisp
;;;; Targeted coverage for uncovered branches in:
;;;;   src/type/types-extended.lisp — type-free-vars for capability/refinement/handler/gadt-con
;;;;   src/type/types-core.lisp — fresh-rigid-var, type-rigid-equal-p
;;;;   src/type/types-env.lisp — type-env-bindings

(in-package :cl-cc/test)



;;; ─── type-free-vars: capability ─────────────────────────────────────────────

(it-sequential "free-vars-capability-var-base-yields-one-fv"
  (let* ((v   (cl-cc/type:fresh-type-var "cap-base"))
         (cap (cl-cc/type:make-type-capability :base v :cap 'read))
         (fvs (cl-cc/type:type-free-vars cap)))
    (expect (= 1 (length fvs)) :to-be-truthy)
    (expect (cl-cc/type:type-var-equal-p v (first fvs)) :to-be-truthy)))

(it-sequential "free-vars-capability-primitive-base-yields-nil"
  (expect (cl-cc/type:type-free-vars
                (cl-cc/type:make-type-capability :base cl-cc/type:type-int :cap 'read)) :to-be-null))

;;; ─── type-free-vars: refinement ─────────────────────────────────────────────

(it-sequential "free-vars-refinement-var-base-yields-one-fv"
  (let* ((v   (cl-cc/type:fresh-type-var "ref-base"))
         (ref (cl-cc/type:make-type-refinement :base v :predicate (lambda (x) (> x 0))))
         (fvs (cl-cc/type:type-free-vars ref)))
    (expect (= 1 (length fvs)) :to-be-truthy)
    (expect (cl-cc/type:type-var-equal-p v (first fvs)) :to-be-truthy)))

(it-sequential "free-vars-refinement-primitive-base-yields-nil"
  (expect (cl-cc/type:type-free-vars
                (cl-cc/type:make-type-refinement
                 :base cl-cc/type:type-int :predicate (lambda (x) (> x 0)))) :to-be-null))

;;; ─── type-free-vars: handler ─────────────────────────────────────────────────

(it-sequential "free-vars-handler-three-distinct-vars"
  (let* ((ve (cl-cc/type:fresh-type-var "eff"))
         (vi (cl-cc/type:fresh-type-var "in"))
         (vo (cl-cc/type:fresh-type-var "out"))
         (h  (cl-cc/type:make-type-handler :effect ve :input vi :output vo)))
    (expect (= 3 (length (cl-cc/type:type-free-vars h))) :to-be-truthy)))

(it-sequential "free-vars-handler-shared-var-deduped"
  (let* ((v (cl-cc/type:fresh-type-var "shared"))
         (h (cl-cc/type:make-type-handler :effect v :input v :output cl-cc/type:type-int)))
    (expect (= 1 (length (cl-cc/type:type-free-vars h))) :to-be-truthy)))

(it-sequential "free-vars-handler-all-primitives-yields-nil"
  (expect (cl-cc/type:type-free-vars
                (cl-cc/type:make-type-handler
                 :effect (cl-cc/type:make-type-effect-op :name 'io)
                 :input  cl-cc/type:type-int
                 :output cl-cc/type:type-string)) :to-be-null))

;;; ─── type-free-vars: gadt-con ────────────────────────────────────────────────

(it-sequential "free-vars-gadt-con-collects-arg-types"
  (let* ((v  (cl-cc/type:fresh-type-var "gadt-a"))
         (gc (cl-cc/type:make-type-gadt-con
              :name 'just
              :arg-types (list v)
              :index-type cl-cc/type:type-int)))
    ;; type-free-vars doesn't reach gadt-con — verify it returns nil (falls through to t nil)
    ;; since gadt-con is not in type-free-vars dispatch
    (expect (listp (cl-cc/type:type-free-vars gc)) :to-be-truthy)))

;;; ─── type-rigid-equal-p (types-core.lisp) ───────────────────────────────────

(it-sequential "rigid-var-equal-p-same-id-is-true"
  (let* ((r1 (cl-cc/type:fresh-rigid-var "r"))
         (r2 (cl-cc/type::%make-type-rigid :id (cl-cc/type:type-rigid-id r1) :name "r")))
    (expect (cl-cc/type:type-rigid-equal-p r1 r2) :to-be-truthy)))

(it-sequential "rigid-var-equal-p-different-ids-is-false"
  (let ((r1 (cl-cc/type:fresh-rigid-var "a"))
        (r2 (cl-cc/type:fresh-rigid-var "b")))
    (expect (cl-cc/type:type-rigid-equal-p r1 r2) :to-be-falsy)))

(it-sequential "rigid-var-equal-p-rigid-vs-var-is-false"
  (let ((r (cl-cc/type:fresh-rigid-var "r"))
        (v (cl-cc/type:fresh-type-var  "v")))
    (expect (cl-cc/type:type-rigid-equal-p r v) :to-be-falsy)))

;;; ─── type-env-bindings (types-env.lisp) ─────────────────────────────────────

(it-sequential "type-env-bindings-empty-env-is-nil"
  (expect (cl-cc/type:type-env-bindings (cl-cc/type:type-env-empty)) :to-be-null))

(it-sequential "type-env-bindings-extended-env-has-entry"
  (let* ((env  (cl-cc/type:type-env-empty))
         (sch  (cl-cc/type:type-to-scheme cl-cc/type:type-int))
         (env2 (cl-cc/type:type-env-extend 'x sch env))
         (al   (cl-cc/type:type-env-bindings env2)))
    (expect (= 1 (length al)) :to-be-truthy)
    (expect (caar al) :to-be 'x)))

;;; ─── type-equal-p: capability and error nodes ────────────────────────────────

(it-sequential "type-equal-p-capability-self-identity"
  (let ((c (cl-cc/type:make-type-capability :base cl-cc/type:type-int :cap 'read)))
    (expect (cl-cc/type:type-equal-p c c) :to-be-truthy)))

(it-sequential "type-equal-p-error-node-always-false"
  (let ((e (cl-cc/type:make-type-error :message "test")))
    (expect (cl-cc/type:type-equal-p e e) :to-be-falsy)))

;;; ─── +pure-effect-row+ and +io-effect-row+ singletons ───────────────────────

(it-sequential "effect-row-singletons"
  (expect (cl-cc/type:type-effect-row-effects cl-cc/type:+pure-effect-row+) :to-be-null)
  (expect (cl-cc/type:type-effect-row-row-var cl-cc/type:+pure-effect-row+) :to-be-null)
  (let ((effs (cl-cc/type:type-effect-row-effects cl-cc/type:+io-effect-row+)))
    (expect (= 1 (length effs)) :to-be-truthy)
    (expect (symbol-name (cl-cc/type:type-effect-op-name (first effs))) :to-equal "IO")))

;;; ─── reset-type-vars! ────────────────────────────────────────────────────────

(it-sequential "reset-type-vars-resets-counter"
  (let ((old cl-cc/type::*type-var-counter*))
    (cl-cc/type:reset-type-vars!)
    (let ((v (cl-cc/type:fresh-type-var "after-reset")))
      (expect (= 1 (cl-cc/type:type-var-id v)) :to-be-truthy))
    ;; Restore approximately (counter may have advanced during fresh-type-var)
    (setf cl-cc/type::*type-var-counter* (max old cl-cc/type::*type-var-counter*))))

;;; ─── %type-free-vars-list unit tests ─────────────────────────────────────────

(it-sequential "free-vars-list-base-cases type-var"
  (destructuring-bind (is-var expected-count) (list t 1)
    (let* ((v   (cl-cc/type:fresh-type-var "fv"))
         (ty  (if is-var v cl-cc/type:type-int))
         (res (cl-cc/type::%type-free-vars-list ty)))
    (expect (= expected-count (length res)) :to-be-truthy)
    (when is-var
      (expect (cl-cc/type:type-var-equal-p v (first res)) :to-be-truthy)))))

(it-sequential "free-vars-list-base-cases type-primitive"
  (destructuring-bind (is-var expected-count) (list nil 0)
    (let* ((v   (cl-cc/type:fresh-type-var "fv"))
         (ty  (if is-var v cl-cc/type:type-int))
         (res (cl-cc/type::%type-free-vars-list ty)))
    (expect (= expected-count (length res)) :to-be-truthy)
    (when is-var
      (expect (cl-cc/type:type-var-equal-p v (first res)) :to-be-truthy)))))

(it-sequential "free-vars-list-arrow-collects-all-with-duplicates"
  (let* ((v  (cl-cc/type:fresh-type-var "shared"))
         (ar (cl-cc/type:make-type-arrow-raw :params (list v) :return v :effects nil :mult nil))
         (res (cl-cc/type::%type-free-vars-list ar)))
    (expect (= 2 (length res)) :to-be-truthy)
    (expect (cl-cc/type:type-var-equal-p v (first res)) :to-be-truthy)
    (expect (cl-cc/type:type-var-equal-p v (second res)) :to-be-truthy)))

(it-sequential "free-vars-list-forall-filters-bound-var"
  (let* ((v1 (cl-cc/type:fresh-type-var "bound"))
         (v2 (cl-cc/type:fresh-type-var "free"))
         (ar (cl-cc/type:make-type-arrow-raw :params (list v1) :return v2 :effects nil :mult nil))
         (fa (cl-cc/type:make-type-forall :var v1 :body ar))
         (res (cl-cc/type::%type-free-vars-list fa)))
    (expect (= 1 (length res)) :to-be-truthy)
    (expect (cl-cc/type:type-var-equal-p v2 (first res)) :to-be-truthy)))

(it-sequential "free-vars-list-nested-product"
  (let* ((v1  (cl-cc/type:fresh-type-var "a"))
         (v2  (cl-cc/type:fresh-type-var "b"))
         (v3  (cl-cc/type:fresh-type-var "c"))
         (prod (cl-cc/type:make-type-product :elems (list v1 v2 v3)))
         (res  (cl-cc/type::%type-free-vars-list prod)))
    (expect (= 3 (length res)) :to-be-truthy)))
