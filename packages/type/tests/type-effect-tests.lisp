;;;; tests/type-effect-tests.lisp - Type Effect, Free Variables, Substitution, and Rank-N Tests
;;;;
;;;; Covers: free variables, substitution, bidirectional checking, typeclass,
;;;; effect types, effect rows, and rank-N polymorphism (forall).

(in-package :cl-cc/test)


(defbefore :each (cl-cc-type-serial-suite)
  (cl-cc/type:reset-type-vars!)
  (setf cl-cc/type:*typeclass-instance-registry* (make-hash-table :test #'equal)))


;;; Free Variables Tests

(it-sequential "free-vars-primitives int"
  (destructuring-bind (ty) (list type-int)
    (expect (type-free-vars ty) :to-be-null)))

(it-sequential "free-vars-primitives string"
  (destructuring-bind (ty) (list type-string)
    (expect (type-free-vars ty) :to-be-null)))

(it-sequential "free-vars-single-var-returns-itself"
  (let* ((v  (fresh-type-var))
         (fv (type-free-vars v)))
    (expect (= 1 (length fv)) :to-be-truthy)
    (expect (type-var-equal-p (first fv) v) :to-be-truthy)))

(it-sequential "free-vars-function-type-returns-param-and-return-vars"
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         (fn (make-type-arrow-raw :params (list v1) :return v2))
         (fv (type-free-vars fn)))
    (expect (= 2 (length fv)) :to-be-truthy)))

;;; Substitution Tests

(it-sequential "substitution-primitive-unchanged"
  (expect (zonk type-int (make-substitution)) :to-be type-int))

(it-sequential "substitution-bound-var-replaced"
  (let* ((v (fresh-type-var))
         (result (zonk v (subst-extend v type-int (make-substitution)))))
    (assert-type-equal result type-int)))

(it-sequential "substitution-unbound-var-is-identity"
  (let* ((v (fresh-type-var))
         (result (zonk v (make-substitution))))
    (expect (type-var-equal-p result v) :to-be-truthy)))

(it-sequential "substitution-through-function-type"
  (let* ((v      (fresh-type-var))
         (fn     (make-type-arrow-raw :params (list v) :return v))
         (result (zonk fn (subst-extend v type-int (make-substitution)))))
    (expect (typep result 'type-arrow) :to-be-truthy)
    (assert-type-equal (first (type-arrow-params result)) type-int)
    (assert-type-equal (type-arrow-return result) type-int)))

;;; Normalize Type Variables Tests

(it-sequential "normalize-type-variables-canonical"
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         (fn (make-type-arrow-raw
                            :params (list v1)
                            :return v2))
         (normalized (normalize-type-variables fn)))
    (expect (typep normalized 'type-arrow) :to-be-truthy)
    ;; The param and return should be different canonical variables
    (let ((p (first (type-arrow-params normalized)))
          (r (type-arrow-return normalized)))
      (expect (typep p 'type-var) :to-be-truthy)
      (expect (typep r 'type-var) :to-be-truthy)
      (expect (type-var-equal-p p r) :to-be-falsy))))

;;; Phase 3: Bidirectional Type Checking Tests

(it-sequential "bidirectional-checking-synthesize-and-check-integer"
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    (multiple-value-bind (ty _subst) (synthesize ast env)
      (declare (ignore _subst))
      (expect (type-equal-p ty type-int) :to-be-truthy))
    (expect (null (check ast type-int env)) :to-be-truthy)
    (expect (null (check ast cl-cc/type:+type-unknown+ env)) :to-be-truthy)))

(it-sequential "bidirectional-checking-check-body-verifies-last-form"
  (let* ((env (type-env-empty))
         (ast1 (make-ast-int :value 1))
         (ast2 (make-ast-int :value 2)))
    (expect (null (check-body (list ast1 ast2) type-int env)) :to-be-truthy)))

;;; Phase 4: Typeclass Tests

(it-sequential "typeclass-instance-registration int-registered"
  (destructuring-bind (query-type expected-p) (list type-int t)
    (let ((cl-cc/type:*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'num-test type-int (list (cons 'plus #'+)))
    (if expected-p
        (expect (has-typeclass-instance-p 'num-test query-type) :to-be-truthy)
        (expect (has-typeclass-instance-p 'num-test query-type) :to-be-falsy)))))

(it-sequential "typeclass-instance-registration string-not-present"
  (destructuring-bind (query-type expected-p) (list type-string nil)
    (let ((cl-cc/type:*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'num-test type-int (list (cons 'plus #'+)))
    (if expected-p
        (expect (has-typeclass-instance-p 'num-test query-type) :to-be-truthy)
        (expect (has-typeclass-instance-p 'num-test query-type) :to-be-falsy)))))

;;; Phase 5: Effect Type Tests


(it-sequential "effect-row-singleton-cases pure"
  (destructuring-bind (row expected-count expected-name) (list +pure-effect-row+ 0 nil)
    (expect (type-effect-row-p row) :to-be-truthy) (expect (= expected-count (length (type-effect-row-effects row))) :to-be-truthy) (when expected-name
      (expect (symbol-name (cl-cc/type:type-effect-op-name (first (type-effect-row-effects row)))) :to-equal expected-name))))

(it-sequential "effect-row-singleton-cases io"
  (destructuring-bind (row expected-count expected-name) (list +io-effect-row+ 1 "IO")
    (expect (type-effect-row-p row) :to-be-truthy) (expect (= expected-count (length (type-effect-row-effects row))) :to-be-truthy) (when expected-name
      (expect (symbol-name (cl-cc/type:type-effect-op-name (first (type-effect-row-effects row)))) :to-equal expected-name))))

(it-sequential "effect-row-singleton-cases custom"
  (destructuring-bind (row expected-count expected-name) (list (make-type-effect-row :effects (list (cl-cc/type:make-type-effect-op :name 'state :args nil)
                                                          (cl-cc/type:make-type-effect-op :name 'error :args nil))
                                           :row-var nil) 2 nil)
    (expect (type-effect-row-p row) :to-be-truthy) (expect (= expected-count (length (type-effect-row-effects row))) :to-be-truthy) (when expected-name
      (expect (symbol-name (cl-cc/type:type-effect-op-name (first (type-effect-row-effects row)))) :to-equal expected-name))))

(it-sequential "effect-row-to-string pure"
  (destructuring-bind (row expected substr-p) (list +pure-effect-row+ "{}" nil)
    (let ((s (type-to-string row)))
    (if substr-p
        (expect (search expected (string-upcase s)) :to-be-truthy)
        (expect s :to-equal expected)))))

(it-sequential "effect-row-to-string io"
  (destructuring-bind (row expected substr-p) (list +io-effect-row+ "IO" t)
    (let ((s (type-to-string row)))
    (if substr-p
        (expect (search expected (string-upcase s)) :to-be-truthy)
        (expect s :to-equal expected)))))


;;; Phase 6: Rank-N Polymorphism Tests

(it-sequential "rankn-forall-creation-and-equality"
  (let* ((a  (fresh-type-var :name 'a))
         (fn (make-type-arrow (list a) a))
         (fa (make-type-forall :var a :body fn)))
    (expect (type-forall-p fa) :to-be-truthy)
    (expect (type-var-equal-p a (type-forall-var fa)) :to-be-truthy)
    (expect (typep (type-forall-body fa) 'type-arrow) :to-be-truthy)
    (let ((s (type-to-string fa)))
      (expect (stringp s) :to-be-truthy)
      (expect (search "A" (string-upcase s)) :to-be-truthy)))
  (let* ((a  (fresh-type-var :name 'a))
         (b  (fresh-type-var :name 'b))
         (fa (make-type-forall :var a :body (make-type-arrow (list a) a)))
         (fb (make-type-forall :var b :body (make-type-arrow (list b) b))))
    (expect (type-equal-p fa fb) :to-be-falsy)))
