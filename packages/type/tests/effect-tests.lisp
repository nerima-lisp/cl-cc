;;;; tests/unit/type/effect-tests.lisp — Algebraic Effect System Tests
;;;;
;;;; Tests for src/type/effect.lisp:
;;;; effect-def, effect registry, effect-row-union, effect-row-subset-p.

(in-package :cl-cc/test)



;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-effect-op (name)
  "Shorthand for a simple effect-op with no args."
  (cl-cc/type:make-type-effect-op :name name :args nil))

(defun make-effect-row (&rest names)
  "Build a closed effect row from effect name symbols."
  (make-type-effect-row
   :effects (mapcar #'make-effect-op names)
   :row-var nil))

(defun make-open-effect-row (var &rest names)
  "Build an open effect row with a row variable."
  (make-type-effect-row
   :effects (mapcar #'make-effect-op names)
   :row-var var))

;;; ─── effect-def struct ──────────────────────────────────────────────────────

(it-sequential "effect-def-creation"
  (let ((ed (cl-cc/type:make-effect-def
             :name 'io
             :type-params nil
             :operations '((read-line . nil) (write-line . nil)))))
    (expect (cl-cc/type:effect-def-name ed) :to-be 'io)
    (expect (length (cl-cc/type:effect-def-operations ed)) :to-equal 2)))

;;; ─── effect registry ────────────────────────────────────────────────────────

(it-sequential "effect-registry-operations"
  (let ((cl-cc/type:*effect-registry* (make-hash-table :test #'eq)))
    (let ((ed (cl-cc/type:make-effect-def :name 'test-eff :type-params nil :operations nil)))
      (cl-cc/type:register-effect 'test-eff ed)
      (expect (cl-cc/type:lookup-effect 'test-eff) :to-be ed))
    (expect (cl-cc/type:lookup-effect 'nonexistent) :to-be-null)))

;;; ─── effect-row-union ───────────────────────────────────────────────────────

(it-sequential "effect-row-union-behavior"
  (expect (length (type-effect-row-effects
                            (cl-cc/type:effect-row-union (make-effect-row :io) (make-effect-row :state)))) :to-equal 2)
  (expect (length (type-effect-row-effects
                            (cl-cc/type:effect-row-union (make-effect-row :io :state) (make-effect-row :io :exn)))) :to-equal 3)
  (expect (type-effect-row-effects
                (cl-cc/type:effect-row-union (make-effect-row) (make-effect-row))) :to-be-null)
  (let* ((rv (fresh-type-var))
         (result (cl-cc/type:effect-row-union (make-effect-row :io) (make-open-effect-row rv :state))))
    (expect (type-var-p (type-effect-row-row-var result)) :to-be-truthy)))

;;; ─── effect-row-subset-p ────────────────────────────────────────────────────

(it-sequential "effect-row-subset-p-behavior"
  (expect (cl-cc/type:effect-row-subset-p (make-effect-row) (make-effect-row :io :state)) :to-be-truthy)
  (let ((r (make-effect-row :io :state)))
    (expect (cl-cc/type:effect-row-subset-p r r) :to-be-truthy))
  (expect (cl-cc/type:effect-row-subset-p (make-effect-row :io) (make-effect-row :io :state)) :to-be-truthy)
  (expect (cl-cc/type:effect-row-subset-p (make-effect-row :io :state) (make-effect-row :io)) :to-be-falsy)
  (expect (cl-cc/type:effect-row-subset-p (make-effect-row :io :state :exn)
                                                  (make-open-effect-row (fresh-type-var))) :to-be-truthy))

(it-sequential "effect-row-extend"
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:effect-row-extend op cl-cc/type:+pure-effect-row+)))
    (expect (cl-cc/type:type-effect-row-p row) :to-be-truthy)
    (expect (length (cl-cc/type:type-effect-row-effects row)) :to-equal 1)
    (expect (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects row))) :to-be 'state))
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (base (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (ext  (cl-cc/type:effect-row-extend op base)))
    (expect (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var ext)) :to-be-truthy)))

(it-sequential "effect-row-restrict"
  (let* ((op1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op2 (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op1 op2) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'io row)))
    (expect (length (cl-cc/type:type-effect-row-effects restricted)) :to-equal 1)
    (expect (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects restricted))) :to-be 'state))
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'state row)))
    (expect (length (cl-cc/type:type-effect-row-effects restricted)) :to-equal 1)))

(it-sequential "effect-row-member-p"
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (expect (cl-cc/type:effect-row-member-p 'io row) :to-be-truthy)
    (expect (cl-cc/type:effect-row-member-p 'state row) :to-be-falsy)
    (expect (cl-cc/type:effect-row-member-p 'io cl-cc/type:+pure-effect-row+) :to-be-falsy)))

(it-sequential "effect-registry"
  (let ((cl-cc/type:*effect-registry* (make-hash-table :test #'eq)))
    (let ((edef (cl-cc/type:make-effect-def :name 'state :type-params nil :operations nil)))
      (cl-cc/type:register-effect 'state edef)
      (let ((found (cl-cc/type:lookup-effect 'state)))
        (expect (cl-cc/type:effect-def-p found) :to-be-truthy)
        (expect (cl-cc/type:effect-def-name found) :to-be 'state)))
    (expect (cl-cc/type:lookup-effect 'nonexistent) :to-be-null)))

(it-sequential "effect-row-union"
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (op-io2   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects (list op-io2 op-state) :row-var nil))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (expect (length (cl-cc/type:type-effect-row-effects merged)) :to-equal 2))
  (let* ((rv (cl-cc/type:fresh-type-var "e"))
         (row1 (cl-cc/type:make-type-effect-row :effects nil :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (expect (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var merged)) :to-be-truthy)))

(it-sequential "effect-row-subset-p"
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (expect (cl-cc/type:effect-row-subset-p cl-cc/type:+pure-effect-row+ row) :to-be-truthy))
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv)))
    (expect (cl-cc/type:effect-row-subset-p row1 row2) :to-be-truthy))
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (big   (cl-cc/type:make-type-effect-row :effects (list op-io op-state) :row-var nil))
         (small (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil)))
    (expect (cl-cc/type:effect-row-subset-p big small) :to-be-falsy)))
