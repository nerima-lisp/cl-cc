;;;; tests/unit/vm/conditions-tests.lisp — VM Condition System Unit Tests
;;;;
;;;; Tests for handler stack management, restart bindings,
;;;; condition construction, and signal dispatch.

(in-package :cl-cc/test)



;;; ─── Condition Construction ───────────────────────────────────────────────

(it-sequential "vm-condition-constructor-slots type-error"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm::make-vm-type-error s 'fixnum "hello")) 'cl-cc:vm-type-error (lambda (c) (and (equal 'fixnum (type-error-expected-type c))
                            (equal "hello" (type-error-datum c)))))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy))))

(it-sequential "vm-condition-constructor-slots unbound-variable"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm::make-vm-unbound-variable s 'x)) 'cl-cc:vm-unbound-variable (lambda (c) (equal 'x (cell-error-name c))))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy))))

(it-sequential "vm-condition-constructor-slots undefined-function"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm::make-vm-undefined-function s 'foo)) 'cl-cc:vm-undefined-function (lambda (c) (equal 'foo (cell-error-name c))))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy))))

(it-sequential "vm-condition-constructor-slots division-by-zero"
  (destructuring-bind (make-fn expected-type check-slots-fn) (list (lambda (s) (cl-cc/vm::make-vm-division-by-zero s 42)) 'cl-cc:vm-division-by-zero (lambda (c) (equal 42 (cl-cc/vm::vm-dividend c))))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (funcall make-fn state)))
    (expect (typep cond expected-type) :to-be-truthy)
    (expect (funcall check-slots-fn cond) :to-be-truthy))))

;;; ─── Condition Hierarchy ──────────────────────────────────────────────────

(it-sequential "condition-inherits-vm-error type-error"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm::make-vm-type-error s 'fixnum 42)))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc:vm-error) :to-be-truthy))))

(it-sequential "condition-inherits-vm-error unbound-var"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm::make-vm-unbound-variable s 'x)))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc:vm-error) :to-be-truthy))))

(it-sequential "condition-inherits-vm-error division-by-zero"
  (destructuring-bind (make-cond-fn) (list (lambda (s) (cl-cc/vm::make-vm-division-by-zero s 42)))
    (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (c (funcall make-cond-fn state)))
    (expect (typep c 'cl-cc:vm-error) :to-be-truthy))))

;;; ─── Condition Report ─────────────────────────────────────────────────────

(it-sequential "type-error-is-printable"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (cl-cc/vm::make-vm-type-error state 'fixnum "hello"))
         (msg (format nil "~A" cond)))
    (expect (stringp msg) :to-be-truthy)))

(it-sequential "vm-conditions-carry-structured-diagnostics"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (fix-it (cl-cc/parse:make-fix-it :text "bind x" :span '(0 . 1)))
         (cond (cl-cc/vm::make-vm-type-error state 'fixnum "hello"
                                             :error-code "E1001"
                                             :fix-it fix-it)))
    (expect (cl-cc:vm-condition-error-code cond) :to-equal "E1001")
    (expect (cl-cc:vm-condition-fix-it cond) :to-be fix-it))
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond (cl-cc/vm::make-vm-unbound-variable state 'x)))
    (expect (cl-cc:vm-condition-error-code cond) :to-be-null)
    (expect (cl-cc:vm-condition-fix-it cond) :to-be-null)))

;;; ─── Handler Stack ────────────────────────────────────────────────────────

(it-sequential "handler-stack-fresh-state-is-empty"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (expect (cl-cc/vm::vm-get-handler-stack state) :to-equal nil)))

(it-sequential "handler-stack-push-pop-roundtrip"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-error #'identity)
    (let ((handler (cl-cc/vm::vm-pop-handler-from-stack state)))
      (expect (not (null handler)) :to-be-truthy)
      (expect (cl-cc/vm::vm-handler-type handler) :to-equal 'cl-cc:vm-error))))

(it-sequential "handler-stack-pop-on-empty-returns-nil"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (expect (cl-cc/vm::vm-pop-handler-from-stack state) :to-equal nil)))

(it-sequential "handler-stack-lifo-order-preserved"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-error #'identity)
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-warning #'identity)
    (let ((first (cl-cc/vm::vm-pop-handler-from-stack state)))
      (expect (cl-cc/vm::vm-handler-type first) :to-equal 'cl-cc:vm-warning))
    (let ((second (cl-cc/vm::vm-pop-handler-from-stack state)))
      (expect (cl-cc/vm::vm-handler-type second) :to-equal 'cl-cc:vm-error))))

(it-sequential "find-handler-behavior"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state))
        (cond-val nil))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-error #'identity)
    (setf cond-val (cl-cc/vm::make-vm-type-error state 'fixnum 42))
    (expect (not (null (cl-cc/vm::vm-find-handler state cond-val))) :to-be-truthy)
    ;; replace with non-matching handler
    (cl-cc/vm::vm-pop-handler-from-stack state)
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-warning #'identity)
    (expect (cl-cc/vm::vm-find-handler state cond-val) :to-equal nil)))

;;; ─── Restart Bindings ─────────────────────────────────────────────────────


(it-sequential "vm-restart-operations"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (expect (cl-cc/vm::vm-get-restarts state) :to-equal nil))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-add-restart state 'continue #'identity)
    (let ((restart (cl-cc/vm::vm-find-restart state 'continue)))
      (expect (not (null restart)) :to-be-truthy)
      (expect (cl-cc/vm::vm-restart-name restart) :to-equal 'continue)))
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (expect (cl-cc/vm::vm-find-restart state 'nonexistent) :to-equal nil)))

;;; ─── Signal Dispatch ──────────────────────────────────────────────────────

(it-sequential "signal-condition-dispatch"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cond-val (cl-cc/vm::make-vm-type-error state 'fixnum 42)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-error #'identity)
    (multiple-value-bind (found handler)
        (cl-cc/vm::vm-signal-condition cond-val state)
      (expect found :to-be-truthy)
      (expect (not (null handler)) :to-be-truthy))
    ;; remove handler, signal again — no match
    (cl-cc/vm::vm-pop-handler-from-stack state)
    (multiple-value-bind (found handler)
        (cl-cc/vm::vm-signal-condition cond-val state)
      (expect found :to-equal nil)
      (expect handler :to-equal nil))))

(it-sequential "signal-condition-error-p-signals"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (let ((cond (cl-cc/vm::make-vm-type-error state 'fixnum 42)))
      (expect (handler-case
           (progn (cl-cc/vm::vm-signal-condition cond state :error-p t) nil)
         (cl-cc:vm-type-error () t)) :to-be-truthy))))

;;; ─── Clear Context ────────────────────────────────────────────────────────

(it-sequential "clear-condition-context"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-push-handler-to-stack state 'cl-cc:vm-error #'identity)
    (cl-cc/vm::vm-add-restart state 'continue #'identity)
    (cl-cc/vm::vm-clear-condition-context state)
    (expect (cl-cc/vm::vm-get-handler-stack state) :to-equal nil)
    (expect (cl-cc/vm::vm-get-restarts state) :to-equal nil)))

(it-sequential "vm-sync-handler-regs-shares-snapshot-across-handler-entries"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-reg-set state :r1 10)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-establish-handler :handler-label "h1" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-establish-handler :handler-label "h2" :result-reg :r0 :error-type 'error)
     state 1 nil)
    (cl-cc/vm::vm-reg-set state :r1 42)
    (cl-cc/vm::execute-instruction (cl-cc:make-vm-sync-handler-regs) state 2 nil)
    (let* ((entries (cl-cc/vm::vm-handler-stack state))
           (snapshot-a (cl-cc/vm::vm-handler-entry-saved-regs (first entries)))
           (snapshot-b (cl-cc/vm::vm-handler-entry-saved-regs (second entries))))
      (expect snapshot-b :to-be snapshot-a)
      (expect (= 42 (gethash :r1 snapshot-a)) :to-be-truthy))))

(it-sequential "vm-sync-handler-regs-updates-catch-frame-saved-regs"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (cl-cc/vm::vm-reg-set state :tag 7)
    (cl-cc/vm::vm-reg-set state :r1 99)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-establish-catch :tag-reg :tag :handler-label "catch" :result-reg :r0)
     state 0 nil)
    (cl-cc/vm::vm-reg-set state :r1 123)
    (cl-cc/vm::execute-instruction (cl-cc:make-vm-sync-handler-regs) state 1 nil)
    (let* ((entry (first (cl-cc/vm::vm-handler-stack state)))
           (snapshot (cl-cc/vm::vm-handler-entry-saved-regs entry)))
      (expect (hash-table-p snapshot) :to-be-truthy)
      (expect (= 123 (gethash :r1 snapshot)) :to-be-truthy))))

(it-sequential "vm-establish-handler-retains-call-and-method-stacks"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (setf (cl-cc/vm::vm-call-stack state) '((1 :r0 nil nil)))
    (setf (cl-cc/vm::vm-method-call-stack state) '((gf nil args)))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-establish-handler :handler-label "h" :result-reg :r0 :error-type 'error)
     state 0 nil)
    (let ((entry (first (cl-cc/vm::vm-handler-stack state))))
      (expect (cl-cc/vm::vm-call-stack state) :to-be (fourth entry))
      (expect (cl-cc/vm::vm-handler-entry-saved-regs entry) :to-be (fifth entry))
      (expect (cl-cc/vm::vm-method-call-stack state) :to-be (sixth entry)))))

(it-sequential "vm-establish-catch-retains-call-and-method-stacks"
  (let ((state (make-instance 'cl-cc/vm::vm-io-state)))
    (setf (cl-cc/vm::vm-call-stack state) '((7 :r1 nil nil)))
    (setf (cl-cc/vm::vm-method-call-stack state) '((gf2 nil args2)))
    (cl-cc/vm::vm-reg-set state :tag 9)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-establish-catch :tag-reg :tag :handler-label "c" :result-reg :r0)
     state 0 nil)
    (let ((entry (first (cl-cc/vm::vm-handler-stack state))))
      (expect (cl-cc/vm::vm-call-stack state) :to-be (fifth entry))
      (expect (cl-cc/vm::vm-method-call-stack state) :to-be (seventh entry)))))
