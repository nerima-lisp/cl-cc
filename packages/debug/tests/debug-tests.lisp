(in-package :cl-cc/test)



(defclass debug-test-object ()
  ((name :initarg :name :accessor debug-test-object-name)
   (count :initarg :count :accessor debug-test-object-count)))

(it-sequential "swank-interactive-eval-evaluates-basic-form"
  (let ((result (cl-cc/debug:interactive-eval "(+ 1 2)")))
    (expect (getf result :ok) :to-be-truthy)
    (expect (getf result :values) :to-equal '(3))))

(it-sequential "swank-completions-and-arglist-are-available"
  (expect (member "CAR" (cl-cc/debug:completions "ca" :package :cl) :test #'string=) :to-be-truthy)
  (expect (listp (cl-cc/debug:operator-arglist 'cl:+)) :to-be-truthy))

(it-sequential "swank-compile-string-compiles-thunk"
  (let* ((result (cl-cc/debug:compile-string "(+ 20 22)"))
         (function (getf result :function)))
    (expect (getf result :ok) :to-be-truthy)
    (expect (functionp function) :to-be-truthy)
    (expect (= 42 (funcall function)) :to-be-truthy)))

(it-sequential "inspector-records-cons-and-hash-table-parts"
  (let* ((before (length cl-cc/debug:*inspected-objects*))
         (cons-result (cl-cc/debug:inspect '(a . b)))
         (table (make-hash-table))
         (table-result nil))
    (setf (gethash :answer table) 42)
    (setf table-result (cl-cc/debug:inspect table))
    (expect (= before (getf cons-result :id)) :to-be-truthy)
    (expect (getf cons-result :parts) :to-equal '((:slot car :value a) (:slot cdr :value b)))
    (expect (aref cl-cc/debug:*inspected-objects* (getf table-result :id)) :to-equal table)
    (expect (getf table-result :parts) :to-equal '((:key :answer :value 42)))))

(it-sequential "inspector-shows-clos-slots"
  (let* ((object (make-instance 'debug-test-object :name "n" :count 7))
         (parts (getf (cl-cc/debug:inspect object) :parts)))
    (expect (find 'name parts :key (lambda (part) (getf part :slot))) :to-be-truthy)
    (expect (find 'count parts :key (lambda (part) (getf part :slot))) :to-be-truthy)))
(it-sequential "step-debugger-signals-at-breakpoints"
  (let ((program
        (cl-cc:make-vm-program
          :instructions
          (list (cl-cc:make-vm-const :dst :r0 :value 10)

(cl-cc:make-vm-halt :reg :r0))
          :result-register
          :r0))
        (seen nil))
    (cl-cc/debug:clear-step-breakpoints)
    (unwind-protect (progn
        (cl-cc/debug:add-step-breakpoint 1)
        (handler-bind ((cl-cc/debug:step-condition
              (lambda (condition)
                (push (cl-cc/debug:step-condition-pc condition) seen))))
          (expect (= 10 (cl-cc:run-compiled program)) :to-be-truthy)))
      (cl-cc/debug:clear-step-breakpoints))
    (expect (nreverse seen) :to-equal (quote (1)))))

(it-sequential "step-macro-enables-step-into-mode"
  (let ((program
        (cl-cc:make-vm-program
          :instructions
          (list (cl-cc:make-vm-const :dst :r0 :value 10) (cl-cc:make-vm-halt :reg :r0))
          :result-register
          :r0))
        (seen nil))
    (cl-cc/debug:clear-step-breakpoints)
    (handler-bind ((cl-cc/debug:step-condition
          (lambda (condition)
            (push (cl-cc/debug:step-condition-pc condition) seen))))
      (expect (= 10 (cl-cc/debug:step (cl-cc:run-compiled program))) :to-be-truthy))
    (expect (nreverse seen) :to-equal (quote (0 1)))))

(it-sequential "watchpoint-detects-register-write"
  (let* ((program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 10)
                                       (cl-cc:make-vm-const :dst :r0 :value 20)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0))
         (seen nil))
    (cl-cc/debug:add-vm-watchpoint :r0)
    (handler-bind ((cl-cc/debug:vm-watchpoint-condition
                     (lambda (c)
                       (push (list (cl-cc/debug:vm-watchpoint-reg c)
                                   (cl-cc/debug:vm-watchpoint-old-value c)
                                   (cl-cc/debug:vm-watchpoint-new-value c))
                             seen))))
      (expect (= 20 (cl-cc:run-compiled program)) :to-be-truthy))
    (cl-cc/debug:clear-vm-watchpoints)
    ;; :r0 was written first with nil→10, then 10→20
    (expect (nreverse seen) :to-equal '((:r0 nil 10) (:r0 10 20)))))
