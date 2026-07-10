;;; packages/prolog/src/prolog-rules.lisp
;;;
;;; Goal and rule representation, rule store primitives, and cut support for
;;; CL-CC Prolog.

(in-package :cl-cc/prolog)

;;; Goal and Rule Representation

(defstruct (prolog-rule (:conc-name rule-))
  "Represents a Prolog rule or fact."
  head
  (body nil))

(declaim (ftype function rename-variables
                prolog-compound-form-parts register-prolog-rule
                prolog-cut-goal-p))

(defun rename-variables (rule)
  "Rename all logic variables in RULE to fresh ones for recursive calls."
  (let ((renaming (make-hash-table :test 'eq)))
    (labels ((rename-term (term)
               (cond ((logic-var-p term)
                      (or (gethash term renaming)
                          (setf (gethash term renaming)
                                (gensym (symbol-name term)))))
                     ((consp term)
                      (cons (rename-term (car term))
                            (rename-term (cdr term))))
                     (t
                      term))))
      (make-prolog-rule :head (rename-term (rule-head rule))
                        :body (mapcar #'rename-term (rule-body rule))))))

;;; Prolog rule state

(defvar *prolog-rules* (make-hash-table :test 'eq)
  "Hash table mapping predicate symbols to lists of rules.")

(defun prolog-compound-form-parts (form)
  "Return predicate and arguments for a Prolog compound FORM."
  (destructuring-bind (predicate &rest arguments) form
    (unless (and predicate (symbolp predicate))
      (error "Prolog predicate must be a non-NIL symbol: ~S" predicate))
    (values predicate arguments)))

(defun register-prolog-rule (head &optional body)
  "Create a rule from HEAD and BODY, then register it under HEAD's predicate."
  (multiple-value-bind (predicate _arguments)
      (prolog-compound-form-parts head)
    (declare (ignore _arguments))
    (let ((rule (make-prolog-rule :head head :body body)))
      (setf (gethash predicate *prolog-rules*)
            (cons rule (gethash predicate *prolog-rules*)))
      rule)))

(defmacro def-rule (head &body body)
  "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
  `(register-prolog-rule ',head ',body))

;;; Cut Operator Support

(defun prolog-cut-goal-p (goal)
  "Return true when GOAL denotes the Prolog cut operator."
  (and (symbolp goal)
       (string= (symbol-name goal) "!")))

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))
