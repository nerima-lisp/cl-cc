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

(defun rename-variables (rule)
  "Rename all logic variables in RULE to fresh ones for recursive calls."
  (let ((renaming (make-hash-table :test 'eq)))
    (labels ((rename-term (term)
               (%walk-prolog-term term
                                  (lambda (node)
                                    (or (gethash node renaming)
                                        (setf (gethash node renaming)
                                              (gensym (symbol-name node)))))
                                  #'identity
                                  (lambda (node)
                                    (cons (rename-term (car node))
                                          (rename-term (cdr node)))))))
      (make-prolog-rule :head (rename-term (rule-head rule))
                        :body (mapcar #'rename-term
                                      (rule-body rule))))))

;;; Prolog rule state

(defvar *prolog-rules* (make-hash-table :test 'eq)
  "Hash table mapping predicate symbols to lists of rules.")

(defun register-prolog-rule (head &optional body)
  "Create a rule from HEAD and BODY, then register it under HEAD's predicate."
  (let ((rule (make-prolog-rule :head head :body body)))
    (setf (gethash (car head) *prolog-rules*)
          (cons rule (gethash (car head) *prolog-rules*)))
    rule))

(defmacro def-rule (head &body body)
  "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
  `(register-prolog-rule ',head ',body))

;;; Cut Operator Support

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))
