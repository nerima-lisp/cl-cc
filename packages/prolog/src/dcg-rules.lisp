;;;; dcg-rules.lisp — DCG rule transformation and rule generation

(in-package :cl-cc/prolog)

(defvar *dcg-counter* 0
  "Counter for generating fresh DCG state variables.")

(defun dcg-fresh-var ()
  (intern (format nil "?S~D" (incf *dcg-counter*))))

(defun %thread-dcg-elements (elements s-in s-out element->goals)
  "Thread DCG state through ELEMENTS, calling ELEMENT->GOALS for each step."
  (if (null elements)
      (list `(= ,s-in ,s-out))
      (labels ((recurse (remaining current)
                 (let* ((element (car remaining))
                        (rest (cdr remaining))
                        (next (if rest
                                  (dcg-fresh-var)
                                  s-out)))
                   (append (funcall element->goals element current next)
                           (when rest
                             (recurse rest next))))))
        (recurse elements s-in))))

(defun dcg-transform-body-element (element s-in s-out)
  "Transform a single DCG body element into a Prolog goal."
  (cond
    ((and (consp element)
          (symbolp (car element))
          (string= (symbol-name (car element)) "TERMINAL"))
     (%thread-dcg-elements
      (cdr element) s-in s-out
      (lambda (terminal current next)
        (list `(dcg-token-match ,terminal ,current ,next)))))
    ((and (consp element)
          (symbolp (car element))
          (string= (symbol-name (car element)) "BRACE"))
     (list `(:when ,(cadr element)) `(= ,s-in ,s-out)))
    ((or (symbolp element) (consp element))
     (list (if (symbolp element)
               (list element s-in s-out)
               (append element (list s-in s-out)))))
    (t (error "DCG: unknown body element ~S" element))))

(defun dcg-transform-body (body s-in s-out)
  "Transform a DCG body (list of elements) into a list of Prolog goals,
   chaining fresh state variables between elements."
  (%thread-dcg-elements body s-in s-out #'dcg-transform-body-element))

(defmacro def-dcg-rule (name &body body)
  "Define a DCG rule. Transforms (name --> body...) into a Prolog rule
   with difference-list state threading.
   Usage: (def-dcg-rule expr term (terminal (+)) term)"
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    `(def-rule (,name ,s-in ,s-out)
       ,@(dcg-transform-body body s-in s-out))))
