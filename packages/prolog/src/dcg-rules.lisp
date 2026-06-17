;;;; dcg-rules.lisp — DCG rule transformation and rule generation

(in-package :cl-cc/prolog)

(defvar *dcg-counter* 0
  "Counter for generating fresh DCG state variables.")

(defun dcg-fresh-var ()
  (intern (format nil "?S~D" (incf *dcg-counter*))))

(defun dcg-reset-counter ()
  "Reset the DCG variable counter (for testing)."
  (setf *dcg-counter* 0))

(defun dcg-transform-body-element (element s-in s-out)
  "Transform a single DCG body element into a Prolog goal."
  (cond
    ((and (consp element)
          (symbolp (car element))
          (string= (symbol-name (car element)) "TERMINAL"))
     (let ((terminals (cdr element)))
       (if (null terminals)
           (list `(= ,s-in ,s-out))
           (let ((goals nil)
                 (current s-in))
             (dolist (term terminals)
               (let ((next (dcg-fresh-var)))
                 (push `(dcg-token-match ,term ,current ,next) goals)
                 (setf current next)))
             (push `(= ,current ,s-out) goals)
             (nreverse goals)))))
    ((symbolp element)
     (list (list element s-in s-out)))
    ((and (consp element)
          (symbolp (car element))
          (string= (symbol-name (car element)) "BRACE"))
     (let ((goal (cadr element)))
       (list `(:when ,goal) `(= ,s-in ,s-out))))
    ((and (consp element) (symbolp (car element)))
     (list (append element (list s-in s-out))))
    (t (error "DCG: unknown body element ~S" element))))

(defun dcg-transform-body (body s-in s-out)
  "Transform a DCG body (list of elements) into a list of Prolog goals,
   chaining fresh state variables between elements."
  (if (null body)
      (list `(= ,s-in ,s-out))
      (labels ((walk (elements current)
                 (let* ((element (car elements))
                        (next (if (null (cdr elements))
                                  s-out
                                  (dcg-fresh-var))))
                   (append (dcg-transform-body-element element current next)
                           (when (cdr elements)
                             (walk (cdr elements) next))))))
        (walk body s-in))))

(defmacro def-dcg-rule (name &body body)
  "Define a DCG rule. Transforms (name --> body...) into a Prolog rule
   with difference-list state threading.
   Usage: (def-dcg-rule expr term (terminal (+)) term)"
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    `(def-rule (,name ,s-in ,s-out)
       ,@(dcg-transform-body body s-in s-out))))
