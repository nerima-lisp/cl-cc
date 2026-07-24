;;; Macro Expansion

(in-package :cl-cc/expand)

(defun %step-cache-and-return (form env result expanded-p)
  "Store (expanded-p . result) in the step cache for FORM/ENV and return (values result expanded-p)."
  (when (and (%cacheable-macroexpansion-p form)
             (or (not expanded-p) (%cacheable-macroexpansion-p result)))
    (%macroexpansion-cache-store form env (cons expanded-p result) *macroexpand-step-cache*))
  (values result expanded-p))

(defun our-macroexpand-1 (form &optional env)
  "Perform a single macro expansion on FORM.
   Returns (VALUES expanded-form expanded-p)."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-step-cache*)
      (when hitp
        (return-from our-macroexpand-1 (values (cdr cached) (car cached))))))
  (if (and (consp form) (symbolp (car form)))
      (let ((macro-fn (lookup-macro (car form))))
        (if macro-fn
            (let ((expanded (invoke-registered-expander macro-fn form env)))
              (if (%same-expansion-p expanded form)
                  (%step-cache-and-return form env form nil)
                  (%step-cache-and-return form env expanded t)))
            (%step-cache-and-return form env form nil)))
      (%step-cache-and-return form env form nil)))

(defun %cache-all-result (form env result)
  "Store RESULT in the all-cache for FORM/ENV when both are safe to cache, then return RESULT."
  (when (and (%cacheable-macroexpansion-p form)
             (%cacheable-macroexpansion-p result))
    (%macroexpansion-cache-store form env result *macroexpand-all-cache*))
  result)

(defun %our-macroexpand-all-recursive (form env)
  "Recursively expand FORM after a single expansion step."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-all-cache*)
      (when hitp (return-from %our-macroexpand-all-recursive cached))))
  (multiple-value-bind (expanded expanded-p)
      (our-macroexpand-1 form env)
    (if expanded-p
        (%cache-all-result form env (%our-macroexpand-all-recursive expanded env))
        (typecase form
          (cons (%cache-all-result form env
                                   (mapcar (lambda (x) (%our-macroexpand-all-recursive x env)) form)))
          (t    (%cache-all-result form env form))))))

(defun our-macroexpand (form &optional env)
  "Fully expand FORM by repeatedly applying macroexpand-1.
   Returns (VALUES expanded-form expanded-p)."
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expanded step-expanded-p)
          (our-macroexpand-1 form env)
        (unless step-expanded-p
          (return (values form expanded-p)))
        (setf form expanded
              expanded-p t)))))

(defun %qq-head-p (form name)
  "Return T when FORM is a list headed by a symbol named NAME."
  (and (consp form) (symbolp (car form)) (string= (symbol-name (car form)) name)))

(defun %expand-qq-element (elem)
  "Expand a single element of a quasiquote list template."
  (cond
    ((%qq-head-p elem "UNQUOTE")          (list 'list (second elem)))
    ((%qq-head-p elem "UNQUOTE-SPLICING") (second elem))
    (t                                    (list 'list (%expand-quasiquote elem)))))

(defun %expand-quasiquote (template)
  "Transform a quasiquote template into list/cons/append calls.
Handles (unquote x) and (unquote-splicing x) within template.
Folds static (list ...) parts together and eliminates nil splices."
  (cond
    ((%qq-head-p template "UNQUOTE")
     (second template))
    ((consp template)
     (let* ((parts (mapcar #'%expand-qq-element template))
            ;; nil arises from (unquote-splicing nil) - remove it
            (non-nil-parts (remove nil parts)))
       (cond
         ((null non-nil-parts) nil)
         (t
          ;; Merge adjacent (list ...) chunks into a single (list ...)
          (let ((merged
                 (loop with result = nil
                       with acc = nil
                       for part in non-nil-parts
                       do (if (and (consp part) (eq (car part) 'list))
                              (setf acc (append acc (cdr part)))
                              (progn
                                (when acc
                                  (push (cons 'list acc) result)
                                  (setf acc nil))
                                (push part result)))
                       finally
                       (when acc (push (cons 'list acc) result))
                       (return (nreverse result)))))
            (cond
              ((null merged) nil)
              ((= (length merged) 1)
               (let ((part (first merged)))
                 (if (and (consp part) (eq (car part) 'list))
                     ;; Pure list splice - return as-is
                     part
                     ;; Single non-list splice like ,@xs - wrap in copy-list
                     (list 'copy-list part))))
              (t (cons 'append merged))))))))
    (t
     (list 'quote template))))

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (cond
    ((or (%qq-head-p form "BACKQUOTE") (%qq-head-p form "QUASIQUOTE"))
     (our-macroexpand-all (%expand-quasiquote (second form)) env))
    ((and (consp form) (eq (car form) 'quote))
     form)
    (t (%our-macroexpand-all-recursive form env))))

;;; Macro Definition Macro

(defmacro our-defmacro (name lambda-list &body body)
  "Define NAME as a macro with LAMBDA-LIST and BODY.
   The macro expander function receives (FORM ENV) as arguments."
  (let* ((form-var (gensym "FORM"))
         (env-var  (gensym "ENV"))
         (info     (parse-lambda-list lambda-list))
         (env-sym  (lambda-list-info-environment info))
         (bindings (%normalize-let-bindings
                    (generate-lambda-bindings lambda-list form-var)))
         (expander-body
           (if env-sym
               `((let ((,env-sym ,env-var))
                   (let* ,bindings ,@body)))
               `((declare (ignore ,env-var))
                 (let* ,bindings ,@body)))))
    `(register-macro ',name (lambda (,form-var ,env-var) ,@expander-body))))

;;; Wire expand functions into VM hooks for runtime macroexpand support
(defun %vm-install-macroexpand-hooks-if-available ()
  (when cl-cc/bootstrap:*vm-macroexpand-hook-installer*
    (funcall cl-cc/bootstrap:*vm-macroexpand-hook-installer* #'our-macroexpand-1 #'our-macroexpand)))

#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  (%vm-install-macroexpand-hooks-if-available))
