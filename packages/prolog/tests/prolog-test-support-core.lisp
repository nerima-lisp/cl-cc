(in-package :cl-cc/test)

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs."
  (loop for (type val) on type-value-pairs by #'cddr
        collect (cons type val)))

(defmacro dcg-goal (name &rest args)
  "Build a raw DCG builtin goal form for NAME and ARGS."
  `(list ',name ,@args))

(defun %expand-prolog-projection-form (form env)
  (cond
    ((and (consp form)
          (eq (car form) 'quote)
          (consp (cdr form))
          (null (cddr form)))
     (%expand-prolog-projection-form (second form) env))
    ((and (symbolp form)
          (let ((name (symbol-name form)))
            (and (> (length name) 0)
                 (char= (char name 0) #\?))))
     `(cl-cc:logic-substitute ',form ,env))
    ((consp form)
     `(,(%expand-prolog-projection-form (car form) env)
       ,@(mapcar (lambda (subform)
                   (%expand-prolog-projection-form subform env))
                 (cdr form))))
    (t form)))

(defun %collect-prolog-projections (runner-form projection-fn)
  (let ((results nil))
    (handler-case
        (funcall runner-form
                 (lambda (result)
                   (push (funcall projection-fn result) results)))
      (cl-cc/prolog::prolog-cut ()))
    (nreverse results)))

(defmacro %with-prolog-fixture (&body body)
  `(with-fresh-prolog
     ,@body))

(defmacro %with-prolog-goal-results ((results goal &optional projection-form) &body body)
  "Bind RESULTS to GOAL solutions projected through PROJECTION-FORM."
  `(let ((,results (%collect-prolog-projections
                    (lambda (emit)
                      (let ((goal-form ,goal))
                        (if (and (consp goal-form)
                                 (consp (car goal-form)))
                            (cl-cc/prolog::solve-conjunction goal-form nil emit)
                            (cl-cc/prolog::solve-goal goal-form nil emit))))
                    ,(if projection-form
                         `(lambda (result)
                            ,(%expand-prolog-projection-form projection-form 'result))
                         `(lambda (result) result)))))
     (declare (ignorable ,results))
     ,@body))
