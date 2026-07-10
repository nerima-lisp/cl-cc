(in-package :cl-cc/test)

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs."
  (labels ((build (pairs)
             (when pairs
               (cons (cons (first pairs) (second pairs))
                     (build (cddr pairs))))))
    (build type-value-pairs)))

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
     `(cl-cc/prolog:logic-substitute ',form ,env))
    ((consp form)
     `(,(%expand-prolog-projection-form (car form) env)
       ,@(mapcar (lambda (subform)
                   (%expand-prolog-projection-form subform env))
                 (cdr form))))
    (t form)))

(defun %prolog-projection-lambda-form (projection-form)
  (if projection-form
      `(lambda (result)
         ,(%expand-prolog-projection-form projection-form 'result))
      `(lambda (result) result)))

(defun %collect-prolog-projections (runner-form projection-fn)
  (cl-cc/prolog::with-prolog-solution-collection (emit results)
    (handler-case
        (funcall runner-form
                 (lambda (result)
                   (emit (funcall projection-fn result))))
      (cl-cc/prolog::prolog-cut ()))))

(defun %run-prolog-goal-form (goal-form env emit)
  (if (and (consp goal-form)
           (consp (first goal-form)))
      (cl-cc/prolog::solve-conjunction goal-form env emit)
      (cl-cc/prolog::solve-goal goal-form env emit)))

(defun %collect-prolog-goal-results (goal env projection-fn)
  "Collect GOAL results after projecting each solution with PROJECTION-FN."
  (%collect-prolog-projections
   (lambda (emit)
     (%run-prolog-goal-form goal env emit))
   projection-fn))

(defvar *prolog-fixture-active* nil)

(defmacro %with-prolog-fixture (&body body)
  `(if *prolog-fixture-active*
       (progn ,@body)
       (with-fresh-prolog
         (let ((*prolog-fixture-active* t))
           ,@body))))

(defmacro %with-prolog-goal-results/internal ((results goal env projection-form) &body body)
  "Bind RESULTS to GOAL solutions projected through PROJECTION-FORM and ENV."
  `(let ((,results (%collect-prolog-goal-results
                    ,goal
                    ,env
                    ,(%prolog-projection-lambda-form projection-form))))
     (declare (ignorable ,results))
     ,@body))

(defmacro assert-prolog-call-arguments-rejected (callee args env continuation-form)
  "Assert that CALLEE rejects malformed ARGS when invoked with ENV."
  `(assert-signals error
     (funcall #',callee ,args ,env ,continuation-form)))
