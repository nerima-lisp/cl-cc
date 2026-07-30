(in-package :cl-cc/cps)

;;; Block and Return-From

(defvar *cps-block-environment* nil "Lexical block names mapped to their CPS exit continuations.")

(define-condition unbound-cps-block (error)
  ((name :initarg :name :reader unbound-cps-block-name))
  (:report (lambda (condition stream)
             (format stream "CPS RETURN-FROM references unknown block ~S."
                     (unbound-cps-block-name condition)))))

(defun %lookup-cps-block (name)
  (or (cdr (assoc name *cps-block-environment* :test #'eq))
      (error 'unbound-cps-block :name name)))

(defmethod cps-transform-ast ((node ast-block) k)
  "Transform a lexical block into an explicit exit continuation."
  (let* ((name (ast-block-name node))
         (body (ast-block-body node))
         (exit-k (gensym "BLOCK-EXIT"))
         (*cps-block-environment*
           (acons name exit-k *cps-block-environment*)))
    (list 'let (list (list exit-k k))
          (cps-transform-sequence body exit-k))))

(defmethod cps-transform-ast ((node ast-return-from) k)
  "Transform RETURN-FROM by invoking its nearest lexical block continuation."
  (declare (ignore k))
  (let* ((name (ast-return-from-name node))
         (exit-k (%lookup-cps-block name))
         (value (ast-return-from-value node))
         (result (gensym "RETURN-VALUE")))
    (cps-transform-ast value
                       (list 'lambda (list result)
                             (list 'funcall exit-k result)))))

;;; Tagbody and Go

(defun %lookup-cps-tag (tag)
  (or
    (cdr (assoc tag *cps-tagbody-environment* :test #'eq))
    (error 'unbound-cps-tag :tag tag)))

(define-condition unbound-cps-tag (error)
  ((tag :initarg :tag :reader unbound-cps-tag-tag))
  (:report
    (lambda (condition stream)
      (format
        stream
        "CPS GO references unknown tag ~S."
        (unbound-cps-tag-tag condition)))))

(defvar *cps-tagbody-environment* nil
  "Lexical tag names mapped to their CPS label continuations.")

(defun cps-transform-tagbody-section (forms continue-form)
  "Transform one TAGBODY section, then invoke its fallthrough continuation."
  (let ((section-result (gensym "TAGBODY-RESULT")))
    (%cps-transform-sequence-step
      forms
      (list
        'lambda
        (list section-result)
        (list 'declare (list 'ignore section-result))
        continue-form)
      continue-form)))

(defmethod cps-transform-ast ((node ast-tagbody) k)
  "Transform TAGBODY into mutually recursive local tag continuations."
  (let ((tags (ast-tagbody-tags node)))
    (if (null tags)
        (list (quote funcall) k nil)
        (let* ((tag-continuations
                 (loop for entry in tags
                       collect (cons (car entry) (gensym "TAG-CONTINUATION"))))
               (*cps-tagbody-environment*
                 (append tag-continuations *cps-tagbody-environment*))
               (bindings
                 (loop for remaining-tags on tags
                       for remaining-continuations on tag-continuations
                       for entry = (car remaining-tags)
                       for continuation = (cdar remaining-continuations)
                       for next-continuation =
                         (and (cdr remaining-continuations)
                              (cdadr remaining-continuations))
                       for continue-form =
                         (if next-continuation
                             (list (quote funcall)
                                   (list (quote function) next-continuation))
                             (list (quote funcall) k nil))
                       collect
                         (list continuation nil
                               (cps-transform-tagbody-section
                                 (cdr entry) continue-form)))))
          (list (quote labels) bindings
                (list (quote funcall)
                      (list (quote function)
                            (cdar tag-continuations))))))))

(defmethod cps-transform-ast ((node ast-go) k)
  "Transform GO by invoking its nearest lexical tag continuation."
  (declare (ignore k))
  (list (quote funcall)
        (list (quote function)
              (%lookup-cps-tag (ast-go-tag node)))))

;;; Catch and Throw

(defmethod cps-transform-ast ((node ast-catch) k) "Transform catch with dynamic tag." (let* ((tag-expr (ast-catch-tag node)) (body (ast-catch-body node)) (tag-v (gensym "TAG")) (result (gensym "RESULT"))) (cps-transform-ast tag-expr (list (quote lambda) (list tag-v) (list (quote funcall) k (list (quote catch) tag-v (cps-transform-sequence body (list (quote lambda) (list result) result))))))))

(defmethod cps-transform-ast ((node ast-throw) k)
  "Transform throw to unwind to matching catch."
  (let* ((tag-expr (ast-throw-tag node))
         (value (ast-throw-value node))
         (tag-v (gensym "TAG"))
         (val-v (gensym "VAL")))
    (cps-transform-ast tag-expr
                       (list 'lambda (list tag-v)
                             (cps-transform-ast value
                                                (list 'lambda (list val-v)
                                                      (list 'throw tag-v val-v)))))))

;;; Unwind-Protect

(defmethod cps-transform-ast ((node ast-unwind-protect) k)
  "Transform unwind-protect with guaranteed cleanup.
The cleanup forms always run, even on non-local exit."
  (let* ((protected (ast-unwind-protected node))
         (cleanup (ast-unwind-cleanup node))
         (result (gensym "RESULT"))
         (cleanup-result (gensym "CLEANUP")))
    (list 'unwind-protect
          (cps-transform-ast protected
                             (list 'lambda (list result)
                                   (list 'funcall k result)))
          (if cleanup
              (cps-transform-sequence cleanup
                                      (list 'lambda (list cleanup-result)
                                            (list 'declare (list 'ignore cleanup-result))
                                            nil))
              nil))))

;;; Flet and Labels (Local Function Bindings)

(defun cps-transform-fn-binding (binding k-var)
  "Transform a function binding (name params . body) to CPS form.
Produces valid FLET/LABELS syntax: (name (params... k-var) body-cps)."
  (let* ((name (first binding))
         (params (second binding))
         (body (cddr binding)))
    (list* name (append params (list k-var))
           (list (cps-transform-sequence body k-var)))))

(defun cps-transform-local-fns (form-kw bindings body k)
  "Transform a flet/labels binding group to CPS.
FORM-KW is either 'flet or 'labels; they share identical CPS structure."
  (let ((fn-k (gensym (if (eq form-kw 'flet) "FLET-K" "LABELS-K"))))
    (list form-kw
          (loop for binding in bindings
                collect (cps-transform-fn-binding binding fn-k))
          (cps-transform-sequence body
                                  (list 'lambda (list fn-k)
                                        (list 'funcall k fn-k))))))

(defmethod cps-transform-ast ((node ast-flet) k)
  "Transform flet (non-recursive local functions)."
  (cps-transform-local-fns 'flet (ast-flet-bindings node) (ast-flet-body node) k))

(defmethod cps-transform-ast ((node ast-labels) k)
  "Transform labels (mutually recursive local functions)."
  (cps-transform-local-fns 'labels (ast-labels-bindings node) (ast-labels-body node) k))
