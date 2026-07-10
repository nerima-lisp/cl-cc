;;;; packages/javascript/src/runtime-method-resolver-dispatch.lisp — JS method resolvers and dispatcher

(in-package :cl-cc/javascript)

(define-js-type-resolver %js-resolve-array-method *js-array-method-table*
  "length" (coerce (length obj) 'double-float))

(define-js-type-resolver %js-resolve-string-method *js-string-method-table*
  "length" (coerce (length obj) 'double-float))

(define-js-type-resolver %js-resolve-map-method *js-map-method-table*
  "size" (coerce (%js-map-size obj) 'double-float))

(define-js-type-resolver %js-resolve-set-method *js-set-method-table*
  "size" (coerce (%js-set-size obj) 'double-float))

(define-js-type-resolver %js-resolve-weak-map-method *js-weak-map-method-table*)
(define-js-type-resolver %js-resolve-weak-set-method *js-weak-set-method-table*)
(define-js-type-resolver %js-resolve-finalization-registry-method
    *js-finalization-registry-method-table*)
(define-js-type-resolver %js-resolve-date-method *js-date-method-table*)

(defparameter *js-object-fallback-method-table*
  (list (cons "hasOwnProperty"
              (lambda (obj) (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj)))))
        (cons "toString"
              (lambda (_obj) (declare (ignore _obj)) (lambda () "[object Object]")))
        (cons "valueOf"
              (lambda (obj) (lambda () obj)))
        (cons "isPrototypeOf"
              (lambda (_obj) (declare (ignore _obj)) (lambda (_p) (declare (ignore _p)) nil)))
        (cons "propertyIsEnumerable"
              (lambda (obj) (lambda (k) (nth-value 1 (gethash (%js-to-string k) obj)))))
        (cons "constructor"
              (lambda (obj) (lambda (&rest _) (declare (ignore _)) obj))))
  "Object.prototype fallback: (name . factory) where factory is (lambda (obj) -> method).")

(defun %js-resolve-promise-method (obj key)
  (cond
    ((string= key "then")
     (lambda (on-fulfilled &optional on-rejected)
       (%js-promise-then obj on-fulfilled on-rejected)))
    ((string= key "catch")
     (lambda (on-rejected)
       (%js-promise-then obj +js-undefined+ on-rejected)))
    ((string= key "finally")
     (lambda (fn)
       (%js-promise-then obj
        (lambda (v) (%js-funcall fn) (%js-promise-resolve v))
        (lambda (r) (%js-funcall fn) (%js-promise-reject r)))))
    (t +js-undefined+)))

(define-js-type-resolver %js-resolve-typed-array-method *js-typed-array-method-table*
  "length" (coerce (js-ta-length obj) 'double-float)
  "byteLength" (coerce (* (js-ta-length obj) (js-ta-element-size obj)) 'double-float)
  "byteOffset" (coerce (js-ta-byte-offset obj) 'double-float)
  "buffer" (%js-make-object "byteLength" (* (js-ta-length obj) (js-ta-element-size obj))))

(define-js-type-resolver %js-resolve-regexp-method nil
  "source" (js-regexp-source obj)
  "flags" (js-regexp-flags obj)
  "global" (js-regexp-global-p obj)
  "ignoreCase" (js-regexp-ignore-case-p obj)
  "multiline" (js-regexp-multiline-p obj)
  "lastIndex" (coerce (js-regexp-last-index obj) 'double-float)
  "test" (let ((re obj)) (lambda (str) (%js-regex-test re str)))
  "exec" (let ((re obj)) (lambda (str) (%js-regex-exec re str 0))))

(defun %js-resolve-object-method (obj key)
  (multiple-value-bind (stored found) (gethash key obj)
    (if found
        stored
        (let ((entry (assoc key *js-object-fallback-method-table* :test #'string=)))
          (if entry
              (funcall (cdr entry) obj)
              +js-undefined+)))))

(defun %js-resolve-number-method (obj key)
  (%js-bound-method *js-number-method-table* obj key))

(define-js-type-resolver %js-resolve-symbol-method *js-symbol-method-table*
  "description" (%js-symbol-description obj))

(defun %js-resolve-weak-ref-method (obj key)
  (if (string= key "deref")
      (lambda () (%js-weak-ref-deref obj))
      +js-undefined+))

(defparameter *js-function-method-table*
  (list
   (cons "bind"
         (lambda (fn)
           (lambda (this-arg &rest partial-args)
             (lambda (&rest args)
               (apply #'%js-funcall fn (list* this-arg (append partial-args args)))))))
   (cons "call"
         (lambda (fn)
           (lambda (this-arg &rest args)
             (apply #'%js-funcall fn (list* this-arg args)))))
   (cons "apply"
         (lambda (fn)
           (lambda (this-arg args-array)
             (apply #'%js-funcall fn
                    (list* this-arg
                           (if (%js-vec-p args-array) (coerce args-array 'list) nil))))))
   (cons "name" (constantly ""))
   (cons "length" (constantly 0.0d0))
   (cons "toString" (constantly (lambda () "function() { [native code] }"))))
  "Alist: Function.prototype property name -> (lambda (fn) -> value).")

(defun %js-resolve-function-method (obj key)
  (let ((entry (assoc key *js-function-method-table* :test #'string=)))
    (if entry
        (funcall (cdr entry) obj)
        +js-undefined+)))

(define-js-type-resolver %js-resolve-bigint-method nil
  "toString" (lambda (&optional radix)
               (%js-bigint-to-string obj
                (if (eq radix +js-undefined+) 10 (truncate (%js-to-number radix)))))
  "valueOf" (lambda () obj)
  "toLocaleString" (lambda (&rest _) (declare (ignore _)) (%js-bigint-to-string obj)))

(defparameter *js-type-dispatch-table*
  (list (cons #'js-promise-p #'%js-resolve-promise-method)
        (cons #'%js-vec-p #'%js-resolve-array-method)
        (cons #'stringp #'%js-resolve-string-method)
        (cons #'js-map-p #'%js-resolve-map-method)
        (cons #'js-date-p #'%js-resolve-date-method)
        (cons #'js-typed-array-p #'%js-resolve-typed-array-method)
        (cons #'js-regexp-p #'%js-resolve-regexp-method)
        (cons #'js-set-p #'%js-resolve-set-method)
        (cons #'js-weak-map-p #'%js-resolve-weak-map-method)
        (cons #'js-weak-set-p #'%js-resolve-weak-set-method)
        (cons #'js-finalization-registry-p #'%js-resolve-finalization-registry-method)
        (cons #'hash-table-p #'%js-resolve-object-method)
        (cons #'numberp #'%js-resolve-number-method)
        (cons #'js-symbol-p #'%js-resolve-symbol-method)
        (cons #'js-weak-ref-p #'%js-resolve-weak-ref-method)
        (cons #'functionp #'%js-resolve-function-method)
        (cons #'js-bigint-p #'%js-resolve-bigint-method))
  "Dispatch table: (predicate . resolver-fn) pairs for %js-resolve-method.")

(defun %js-resolve-method (obj key)
  "Resolve OBJ.KEY to a bound method closure, or +js-undefined+."
  (dolist (entry *js-type-dispatch-table* +js-undefined+)
    (when (funcall (car entry) obj)
      (return (funcall (cdr entry) obj key)))))

(setf *js-method-resolver* #'%js-resolve-method)
