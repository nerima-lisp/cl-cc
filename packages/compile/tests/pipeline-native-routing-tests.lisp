(in-package :cl-cc/test)


(defmacro with-native-build-stubs ((&key cache-path) &body body)
  "Stub native binary emission helpers for routing-focused tests." 
  `(with-replaced-function (cl-cc:compile-to-x86-64-bytes
                            (lambda (program &rest args)
                              (declare (ignore program args))
                              #(1 2 3)))
     (with-replaced-function (cl-cc/binary:make-mach-o-builder
                              (lambda (&rest args)
                                (declare (ignore args))
                                :builder))
        (with-replaced-function (cl-cc::%write-native-binary
                                 (lambda (builder code-bytes output-path &key &allow-other-keys)
                                   (declare (ignore builder code-bytes))
                                   output-path))
         ,(if cache-path
              `(with-replaced-function (cl-cc::%compile-cache-path
                                        (lambda (&rest args)
                                          (declare (ignore args))
                                          ,cache-path))
                 (with-replaced-function (cl-cc::%copy-file-bytes
                                          (lambda (from to)
                                            (declare (ignore from))
                                            to))
                   ,@body))
              `(progn ,@body))))))

(it-sequential "pipeline-native-cps-safe-ast-p-is-disabled-until-native-closures-exist"
  (let ((safe-ast (cl-cc:make-ast-let
                   :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                   :body (list (cl-cc:make-ast-binop
                                :op '+
                                :lhs (cl-cc:make-ast-var :name 'x)
                                :rhs (cl-cc:make-ast-int :value 2)))))
        (call-ast (cl-cc:make-ast-call
                   :func 'f
                   :args (list (cl-cc:make-ast-int :value 1))))
        (unsafe-ast (cl-cc/ast:make-ast-make-instance
                     :class (cl-cc:make-ast-quote :value 'point)
                     :initargs nil)))
    (expect (cl-cc::%cps-native-compile-safe-ast-p safe-ast) :to-be-falsy)
    (expect (cl-cc::%cps-native-compile-safe-ast-p call-ast) :to-be-falsy)
    (expect (cl-cc::%cps-native-compile-safe-ast-p unsafe-ast) :to-be-falsy)))

(it-sequential "pipeline-native-maybe-compile-via-cps-is-disabled"
  (let ((compiled-form nil))
    (with-replaced-function (cl-cc:compile-expression
                             (lambda (form &rest args)
                               (declare (ignore args))
                                (setf compiled-form form)
                                (cl-cc/compile:make-compilation-result :program :dummy)))
      (multiple-value-bind (result used-cps)
          (cl-cc::%maybe-compile-native-via-cps '(+ 1 2) :x86_64 nil)
        (expect used-cps :to-be-falsy)
        (expect result :to-be-null)
        (expect compiled-form :to-be-null)))))

(it-sequential "pipeline-native-maybe-compile-via-cps-skips-cps-transform"
  (let ((compiled-form nil))
    (with-replaced-function (cl-cc/compile:cps-transform-ast*
                             (lambda (ast)
                               (declare (ignore ast))
                               '((lambda (f) (funcall f #'identity))
                                 (lambda (k) (funcall k 3)))))
      (with-replaced-function (cl-cc:compile-expression
                               (lambda (form &rest args)
                                 (declare (ignore args))
                                  (setf compiled-form form)
                                  (cl-cc/compile:make-compilation-result :program :dummy)))
         (multiple-value-bind (result used-cps)
             (cl-cc::%maybe-compile-native-via-cps '(+ 1 2) :x86_64 nil)
          (expect used-cps :to-be-falsy)
          (expect result :to-be-null)
          (expect compiled-form :to-be-null))))))

(it-sequential "pipeline-native-compile-to-native-string-single-form-uses-direct-path"
  (let ((helper-called nil)
         (compile-toplevel-called nil))
    (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                              (lambda (form &rest args)
                                (declare (ignore form args))
                                (setf helper-called t)
                                (values nil nil)))
      (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  (setf compile-toplevel-called t)
                                  (cl-cc/compile:make-compilation-result :program :fallback)))
          (with-native-build-stubs ()
            (expect (cl-cc::compile-to-native "(+ 1 2)"
                                                    :output-file #P"out.bin"
                                                    :language :lisp) :to-equal #P"out.bin")
          (expect helper-called :to-be-truthy)
          (expect compile-toplevel-called :to-be-truthy))))))

(it-sequential "pipeline-native-compile-to-native-string-single-form-elisp-uses-direct-path"
  (let ((helper-called nil)
         (compile-toplevel-called nil))
    (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                              (lambda (form &rest args)
                                (declare (ignore form args))
                                (setf helper-called t)
                                (values nil nil)))
      (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  (setf compile-toplevel-called t)
                                  (cl-cc/compile:make-compilation-result :program :fallback)))
          (with-native-build-stubs ()
            (expect (cl-cc::compile-to-native "(+ 1 2)"
                                                    :output-file #P"out.bin"
                                                    :language :elisp) :to-equal #P"out.bin")
          (expect helper-called :to-be-truthy)
          (expect compile-toplevel-called :to-be-truthy))))))

(it-sequential "pipeline-native-compile-file-single-safe-form-uses-direct-path"
  (uiop:with-temporary-file (:pathname input :type "lisp" :keep t)
    (let ((helper-form nil)
          (compile-toplevel-called nil))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "(in-package :cl-user)" stream)
        (write-line "(+ 1 2)" stream))
      (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                               (lambda (form &rest args)
                                  (declare (ignore args))
                                  (setf helper-form form)
                                  (values nil nil)))
         (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                  (lambda (&rest args)
                                    (declare (ignore args))
                                    (setf compile-toplevel-called t)
                                    (cl-cc/compile:make-compilation-result :program :fallback)))
           (with-native-build-stubs (:cache-path #P"./tmp-native-cache.bin")
            (expect (cl-cc::compile-file-to-native input :output-file #P"out.bin" :language :lisp) :to-equal #P"out.bin")
            (expect helper-form :to-equal '(+ 1 2))
            (expect compile-toplevel-called :to-be-truthy))))))
  (ignore-errors (delete-file input)))

(it-sequential "pipeline-native-compile-file-elisp-single-safe-form-auto-detects-extension"
  (uiop:with-temporary-file (:pathname input :type "el" :keep t)
    (let ((helper-form nil)
          (compile-toplevel-called nil))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "(in-package :cl-user)" stream)
        (write-line "(+ 1 2)" stream))
      (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                               (lambda (form &rest args)
                                 (declare (ignore args))
                                 (setf helper-form form)
                                 (values nil nil)))
        (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                 (lambda (&rest args)
                                   (declare (ignore args))
                                   (setf compile-toplevel-called t)
                                   (cl-cc/compile:make-compilation-result :program :fallback)))
          (with-native-build-stubs (:cache-path #P"./tmp-native-cache.bin")
            (expect (cl-cc::compile-file-to-native input :output-file #P"out.bin") :to-equal #P"out.bin")
            (expect helper-form :to-equal '(+ 1 2))
             (expect compile-toplevel-called :to-be-truthy)))))
       (ignore-errors (delete-file input)))
  (it-sequential "pipeline-native-compile-file-multi-form-uses-cps-aware-toplevel"
  (uiop:with-temporary-file (:pathname input :type "lisp" :keep t)
    (let ((helper-called nil)
          (compile-toplevel-called nil))
      (with-open-file (stream input :direction :output :if-exists :supersede)
        (write-line "(+ 1 2)" stream)
        (write-line "(+ 3 4)" stream))
      (with-replaced-function (cl-cc::%maybe-compile-native-via-cps
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf helper-called t)
                                 (values (cl-cc/compile:make-compilation-result :program :dummy) t)))
        (with-replaced-function (cl-cc/compile:compile-toplevel-forms
                                 (lambda (&rest args)
                                   (declare (ignore args))
                                   (setf compile-toplevel-called t)
                                   (cl-cc/compile:make-compilation-result :program :fallback)))
          (with-native-build-stubs (:cache-path #P"./tmp-native-cache.bin")
            (expect (cl-cc::compile-file-to-native input :output-file #P"out.bin" :language :lisp) :to-equal #P"out.bin")
            (expect helper-called :to-be-falsy)
             (expect compile-toplevel-called :to-be-truthy)))))
       (ignore-errors (delete-file input)))))
