(in-package :cl-cc/test)


(it-sequential "fr-500-lto-ir-roundtrip"
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "unit" :target :x86-64 :language :lisp
                  :instructions (list (make-vm-const :dst :r0 :value 42)
                                      (make-vm-ret :reg :r0))))
         (bytes (cl-cc/pipeline:serialize-lto-ir (list module)))
         (roundtrip (cl-cc/pipeline:deserialize-lto-ir bytes)))
    (expect (= 1 (length roundtrip)) :to-be-truthy)
    (expect (typep (first (cl-cc/pipeline::lto-module-instructions (first roundtrip)))
                        'vm-const) :to-be-truthy)))

(it-sequential "fr-501-thin-lto-summary-generation"
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "thin" :target :x86-64 :language :lisp
                  :instructions (list (make-vm-closure :dst :r0 :label "f" :params '(:r1) :captured nil)
                                      (make-vm-label :name "f")
                                      (make-vm-ret :reg :r1))))
         (summary (cl-cc/pipeline:generate-thin-lto-summaries module))
         (functions (cl-cc/pipeline::thin-lto-summary-functions summary)))
    (expect (= 1 (length functions)) :to-be-truthy)
    (expect (cl-cc/pipeline::thin-lto-function-summary-signature (first functions)) :to-equal '(:r1))))

(defun %write-test-source (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

(it-sequential "pipeline-incremental-skips-unchanged-source"
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (let ((output (make-pathname :type "bin" :defaults source)))
      (%write-test-source source "(defun inc (x) (+ x 1))")
      (%write-test-source output "binary")
      (let ((state (cl-cc/pipeline:prepare-incremental-compilation source output :language :lisp)))
        (expect (cl-cc/pipeline:incremental-state-dirty-p state) :to-be-truthy)
        (cl-cc/pipeline:commit-incremental-compilation state))
      (let ((state (cl-cc/pipeline:prepare-incremental-compilation source output :language :lisp)))
        (expect (cl-cc/pipeline:incremental-state-dirty-p state) :to-be-falsy)
        (expect (cl-cc/pipeline:incremental-state-reason state) :to-be :unchanged))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file output)))))

(it-sequential "pipeline-incremental-macro-dependency-dirties-user"
  (uiop:with-temporary-file (:pathname macro-source :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname user-source :type "lisp" :keep t)
      (let ((macro-output (make-pathname :type "bin" :defaults macro-source))
            (user-output (make-pathname :type "bin" :defaults user-source)))
        (%write-test-source macro-source "(defmacro twice (x) `(+ ,x ,x))")
        (%write-test-source user-source "(twice 21)")
        (%write-test-source macro-output "binary")
        (%write-test-source user-output "binary")
        (cl-cc/pipeline:commit-incremental-compilation
         (cl-cc/pipeline:prepare-incremental-compilation macro-source macro-output :language :lisp))
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (expect (cl-cc/pipeline:incremental-state-dirty-p state) :to-be-truthy)
          (cl-cc/pipeline:commit-incremental-compilation state))
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (expect (cl-cc/pipeline:incremental-state-dirty-p state) :to-be-falsy))
        (%write-test-source macro-source "(defmacro twice (x) `(* 2 ,x))")
        (let ((state (cl-cc/pipeline:prepare-incremental-compilation user-source user-output :language :lisp)))
          (expect (cl-cc/pipeline:incremental-state-dirty-p state) :to-be-truthy)
          (expect (cl-cc/pipeline:incremental-state-reason state) :to-be :dependency-changed))
        (mapc (lambda (p) (ignore-errors (delete-file p)))
              (list macro-source user-source macro-output user-output))))))

(it-sequential "pipeline-hot-reload-entry-swaps-after-quiescence"
  (let* ((entered (sb-thread:make-semaphore :count 0))
         (release (sb-thread:make-semaphore :count 0))
         (entry (cl-cc/pipeline:make-hot-reload-entry
                 'f
                 (lambda ()
                   (sb-thread:signal-semaphore entered)
                   (sb-thread:wait-on-semaphore release)
                   :old)))
         (done nil))
    (let ((caller (sb-thread:make-thread
                   (lambda ()
                     (expect (cl-cc/pipeline:hot-reload-call entry) :to-be :old)))))
      (sb-thread:wait-on-semaphore entered)
      (let ((swapper (sb-thread:make-thread
                      (lambda ()
                        (cl-cc/pipeline:hot-reload-swap entry (lambda () :new))
                        (setf done t)))))
        (expect done :to-be-falsy)
        (sb-thread:signal-semaphore release)
        (sb-thread:join-thread caller)
        (sb-thread:join-thread swapper)
        (expect done :to-be-truthy)
        (expect (cl-cc/pipeline:hot-reload-call entry) :to-be :new)))))

(it-sequential "pipeline-parallel-respects-dependency-waves"
  (uiop:with-temporary-file (:pathname a :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname b :type "lisp" :keep t)
      (%write-test-source a "(defmacro m () 1)")
      (%write-test-source b "(m)")
      (cl-cc/pipeline::%write-deps-file b b (list (namestring (truename a))))
      (let ((order nil))
        (cl-cc/pipeline:compile-files-parallel
         (list a b)
         :workers 2
         :compile-function (lambda (file &rest args)
                             (declare (ignore args))
                             (push (pathname-name file) order)
                             file))
        (expect (reverse order) :to-equal (list a b)))
      (ignore-errors (delete-file a))
      (ignore-errors (delete-file b)))))
