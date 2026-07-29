(in-package :cl-cc/test)


(it-todo "fr-500-lto-ir-roundtrip"
  "pre-existing base API drift: the LTO/incremental pipeline entry points no longer accept :TARGET. Needs a base-API update to the current signature.")

(it-todo "fr-501-thin-lto-summary-generation"
  "pre-existing base API drift: the LTO/incremental pipeline entry points no longer accept :TARGET. Needs a base-API update to the current signature.")

(defun %write-test-source (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

(it-todo "pipeline-incremental-skips-unchanged-source"
  "pre-existing base API drift: the LTO/incremental pipeline entry points no longer accept :TARGET. Needs a base-API update to the current signature.")

(it-todo "pipeline-incremental-macro-dependency-dirties-user"
  "pre-existing base API drift: the LTO/incremental pipeline entry points no longer accept :TARGET. Needs a base-API update to the current signature.")

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

(it-todo "pipeline-parallel-respects-dependency-waves"
  "pre-existing base API drift: the LTO/incremental pipeline entry points no longer accept :TARGET. Needs a base-API update to the current signature.")
