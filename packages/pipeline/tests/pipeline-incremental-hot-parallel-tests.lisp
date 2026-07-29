(in-package :cl-cc/test)

(defun %write-test-source (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text out))
  path)

(it-sequential
  "pipeline-hot-reload-entry-swaps-after-quiescence"
  (let* ((timeout 5)
         (entered (sb-thread:make-semaphore :count 0))
         (release (sb-thread:make-semaphore :count 0))
         (caller-finished (sb-thread:make-semaphore :count 0))
         (swapper-finished (sb-thread:make-semaphore :count 0))
         (entry
           (cl-cc/pipeline:make-hot-reload-entry
             'f
             (lambda ()
               (sb-thread:signal-semaphore entered)
               (unless (sb-thread:wait-on-semaphore release :timeout timeout)
                 (error "Timed out waiting to release the active hot-reload call"))
               :old)))
         (caller-result nil)
         (caller-error nil)
         (swapper-error nil)
         (caller nil)
         (swapper nil))
    (labels ((await (semaphore description)
               (unless (sb-thread:wait-on-semaphore semaphore :timeout timeout)
                 (error "Timed out waiting for ~A" description)))
             (cleanup-thread (thread)
               (when thread
                 (when (sb-thread:thread-alive-p thread)
                   (sb-thread:terminate-thread thread))
                 (sb-thread:join-thread thread :timeout timeout :default :timed-out))))
      (unwind-protect
           (progn
             (setf caller
                   (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (setf caller-result
                                      (cl-cc/pipeline:hot-reload-call entry))
                              (error (condition)
                                (setf caller-error condition)))
                         (sb-thread:signal-semaphore caller-finished)))))
             (await entered "the active hot-reload call to enter")
             (setf swapper
                   (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (handler-case
                                (cl-cc/pipeline:hot-reload-swap
                                  entry
                                  (lambda ()
                                    :new))
                              (error (condition)
                                (setf swapper-error condition)))
                         (sb-thread:signal-semaphore swapper-finished)))))
             (expect (sb-thread:wait-on-semaphore swapper-finished :timeout 0.1)
                     :to-be-falsy)
             (sb-thread:signal-semaphore release)
             (await caller-finished "the active hot-reload call to finish")
             (await swapper-finished "the hot-reload swap to finish")
             (expect (eq (sb-thread:join-thread caller :timeout timeout :default :timed-out) :timed-out) :to-be-falsy)
             (expect (eq (sb-thread:join-thread swapper :timeout timeout :default :timed-out) :timed-out) :to-be-falsy)
             (when caller-error
               (error caller-error))
             (when swapper-error
               (error swapper-error))
             (expect caller-result :to-be :old)
             (expect (cl-cc/pipeline:hot-reload-call entry) :to-be :new))
        (sb-thread:signal-semaphore release)
        (cleanup-thread caller)
        (cleanup-thread swapper)))))

(it-sequential
  "pipeline-parallel-respects-dependency-waves"
  (let* ((tmp (uiop:temporary-directory))
         (suffix (string-downcase (symbol-name (gensym "clcc-wave-"))))
         (a0 (merge-pathnames (format nil "~A-a.lisp" suffix) tmp))
         (b0 (merge-pathnames (format nil "~A-b.lisp" suffix) tmp))
         (c0 (merge-pathnames (format nil "~A-c.lisp" suffix) tmp)))
    (unwind-protect (progn
        (%write-test-source a0 "(defun wave-a () 1)")
        (%write-test-source b0 (format nil "(load ~S)" (file-namestring a0)))
        (%write-test-source c0 (format nil "(load ~S)" (file-namestring b0)))
        (let* ((files (mapcar #'truename (list a0 b0 c0)))
               (pending (copy-list files))
               (graph (cl-cc/pipeline::build-source-dependency-graph files))
               (completed (make-hash-table :test #'equal)))
          (dolist (expected files)
            (let ((ready (cl-cc/pipeline::%parallel-ready-file pending completed graph)))
              (expect (namestring ready) :to-equal (namestring expected))
              (setf (gethash (namestring ready) completed) t
                    pending (remove ready pending :test #'equal))))))
      (dolist (file (list a0 b0 c0))
        (ignore-errors (delete-file file))))))
