(in-package :cl-cc/test)

(defun %expected-failure-print-value (value)
  (let ((*print-pretty* nil)
        (*print-circle* t)
        (*print-level* nil)
        (*print-length* nil))
    (write-to-string value :pretty nil)))

(defun %expected-failure-collapse-whitespace (text)
  (with-output-to-string (stream)
    (let ((written nil)
          (pending-space nil))
      (loop for char across text
            do (if (find char '(#\Space #\Tab #\Newline #\Return #\Page))
                   (when written (setf pending-space t))
                   (progn
                     (when pending-space (write-char #\Space stream))
                     (write-char char stream)
                     (setf written t
                           pending-space nil)))))))

(defun %expected-failure-signature (entry expectedp)
  (let ((status (if expectedp
                    (getf entry :expected-status)
                    (getf entry :status))))
    (if (eq status :error)
        (list :status status
              :condition-class (getf entry :condition-class)
              :condition-message (getf entry :condition-message))
        (list :status status
              :condition-class (getf entry :condition-class)
              :assertion-form (getf entry :assertion-form)
              :matcher (getf entry :matcher)
              :actual (getf entry :actual)
              :expected (getf entry :expected)
              :negated (getf entry :negated)))))

(defun %expected-failure-result-from-event (event)
  (let* ((status (cl-weave::test-event-status event))
         (condition (cl-weave::test-event-condition event))
         (assertion (cl-weave::test-event-assertion event))
         (result (list :path (cl-weave::path-string
                              (cl-weave::test-event-path event))
                       :status status
                       :condition-class (when condition
                                         (%expected-failure-print-value
                                          (type-of condition))))))
    (if (eq status :error)
        (append result
                (list :condition-message (princ-to-string condition)))
        (if assertion
            (append result
                    (list :assertion-form
                          (%expected-failure-collapse-whitespace
                           (%expected-failure-print-value
                            (cl-weave::assertion-detail-form assertion)))
                          :matcher (%expected-failure-print-value
                                    (cl-weave::assertion-detail-matcher assertion))
                          :actual (%expected-failure-print-value
                                   (cl-weave::assertion-detail-actual assertion))
                          :expected (%expected-failure-print-value
                                     (cl-weave::assertion-detail-expected assertion))
                          :negated (%expected-failure-print-value
                                    (cl-weave::assertion-detail-negated assertion)))
            result)))))

(defun %expected-failure-valid-entry-p (entry)
  (and (listp entry)
       (evenp (length entry))
       (loop for tail on entry by #'cddr
             always (keywordp (car tail)))
       (stringp (getf entry :path))
       (member (getf entry :expected-status) '(:fail :error))
       (stringp (getf entry :condition-class))
       (stringp (getf entry :owner-repository))
       (stringp (getf entry :work-order))
       (if (eq (getf entry :expected-status) :error)
           (stringp (getf entry :condition-message))
           (every (lambda (key) (stringp (getf entry key)))
                  '(:assertion-form :matcher :actual :expected :negated)))))

(defun %validate-expected-failure-manifest (manifest)
  (let ((paths (make-hash-table :test #'equal)))
    (dolist (entry manifest)
      (unless (%expected-failure-valid-entry-p entry)
        (error "Invalid expected-failure manifest entry: ~S" entry))
      (let ((path (getf entry :path)))
        (when (gethash path paths)
          (error "Duplicate expected-failure test path: ~A" path))
        (setf (gethash path paths) t)))
    manifest))

(defun read-expected-failure-manifest
    (&optional (pathname (asdf:system-relative-pathname
                          "cl-cc/test"
                          "packages/testing-framework/expected-failures.sexp")))
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (%validate-expected-failure-manifest
     (let ((*read-eval* nil)
           (*package* (find-package :cl-cc/test)))
       (loop for entry = (read stream nil :eof)
             until (eq entry :eof)
             collect entry)))))

(defun audit-expected-failure-results (manifest results &key require-complete)
  (let ((manifest-by-path (make-hash-table :test #'equal))
        (result-paths (make-hash-table :test #'equal))
        (matched nil)
        (violations nil))
    (dolist (entry manifest)
      (unless (%expected-failure-valid-entry-p entry)
        (push (list :kind :invalid-manifest-entry :path (getf entry :path))
              violations))
      (let ((path (getf entry :path)))
        (if (gethash path manifest-by-path)
            (push (list :kind :duplicate-manifest-entry :path path) violations)
            (setf (gethash path manifest-by-path) entry))))
    (dolist (result results)
      (let ((path (getf result :path))
            (status (getf result :status)))
        (cond
          ((not (stringp path))
           (push (list :kind :invalid-result-path :status status) violations))
          ((gethash path result-paths)
           (push (list :kind :duplicate-result :path path :status status)
                 violations))
          (t
           (setf (gethash path result-paths) result)
           (let ((expected (gethash path manifest-by-path)))
             (cond
               ((and (null expected) (member status '(:fail :error)))
                (push (list :kind :unregistered-failure
                            :path path :status status)
                      violations))
               ((and expected (eq status :pass))
                (push (list :kind :unexpected-pass :path path :status status)
                      violations))
               ((and expected
                     (member status '(:fail :error))
                     (equal (%expected-failure-signature expected t)
                            (%expected-failure-signature result nil)))
                (push expected matched))
               (expected
                (push (list :kind :condition-changed
                            :path path :status status
                            :expected (%expected-failure-signature expected t)
                            :actual (%expected-failure-signature result nil))
                      violations))))))))
    (progn
      (when require-complete
        (maphash (lambda (path entry)
                   (declare (ignore entry))
                   (unless (gethash path result-paths)
                     (push (list :kind :not-run :path path) violations)))
                 manifest-by-path))
      (values (nreverse matched) (nreverse violations)))))

(defun %expected-failure-count-status (results status)
  (count status results :key (lambda (result) (getf result :status))))

(defun %report-expected-failure-violation (violation stream)
  (format stream "# ~A ~A~%"
          (string-upcase (symbol-name (getf violation :kind)))
          (or (getf violation :path) "manifest")))
(defun %ensure-no-expected-failure-violations (violations)
  (when violations
    (error "cl-cc expected-failure manifest rejected ~D test result(s)."
           (length violations)))
  t)

(defun run-tests (&key (reporter :spec) name-filter max-workers
                    (stream *standard-output*))
  (let* ((manifest (read-expected-failure-manifest))
         (results (mapcar #'%expected-failure-result-from-event
                          (cl-weave::normalize-run-results
                           (cl-weave:run (cl-weave::root-suite)
                                         :reporter reporter
                                         :stream stream
                                         :name-filter name-filter
                                         :max-workers max-workers)))))
    (multiple-value-bind (matched violations)
        (audit-expected-failure-results manifest results
                                        :require-complete (null name-filter))
      (dolist (entry matched)
        (format stream "# [XFAIL] ~A owner=~A work-order=~A~%"
                (getf entry :path)
                (getf entry :owner-repository)
                (getf entry :work-order)))
      (dolist (violation violations)
        (%report-expected-failure-violation violation stream))
      (let* ((errored (count-if (lambda (violation)
                                  (eq (getf violation :status) :error))
                                violations))
             (failed (- (length violations) errored)))
        (format stream
                "# cl-cc-tests summary: ~D passed, ~D expected-failed, ~D skipped, ~D todo, ~D failed, ~D errored, ~D total~%"
                (%expected-failure-count-status results :pass)
                (length matched)
                (%expected-failure-count-status results :skip)
                (%expected-failure-count-status results :todo)
                failed
                errored
                (length results)))
      (%ensure-no-expected-failure-violations violations))))
