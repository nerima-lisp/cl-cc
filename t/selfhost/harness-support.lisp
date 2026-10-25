(require :asdf)

(defpackage #:cl-cc/selfhost-harness
            (:use #:cl)
            (:export #:*project-root*
                     #:configure-project-asdf
                     #:read-expected-mismatches
                     #:compare-mismatch-sets
                     #:check-mismatch-set
                     #:assert-matcher-controls))

(in-package #:cl-cc/selfhost-harness)

(defparameter *harness-directory*
  (uiop:ensure-directory-pathname
   (make-pathname :defaults (truename *load-truename*) :name nil :type nil)))

(defvar *project-root* nil)
  (defvar *reader-eval-control-fired* nil)

(defun harness-path (relative)
  (merge-pathnames relative *harness-directory*))

(defun read-octets (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (unless (= size (read-sequence octets stream))
        (error "Could not read complete output: ~A" pathname))
      octets)))

(defun configure-project-asdf ()
  (let* ((candidate-root (truename (merge-pathnames #P"../../" *harness-directory*)))
         (project-asd (merge-pathnames "cl-cc.asd" candidate-root)))
    (asdf:initialize-source-registry
     `(:source-registry (:directory ,(namestring *harness-directory*))
                        (:tree ,(namestring candidate-root))
                        :inherit-configuration))
    (when (probe-file project-asd)
      (asdf:load-asd project-asd))
    (unless (find-package "CL-CC")
      (asdf:load-system "cl-cc"))
    (setf *project-root* (truename (asdf:system-source-directory "cl-cc")))
    (unless (probe-file (merge-pathnames "cl-cc.asd" *project-root*))
      (error "ASDF cl-cc source has no root ASD: ~A" *project-root*))
    t))

(defun %mismatch-entry-p (entry)
  (and (listp entry)
       (evenp (length entry))
       (member (getf entry :check) '(:determinism :stage2-fixpoint) :test #'eq)
       (keywordp (getf entry :id))
       (member (getf entry :classification) '(:bug :unimplemented) :test #'eq)
       (stringp (getf entry :reason))))

(defun %validate-mismatch-entries (entries source)
  (unless (and (listp entries) (every #'%mismatch-entry-p entries))
    (error "Invalid mismatch entries in ~A: ~S" source entries))
  (let ((keys
         (mapcar
          (lambda (entry)
            (list (getf entry :check) (getf entry :id)))
          entries)))
    (unless (= (length keys) (length (remove-duplicates keys :test #'equal)))
      (error "Duplicate mismatch identity in ~A" source)))
  entries)

(defun %read-expected-mismatches-stream (stream source)
    (let* ((*read-eval* nil)
           (entries (read stream nil :eof)))
      (when (eq entries :eof)
        (error "Expected mismatch file is empty: ~A" source))
      (unless (eq (read stream nil :eof) :eof)
        (error "Expected mismatch file must contain one list: ~A" source))
      (%validate-mismatch-entries entries source)))
  (defun read-expected-mismatches
      (&optional (pathname (harness-path "expected-mismatches.sexp")))
    (with-open-file (stream pathname :direction :input)
      (%read-expected-mismatches-stream stream pathname)))

(defun compare-mismatch-sets (actual expected &optional (stream *standard-output*))
  (%validate-mismatch-entries actual :actual)
  (%validate-mismatch-entries expected :expected)
  (let ((unknown (set-difference actual expected :test #'equal))
        (stale (set-difference expected actual :test #'equal)))
    (format stream "actual mismatches: ~S~%" actual)
    (format stream "expected mismatches: ~S~%" expected)
    (format stream "unknown mismatches: ~S~%" unknown)
    (format stream "stale expected mismatches: ~S~%" stale)
    (values (and (null unknown) (null stale)) unknown stale)))

(defun check-mismatch-set (check actual expected &optional (stream *standard-output*))
  (%validate-mismatch-entries actual :actual)
  (%validate-mismatch-entries expected :expected)
  (unless
      (every
       (lambda (entry)
         (eq (getf entry :check) check))
       actual)
    (error "Actual mismatch set contains an entry for another check: ~S" actual))
  (compare-mismatch-sets actual
                         (remove-if-not
                          (lambda (entry)
                            (eq (getf entry :check) check))
                          expected)
                         stream))

(defun assert-reader-eval-control ()
    (let ((*reader-eval-control-fired* nil)
          (reader-rejected-p nil))
      (with-input-from-string
          (stream "#.(setf cl-cc/selfhost-harness::*reader-eval-control-fired* t)")
        (handler-case
            (%read-expected-mismatches-stream stream :reader-control)
          (error (condition)
            (setf reader-rejected-p
                  (typep condition (quote reader-error))))))
      (unless (and reader-rejected-p (not *reader-eval-control-fired*))
        (error "Mismatch reader did not reject #. before evaluation"))
      t))
  (defun assert-matcher-controls ()
    (let ((probe
            (list :check :determinism
                  :id :matcher-control-probe
                  :classification :bug
                  :reason "synthetic matcher control entry"))
          (sink (make-broadcast-stream)))
      (multiple-value-bind (empty-matches unknown stale)
          (compare-mismatch-sets nil nil sink)
        (declare (ignore unknown stale))
        (unless empty-matches
          (error "Mismatch matcher rejected identical empty sets")))
      (multiple-value-bind (unknown-matches unknown stale)
          (compare-mismatch-sets (list probe) nil sink)
        (declare (ignore stale))
        (when (or unknown-matches (not (equal unknown (list probe))))
          (error "Mismatch matcher did not reject a synthetic unknown mismatch")))
      (multiple-value-bind (stale-matches unknown stale)
          (compare-mismatch-sets nil (list probe) sink)
        (declare (ignore unknown))
        (when (or stale-matches (not (equal stale (list probe))))
          (error "Mismatch matcher did not reject a synthetic stale entry")))
      (assert-reader-eval-control)
      t))
  (defun %asdf-source-files (system-name project-prefix)
    (let* ((system
             (or (asdf:find-system system-name nil)
                 (error "Required selfhost ASDF system is missing: ~A" system-name)))
           (system-root (asdf:system-source-directory system))
           (result nil))
      (labels ((walk (component)
                 (typecase component
                   (asdf:cl-source-file
                    (let* ((source-path (asdf:component-pathname component))
                           (relative-path (enough-namestring source-path system-root)))
                      (push (cons (concatenate (quote string) project-prefix relative-path)
                                  source-path)
                            result)))
                   (asdf:module
                    (dolist (child (asdf:component-children component))
                      (walk child))))))
        (dolist (component (asdf:component-children system))
          (walk component)))
      (nreverse result)))
  (defun selfhost-all-source-files ()
    (append
     (%asdf-source-files :cl-cc-bootstrap "packages/bootstrap/")
     (%asdf-source-files :cl-cc-ast "packages/ast/")
     (%asdf-source-files :cl-cc-mir "packages/mir/")
     (%asdf-source-files :cl-cc-binary "packages/binary/")
     (%asdf-source-files :cl-cc-runtime "packages/runtime/")
     (%asdf-source-files :cl-cc-parse "packages/parse/")
     (%asdf-source-files :cl-cc-type "packages/type/")
     (%asdf-source-files :cl-cc-vm "packages/vm/")
     (%asdf-source-files :cl-cc-optimize "packages/optimize/")
     (%asdf-source-files :cl-cc-emit "packages/emit/")
     (%asdf-source-files :cl-cc-compile "packages/compile/")
     (%asdf-source-files :cl-cc-stdlib "packages/stdlib/")
     (list
      (or (assoc "src/package.lisp"
                 (%asdf-source-files :cl-cc "")
                 :test (function string=))
          (error "The cl-cc ASDF system has no src/package.lisp source")))
     (%asdf-source-files :cl-cc-pipeline "packages/pipeline/")))
