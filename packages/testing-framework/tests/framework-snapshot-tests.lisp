(in-package :cl-cc/test)

(defmacro with-private-snapshot-directory ((directory) &body body)
  (let ((anchor (gensym "ANCHOR")))
    `(uiop:with-temporary-file (:pathname ,anchor)
       (let* ((,directory
                (make-pathname :defaults ,anchor :name nil :type nil
                               :directory (append (pathname-directory ,anchor)
                                                  (list (pathname-name ,anchor)))))
              (*snapshot-dir* (namestring ,directory))
              (*update-snapshots* nil))
         (declare (ignorable ,directory))
         (unwind-protect
              (progn
                (ensure-directories-exist (merge-pathnames "placeholder" ,directory))
                ,@body)
           (when (probe-file ,directory)
             (uiop:delete-directory-tree ,directory :validate t)))))))

(cl-weave:it-sequential "snapshot default is bound and relative"
  (cl-weave:expect (boundp '*snapshot-dir*))
  (let ((default *snapshot-dir*))
    (with-private-snapshot-directory (directory)
      (let ((*snapshot-dir* default))
        (cl-weave:expect (stringp *snapshot-dir*))
        (cl-weave:expect (eq :relative (first (pathname-directory (pathname *snapshot-dir*)))))
        (cl-weave:expect (string= "tests/snapshots/default.snap" (%snapshot-path "default"))))
      (cl-weave:expect (assert-snapshot "default" '(default value)))
      (cl-weave:expect (equal '(default value) (%read-snapshot (%snapshot-path "default")))))))

(cl-weave:it-sequential "snapshot initial save"
  (with-private-snapshot-directory (directory)
    (cl-weave:expect (eq :snapshot-not-found (%read-snapshot (%snapshot-path "first"))))
    (cl-weave:expect (assert-snapshot "first" '(1 2)))
    (cl-weave:expect (probe-file (%snapshot-path "first")))
    (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "first"))))))

(cl-weave:it-sequential "snapshot equal leaves saved value unchanged"
  (with-private-snapshot-directory (directory)
    (%write-snapshot (%snapshot-path "value") '(1 2))
    (cl-weave:expect (assert-snapshot "value" '(1 2)))
    (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "value"))))))

(cl-weave:it-sequential "snapshot mismatch signals assertion failure without updating"
  (with-private-snapshot-directory (directory)
    (%write-snapshot (%snapshot-path "value") '(1 2))
    (cl-weave:signals cl-weave:assertion-failure (assert-snapshot "value" '(3 4)))
    (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "value"))))))

(cl-weave:it-sequential "snapshot explicit update replaces saved value"
  (with-private-snapshot-directory (directory)
    (%write-snapshot (%snapshot-path "value") '(1 2))
    (let ((*update-snapshots* t))
      (cl-weave:expect (assert-snapshot "value" '(3 4))))
    (cl-weave:expect (equal '(3 4) (%read-snapshot (%snapshot-path "value"))))))

(cl-weave:it-sequential "snapshot sentinel is a saved value, not an absent file"
  (with-private-snapshot-directory (directory)
    (cl-weave:expect (assert-snapshot "sentinel" :snapshot-not-found))
    (cl-weave:expect (eq :snapshot-not-found (%read-snapshot (%snapshot-path "sentinel"))))
    (cl-weave:expect (assert-snapshot "sentinel" :snapshot-not-found))
    (cl-weave:signals cl-weave:assertion-failure
      (assert-snapshot "sentinel" '(different value)))
    (multiple-value-bind (saved present-p) (%read-snapshot (%snapshot-path "sentinel"))
      (cl-weave:expect (eq :snapshot-not-found saved))
      (cl-weave:expect present-p))
    (let ((*update-snapshots* t))
      (cl-weave:expect (assert-snapshot "sentinel" '(updated value))))
    (cl-weave:expect (equal '(updated value) (%read-snapshot (%snapshot-path "sentinel"))))))

(cl-weave:it-sequential "snapshot read file error is not an absent file"
  (with-private-snapshot-directory (directory)
    (%write-snapshot (%snapshot-path "value") '(unchanged value))
    ;; OPEN rejects wildcard pathnames even when missing files return NIL.
    (cl-weave:signals file-error (%read-snapshot (%snapshot-path "value*")))
    (cl-weave:signals file-error (assert-snapshot "value*" '(different value)))
    (cl-weave:expect (equal '(unchanged value) (%read-snapshot (%snapshot-path "value"))))))

(cl-weave:it-sequential "snapshot evaluates form once on every branch"
  (with-private-snapshot-directory (directory)
    (let ((calls 0))
      (cl-weave:expect (assert-snapshot "value" (progn (incf calls) '(1 2))))
      (cl-weave:expect (= 1 calls))
      (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "value")))))
    (let ((calls 0))
      (cl-weave:expect (assert-snapshot "value" (progn (incf calls) '(1 2))))
      (cl-weave:expect (= 1 calls))
      (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "value")))))
    (let ((calls 0))
      (cl-weave:signals cl-weave:assertion-failure
        (assert-snapshot "value" (progn (incf calls) '(3 4))))
      (cl-weave:expect (= 1 calls))
      (cl-weave:expect (equal '(1 2) (%read-snapshot (%snapshot-path "value")))))
    (let ((calls 0)
          (*update-snapshots* t))
      (cl-weave:expect (assert-snapshot "value" (progn (incf calls) '(3 4))))
      (cl-weave:expect (= 1 calls))
      (cl-weave:expect (equal '(3 4) (%read-snapshot (%snapshot-path "value")))))))

(cl-weave:it-sequential "snapshot directory binding is dynamic and isolated"
  (let ((default *snapshot-dir*))
    (with-private-snapshot-directory (outer-directory)
      (let ((outer (%snapshot-path "same")))
        (cl-weave:expect (assert-snapshot "same" '(outer)))
        (with-private-snapshot-directory (inner-directory)
          (cl-weave:expect (not (string= outer (%snapshot-path "same"))))
          (cl-weave:expect (assert-snapshot "same" '(inner)))
          (cl-weave:expect (equal '(inner) (%read-snapshot (%snapshot-path "same")))))
        (cl-weave:expect (string= outer (%snapshot-path "same")))
        (cl-weave:expect (equal '(outer) (%read-snapshot outer)))))
    (cl-weave:expect (string= default *snapshot-dir*))))
