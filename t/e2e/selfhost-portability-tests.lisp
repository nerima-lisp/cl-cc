;;;; tests/e2e/selfhost-portability-tests.lisp — SBCL Dependency Ratchet
(in-package :cl-cc/test)



(defparameter *disallowed-sbcl-packages*
  '("SB-EXT:" "SB-THREAD:" "SB-POSIX:" "SB-ALIEN:" "SB-GRAY:"
    "SB-MOP:" "SB-KERNEL:" "SB-INT:" "SB-INTROSPECT:" "SB-SYS:"
    "SB-BSD-SOCKETS:" "SB-DEBUG:" "SB-VM:" "SB-UNIX:" "SB-IMPL:"
    "SB-COVER:" "SB-PCL:")
  "SBCL package prefixes that must NOT appear in selfhost core source.")

(defparameter *selfhost-sbcl-allowlist*
  '("packages/runtime/src/portable.lisp"
    "packages/sb-mop/src/package.lisp"
    "packages/sb-pcl/"
    "packages/closer-mop/")
  "Files/directories allowed to reference SBCL packages in selfhost mode.")

(defun %file-on-allowlist-p (file-path)
  (dolist (entry *selfhost-sbcl-allowlist* nil)
    (when (or (string= file-path entry)
              (and (> (length file-path) (length entry))
                   (string= entry (subseq file-path 0 (length entry)))))
      (return t))))

(it-sequential "selfhost-no-sbcl-references-in-core"
  (let ((violations nil) (total-files 0))
    (dolist (source-file (selfhost-all-source-files))
      (let ((file-path (car source-file))
            (source-path (cdr source-file)))
        (incf total-files)
        (unless (%file-on-allowlist-p file-path)
          (handler-case
              (with-open-file (stream source-path :direction :input
                                                :if-does-not-exist nil)
                (when stream
                  (loop for line = (read-line stream nil nil)
                        for line-number from 1
                        while line
                        do (dolist (pkg *disallowed-sbcl-packages*)
                             (let ((pos (search pkg line :test #'char-equal)))
                               (when pos
                                 (unless (search ";" (subseq line 0 (min pos 2))
                                                 :test #'char=)
                                   (push (list file-path line-number
                                               (string-trim " " line))
                                         violations))))))))
            (file-error (condition)
              (push (list file-path 0 (princ-to-string condition))
                    violations))))))
    (expect (nreverse violations) :to-equal nil)
    (expect (> total-files 10) :to-be-truthy)))
