;;;; tests/unit/compile/stdlib-source-tests.lisp — Standard Library Source tests
(in-package :cl-cc/test)

(defun %count-substring (needle haystack)
  (let ((count 0)
        (start 0)
        (step (length needle)))
    (loop for pos = (search needle haystack :start2 start)
          while pos do
            (incf count)
            (setf start (+ pos step)))
    count))

(it-sequential "stdlib-source-string-present"
  (expect (stringp cl-cc::*standard-library-source*) :to-be-truthy)
  (expect (> (length cl-cc::*standard-library-source*) 0) :to-be-truthy))

(it-sequential "stdlib-source-contains-key-defuns mapcar"
  (destructuring-bind (needle) (list "(defun mapcar")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns reduce"
  (destructuring-bind (needle) (list "(defun reduce")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns class-precedence-list"
  (destructuring-bind (needle) (list "(defun class-precedence-list")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns proclaim"
  (destructuring-bind (needle) (list "(defun proclaim")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns short-site-name"
  (destructuring-bind (needle) (list "(defun short-site-name")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns long-site-name"
  (destructuring-bind (needle) (list "(defun long-site-name")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns with-compilation-unit"
  (destructuring-bind (needle) (list "(defmacro with-compilation-unit")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns get-setf-expansion"
  (destructuring-bind (needle) (list "(defun get-setf-expansion")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns eval-when"
  (destructuring-bind (needle) (list "(define-expander-for eval-when")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns symbol-macrolet"
  (destructuring-bind (needle) (list "(define-expander-for symbol-macrolet")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns define-symbol-macro"
  (destructuring-bind (needle) (list "(define-expander-for define-symbol-macro")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns string-left-trim"
  (destructuring-bind (needle) (list "(defun string-left-trim")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns string-right-trim"
  (destructuring-bind (needle) (list "(defun string-right-trim")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns string-upcase"
  (destructuring-bind (needle) (list "(defun string-upcase")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns string-downcase"
  (destructuring-bind (needle) (list "(defun string-downcase")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns string-capitalize"
  (destructuring-bind (needle) (list "(defun string-capitalize")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns nstring-upcase"
  (destructuring-bind (needle) (list "(defun nstring-upcase")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns nstring-downcase"
  (destructuring-bind (needle) (list "(defun nstring-downcase")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns nstring-capitalize"
  (destructuring-bind (needle) (list "(defun nstring-capitalize")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns pathname-match-p"
  (destructuring-bind (needle) (list "(defun pathname-match-p")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns translate-pathname"
  (destructuring-bind (needle) (list "(defun translate-pathname")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns set"
  (destructuring-bind (needle) (list "(defun set (sym val) (setf (symbol-value sym) val) val)")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-contains-key-defuns set-fdefinition"
  (destructuring-bind (needle) (list "(defun set-fdefinition")
    (expect (search needle cl-cc::*standard-library-source*) :to-be-truthy)))

(it-sequential "stdlib-source-has-many-defun-forms"
  (expect (> (%count-substring "(defun " cl-cc::*standard-library-source*) 20) :to-be-truthy))

(it-sequential "stdlib-source-omits-sbcl-compatibility-stubs"
  (expect (search "without-package-locks" cl-cc::*standard-library-source*) :to-be-falsy))
(it-sequential "stdlib-compile-file-creates-loadable-output"
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "cl-cc-compile-file-~A/" (gensym))
                                 (uiop:temporary-directory))))
         (source-path (merge-pathnames #P"input.lisp" root))
         (output-path (compile-file-pathname source-path))
         (explicit-output-path (merge-pathnames #P"explicit.fasl" root))
         (value-symbol (intern (format nil "*COMPILE-FILE-OUTPUT-~A*" (gensym))
                               :cl-cc/test)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (with-open-file (output source-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (format output "(in-package :cl-cc/test)~%(defparameter ~A 42)~%"
                     (symbol-name value-symbol)))
           (with-reset-repl-state
             (let ((compiled-path
                     (run-string
                      (format nil "(compile-file ~S)" (namestring source-path))
                      :stdlib t)))
               (expect (namestring compiled-path)
                       :to-equal (namestring (truename output-path)))
               (expect (probe-file output-path) :to-be-truthy))
             (let ((compiled-path
                     (run-string
                      (format nil "(compile-file ~S :output-file ~S)"
                              (namestring source-path)
                              (namestring explicit-output-path))
                      :stdlib t)))
               (expect (namestring compiled-path)
                       :to-equal (namestring (truename explicit-output-path)))
               (expect (probe-file explicit-output-path) :to-be-truthy))
             (cl-cc/repl::our-load output-path)
             (expect (gethash value-symbol
                              (cl-cc/vm:vm-global-vars cl-cc::*repl-vm-state*))
                     :to-be 42)))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))
(it-sequential "stdlib-compile-file-preserves-source-when-output-is-input"
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "cl-cc-compile-file-~A/" (gensym))
                                 (uiop:temporary-directory))))
         (source-path (merge-pathnames #P"input.lisp" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist source-path)
           (with-open-file (stream source-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "(defun compiled-copy () 42)" stream))
           (let ((source (uiop:read-file-string source-path)))
             (with-reset-repl-state
               (let ((compiled-path
                       (run-string
                        (format nil "(compile-file ~S :output-file ~S)"
                                (namestring source-path)
                                (namestring source-path))
                        :stdlib t)))
                 (expect (namestring compiled-path)
                         :to-equal (namestring (truename source-path)))
                 (expect (uiop:read-file-string source-path) :to-equal source)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t)))))
