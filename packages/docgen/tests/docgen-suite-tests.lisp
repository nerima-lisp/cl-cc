;;;; tests/docgen-suite-tests.lisp - API documentation generator suite tests
;;;
;;; Wires the docgen fixture test into the cl-cc/test suite so it runs under
;;; the canonical `cl-weave:run-all` plan (nix run .#test). The standalone
;;; cl-cc-docgen/test system (docgen-tests.lisp / run-docgen-tests) remains an
;;; independent entry point.
;;;
;;; The fixture is resolved relative to *default-pathname-defaults* (reset to
;;; the runtime CWD / repo root by nix run .#test) rather than *load-pathname*,
;;; which would bake the build-sandbox FASL path into the compiled test.

(in-package :cl-cc/test)

(defun %docgen-fixture-path ()
  "Return the docgen fixture source path relative to the runtime CWD."
  (merge-pathnames "packages/docgen/tests/fixture-source.lisp"
                   *default-pathname-defaults*))

(it-sequential "docgen-generate-api-docs-emits-documented-definitions"
  (let ((markdown (cl-cc/docgen:generate-api-docs (%docgen-fixture-path))))
    (expect (search "## defun documented-function" markdown) :to-be-truthy)
    (expect (search "Return X unchanged." markdown) :to-be-truthy)
    (expect (search "## defmacro documented-macro" markdown) :to-be-truthy)
    (expect (search "Evaluate BODY in order." markdown) :to-be-truthy)
    (expect (search "## defstruct documented-struct" markdown) :to-be-truthy)
    (expect (search "A documented fixture structure." markdown) :to-be-truthy)
    (expect (search "## defclass documented-class" markdown) :to-be-truthy)
    (expect (search "A documented fixture class." markdown) :to-be-truthy)
    (expect (search "## defparameter *documented-parameter*" markdown) :to-be-truthy)
    (expect (search "A documented fixture parameter." markdown) :to-be-truthy)
    (expect (search "## defvar *documented-var*" markdown) :to-be-truthy)
    (expect (search "A documented fixture variable." markdown) :to-be-truthy)
    ;; Undocumented definitions must be excluded from the output.
    (expect (search "undocumented-function" markdown) :to-be-null)))
