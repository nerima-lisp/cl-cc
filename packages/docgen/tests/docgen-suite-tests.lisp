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

(in-suite cl-cc-unit-suite)

(defun %docgen-fixture-path ()
  "Return the docgen fixture source path relative to the runtime CWD."
  (merge-pathnames "packages/docgen/tests/fixture-source.lisp"
                   *default-pathname-defaults*))

(deftest docgen-generate-api-docs-emits-documented-definitions
  "generate-api-docs renders documented functions, macros, structs, classes, parameters, and variables, and omits undocumented definitions."
  (let ((markdown (cl-cc/docgen:generate-api-docs (%docgen-fixture-path))))
    (assert-true (search "## defun documented-function" markdown))
    (assert-true (search "Return X unchanged." markdown))
    (assert-true (search "## defmacro documented-macro" markdown))
    (assert-true (search "Evaluate BODY in order." markdown))
    (assert-true (search "## defstruct documented-struct" markdown))
    (assert-true (search "A documented fixture structure." markdown))
    (assert-true (search "## defclass documented-class" markdown))
    (assert-true (search "A documented fixture class." markdown))
    (assert-true (search "## defparameter *documented-parameter*" markdown))
    (assert-true (search "A documented fixture parameter." markdown))
    (assert-true (search "## defvar *documented-var*" markdown))
    (assert-true (search "A documented fixture variable." markdown))
    ;; Undocumented definitions must be excluded from the output.
    (assert-null (search "undocumented-function" markdown))))
