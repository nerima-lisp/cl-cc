;;;; packages/compile/tests/fr-318-compiler-warning-tests.lisp
;;;; FR-318: Compiler Warning System.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun fr-318-warning-messages (warnings)
  (mapcar #'cl-cc/parse:diagnostic-message warnings))

(deftest fr-318-unused-let-binding-emits-structured-warning
  "FR-318: codegen records an unused lexical variable as a structured warning."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result))
           (warning (first warnings)))
      (assert-= 1 (length warnings))
      (assert-eq :warning (cl-cc/parse:diagnostic-severity warning))
      (assert-equal "W0001" (cl-cc/parse:diagnostic-error-code warning))
      (assert-true (search "UNUSED" (cl-cc/parse:diagnostic-message warning)))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-ignore-declaration-suppresses-unused-warning
  "FR-318: `(declare (ignore ...))` suppresses the unused binding warning."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((ignored 1)) (declare (ignore ignored)) 42))
                    :target :vm))
           (messages (fr-318-warning-messages
                      (cl-cc/compile:compilation-result-warnings result))))
      (assert-false (some (lambda (message)
                            (search "IGNORED" message))
                          messages))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-cps-toplevel-exposes-unused-warning
  "FR-318: CPS top-level compilation propagates nested codegen warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* t))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-used-let-binding-does-not-warn
  "FR-318: used lexical variables do not produce unused-variable warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let ((result (cl-cc/compile:compile-toplevel-forms
                   '((let ((used 1)) used))
                   :target :vm)))
      (assert-null (cl-cc/compile:compilation-result-warnings result))
      (assert-null (cl-cc/compile:compilation-result-errors result)))))

(deftest fr-318-type-check-signals-error
  "FR-318: invalid type-check inputs signal a host type error."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (assert-signals error
      (cl-cc/compile:type-check-ast
       (cl-cc:make-ast-var :name 'nonexistent-var-xyz)))))

(deftest fr-318-compile-expression-exposes-unused-warning
  "FR-318: public single-form compilation exposes codegen warnings."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc:compile-expression '(let ((unused 1)) 42) :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (assert-true (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings)))))

(deftest fr-318-abi-symbol-mangling-roundtrip
  "FR-318: mangled ABI symbols round-trip through the local parser."
  :tags '(:fr-318)
    (let* ((mangled (cl-cc/compile:mangle-function-name 'foo
                                                      :package :cl-cc
                                                      :specializers '(integer string)))
           (demangled (cl-cc/compile:demangle-name mangled)))
    (assert-equal "_ZN5cl-cc3fooI7integer6stringEE" mangled)
    (assert-equal "cl-cc:foo<integer, string>" demangled)))

(deftest fr-318-abi-manifest-dump-and-compatibility
  "FR-318: ABI manifests are serializable and diffable."
  :tags '(:fr-318)
  (uiop:with-temporary-file (:pathname manifest-path :type "sexp" :keep t)
    (let ((manifest (cl-cc/compile:dump-abi-manifest manifest-path)))
      (assert-true (probe-file manifest-path))
      (with-open-file (in manifest-path :direction :input)
        (assert-equal '(:version "1.0.0"
                        :exports nil
                        :struct-layouts nil
                        :checksum 0)
                      (read in)))
      (assert-null (cl-cc/compile:check-abi-compatibility manifest manifest))
      (assert-equal '(:exports)
                    (cl-cc/compile:check-abi-compatibility
                     '(:version "1.0.0" :exports ("FOO") :struct-layouts nil :checksum 0)
                     '(:version "1.0.0" :exports ("BAR") :struct-layouts nil :checksum 0))))))

(deftest fr-318-namespace-cycle-detection-finds-loop
  "FR-318: namespace dependency cycles are detected."
  :tags '(:fr-318)
  (let ((cl-cc/compile:*namespaces* (make-hash-table :test #'equal)))
    (cl-cc/compile:define-namespace 'alpha :imports '(beta))
    (cl-cc/compile:define-namespace 'beta :imports '(alpha))
    (let ((issues (cl-cc/compile:check-namespace-deps)))
      (assert-true (some (lambda (issue)
                           (and (consp issue)
                                (eq (first issue) :cycle)))
                         issues)))))
