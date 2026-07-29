;;;; packages/compile/tests/fr-318-compiler-warning-tests.lisp
;;;; FR-318: Compiler Warning System.

(in-package :cl-cc/test)

(defun fr-318-warning-messages (warnings)
  (mapcar #'cl-cc/parse:diagnostic-message warnings))

(it-sequential "fr-318-unused-let-binding-emits-structured-warning"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result))
           (warning (first warnings)))
      (expect (= 1 (length warnings)) :to-be-truthy)
      (expect (cl-cc/parse:diagnostic-severity warning) :to-be :warning)
      (expect (cl-cc/parse:diagnostic-error-code warning) :to-equal "W0001")
      (expect (search "UNUSED" (cl-cc/parse:diagnostic-message warning)) :to-be-truthy)
      (expect (cl-cc/compile:compilation-result-errors result) :to-be-null))))

(it-sequential "fr-318-ignore-declaration-suppresses-unused-warning"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((ignored 1)) (declare (ignore ignored)) 42))
                    :target :vm))
           (messages (fr-318-warning-messages
                      (cl-cc/compile:compilation-result-warnings result))))
      (expect (some (lambda (message)
                            (search "IGNORED" message))
                          messages) :to-be-falsy)
      (expect (cl-cc/compile:compilation-result-errors result) :to-be-null))))

(it-sequential "fr-318-cps-toplevel-exposes-unused-warning"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* t))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((let ((unused 1)) 42))
                    :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (expect (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings) :to-be-truthy)
      (expect (cl-cc/compile:compilation-result-errors result) :to-be-null))))

(it-sequential "fr-318-used-let-binding-does-not-warn"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let ((result (cl-cc/compile:compile-toplevel-forms
                   '((let ((used 1)) used))
                   :target :vm)))
      (expect (cl-cc/compile:compilation-result-warnings result) :to-be-null)
      (expect (cl-cc/compile:compilation-result-errors result) :to-be-null))))

(it-sequential "fr-318-type-check-signals-error"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (signals error (cl-cc/compile:type-check-ast
       (cl-cc:make-ast-var :name 'nonexistent-var-xyz)))))

(it-sequential "fr-318-compile-expression-exposes-unused-warning"
  (let ((cl-cc/compile:*enable-cps-vm-primary-path* nil))
    (let* ((result (cl-cc:compile-expression '(let ((unused 1)) 42) :target :vm))
           (warnings (cl-cc/compile:compilation-result-warnings result)))
      (expect (some (lambda (warning)
                           (equal (cl-cc/parse:diagnostic-error-code warning) "W0001"))
                         warnings) :to-be-truthy))))

(it-sequential "fr-318-abi-symbol-mangling-roundtrip"
  (let* ((mangled (cl-cc/compile:mangle-function-name 'foo
                                                      :package :cl-cc
                                                      :specializers '(integer string)))
           (demangled (cl-cc/compile:demangle-name mangled)))
    (expect mangled :to-equal "_ZN5cl-cc3fooI7integer6stringEE")
    (expect demangled :to-equal "cl-cc:foo<integer, string>")))

(it-sequential "fr-318-abi-manifest-dump-and-compatibility"
  (uiop:with-temporary-file (:pathname manifest-path :type "sexp" :keep t)
    (let ((manifest (cl-cc/compile:dump-abi-manifest manifest-path)))
      (expect (probe-file manifest-path) :to-be-truthy)
      (with-open-file (in manifest-path :direction :input)
        (expect (read in) :to-equal '(:version "1.0.0"
                        :exports nil
                        :struct-layouts nil
                        :checksum 0)))
      (expect (cl-cc/compile:check-abi-compatibility manifest manifest) :to-be-null)
      (expect (cl-cc/compile:check-abi-compatibility
                     '(:version "1.0.0" :exports ("FOO") :struct-layouts nil :checksum 0)
                     '(:version "1.0.0" :exports ("BAR") :struct-layouts nil :checksum 0)) :to-equal '(:exports)))))

(it-sequential "fr-318-namespace-cycle-detection-finds-loop"
  (let ((cl-cc/compile:*namespaces* (make-hash-table :test #'equal)))
    (cl-cc/compile:define-namespace 'alpha :imports '(beta))
    (cl-cc/compile:define-namespace 'beta :imports '(alpha))
    (let ((issues (cl-cc/compile:check-namespace-deps)))
      (expect (some (lambda (issue)
                           (and (consp issue)
                                (eq (first issue) :cycle)))
                         issues) :to-be-truthy))))
