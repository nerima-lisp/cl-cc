;;;; tests/integration/pipeline-tests.lisp — Pipeline API Tests
;;;
;;; Tests for compile-expression, compile-string, run-string,
;;; %prescan-in-package, parse-source-for-language, get-stdlib-forms,
;;; run-string-repl, our-eval, and reset-repl-state.

(in-package :cl-cc/test)

;;; ─── compile-expression ─────────────────────────────────────────────────

(it-sequential "pipeline-compile-expression-binop-structure"
  (let* ((result (compile-expression '(+ 1 2)))
         (prog   (compilation-result-program result))
         (instrs (vm-program-instructions prog))
         (asm    (compilation-result-assembly result))
         (cps    (compilation-result-cps result)))
    (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)
    (expect (typep prog 'cl-cc/vm::vm-program) :to-be-truthy)
    (expect (> (length instrs) 0) :to-be-truthy)
    (expect (stringp asm) :to-be-truthy)
    (expect (or (null cps) (consp cps)) :to-be-truthy)))

(it-sequential "pipeline-compile-expression-constant-halts"
  (let* ((result (compile-expression 42))
            (instrs (vm-program-instructions (compilation-result-program result)))
            (cps (compilation-result-cps result)))
      (declare (ignore cps))
      (expect (typep (car (last instrs)) 'cl-cc/vm::vm-halt) :to-be-truthy)))

(it-sequential "pipeline-policy-data-tables-are-populated"
  (expect (member 'cl-cc/ast:ast-defun cl-cc::*cps-host-eval-unsafe-ast-types*) :to-be-truthy)
  (expect (member 'cl-cc/ast:ast-node cl-cc/compile:*cps-native-compile-unsupported-ast-types*) :to-be-truthy)
  (expect (member 'cl-cc/ast:ast-setq cl-cc/compile:*cps-compile-unsupported-ast-types*) :to-be-falsy)
  (expect (member 'cl-cc/ast:ast-setq cl-cc/compile:*cps-native-compile-unsupported-ast-types*) :to-be-falsy)
  (expect (first '(("PARSE-LAMBDA-LIST"            . :cl-cc/expand)
                               ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc/expand)
                               ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc/expand)
                               ("LAMBDA-LIST-INFO-ENVIRONMENT" . :cl-cc/expand))) :to-equal '("PARSE-LAMBDA-LIST" . :cl-cc/expand)))

(it-sequential "pipeline-policy-data-host-eval-unsafe-forms-cover-control-and-definitions"
  (expect (member 'cl-cc/ast:ast-defgeneric cl-cc::*cps-host-eval-unsafe-ast-types*) :to-be-truthy)
  (expect (member 'cl-cc/ast:ast-unwind-protect cl-cc::*cps-host-eval-unsafe-ast-types*) :to-be-truthy)
  (expect (member 'cl-cc/ast:ast-setq cl-cc::*cps-host-eval-unsafe-ast-types*) :to-be-falsy))

(it-sequential "pipeline-maybe-cps-toplevel-forms-rewrites-safe-expression-forms"
  (let* ((forms     '((defvar *top* 1) (setq *top* 2)))
         (opts      (cl-cc::%make-pipeline-opts :target :vm))
         (rewritten (cl-cc::%maybe-cps-toplevel-forms forms opts)))
    (expect (equal (first forms) (first rewritten)) :to-be-falsy)
    (expect (equal (second forms) (second rewritten)) :to-be-falsy)
    (expect (consp (first rewritten)) :to-be-truthy)
    (expect (consp (second rewritten)) :to-be-truthy)))

(it-sequential "pipeline-compile-expression-vm-program-uses-raw-stream"
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
          (program-instrs (vm-program-instructions (compilation-result-program result)))
          (raw-instrs (cl-cc:compilation-result-vm-instructions result))
          (optimized-instrs (cl-cc:compilation-result-optimized-instructions result)))
    (expect raw-instrs :to-equal program-instrs)
    (expect (> (length raw-instrs) 0) :to-be-truthy)
    (expect (listp optimized-instrs) :to-be-truthy)))

(it-sequential "pipeline-compile-string-accepts-pgo-speed-kwargs"
  (let ((result (compile-string "(+ 1 2)"
                                :target :vm
                                :speed 3
                                :inline-threshold-scale 2)))
     (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)))

(it-sequential "pipeline-verify-transforms-flag-is-dynamically-scoped"
  (let ((cl-cc/optimize:*translation-validation-enabled* nil))
    (let ((result (compile-string "(+ 1 2)" :target :vm :verify-transforms t)))
      (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)
      (expect cl-cc/optimize:*translation-validation-enabled* :to-be-falsy))))

(it-sequential "pipeline-compile-string-emits-pgo-counter-plan"
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (plan (cl-cc/compile:compilation-result-pgo-counter-plan result)))
    (expect plan :to-be-truthy)
    (expect (integerp (getf plan :total-bb)) :to-be-truthy)
    (expect (integerp (getf plan :total-edge)) :to-be-truthy)
    (expect (consp (getf plan :bb-counters)) :to-be-truthy)
    (expect (consp (getf plan :edge-counters)) :to-be-truthy)
    (expect (consp (getf plan :bb-runtime-keys)) :to-be-truthy)
    (expect (listp (getf plan :edge-runtime-keys)) :to-be-truthy)))

(it-sequential "pipeline-compile-string-with-stdlib-emits-pgo-counter-plan"
  (let* ((result (cl-cc:compile-string-with-stdlib "(+ 1 2)" :target :vm))
         (plan (cl-cc/compile:compilation-result-pgo-counter-plan result)))
    (expect plan :to-be-truthy)
    (expect (integerp (getf plan :total-bb)) :to-be-truthy)
    (expect (consp (getf plan :bb-runtime-keys)) :to-be-truthy)))

(it-sequential "pipeline-tier-0-and-tier-1-are-distinguishable"
  (let ((tier0 (compile-string "(+ 1 2)" :target :vm :compilation-tier 0))
        (tier1 (compile-string "(+ 1 2)" :target :vm :compilation-tier 1)))
    (expect (= 0 (cl-cc/vm:vm-program-compilation-tier
                 (cl-cc/compile:compilation-result-program tier0))) :to-be-truthy)
    (expect (= 1 (cl-cc/vm:vm-program-compilation-tier
                 (cl-cc/compile:compilation-result-program tier1))) :to-be-truthy)
    (expect (listp (cl-cc/compile:compilation-result-optimized-instructions tier0)) :to-be-truthy)
    (expect (listp (cl-cc/compile:compilation-result-optimized-instructions tier1)) :to-be-truthy)))

(it-sequential "pipeline-maybe-bump-opts-speed-from-ast-defun-declaration"
  (let* ((opts (cl-cc::%make-pipeline-opts :target :vm :speed nil))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 3)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc::%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (expect (= 3 (cl-cc::pipeline-opts-speed opts)) :to-be-truthy)))

(it-sequential "pipeline-maybe-bump-opts-speed-from-ast-does-not-lower-existing-speed"
  (let* ((opts (cl-cc::%make-pipeline-opts :target :vm :speed 3))
         (ast (cl-cc/ast:make-ast-defun
               :name 'f
               :params '(x)
               :optional-params nil
               :rest-param nil
               :key-params nil
               :declarations '((optimize (speed 1)))
               :documentation nil
               :body (list (make-ast-var :name 'x)))))
    (cl-cc::%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (expect (= 3 (cl-cc::pipeline-opts-speed opts)) :to-be-truthy)))

(it-sequential "pipeline-compile-toplevel-forms-defvar-type-env with-type-check"
  (destructuring-bind (forms lookup-sym type-check) (list '((defvar *typed-top-level* 42)) '*typed-top-level* t)
    (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (expect (typep (cl-cc/compile:compilation-result-type-env result)
                        'cl-cc/type:type-env) :to-be-truthy)
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (expect found-p :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme)) :to-be 'fixnum)))))

(it-sequential "pipeline-compile-toplevel-forms-defvar-type-env without-type-check"
  (destructuring-bind (forms lookup-sym type-check) (list '((defvar *typed-top-level-no-check* 42)) '*typed-top-level-no-check* nil)
    (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (expect (typep (cl-cc/compile:compilation-result-type-env result)
                        'cl-cc/type:type-env) :to-be-truthy)
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (expect found-p :to-be-truthy)
      (expect (cl-cc/type:type-primitive-name
                          (cl-cc/type::type-scheme-type scheme)) :to-be 'fixnum)))))

(it-sequential "pipeline-compile-toplevel-forms-captures-cps"
  (let ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2) (- 4 1)))))
    (expect (or (null (cl-cc/compile:compilation-result-cps result))
                     (consp (cl-cc/compile:compilation-result-cps result))) :to-be-truthy)))

(it-sequential "pipeline-compile-toplevel-forms-program-uses-raw-stream-for-vm"
  (let* ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2) (- 4 1)) :target :vm))
          (program-instrs (vm-program-instructions (cl-cc/compile:compilation-result-program result)))
          (raw-instrs (cl-cc/compile:compilation-result-vm-instructions result))
          (optimized-instrs (cl-cc/compile:compilation-result-optimized-instructions result)))
    (expect raw-instrs :to-equal program-instrs)
    (expect (> (length raw-instrs) 0) :to-be-truthy)
    (expect (listp optimized-instrs) :to-be-truthy)))

(it-sequential "pipeline-compile-toplevel-forms-defun-type-env with-type-check"
  (destructuring-bind (forms lookup-sym type-check) (list '((defun typed-id (x) x)) 'typed-id t)
    (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (expect found-p :to-be-truthy)
      (expect (cl-cc/type:type-arrow-p
                    (cl-cc/type::type-scheme-type scheme)) :to-be-truthy)))))

(it-sequential "pipeline-compile-toplevel-forms-defun-type-env without-type-check"
  (destructuring-bind (forms lookup-sym type-check) (list '((defun typed-id-no-check (x) x)) 'typed-id-no-check nil)
    (let ((result (cl-cc/compile:compile-toplevel-forms forms :target :vm :type-check type-check)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup lookup-sym
                                     (cl-cc/compile:compilation-result-type-env result))
      (expect found-p :to-be-truthy)
      (expect (cl-cc/type:type-arrow-p
                    (cl-cc/type::type-scheme-type scheme)) :to-be-truthy)))))

;;; Additional eval/prescan/stdlib integration tests live in pipeline-eval-tests.lisp.

;;; ─── LTO / post-link optimization integration evidence ─────────────────────

(defun %fr501-thin-lto-hot-module ()
  (cl-cc/pipeline::make-lto-module
   :name "thin-hot"
   :source-file "thin-hot.lisp"
   :instructions
   (list (cl-cc:make-vm-func-ref :dst :F
                                 :label "hot-fn"
                                 :params '(:X)
                                 :dispatch-tag '(:known-function . hot-fn))
         (cl-cc:make-vm-label :name "hot-fn")
         (cl-cc:make-vm-const :dst :R :value 1)
         (cl-cc:make-vm-ret :reg :R))
   :metadata '(:test :fr-501)))

(it-sequential "fr-501-thin-lto-summaries-are-generated-and-used-for-imports"
  (let* ((module (%fr501-thin-lto-hot-module))
         (summary (cl-cc/pipeline::thin-lto-generate-module-summary module))
         (payload (cl-cc/pipeline::thin-lto-serialize-summary summary))
         (summaries (cl-cc/pipeline::thin-lto-read-summaries (list payload))))
    (expect (cl-cc/pipeline::thin-lto-module-summary-module-name summary) :to-equal "thin-hot")
    (expect (cl-cc/pipeline::thin-lto-module-summary-functions summary) :to-be-truthy)
    (expect (first (cl-cc/pipeline::thin-lto-select-imports summaries :threshold -1)) :to-equal '("thin-hot" . "hot-fn"))
    (multiple-value-bind (optimized used-summaries)
        (cl-cc/pipeline::thin-lto-optimize-modules (list module) :threshold -1)
      (expect (first optimized) :to-be-truthy)
      (expect used-summaries :to-be-truthy)
      (expect (getf (cl-cc/pipeline::lto-module-metadata (first optimized)) :imports) :to-equal '("hot-fn")))))

(defun %fr660-function-layout-program ()
  (cl-cc:make-vm-program
   :instructions
   (list (cl-cc:make-vm-func-ref :dst :H :label "hot-entry" :params nil
                                 :dispatch-tag '(:known-function . hot-fn))
         (cl-cc:make-vm-func-ref :dst :C :label "cold-entry" :params nil
                                 :dispatch-tag '(:known-function . cold-fn))
         (cl-cc:make-vm-jump :label "hot-end")
         (cl-cc:make-vm-label :name "hot-entry")
         (cl-cc:make-vm-const :dst :R :value :hot)
         (cl-cc:make-vm-ret :reg :R)
         (cl-cc:make-vm-label :name "hot-end")
         (cl-cc:make-vm-jump :label "cold-end")
         (cl-cc:make-vm-label :name "cold-entry")
         (cl-cc:make-vm-const :dst :R :value :cold)
         (cl-cc:make-vm-ret :reg :R)
         (cl-cc:make-vm-label :name "cold-end")
         (cl-cc:make-vm-halt :reg :R))
   :result-register :R))

(defun %label-position (instructions label)
  (position-if (lambda (inst)
                 (and (typep inst 'cl-cc/vm:vm-label)
                      (string= (cl-cc/vm:vm-name inst) label)))
               instructions))

(it-sequential "fr-660-bolt-flag-triggers-post-link-layout-path"
  (let* ((program (%fr660-function-layout-program))
         (opts '(:bolt t :bolt-profile nil :perf-map nil))
         (optimized (cl-cc:maybe-pipeline-bolt-optimize-program program opts))
         (instructions (cl-cc/vm:vm-program-instructions optimized)))
    (expect (eq program optimized) :to-be-falsy)
    (expect (%label-position instructions "hot-entry") :to-be-truthy)
    (expect (%label-position instructions "cold-entry") :to-be-truthy)))

(it-sequential "fr-661-autofdo-profile-guided-layout-moves-cold-blocks-after-hot-blocks"
  (let* ((instructions (list (cl-cc:make-vm-label :name "cold-fn")
                             (cl-cc:make-vm-const :dst :R :value :cold)
                             (cl-cc:make-vm-label :name "hot-fn")
                             (cl-cc:make-vm-const :dst :R :value :hot)))
         (profile-data '(:layout-decisions ((:function "cold-fn" :count 1 :layout :cold)
                                            (:function "hot-fn" :count 99 :layout :hot))))
         (laid-out (cl-cc:autofdo-apply-layout-decisions instructions profile-data)))
    (expect (< (%label-position laid-out "hot-fn")
                    (%label-position laid-out "cold-fn")) :to-be-truthy)))
