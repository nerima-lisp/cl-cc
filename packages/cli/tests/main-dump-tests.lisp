;;;; tests/unit/cli/main-dump-tests.lisp — dump/compile-option helper tests

(in-package :cl-cc/test)


(it-sequential "cli-string-suffix-p-basic-cases php-match"
  (destructuring-bind (expected suffix str) (list t ".php" "hello.php")
    (expect (if (cl-cc/cli::%string-suffix-p suffix str) t nil) :to-be expected)))

(it-sequential "cli-string-suffix-p-basic-cases out-match"
  (destructuring-bind (expected suffix str) (list t "out" "a.out")
    (expect (if (cl-cc/cli::%string-suffix-p suffix str) t nil) :to-be expected)))

(it-sequential "cli-string-suffix-p-basic-cases lisp-mismatch"
  (destructuring-bind (expected suffix str) (list nil ".lisp" "hello.php")
    (expect (if (cl-cc/cli::%string-suffix-p suffix str) t nil) :to-be expected)))

(it-sequential "cli-string-suffix-p-basic-cases too-long"
  (destructuring-bind (expected suffix str) (list nil "longer" "short")
    (expect (if (cl-cc/cli::%string-suffix-p suffix str) t nil) :to-be expected)))

(it-sequential "cli-arch-keyword-parses-supported-architectures x86-64"
  (destructuring-bind (expected input) (list :x86-64 "x86-64")
    (expect (cl-cc/cli::%arch-keyword input) :to-be expected)))

(it-sequential "cli-arch-keyword-parses-supported-architectures x86_64"
  (destructuring-bind (expected input) (list :x86-64 "x86_64")
    (expect (cl-cc/cli::%arch-keyword input) :to-be expected)))

(it-sequential "cli-arch-keyword-parses-supported-architectures arm64"
  (destructuring-bind (expected input) (list :arm64 "arm64")
    (expect (cl-cc/cli::%arch-keyword input) :to-be expected)))

(it-sequential "cli-arch-keyword-parses-supported-architectures aarch64"
  (destructuring-bind (expected input) (list :arm64 "aarch64")
    (expect (cl-cc/cli::%arch-keyword input) :to-be expected)))

(it-sequential "cli-arch-keyword-invalid-exits-2"
  (let ((stderr (make-string-output-stream)))
    (let ((code (with-fake-quit
                  (let ((*error-output* stderr))
                    (cl-cc/cli::%arch-keyword "mips")
                    (expect t :to-be-falsy)))))
      (expect (= 2 code) :to-be-truthy))
    (expect (search "Unknown architecture" (get-output-stream-string stderr)) :to-be-truthy)))

(it-sequential "cli-compile-target-keyword-parses-supported-architectures x86-64"
  (destructuring-bind (expected input) (list :x86_64 "x86-64")
    (expect (cl-cc/cli::%compile-target-keyword input) :to-be expected)))

(it-sequential "cli-compile-target-keyword-parses-supported-architectures x86_64"
  (destructuring-bind (expected input) (list :x86_64 "x86_64")
    (expect (cl-cc/cli::%compile-target-keyword input) :to-be expected)))

(it-sequential "cli-compile-target-keyword-parses-supported-architectures arm64"
  (destructuring-bind (expected input) (list :aarch64 "arm64")
    (expect (cl-cc/cli::%compile-target-keyword input) :to-be expected)))

(it-sequential "cli-compile-target-keyword-parses-supported-architectures aarch64"
  (destructuring-bind (expected input) (list :aarch64 "aarch64")
    (expect (cl-cc/cli::%compile-target-keyword input) :to-be expected)))

(it-sequential "cli-compile-target-keyword-invalid-signals-error"
  (handler-case
      (progn
        (cl-cc/cli::%compile-target-keyword "weird")
        (expect t :to-be-falsy))
    (error (e)
      (expect (search "Unknown architecture for compilation"
                           (princ-to-string e)) :to-be-truthy))))

(it-sequential "cli-parse-opt-remarks-mode-cases nil-input"
  (destructuring-bind (expected input) (list nil nil)
    (expect (cl-cc/cli::%parse-opt-remarks-mode input) :to-be expected)))

(it-sequential "cli-parse-opt-remarks-mode-cases empty-str"
  (destructuring-bind (expected input) (list nil "")
    (expect (cl-cc/cli::%parse-opt-remarks-mode input) :to-be expected)))

(it-sequential "cli-parse-opt-remarks-mode-cases all"
  (destructuring-bind (expected input) (list :all "all")
    (expect (cl-cc/cli::%parse-opt-remarks-mode input) :to-be expected)))

(it-sequential "cli-parse-opt-remarks-mode-cases changed"
  (destructuring-bind (expected input) (list :changed "changed")
    (expect (cl-cc/cli::%parse-opt-remarks-mode input) :to-be expected)))

(it-sequential "cli-parse-opt-remarks-mode-cases missed"
  (destructuring-bind (expected input) (list :missed "missed")
    (expect (cl-cc/cli::%parse-opt-remarks-mode input) :to-be expected)))

(it-sequential "cli-parse-opt-remarks-mode-invalid-exits-2"
  (let ((stderr (make-string-output-stream)))
    (let ((code (with-fake-quit
                  (let ((*error-output* stderr))
                    (cl-cc/cli::%parse-opt-remarks-mode "bogus")
                    (expect t :to-be-falsy)))))
      (expect (= 2 code) :to-be-truthy))
    (expect (search "Unknown opt-remarks mode" (get-output-stream-string stderr)) :to-be-truthy)))

(it-sequential "cli-parse-opt-remarks-mode-invalid-shows-did-you-mean"
  (let ((stderr (make-string-output-stream)))
    (let ((code (with-fake-quit
                  (let ((*error-output* stderr))
                    (cl-cc/cli::%parse-opt-remarks-mode "chagned")
                    (expect t :to-be-falsy)))))
      (expect (= 2 code) :to-be-truthy))
    (let ((out (get-output-stream-string stderr)))
      (expect (search "did you mean" out) :to-be-truthy)
      (expect (search "changed" out) :to-be-truthy))))

(it-sequential "cli-parse-compile-opts-reads-shared-flags"
  (let* ((parsed (make-cli-parsed
                  :command "compile"
                 :flags '(("--pass-pipeline" . t)
                          ("--time-passes" . t)
                          ("--trace-json" . "trace.json")
                          ("--flamegraph" . "fg.svg")
                           ("--stats" . t)
                           ("--trace-emit" . t)
                           ("--verify-transforms" . t)
                           ("--shadow-stack" . t)
                          ("--opt-remarks" . "changed"))))
         (opts (cl-cc/cli::%parse-compile-opts parsed)))
    (expect (cl-cc/cli::compile-opts-pass-pipeline opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-print-pass-timings opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-trace-json-path opts) :to-equal "trace.json")
    (expect (cl-cc/cli::compile-opts-flamegraph-path opts) :to-equal "fg.svg")
    (expect (cl-cc/cli::compile-opts-print-pass-stats opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-trace-emit opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-verify-transforms opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-shadow-stack opts) :to-be-truthy)
    (expect (cl-cc/cli::compile-opts-opt-remarks-mode opts) :to-be :changed)))

(it-sequential "cli-compile-opts-kwargs-expands-struct"
  (let* ((opts (cl-cc/cli::make-compile-opts
                :pass-pipeline t
                :print-pass-timings t
                :trace-json-path "trace.json"
                :print-pass-stats t
                 :trace-emit t
                 :verify-transforms t
                 :shadow-stack t
                :opt-remarks-mode :missed))
         (kwargs (cl-cc/cli::%compile-opts-kwargs opts :trace-stream)))
    (expect kwargs :to-equal '(:trace-json-stream :trace-stream
                    :print-pass-stats t
                    :pass-pipeline t
                     :opt-bisect-limit nil
                     :inline-threshold-scale 1
                     :verify-transforms t
                     :shadow-stack t
                    :print-pass-timings t
                    :print-opt-remarks t
                    :opt-remarks-mode :missed))))

(it-sequential "cli-dump-ir-phase-invalid-signals-error"
  (handler-case
      (progn
        (cl-cc/cli::%dump-ir-phase :bogus nil *standard-output* nil)
        (expect t :to-be-falsy))
    (error (e)
      (expect (search "Unknown IR phase" (princ-to-string e)) :to-be-truthy))))

;;; FR-463 regression: %dump-ir-phase dispatches all 6 recognized IR phases
;;; and produces non-empty output for a minimal compilation-result.

(defun %make-minimal-compilation-result (&key (source-location-p t))
  "Build a compilation-result with enough fields populated that every IR phase
dump function can write at least one line without erroring."
  (let ((ast (cl-cc:make-ast-int :value 42
                                 :source-file (and source-location-p "test.lisp")
                                 :source-line (and source-location-p 1)
                                 :source-column (and source-location-p 0))))
    (cl-cc:make-compilation-result
     :program (cl-cc:make-vm-program
               :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)))
     :assembly "mov rax, 42"
     :globals (make-hash-table :test #'equal)
     :type nil
     :type-env nil
     :cps '(lambda (k) (funcall k 42))
     :ast ast
     :vm-instructions (list (cl-cc:make-vm-const :dst :r0 :value 42))
     :optimized-instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases ast"
  (destructuring-bind (phase) (list :ast)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases cps"
  (destructuring-bind (phase) (list :cps)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases ssa"
  (destructuring-bind (phase) (list :ssa)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases vm"
  (destructuring-bind (phase) (list :vm)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases opt"
  (destructuring-bind (phase) (list :opt)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-dispatches-all-phases asm"
  (destructuring-bind (phase) (list :asm)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-annotate-source-writes-comment-for-ast"
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :ast result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (search "; source:" output) :to-be-truthy))))

(it-sequential "cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt vm"
  (destructuring-bind (phase) (list :vm)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (search "; source:" output) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt opt"
  (destructuring-bind (phase) (list :opt)
    (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (search "; source:" output) :to-be-truthy)))))

(it-sequential "cli-dump-ir-phase-asm-output-is-ansi-colored"
  (let* ((result (%make-minimal-compilation-result))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase :asm result stream nil)
    (let ((output (get-output-stream-string stream)))
      (expect (search cl-cc/cli::+ansi-opcode+ output) :to-be-truthy)
      (expect (search cl-cc/cli::+ansi-reset+ output) :to-be-truthy))))

(it-sequential "cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location ast"
  (destructuring-bind (phase) (list :ast)
    (let* ((result (%make-minimal-compilation-result :source-location-p nil))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)
      (expect (search "; source:" output) :to-be-falsy)))))

(it-sequential "cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location vm"
  (destructuring-bind (phase) (list :vm)
    (let* ((result (%make-minimal-compilation-result :source-location-p nil))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)
      (expect (search "; source:" output) :to-be-falsy)))))

(it-sequential "cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location opt"
  (destructuring-bind (phase) (list :opt)
    (let* ((result (%make-minimal-compilation-result :source-location-p nil))
         (stream (make-string-output-stream)))
    (cl-cc/cli::%dump-ir-phase phase result stream t)
    (let ((output (get-output-stream-string stream)))
      (expect (> (length output) 0) :to-be-truthy)
      (expect (search "; source:" output) :to-be-falsy)))))

(it-sequential "cli-dump-ir-phase-phase-table-covers-all-recognized-phases"
  (dolist (phase cl-cc/cli::*ir-phases*)
    (expect (cdr (assoc phase cl-cc/cli::*ir-phase-dump-fns*)) :to-be-truthy)))
