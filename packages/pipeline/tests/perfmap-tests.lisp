(in-package :cl-cc/test)

(in-suite pipeline-native-suite)

(deftest pipeline-perf-map-line-validates-format
  "FR-553 perf map lines use HEX_ADDR HEX_SIZE SYMBOL_NAME format."
  (assert-true (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO"))
  (assert-false (cl-cc/pipeline:perf-map-line-valid-p "1000 2A")))

(deftest pipeline-write-perf-map-entry-emits-hex-fields
  "write-perf-map-entry writes perf-compatible hex address and size fields."
  (let ((stream (make-string-output-stream)))
    (cl-cc/pipeline:write-perf-map-entry stream #x1000 #x2a 'sample-function)
    (let ((line (string-trim '(#\Newline #\Return)
                             (get-output-stream-string stream))))
      (assert-string= "1000 2A SAMPLE-FUNCTION" line)
      (assert-true (cl-cc/pipeline:perf-map-line-valid-p line)))))

(deftest pipeline-write-perf-map-for-native-code-appends-map-line
  "write-perf-map-for-native-code records a map line for native code bytes."
  (uiop:with-temporary-file (:pathname path :type "map" :keep t)
    (let ((program (cl-cc:make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                        (cl-cc:make-vm-halt :reg :r0))))
          (bytes #(1 2 3 4)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (let ((cl-cc/pipeline:*perf-map-stream* out))
          (cl-cc/pipeline:write-perf-map-for-native-code program bytes :output-file "unit-main")))
      (let ((line (with-open-file (in path :direction :input) (read-line in nil nil))))
        (assert-true (cl-cc/pipeline:perf-map-line-valid-p line))
        (assert-true (search "unit-main" line :test #'char-equal))))
    (ignore-errors (delete-file path))))

;;;; ─── FR-500 Link-Time Optimization (pipeline-lto.lisp) ──────────────────

(defun %lto-sample-instructions ()
  "A single closed-world unit: LIVE is rooted, DEAD is defined but unreachable."
  (list (cl-cc:make-vm-closure :dst :r0 :label "live")
        (cl-cc:make-vm-register-function :name "live" :src :r0)
        (cl-cc:make-vm-closure :dst :r1 :label "dead")
        (cl-cc:make-vm-label :name "live")
        (cl-cc:make-vm-ret :reg :r0)
        (cl-cc:make-vm-label :name "dead")
        (cl-cc:make-vm-const :dst :r2 :value 99)
        (cl-cc:make-vm-ret :reg :r2)))

(defun %lto-label-names (instructions)
  (loop for inst in instructions
        when (typep inst 'cl-cc:vm-label)
          collect (cl-cc:vm-name inst)))

(deftest lto-serialize-instructions-round-trips
  "FR-500: LTO serializes VM instructions to a readable payload and back."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 42)
                      (cl-cc:make-vm-ret :reg :r0)))
         (payload (cl-cc/pipeline::lto-serialize-instructions insts))
         (restored (cl-cc/pipeline::lto-deserialize-instructions payload)))
    (assert-true (stringp payload))
    (assert-= 2 (length restored))
    (assert-true (typep (first restored) 'cl-cc:vm-const))
    (assert-= 42 (cl-cc:vm-value (first restored)))))

(deftest lto-serialize-module-round-trips
  "FR-500: an LTO module payload preserves name and instructions across a read/print cycle."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-ret :reg :r0)))
         (payload (cl-cc/pipeline:lto-serialize-module "mod" insts))
         (module (cl-cc/pipeline:lto-deserialize-module payload)))
    (assert-equal "mod" (cl-cc/pipeline::lto-module-name module))
    (assert-= 2 (length (cl-cc/pipeline::lto-module-instructions module)))
    (assert-true (typep (first (cl-cc/pipeline::lto-module-instructions module))
                        'cl-cc:vm-const))))

(deftest lto-deserialize-module-rejects-invalid-payload
  "FR-500: deserializing a payload without the module tag signals an error."
  (assert-signals error (cl-cc/pipeline:lto-deserialize-module "(:not-a-module 1 2)")))

(deftest lto-make-bitcode-section-from-module-and-payload
  "FR-500: bitcode sections describe an object-file section; raw payloads pass through."
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "b"
                  :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                      (cl-cc:make-vm-ret :reg :r0))))
         (section (cl-cc/pipeline::lto-make-bitcode-section module)))
    (assert-equal "__bitcode" (getf section :name))
    (assert-eq :progbits (getf section :type))
    (assert-true (stringp (getf section :payload))))
  (let ((section (cl-cc/pipeline::lto-make-bitcode-section "rawpayload")))
    (assert-equal "rawpayload" (getf section :payload))))

(deftest lto-merge-modules-concatenates-instruction-streams
  "FR-500: merging modules appends their instruction streams in order."
  (let* ((m1 (cl-cc/pipeline:make-lto-module
              :name "a" :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1))))
         (m2 (cl-cc/pipeline:make-lto-module
              :name "b" :instructions (list (cl-cc:make-vm-const :dst :r1 :value 2)
                                            (cl-cc:make-vm-ret :reg :r1))))
         (merged (cl-cc/pipeline:lto-merge-modules (list m1 m2))))
    (assert-= 3 (length merged))))

(deftest lto-eliminates-unreachable-functions
  "FR-500: closed-world DCE drops function bodies unreachable from any root."
  (let* ((instructions (%lto-sample-instructions))
         (graph (cl-cc/pipeline::lto-build-cross-module-call-graph (list instructions)))
         (pruned (cl-cc/pipeline::lto-eliminate-dead-functions instructions graph))
         (labels (%lto-label-names pruned)))
    (assert-true (member "live" labels :test #'string=))
    (assert-false (member "dead" labels :test #'string=))))

(deftest lto-optimize-modules-gates-dce-with-run-dce
  "FR-500: lto-optimize-modules runs DCE only when :RUN-DCE is true and returns the call graph."
  (let ((module (cl-cc/pipeline:make-lto-module
                 :name "m" :instructions (%lto-sample-instructions))))
    (multiple-value-bind (pruned graph)
        (cl-cc/pipeline:lto-optimize-modules (list module) :run-ipcp nil :run-dce t)
      (assert-true (cl-cc/pipeline::lto-call-graph-roots graph))
      (assert-false (member "dead" (%lto-label-names pruned) :test #'string=)))
    (let ((kept (cl-cc/pipeline:lto-optimize-modules
                 (list module) :run-ipcp nil :run-dce nil)))
      (assert-true (member "dead" (%lto-label-names kept) :test #'string=)))))

(deftest lto-apply-to-program-passes-through-non-programs
  "FR-500: lto-apply-to-program only rewrites VM programs; other values pass through."
  (assert-eq :not-a-program (cl-cc/pipeline::lto-apply-to-program :not-a-program)))

(deftest lto-apply-to-program-rewrites-vm-program-and-records-section
  "FR-500: applying LTO to a program returns a new program and records a bitcode section."
  (let* ((program (cl-cc:make-vm-program :instructions (%lto-sample-instructions)))
         (result (cl-cc/pipeline::lto-apply-to-program program :module-name "prog")))
    (assert-true (typep result 'cl-cc:vm-program))
    (assert-true (listp (cl-cc:vm-program-instructions result)))
    (assert-true cl-cc/pipeline::*last-lto-bitcode-section*)))

;;;; ─── FR-640/641 Incremental compilation + hot reload (pipeline-incremental.lisp) ───

(defun %incremental-temp-cache ()
  (ensure-directories-exist
   (merge-pathnames (format nil "clcc-inc-~36R/" (random (expt 36 10)))
                    (uiop:temporary-directory))))

(deftest pipeline-incremental-hash-tracking-lifecycle
  "FR-640: a new source is dirty, clean after commit, and dirty again once edited."
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (let ((cache (%incremental-temp-cache)))
      (unwind-protect
           (progn
             (with-open-file (o source :direction :output :if-exists :supersede)
               (write-string "(defun f (x) x)" o))
             (assert-true (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache))
             (assert-true (cl-cc/pipeline:incremental-state-dirty-p))
             (assert-equal "source hash changed" (cl-cc/pipeline:incremental-state-reason))
             (cl-cc/pipeline:commit-incremental-compilation source :cache-dir cache)
             (assert-false (cl-cc/pipeline:incremental-state-dirty-p))
             (assert-false (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache))
             (assert-false (cl-cc/pipeline:incremental-state-dirty-p))
             (with-open-file (o source :direction :output :if-exists :supersede)
               (write-string "(defun f (x) (+ x 1))" o))
             (assert-true (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache)))
        (ignore-errors (uiop:delete-directory-tree cache :validate (constantly t)))
        (ignore-errors (delete-file source))))))

(deftest pipeline-incremental-missing-source-is-clean
  "FR-640: a source file that does not exist is treated as clean, not dirty."
  (assert-false (cl-cc/pipeline:prepare-incremental-compilation
                 "/nonexistent/clcc-definitely-absent.lisp"))
  (assert-false (cl-cc/pipeline:incremental-state-dirty-p))
  (assert-equal "source file does not exist" (cl-cc/pipeline:incremental-state-reason)))

(deftest pipeline-incremental-dependency-change-dirties-dependent
  "FR-640: a dependent is dirtied when one of its recorded dependencies changes."
  (uiop:with-temporary-file (:pathname dep :type "lisp" :keep t)
    (uiop:with-temporary-file (:pathname main :type "lisp" :keep t)
      (let ((cache (%incremental-temp-cache)))
        (unwind-protect
             (progn
               (with-open-file (o dep :direction :output :if-exists :supersede)
                 (write-string "(defmacro twice (x) `(+ ,x ,x))" o))
               (with-open-file (o main :direction :output :if-exists :supersede)
                 (write-string "(twice 21)" o))
               (cl-cc/pipeline:commit-incremental-compilation dep :cache-dir cache)
               (cl-cc/pipeline:commit-incremental-compilation
                main :dependencies (list (namestring (truename dep))) :cache-dir cache)
               (assert-false (cl-cc/pipeline:prepare-incremental-compilation main :cache-dir cache))
               (with-open-file (o dep :direction :output :if-exists :supersede)
                 (write-string "(defmacro twice (x) `(* 2 ,x))" o))
               (assert-true (cl-cc/pipeline:prepare-incremental-compilation main :cache-dir cache))
               (assert-true (search "dependency" (cl-cc/pipeline:incremental-state-reason))))
          (ignore-errors (uiop:delete-directory-tree cache :validate (constantly t)))
          (ignore-errors (delete-file dep))
          (ignore-errors (delete-file main)))))))

(deftest hot-reload-swap-replaces-implementation-single-threaded
  "FR-641: hot-reload-swap returns the old function and installs the new one when idle."
  (let ((entry (cl-cc/pipeline:make-hot-reload-entry 'sample (lambda () :old))))
    (assert-eq :old (cl-cc/pipeline:hot-reload-call entry))
    (let ((old (cl-cc/pipeline:hot-reload-swap entry (lambda () :new))))
      (assert-eq :old (funcall old))
      (assert-eq :new (cl-cc/pipeline:hot-reload-call entry)))))

;;;; ─── FR-632 Parallel-compilation dependency analysis (pipeline-parallel.lisp) ───

(deftest pipeline-parallel-source-dependency-graph
  "FR-632: build-source-dependency-graph links a file to in-set LOAD targets;
independent-source-files excludes files that depend on another."
  (let* ((tmp (uiop:temporary-directory))
         (a0 (merge-pathnames "clcc-par-a.lisp" tmp))
         (b0 (merge-pathnames "clcc-par-b.lisp" tmp)))
    (unwind-protect
         (progn
           (with-open-file (o a0 :direction :output :if-exists :supersede)
             (write-string "(defun a () 1)" o))
           (with-open-file (o b0 :direction :output :if-exists :supersede)
             (write-string "(load \"clcc-par-a.lisp\")" o))
           ;; Resolve to truenames so the graph's known-set keys (which are
           ;; truenamed) match the merged LOAD-target namestrings.
           (let* ((a (truename a0))
                  (b (truename b0))
                  (graph (cl-cc/pipeline::build-source-dependency-graph (list a b)))
                  (independent (cl-cc/pipeline::independent-source-files (list a b) graph)))
             (assert-= 1 (length independent))
             (assert-equal (namestring (truename a))
                           (namestring (truename (first independent))))))
      (ignore-errors (delete-file a0))
      (ignore-errors (delete-file b0)))))
