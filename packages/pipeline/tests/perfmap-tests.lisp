(in-package :cl-cc/test)


(it-sequential "pipeline-perf-map-line-validates-format"
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO") :to-be-truthy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO") :to-be-falsy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A") :to-be-falsy))

(it-sequential "pipeline-write-perf-map-entry-emits-hex-fields"
  (let ((stream (make-string-output-stream)))
    (cl-cc/pipeline:write-perf-map-entry stream #x1000 #x2a 'sample-function)
    (let ((line (string-trim '(#\Newline #\Return)
                             (get-output-stream-string stream))))
      (expect line :to-equal "1000 2A SAMPLE-FUNCTION")
      (expect (cl-cc/pipeline:perf-map-line-valid-p line) :to-be-truthy))))

(it-sequential "pipeline-write-perf-map-for-native-code-appends-map-line"
  (uiop:with-temporary-file (:pathname path :type "map" :keep t)
    (let ((program (cl-cc:make-vm-program
                    :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                        (cl-cc:make-vm-halt :reg :r0))))
          (bytes #(1 2 3 4)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (let ((cl-cc/pipeline:*perf-map-stream* out))
          (cl-cc/pipeline:write-perf-map-for-native-code program bytes :output-file "unit-main")))
      (let ((line (with-open-file (in path :direction :input) (read-line in nil nil))))
        (expect (cl-cc/pipeline:perf-map-line-valid-p line) :to-be-truthy)
        (expect (search "unit-main" line :test #'char-equal) :to-be-truthy)))
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

(it-sequential "lto-serialize-instructions-round-trips"
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 42)
                      (cl-cc:make-vm-ret :reg :r0)))
         (payload (cl-cc/pipeline::lto-serialize-instructions insts))
         (restored (cl-cc/pipeline::lto-deserialize-instructions payload)))
    (expect (stringp payload) :to-be-truthy)
    (expect (= 2 (length restored)) :to-be-truthy)
    (expect (typep (first restored) 'cl-cc:vm-const) :to-be-truthy)
    (expect (= 42 (cl-cc:vm-value (first restored))) :to-be-truthy)))

(it-sequential "lto-serialize-module-round-trips"
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-ret :reg :r0)))
         (payload (cl-cc/pipeline:lto-serialize-module "mod" insts))
         (module (cl-cc/pipeline:lto-deserialize-module payload)))
    (expect (cl-cc/pipeline::lto-module-name module) :to-equal "mod")
    (expect (= 2 (length (cl-cc/pipeline::lto-module-instructions module))) :to-be-truthy)
    (expect (typep (first (cl-cc/pipeline::lto-module-instructions module))
                        'cl-cc:vm-const) :to-be-truthy)))

(it-sequential "lto-deserialize-module-rejects-invalid-payload"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/pipeline:lto-deserialize-module "(:not-a-module 1 2)")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "lto-make-bitcode-section-from-module-and-payload"
  (let* ((module (cl-cc/pipeline:make-lto-module
                  :name "b"
                  :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1)
                                      (cl-cc:make-vm-ret :reg :r0))))
         (section (cl-cc/pipeline::lto-make-bitcode-section module)))
    (expect (getf section :name) :to-equal "__bitcode")
    (expect (getf section :type) :to-be :progbits)
    (expect (stringp (getf section :payload)) :to-be-truthy))
  (let ((section (cl-cc/pipeline::lto-make-bitcode-section "rawpayload")))
    (expect (getf section :payload) :to-equal "rawpayload")))

(it-sequential "lto-merge-modules-concatenates-instruction-streams"
  (let* ((m1 (cl-cc/pipeline:make-lto-module
              :name "a" :instructions (list (cl-cc:make-vm-const :dst :r0 :value 1))))
         (m2 (cl-cc/pipeline:make-lto-module
              :name "b" :instructions (list (cl-cc:make-vm-const :dst :r1 :value 2)
                                            (cl-cc:make-vm-ret :reg :r1))))
         (merged (cl-cc/pipeline:lto-merge-modules (list m1 m2))))
    (expect (= 3 (length merged)) :to-be-truthy)))

(it-sequential "lto-eliminates-unreachable-functions"
  (let* ((instructions (%lto-sample-instructions))
         (graph (cl-cc/pipeline::lto-build-cross-module-call-graph (list instructions)))
         (pruned (cl-cc/pipeline::lto-eliminate-dead-functions instructions graph))
         (labels (%lto-label-names pruned)))
    (expect (member "live" labels :test #'string=) :to-be-truthy)
    (expect (member "dead" labels :test #'string=) :to-be-falsy)))

(it-sequential "lto-optimize-modules-gates-dce-with-run-dce"
  (let ((module (cl-cc/pipeline:make-lto-module
                 :name "m" :instructions (%lto-sample-instructions))))
    (multiple-value-bind (pruned graph)
        (cl-cc/pipeline:lto-optimize-modules (list module) :run-ipcp nil :run-dce t)
      (expect (cl-cc/pipeline::lto-call-graph-roots graph) :to-be-truthy)
      (expect (member "dead" (%lto-label-names pruned) :test #'string=) :to-be-falsy))
    (let ((kept (cl-cc/pipeline:lto-optimize-modules
                 (list module) :run-ipcp nil :run-dce nil)))
      (expect (member "dead" (%lto-label-names kept) :test #'string=) :to-be-truthy))))

(it-sequential "lto-apply-to-program-passes-through-non-programs"
  (expect (cl-cc/pipeline::lto-apply-to-program :not-a-program) :to-be :not-a-program))

(it-sequential "lto-apply-to-program-rewrites-vm-program-and-records-section"
  (let* ((program (cl-cc:make-vm-program :instructions (%lto-sample-instructions)))
         (result (cl-cc/pipeline::lto-apply-to-program program :module-name "prog")))
    (expect (typep result 'cl-cc:vm-program) :to-be-truthy)
    (expect (listp (cl-cc:vm-program-instructions result)) :to-be-truthy)
    (expect cl-cc/pipeline::*last-lto-bitcode-section* :to-be-truthy)))

;;;; ─── FR-640/641 Incremental compilation + hot reload (pipeline-incremental.lisp) ───

(defun %incremental-temp-cache ()
  (ensure-directories-exist
   (merge-pathnames (format nil "clcc-inc-~36R/" (random (expt 36 10)))
                    (uiop:temporary-directory))))

(it-sequential "pipeline-incremental-hash-tracking-lifecycle"
  (uiop:with-temporary-file (:pathname source :type "lisp" :keep t)
    (let ((cache (%incremental-temp-cache)))
      (unwind-protect
           (progn
             (with-open-file (o source :direction :output :if-exists :supersede)
               (write-string "(defun f (x) x)" o))
             (expect (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache) :to-be-truthy)
             (expect (cl-cc/pipeline:incremental-state-dirty-p) :to-be-truthy)
             (expect (cl-cc/pipeline:incremental-state-reason) :to-equal "source hash changed")
             (cl-cc/pipeline:commit-incremental-compilation source :cache-dir cache)
             (expect (cl-cc/pipeline:incremental-state-dirty-p) :to-be-falsy)
             (expect (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache) :to-be-falsy)
             (expect (cl-cc/pipeline:incremental-state-dirty-p) :to-be-falsy)
             (with-open-file (o source :direction :output :if-exists :supersede)
               (write-string "(defun f (x) (+ x 1))" o))
             (expect (cl-cc/pipeline:prepare-incremental-compilation source :cache-dir cache) :to-be-truthy))
        (ignore-errors (uiop:delete-directory-tree cache :validate (constantly t)))
        (ignore-errors (delete-file source))))))

(it-sequential "pipeline-incremental-missing-source-is-clean"
  (expect (cl-cc/pipeline:prepare-incremental-compilation
                 "/nonexistent/clcc-definitely-absent.lisp") :to-be-falsy)
  (expect (cl-cc/pipeline:incremental-state-dirty-p) :to-be-falsy)
  (expect (cl-cc/pipeline:incremental-state-reason) :to-equal "source file does not exist"))

(it-sequential "pipeline-incremental-dependency-change-dirties-dependent"
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
               (expect (cl-cc/pipeline:prepare-incremental-compilation main :cache-dir cache) :to-be-falsy)
               (with-open-file (o dep :direction :output :if-exists :supersede)
                 (write-string "(defmacro twice (x) `(* 2 ,x))" o))
               (expect (cl-cc/pipeline:prepare-incremental-compilation main :cache-dir cache) :to-be-truthy)
               (expect (search "dependency" (cl-cc/pipeline:incremental-state-reason)) :to-be-truthy))
          (ignore-errors (uiop:delete-directory-tree cache :validate (constantly t)))
          (ignore-errors (delete-file dep))
          (ignore-errors (delete-file main)))))))

(it-sequential "hot-reload-swap-replaces-implementation-single-threaded"
  (let ((entry (cl-cc/pipeline:make-hot-reload-entry 'sample (lambda () :old))))
    (expect (cl-cc/pipeline:hot-reload-call entry) :to-be :old)
    (let ((old (cl-cc/pipeline:hot-reload-swap entry (lambda () :new))))
      (expect (funcall old) :to-be :old)
      (expect (cl-cc/pipeline:hot-reload-call entry) :to-be :new))))

;;;; ─── FR-632 Parallel-compilation dependency analysis (pipeline-parallel.lisp) ───

(it-sequential "pipeline-parallel-source-dependency-graph"
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
             (expect (= 1 (length independent)) :to-be-truthy)
             (expect (namestring (truename (first independent))) :to-equal (namestring (truename a)))))
      (ignore-errors (delete-file a0))
      (ignore-errors (delete-file b0)))))
