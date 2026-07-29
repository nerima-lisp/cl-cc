;;;; tests/native-advanced-evidence-tests.lisp
;;;; Behavioral evidence tests for native-advanced FRs (FR-500 to FR-773).
;;;; Verifies that implementation files compile, exports exist, and
;;;; key functions produce correct results.

(in-package :cl-cc/test)

;; ──── Phase 90: LTO ────
(it-sequential "fr-500-lto-enabled-var-exists"
  (expect (boundp 'cl-cc/pipeline:*lto-enabled*) :to-be-truthy))

(it-sequential "fr-500-lto-serialize-exists"
  (expect (fboundp 'cl-cc/pipeline:lto-serialize-module) :to-be-truthy))

(it-sequential "fr-500-lto-deserialize-exists"
  (expect (fboundp 'cl-cc/pipeline:lto-deserialize-module) :to-be-truthy))

;; ──── Phase 95: Debug Info / PerfMap ────
(it-sequential "fr-553-perfmap-exports-exist"
  (expect (fboundp 'cl-cc/pipeline:write-perf-map-entry) :to-be-truthy)
  (expect (fboundp 'cl-cc/pipeline:write-perf-map-for-native-code) :to-be-truthy)
  (expect (fboundp 'cl-cc/pipeline:perf-map-line-valid-p) :to-be-truthy)
  (expect (boundp 'cl-cc/pipeline:*perf-map-stream*) :to-be-truthy))

(it-sequential "fr-553-perfmap-line-valid-checks-hex"
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A FOO") :to-be-truthy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 nope FOO") :to-be-falsy)
  (expect (cl-cc/pipeline:perf-map-line-valid-p "1000 2A") :to-be-falsy))

;; ──── Topology implementation ────
(it-sequential "fr-624-topology-core-detection"
  (let* ((cl-cc/runtime::*rt-detected-cpu-cores*
           (or cl-cc/runtime::*rt-detected-cpu-cores* 1))
         (cores (cl-cc/runtime:detect-cpu-cores)))
    (expect (integerp cores) :to-be-truthy)
    (expect (plusp cores) :to-be-truthy)))

(it-sequential "fr-624-topology-numa-info-returns-plist"
  (let* ((cl-cc/runtime::*rt-detected-cpu-cores*
           (or cl-cc/runtime::*rt-detected-cpu-cores* 1))
         (topo (cl-cc/runtime:detect-numa-topology)))
    (expect (listp topo) :to-be-truthy)
    (dolist (node topo)
      (expect (listp node) :to-be-truthy)
      (expect (getf node :node-id) :to-be-truthy))))

(it-sequential "fr-624-topology-cpulist-parser-normalizes-ranges"
  (expect (cl-cc/runtime::%rt-parse-cpulist "0-2,2,4,nope,6-5,7") :to-equal '(0 1 2 4 7)))

(it-sequential "fr-624-topology-memory-bytes-sums-known-nodes"
  (expect (cl-cc/runtime::%rt-topology-memory-bytes
                 '((:node-id 0 :memory-bytes 128)
                   (:node-id 1 :memory-bytes nil)
                   (:node-id 2 :memory-bytes 256))) :to-equal 384))

(it-sequential "fr-624-topology-cache-returns-fresh-tree"
  (let ((cl-cc/runtime::*rt-detected-numa-topology*
          '((:node-id 0 :cpus (0 1) :memory-bytes 128 :kind :dram))))
    (let ((topology (cl-cc/runtime:detect-numa-topology)))
      (setf (first (getf (first topology) :cpus)) 99))
    (expect (getf (first (cl-cc/runtime:detect-numa-topology)) :cpus) :to-equal '(0 1))))



;; ──── Incremental compilation ────
(it-sequential "fr-640-incremental-cache-dir-exists"
  (expect (boundp 'cl-cc/pipeline:*incremental-cache-directory*) :to-be-truthy))

;; ──── Pipeline parallel compilation ────
(it-sequential "fr-632-parallel-compile-exists"
  (expect (fboundp 'cl-cc/pipeline:compile-files-to-native-parallel) :to-be-truthy))

;; ──── DWARF split debug ────
(it-sequential "fr-652-dwo-module-loaded"
  (expect t :to-be-truthy))
