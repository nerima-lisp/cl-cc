(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Internal state still used by the retained compatibility macros
;;; (definvariant/defmetamorphic/defbenchmark/assert-snapshot). Everything
;;; else that used to live here (test-failure/skip-condition/pending-
;;; condition/expected-fail-condition, *suite-registry*/*test-registry*/
;;; *current-suite*, *tap-mutex*, *coverage-reload-in-progress*) belonged to
;;; the removed custom registry/TAP/coverage runner and has no cl-weave
;;; equivalent to preserve.
;;; ------------------------------------------------------------

(defvar *invariant-registry* '()
  "List of invariant functions called after every test.")

(defvar *snapshot-dir* "tests/snapshots/"
  "Directory for snapshot files.")

(defvar *metamorphic-relations* '()
  "List of metamorphic relation plists.")

(defvar *benchmark-registry* '()
  "List of benchmark plists (:NAME :FN :WARMUP :ITERATIONS :DOCSTRING).
Registered automatically by DEFBENCHMARK.")
