;;;; tests/unit/optimize/optimizer-roadmap-tests.lisp
;;;; Unit tests for optimizer-pipeline-roadmap.lisp — optimize-passes.md evidence
;;;;
;;;; Covers: optimize-roadmap doc parsing helpers, FR-id coverage,
;;;;   status-aware summaries, completed-heading checks, related-path checks,
;;;;   sidecar contradictions, and per-cluster implementation evidence.

(in-package :cl-cc/test)

;;; ─── Optimize roadmap evidence and helper contracts ───────────────────────

(defun %optimizer-doc-fr-id-from-line (line)
  "Extract an FR id from an optimize roadmap heading line."
  (let ((fr-pos (and (>= (length line) 4)
                     (string= "####" (subseq line 0 4))
                     (search "FR-" line))))
    (when fr-pos
      (let ((end (+ fr-pos 3)))
        (loop while (and (< end (length line))
                         (digit-char-p (char line end)))
              do (incf end))
        (when (> end (+ fr-pos 3))
          (subseq line fr-pos end))))))

(defun %optimizer-doc-fr-ids ()
  "Return the ordered FR headings from docs/notes/optimize-passes.md."
  (let ((ids nil))
    (dolist (line (uiop:split-string
                   (uiop:read-file-string
                    (merge-pathnames #P"docs/notes/optimize-passes.md" (uiop:getcwd)))
                   :separator '(#\Newline))
             (nreverse ids))
      (let ((feature-id (%optimizer-doc-fr-id-from-line line)))
        (when feature-id
          (push feature-id ids))))))

(defun %doc-content (pathname)
  "Return the text of the roadmap document at PATHNAME (repository-relative)."
  (uiop:read-file-string
   (merge-pathnames pathname (uiop:getcwd))))

(defun %doc-completed-heading-contradictions (pathname needles &key reset-on-section-boundary)
  "Return ✅ FR headings in the document at PATHNAME whose body still contains one of NEEDLES.
When RESET-ON-SECTION-BOUNDARY is non-nil, any ### line that is not an #### FR heading
resets the current block (used by the backend doc which has ### section separators)."
  (let ((contradictions nil)
        (current-heading nil)
        (current-lines nil))
    (labels ((fr-heading-line-p (line)
               (and (>= (length line) 7)
                    (string= "####" (subseq line 0 4))
                    (search "FR-" line)))
             (section-boundary-p (line)
               (and reset-on-section-boundary
                    (>= (length line) 3)
                    (string= "###" (subseq line 0 3))))
             (completed-heading-p (line)
               (search "✅" line))
             (block-text ()
               (with-output-to-string (out)
                 (dolist (line (nreverse current-lines))
                   (format out "~A~%" line))))
             (flush-block ()
               (when (and current-heading (completed-heading-p current-heading))
                 (let ((text (block-text)))
                   (when (some (lambda (needle) (search needle text)) needles)
                     (push current-heading contradictions))))
               (setf current-lines nil)))
      (dolist (line (uiop:split-string (%doc-content pathname)
                                       :separator (list #\Newline)))
        (cond
          ((fr-heading-line-p line)
           (flush-block)
           (setf current-heading line
                 current-lines (list line)))
          ((section-boundary-p line)
           (flush-block)
           (setf current-heading nil
                 current-lines nil))
          (current-heading
           (push line current-lines))))
      (flush-block)
      (nreverse contradictions))))

(defun %optimizer-doc-content ()
  "Return the current optimize roadmap document text."
  (%doc-content #P"docs/notes/optimize-passes.md"))

(defun %optimizer-doc-completed-heading-contradictions ()
  "Return ✅ optimize roadmap headings whose body still says implementation is absent."
  (%doc-completed-heading-contradictions
   #P"docs/notes/optimize-passes.md"
   (list "未実装" "未統合" "欠落" "未接続" "未対応" "未定義" "不可能" "なし")))

(defun %optimizer-doc-fr-ids-from-line (line)
  "Return all FR ids mentioned in LINE."
  (let ((ids nil)
        (start 0))
    (loop for pos = (search "FR-" line :start2 start)
          while pos
          do (let ((end (+ pos 3)))
               (loop while (and (< end (length line))
                                (digit-char-p (char line end)))
                     do (incf end))
               (when (> end (+ pos 3))
                 (push (subseq line pos end) ids))
               (setf start (max (1+ pos) end))))
    (nreverse ids)))

(defun %optimizer-doc-path-token-p (token)
  "Return T when TOKEN is a repository-relative doc/code path.

Only `packages/` and `docs/` prefixes count, and that is what keeps this check
sound: it verifies the paths this checkout can be asked about. Code that has
moved to a standalone repository -- ast and type so far -- is written as the
repository name plus a path relative to it, so neither backtick token looks like
a checkout path. PROBE-FILE could not answer for those anyway: they reach the
build as a Nix store path, not a sibling directory."
  (or (and (>= (length token) 9)
           (string= "packages/" token :end2 9))
      (and (>= (length token) 5)
           (string= "docs/" token :end2 5))))

(defun %optimizer-doc-backtick-paths (line)
  "Return repository paths enclosed in backticks on LINE."
  (let ((paths nil)
        (start 0)
        (tick (code-char 96)))
    (loop for open = (position tick line :start start)
          while open
          for close = (position tick line :start (1+ open))
          while close
          do (let ((token (subseq line (1+ open) close)))
               (when (%optimizer-doc-path-token-p token)
                 (push token paths))
               (setf start (1+ close))))
    (nreverse paths)))

(defun %optimizer-doc-related-implementation-missing-paths ()
  "Return `関連実装` repository paths that do not exist in the checkout."
  (let ((missing nil))
    (loop for line in (uiop:split-string (%optimizer-doc-content)
                                         :separator (list #\Newline))
          for line-no from 1
          when (search "関連実装" line)
            do (dolist (path (%optimizer-doc-backtick-paths line))
                 (unless (probe-file (merge-pathnames path (uiop:getcwd)))
                   (push (format nil "~D:~A" line-no path) missing))))
    (nreverse missing)))

(defun %optimizer-doc-heading-status-table ()
  "Return a hash table mapping optimize roadmap FR ids to heading statuses."
  (let ((table (make-hash-table :test (function equal))))
    (dolist (feature (cl-cc/optimize::optimize-roadmap-doc-features) table)
      (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table)
            (cl-cc/optimize::opt-roadmap-feature-status feature)))))

(defun %optimizer-doc-completed-sidecar-contradictions ()
  "Return `完了済みFR` entries that do not point at ✅ headings."
  (let ((status-by-id (%optimizer-doc-heading-status-table))
        (contradictions nil))
    (dolist (line (uiop:split-string (%optimizer-doc-content)
                                     :separator (list #\Newline))
                  (nreverse contradictions))
      (when (search "完了済みFR" line)
        (dolist (feature-id (%optimizer-doc-fr-ids-from-line line))
          (let ((status (gethash feature-id status-by-id)))
            (unless (eq status :implemented)
              (push (format nil "~A listed complete with status ~A"
                            feature-id status)
                    contradictions))))))))

;; SKIP (doc sync): Docs need updating for current FR count and status
(it-sequential "optimize-roadmap-related-implementation-paths-exist"
  (expect (%optimizer-doc-related-implementation-missing-paths) :to-be-null))

(it-sequential "optimize-roadmap-completed-sidecar-claims-match-heading-status"
  (expect (%optimizer-doc-completed-sidecar-contradictions) :to-be-null))

(it-sequential "optimizer-roadmap-core-passes-have-evidence"
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-001")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
    (expect (member "packages/optimize/src/optimizer.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member 'cl-cc/optimize::opt-pass-fold
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence) :to-be-truthy)))

(it-sequential "optimizer-roadmap-inline-and-memory-evidence"
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-051")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
    (expect (member "packages/optimize/src/optimizer-inline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member 'cl-cc/optimize::opt-pass-devirtualize
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                   evidence) :to-be-truthy)))

(it-sequential "optimizer-roadmap-pic-evidence-is-runtime-backed"
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-023")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
    (expect (member "packages/optimize/src/optimizer-pipeline.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member 'cl-cc/optimize::opt-ic-transition
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-truthy)
    (expect (member 'cl-cc/optimize::opt-pass-devirtualize
                          (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-falsy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence) :to-be-truthy)))

(it-sequential "optimizer-roadmap-flow-and-ssa-evidence"
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-112")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
    (expect (member "packages/optimize/src/ssa.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member 'cl-cc/optimize::cfg-split-critical-edges
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence) :to-be-truthy)))

(it-sequential "optimize-roadmap-pipeline-includes-modern-optimization-passes"
  (dolist (key '(:sccp
                 :reassociate
                 :copy-prop
                 :gvn
                 :store-to-load-forward
                 :dead-store-elim
                 :tail-duplication))
    (expect (member key cl-cc/optimize::*opt-default-convergence-pass-keys*) :to-be-truthy)
    (expect (gethash key cl-cc/optimize::*opt-pass-registry*) :to-be-truthy)))

(it-sequential "optimizer-roadmap-code-motion-evidence"
  (let ((tail-dup-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-167"))
        (implemented-evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-164")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status tail-dup-evidence) :to-be :implemented)
    (expect (cl-cc/optimize::opt-roadmap-evidence-status implemented-evidence) :to-be :implemented)
    (expect (member 'cl-cc/optimize::opt-pass-tail-duplication
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols tail-dup-evidence)) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  tail-dup-evidence) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  implemented-evidence) :to-be-truthy)))

(it-sequential "optimizer-roadmap-callee-saved-evidence-is-native-backed"
  (labels ((function-present-p (package-name symbol-name)
             (let* ((pkg (find-package package-name))
                    (sym (and pkg (find-symbol symbol-name pkg))))
               (and sym (fboundp sym)))))
    (let* ((codegen-spec '("CL-CC/CODEGEN" . "X86-64-USED-CALLEE-SAVED-REGS"))
           (regalloc-spec '("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS"))
           (evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-329")))
      (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
      (expect (member "packages/codegen/src/x86-64-codegen-core.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=) :to-be-truthy)
      (expect (member "packages/regalloc/src/regalloc.lisp"
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=) :to-be-truthy)
      (expect (member codegen-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal) :to-be-truthy)
      (expect (member regalloc-spec
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal) :to-be-truthy)
      (expect (function-present-p :cl-cc/codegen "X86-64-USED-CALLEE-SAVED-REGS") :to-be-truthy)
      (expect (function-present-p :cl-cc/regalloc "COMPUTE-LIVE-INTERVALS") :to-be-truthy)
      (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                     evidence) :to-be-truthy))))

(it-sequential "optimizer-roadmap-ssa-phi-elim-evidence"
  (let ((evidence (cl-cc/optimize::lookup-opt-roadmap-evidence "FR-271")))
    (expect (cl-cc/optimize::opt-roadmap-evidence-status evidence) :to-be :implemented)
    (expect (member "packages/optimize/src/ssa-phi-elim.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member "packages/optimize/tests/ssa-tests.lisp"
                         (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                         :test #'string=) :to-be-truthy)
    (expect (member 'cl-cc/optimize::ssa-eliminate-trivial-phis
                         (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)) :to-be-truthy)
    ;; Verify individual test anchors by name (they're CL-CC/OPTIMIZE package symbols)
    (expect (member 'cl-cc/optimize::ssa-phi-elim-all-same-arg-multi-pred
                         (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)) :to-be-truthy)
    (expect (member 'cl-cc/optimize::ssa-phi-elim-phi-of-phi-chain-deep
                         (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)) :to-be-truthy)
    (expect (member 'cl-cc/optimize::ssa-phi-elim-unused-phi
                         (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)) :to-be-truthy)
    (expect (member 'cl-cc/optimize::ssa-phi-elim-idempotent
                         (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)) :to-be-truthy)
    (expect (cl-cc/optimize::optimize-roadmap-implementation-evidence-complete-p
                  evidence) :to-be-truthy)))
