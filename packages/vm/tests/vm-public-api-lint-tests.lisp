;;;; vm-public-api-lint-tests.lisp — the ::-into-cl-cc/vm ratchet
;;;;
;;;; §5-2 of docs/notes/repo-split-design.md makes hardening cl-cc/vm's public
;;;; contract the precondition for extracting optimize and native codegen: an
;;;; external repository cannot reach an internal symbol, so every
;;;; `cl-cc/vm::foo` in a consumer is a place the extraction would break.
;;;;
;;;; A lint that demands zero would have to be written last, after the contract
;;;; is settled -- which is exactly when nobody writes it. This one records
;;;; today's counts and refuses to let them grow. Lowering a number is the work;
;;;; the test only stops the number going the other way.
;;;;
;;;; Counting distinct symbols rather than references is deliberate: a pass that
;;;; reads VM-SLOT-READ-DST in six places has one contract gap, not six.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defparameter *vm-internal-reference-budget*
  '(("optimize" . 16)
    ("codegen"  . 34)
    ("compile"  . 6)
    ("pipeline" . 7)
    ("expand"   . 7)
    ;; Already clean. These must stay at zero.
    ("regalloc" . 0)
    ("emit"     . 0)
    ("mir"      . 0)
    ("parse"    . 0))
  "Per-package ceiling on distinct CL-CC/VM internal symbols referenced.

Measured 2026-07-27, after rewriting the 37 references that named an
already-exported symbol with `::` out of habit -- those were never contract gaps,
only noise hiding the ones that are. What remains is mostly instruction types and
their slot accessors, which is the IR contract §5-2 says cl-cc/vm should be
exporting; deciding that contract is the next step, and each symbol it exports
lowers a number here.")

(defun %vm-internal-references (package-name)
  "Return the distinct CL-CC/VM internal symbol names referenced under packages/NAME/src."
  (let* ((dir (asdf:system-relative-pathname
               :cl-cc (format nil "packages/~A/src/" package-name)))
         (names '()))
    (dolist (file (directory (merge-pathnames "**/*.lisp" dir))
                  (sort (remove-duplicates names :test #'string=) #'string<))
      (with-open-file (in file :external-format :utf-8)
        (loop for line = (read-line in nil)
              while line
              do (let ((start 0))
                   (loop for hit = (search "cl-cc/vm::" line :start2 start :test #'char-equal)
                         while hit
                         do (let* ((from (+ hit (length "cl-cc/vm::")))
                                   (to (or (position-if-not
                                            (lambda (c)
                                              (or (alphanumericp c)
                                                  (find c "%*+-/=<>?!_")))
                                            line :start from)
                                           (length line))))
                              (when (> to from)
                                (push (subseq line from to) names))
                              (setf start (max (1+ hit) to))))))))))

(deftest-each vm-internal-reference-count-does-not-grow
  "No package may reference more distinct cl-cc/vm internals than it does today."
  :cases (("optimize" "optimize") ("codegen" "codegen") ("compile" "compile")
          ("pipeline" "pipeline") ("expand" "expand")
          ("regalloc" "regalloc") ("emit" "emit") ("mir" "mir") ("parse" "parse"))
  (package-name)
  (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (assert-true budget)
    (assert-true (<= actual budget))))

(deftest vm-internal-reference-budget-is-not-stale
  "A package that has been cleaned up must have its ceiling lowered to match.

Without this the budget rots upward-only: someone exports the right symbols,
the count drops, and the ceiling keeps claiming the old number is acceptable."
  (let ((slack '()))
    (dolist (entry *vm-internal-reference-budget*)
      (let ((actual (length (%vm-internal-references (car entry)))))
        (when (< actual (cdr entry))
          (push (list (car entry) :budget (cdr entry) :actual actual) slack))))
    (assert-null slack)))
