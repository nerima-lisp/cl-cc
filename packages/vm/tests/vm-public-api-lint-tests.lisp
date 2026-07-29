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
(defparameter *vm-internal-reference-budget*
  '(("optimize" . 0)
    ("codegen"  . 0)
    ("compile"  . 3)
    ("pipeline" . 5)
    ("expand"   . 7)
    ("regalloc" . 0)
    ("emit"     . 0)
    ("mir"      . 0)
    ("parse"    . 0))
  "Per-package ceiling on distinct CL-CC/VM internal symbols referenced.

§5-2 is satisfied for the two Phase C extraction candidates: optimize and
codegen are both at zero, down from 16 and 34. Neither names a cl-cc/vm internal
any more, which is the property an out-of-tree pass needs -- a separate
repository cannot reach one.

Getting there took three passes. 37 of the original 106 already named an
exported symbol and were written with `::` out of habit. cl-cc/vm then exported
the IR contract §5-2 called for: instruction types, slot accessors, the
exception-table record, atomics and fences. What was left in optimize was not
IR at all but two mistakes -- a pass reading VM-BINOP slots that do not exist,
and a slot compared as a package-qualified literal where its name would do.

The rest is deliberately not IR, and each package's residue is a different thing:

  compile    %REGISTER-DOCUMENTATION and the CLOS emulation internals
             (CLASS-DIRECT-SLOTS, METHOD-SPECIALIZERS).
  pipeline   %VM-CALL-CLOSURE-SYNC / %VM-CLOSURE-OBJECT-P and friends, which the
             backend-protocol VM integration is built from, plus one tuning
             constant. Core reaching into core.
  expand     The package-lock and sequence-protocol emulation.

None of the three is an extraction candidate, so none blocks anything. They stay
non-zero on purpose: a ceiling of 0 everywhere would be a claim this file cannot
back.")

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

(it-sequential "vm-internal-reference-count-does-not-grow optimize" (destructuring-bind (package-name) (list "optimize")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow codegen"
  (destructuring-bind (package-name) (list "codegen")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow compile"
  (destructuring-bind (package-name) (list "compile")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow pipeline"
  (destructuring-bind (package-name) (list "pipeline")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow expand"
  (destructuring-bind (package-name) (list "expand")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow regalloc"
  (destructuring-bind (package-name) (list "regalloc")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow emit"
  (destructuring-bind (package-name) (list "emit")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow mir"
  (destructuring-bind (package-name) (list "mir")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-count-does-not-grow parse"
  (destructuring-bind (package-name) (list "parse")
    (let ((budget (cdr (assoc package-name *vm-internal-reference-budget* :test #'string=)))
        (actual (length (%vm-internal-references package-name))))
    (expect budget :to-be-truthy)
    (expect (<= actual budget) :to-be-truthy))))

(it-sequential "vm-internal-reference-budget-is-not-stale"
  (let ((slack '()))
    (dolist (entry *vm-internal-reference-budget*)
      (let ((actual (length (%vm-internal-references (car entry)))))
        (when (< actual (cdr entry))
          (push (list (car entry) :budget (cdr entry) :actual actual) slack))))
    (expect slack :to-be-null)))
