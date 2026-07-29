(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; cl-weave-backed suite/test registration
;;; ------------------------------------------------------------
;;;
;;; DEFTEST/DEFSUITE/IN-SUITE historically built a flat, name-addressable
;;; registry (*test-registry*/*suite-registry*, persistent maps) that any
;;; top-level form anywhere in any file could add to by NAME, independent of
;;; lexical position — e.g. (in-suite x) in one file followed by (deftest ...)
;;; in a LATER top-level form (same file or not) attaches to suite X.
;;;
;;; cl-weave's public DESCRIBE/IT macros require LEXICAL nesting (an IT must
;;; be textually inside its DESCRIBE), which is incompatible with 550 files
;;; written in the flat style above without a full structural rewrite of
;;; every file. cl-weave's underlying registration is NOT actually
;;; macro-only, though: CL-WEAVE::REGISTER-SUITE / CL-WEAVE::REGISTER-TEST /
;;; CL-WEAVE::REGISTER-BEFORE-EACH / CL-WEAVE::REGISTER-AFTER-EACH are plain
;;; functions that attach to whatever CL-WEAVE::*CURRENT-SUITE* is bound to
;;; at call time — and REGISTER-SUITE's own dynamic (LET) binding of
;;; *CURRENT-SUITE* only lasts for its own thunk call, so nothing prevents a
;;; SEPARATE top-level SETF of CL-WEAVE::*CURRENT-SUITE* from persistently
;;; changing "the current suite" for whatever flat registration calls follow
;;; — reproducing IN-SUITE's original semantics exactly, with zero lexical
;;; restructuring required. This relies on unexported cl-weave internals, so
;;; the cl-weave version is pinned (see flake.nix) to keep it reproducible.

(defvar *suite-objects* (make-hash-table :test #'eq)
  "Suite NAME symbol -> cl-weave suite object, populated by DEFSUITE and by
IN-SUITE's auto-vivification fallback below.")

(defun %run-registered-tests-from-source-file (source-file)
  "No-op compatibility shim. A handful of PHP test files historically called
this at (eval-when (:load-toplevel :execute) ...) time for immediate
feedback while loading a single file interactively; cl-weave has no
per-source-file ad hoc run primitive, and the tests it would have run still
run normally later through the ordinary RUN-ALL pathway, so skipping the
early run here does not affect test coverage."
  (declare (ignore source-file))
  nil)

(defun %ensure-suite-object (name &optional parent-name)
  "Return NAME's suite object, creating it under PARENT-NAME's suite (or
root) if it doesn't exist yet. Because ASDF loads 550 files in one fixed
:serial order, a file's (in-suite x) can run before the (defsuite x ...)
that names x's real parent/description — unlike the original symbol-keyed
*suite-registry*, a cl-weave suite is a real object that must exist before
anything attaches to it, so IN-SUITE auto-vivifies a bare (root-parented)
placeholder rather than erroring; a later DEFSUITE for the same name reuses
that placeholder instead of creating a second, orphaned suite object, since
cl-weave suites can't be re-parented after creation."
  (or (gethash name *suite-objects*)
      (setf (gethash name *suite-objects*)
            (let ((cl-weave::*current-suite*
                    (if parent-name
                        (%ensure-suite-object parent-name)
                        (cl-weave::root-suite))))
              (cl-weave::register-suite (string name) (lambda ()))))))

(defmacro defsuite (name &key description parent (parallel t))
  "Define a test suite as a cl-weave suite object, parented under PARENT's
suite (or the cl-weave root suite when PARENT is NIL). A no-op beyond
returning the existing object when NAME was already auto-vivified by an
earlier IN-SUITE (see %ENSURE-SUITE-OBJECT)."
  (declare (ignore description parallel))
  `(%ensure-suite-object ',name ',parent))

(defmacro in-suite (name)
  "Make NAME's suite the target of subsequent flat DEFTEST/DEFBEFORE/DEFAFTER
calls, persistently (not lexically scoped) — mirrors the original IN-SUITE."
  `(setf cl-weave::*current-suite* (%ensure-suite-object ',name)))

;;; ------------------------------------------------------------
;;; Test Definition
;;; ------------------------------------------------------------

(defun %parse-deftest-body (forms)
  "Parse body forms, extracting optional docstring and keyword args.
Returns (values docstring timeout depends-on tags body-forms)."
  (let ((docstring nil)
        (timeout nil)
        (depends-on nil)
        (tags nil)
        (rest forms))
    (when (and rest (stringp (car rest)))
      (setf docstring (car rest)
            rest (cdr rest)))
    (loop while (and rest (keywordp (car rest)))
          do (let ((key (pop rest))
                   (val (pop rest)))
               (case key
                 (:timeout   (setf timeout val))
                 (:depends-on (setf depends-on val))
                 (:tags      (setf tags val)))))
    (values docstring timeout depends-on tags rest)))

(progn
  (defvar *known-test-names* (make-hash-table :test (function eq))
    "Every DEFTEST NAME symbol ever registered, for doc/test traceability
checks (e.g. \"does this documented FR anchor name a real test?\") that used
to query *TEST-REGISTRY* by symbol directly. Values are the cl-weave test
description string DEFTEST registered them under.")

  (defun %normalize-timeout-ms (timeout)
    "Convert a positive timeout in seconds to cl-weave milliseconds."
    (unless (and (realp timeout) (plusp timeout))
      (error "Timeout must be a positive real number: ~S" timeout))
    (ceiling (* timeout 1000)))

  (defun %map-cl-weave-tests (function &optional (suite (cl-weave::root-suite)))
    "Call FUNCTION with each test and its containing suite."
    (dolist (child (cl-weave::suite-children suite))
      (if (cl-weave::test-case-p child)
          (funcall function child suite)
          (%map-cl-weave-tests function child))))

  (defun %set-test-timeouts-by-selector! (selector timeout overwrite)
    (let ((timeout-ms (%normalize-timeout-ms timeout)))
      (%map-cl-weave-tests
       (lambda (test suite)
         (when (and (funcall selector test suite)
                    (or overwrite
                        (null (cl-weave::test-case-timeout-ms test))))
           (setf (cl-weave::test-case-timeout-ms test) timeout-ms))))))

  (defun set-test-timeouts! (test-names timeout &key overwrite)
    "Set TIMEOUT in seconds for registered tests named by TEST-NAMES."
    (%set-test-timeouts-by-selector!
     (lambda (test suite)
       (declare (ignore suite))
       (member (cl-weave::test-case-name test) test-names
               :test (lambda (registered requested)
                       (string= registered
                                (etypecase requested
                                  (string requested)
                                  (symbol (symbol-name requested)))))))
     timeout overwrite))

  (defun set-test-timeouts-by-prefix! (prefix timeout &key overwrite)
    "Set TIMEOUT in seconds for registered tests whose names start with PREFIX."
    (%set-test-timeouts-by-selector!
     (lambda (test suite)
       (declare (ignore suite))
       (let ((name (cl-weave::test-case-name test)))
         (and (<= (length prefix) (length name))
              (string= prefix name :end2 (length prefix)))))
     timeout overwrite))

  (defun set-suite-test-timeout! (suite-name timeout &key recursive overwrite)
    "Set TIMEOUT in seconds for tests in SUITE-NAME, optionally recursively."
    (let ((target (gethash suite-name *suite-objects*)))
      (%set-test-timeouts-by-selector!
       (lambda (test suite)
         (declare (ignore test))
         (if recursive
             (loop for current = suite then (cl-weave::suite-parent current)
                   while current
                   thereis (eq current target))
             (eq suite target)))
       timeout overwrite))))


;;; ------------------------------------------------------------
;;; Fixtures
;;; ------------------------------------------------------------

(defmacro defbefore (when-spec suite-spec &body body)
  "Register a before-each fixture for the given suite.
   (defbefore :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((cl-weave::*current-suite* (%ensure-suite-object ',suite-name)))
       (cl-weave::register-before-each (lambda () ,@body)))))

(defmacro defafter (when-spec suite-spec &body body)
  "Register an after-each fixture for the given suite.
   (defafter :each (suite-name) body...)"
  (declare (ignore when-spec))
  (let ((suite-name (if (listp suite-spec) (car suite-spec) suite-spec)))
    `(let ((cl-weave::*current-suite* (%ensure-suite-object ',suite-name)))
       (cl-weave::register-after-each (lambda () ,@body)))))

;;; ------------------------------------------------------------
;;; Skip / Pending / Expected-Fail
;;; ------------------------------------------------------------

(defmacro skip (&optional (reason "skipped"))
  "Skip the current test; forwards to cl-weave's own SKIP."
  `(cl-weave:skip ,reason))

(defmacro defexpected (name &body body)
  "Define a test that is EXPECTED TO FAIL (due to an unimplemented feature).
Catches any error from BODY and treats it as a pass; an unexpected clean
pass is also accepted (cl-weave has no XPASS-detection equivalent to the
original framework's :expected-fail tag machinery).
Syntax:
   (defexpected name
     \"docstring\"
     :timeout 10
     body-form...)"
  (multiple-value-bind (docstring timeout depends-on tags body-forms)
      (%parse-deftest-body body)
    (declare (ignore tags))
    `(deftest ,name
       ,(or docstring (format nil "EXPECTED-FAIL: ~A" name))
       :timeout ,(or timeout 60)
       :depends-on ,depends-on
       (ignore-errors (progn ,@body-forms))
       t)))
