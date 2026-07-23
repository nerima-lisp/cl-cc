;;;; cli/src/boundaries.lisp — CLI process I/O boundaries (cl-boundary-kit)
;;;;
;;;; The CLI's external effects flow through an explicit cl-boundary-kit
;;;; boundary context instead of calling host primitives ad hoc.  The default
;;;; context uses the native boundaries (real process exit, console, argv); the
;;;; native system boundary terminates via `uiop:quit`, so the existing tests
;;;; that model quit as a non-local exit keep working.  Tests can bind
;;;; *CLI-BOUNDARIES* to a context assembled from cl-boundary-kit's make-test-*
;;;; boundaries to capture exit codes and console output deterministically,
;;;; without the process actually dying.

(in-package :cl-cc/cli)

(defvar *cli-boundaries* nil
  "Active cl-boundary-kit boundary context for CLI process I/O.  NIL selects a
freshly-built native context (real exit / console / argv).")

(defun %make-native-boundaries ()
  "Assemble the native CLI boundary context (system + console + args)."
  (cl-boundary-kit:make-boundary-context
   :system  (cl-boundary-kit:make-system-boundary)
   :console (cl-boundary-kit:make-console)
   :args    (cl-boundary-kit:make-args)))

(defun %cli-context ()
  "Return the active boundary context, defaulting to a fresh native one so it
reflects the current *standard-output* / argv at call time."
  (or *cli-boundaries* (%make-native-boundaries)))

(defun %cli-exit (&optional (code 0))
  "Terminate the CLI through the system boundary.  With the native boundary
this calls `uiop:quit`; with a test system boundary it records the code and
returns, so unit tests can assert on exit codes without exiting the image."
  (cl-boundary-kit:system-exit
   (cl-boundary-kit:boundary-context-get (%cli-context) :system)
   code))
