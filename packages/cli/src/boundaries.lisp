;;;; cli/src/boundaries.lisp — CLI process I/O boundaries (cl-boundary-kit)
;;;;
;;;; The CLI's external effects flow through an explicit cl-boundary-kit
;;;; boundary context instead of calling host primitives ad hoc. The default
;;;; context uses native boundaries, whose system boundary terminates the
;;;; process. Tests that exercise exit paths bind a non-terminating system
;;;; boundary so the requested code can be observed without exiting the image.

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
  "Terminate through the system boundary. The native boundary exits the
process; a test boundary can capture the code and return."
  (cl-boundary-kit:system-exit
   (cl-boundary-kit:boundary-context-get (%cli-context) :system)
   code))
