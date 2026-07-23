;;;; cli/src/tty.lisp — Terminal styling backed by cl-tty-kit
;;;;
;;;; All ANSI styling in the CLI/REPL flows through cl-tty-kit's ANSI builders
;;;; (`ansi-sgr` and friends) rather than hand-assembled escape strings.  Color
;;;; is emitted only for an interactive terminal (and never when NO_COLOR is
;;;; set), so captured/piped output — including the test suite's
;;;; with-output-to-string — stays plain text.

(in-package :cl-cc/cli)

(defvar *repl-color* :auto
  "REPL/CLI color policy: :auto (color only on an interactive TTY), T (always),
or NIL (never).")

(defun %color-output-p (&optional (stream *standard-output*))
  "True when styled output should be emitted to STREAM under *REPL-COLOR*."
  (case *repl-color*
    ((t) t)
    ((nil) nil)
    (t (and (null (uiop:getenv "NO_COLOR"))
            (ignore-errors (interactive-stream-p stream))))))

(defun %sgr (text &rest codes)
  "Wrap TEXT in a cl-tty-kit SGR sequence built from CODES, resetting after,
when color is enabled; otherwise return TEXT unchanged."
  (if (and codes (%color-output-p))
      (concatenate 'string
                   (apply #'cl-tty-kit:ansi-sgr codes)
                   text
                   (cl-tty-kit:ansi-sgr 0))
      text))

;;; Semantic REPL styles (SGR codes: 1 bold, 2 dim, 31 red, 32 green,
;;; 36 cyan, 34 blue, 90 bright-black).
(defun %style-banner (text)  (%sgr text 1 34))
(defun %style-prompt (text)  (%sgr text 32))
(defun %style-result (text)  (%sgr text 36))
(defun %style-error  (text)  (%sgr text 31))
(defun %style-note   (text)  (%sgr text 90))
