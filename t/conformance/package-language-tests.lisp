;;;; tests/conformance/package-language-tests.lisp
;;;; ANSI CL Package System Conformance Tests
;;;;
;;;; Tests package operations that should work per ANSI CL. These run as
;;;; regular conformance tests against the runtime package registry.

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────
;;; Helper: compile and run a string, capturing stdout
;;; ──────────────────────────────────────────────────────────────────────

(defun run-cl-string (code &key (capture-output t))
  "Compile and run CODE string through cl-cc pipeline.
When output is captured, return stdout if the program wrote any; otherwise
return the printed representation of the primary result."
  (let ((out (make-string-output-stream)))
    (let* ((*standard-output* (if capture-output out *standard-output*))
           (value (cl-cc:run-string code)))
      (if capture-output
          (let ((output (get-output-stream-string out)))
            (if (plusp (length output))
                output
                (princ-to-string value)))
          value))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Basic Package Operations
;;; ──────────────────────────────────────────────────────────────────────
;;; These test core package registry operations through the cl-cc runtime.

;;; ──────────────────────────────────────────────────────────────────────
;;; Package Name Operations
;;; ──────────────────────────────────────────────────────────────────────

;;; ──────────────────────────────────────────────────────────────────────
;;; Symbol Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-gensym-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (let ((g1 (cl-cc/vm::vm-gensym-inst "G" nil))
          (g2 (cl-cc/vm::vm-gensym-inst "G" nil)))
      (expect g1 :to-be-truthy)
      (expect g2 :to-be-truthy)
      (expect (eq g1 g2) :to-be-falsy))))

;;; ──────────────────────────────────────────────────────────────────────
;;; defpackage Macro (E2E via run-string)
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-defpackage-e2e"
  (let ((result (run-cl-string
                 "(progn
                    (defpackage :e2e-pkg
                      (:use :cl)
                      (:export :hello-world))
                    (in-package :e2e-pkg)
                    (defun hello-world () \"Hello from e2e-pkg\")
                    (in-package :cl-user)
                    (e2e-pkg:hello-world))"
                 :capture-output t)))
    (expect result :to-equal "Hello from e2e-pkg")))

(it-sequential "pkg-defpackage-conflict-detection"
  (let ((result (run-cl-string
                 "(progn
                    (defpackage :conflict-a (:export :dup))
                    (defpackage :conflict-b (:export :dup))
                    (handler-case
                        (progn
                          (defpackage :conflict-user (:use :conflict-a :conflict-b))
                          :no-error)
                      (error (c) :conflict-detected)))"
                 :capture-output t)))
    ;; ANSI CL requires conflict detection on :USE
    (expect result :to-equal "CONFLICT-DETECTED")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Package Iteration
;;; ──────────────────────────────────────────────────────────────────────

;;; ──────────────────────────────────────────────────────────────────────
;;; delete-package / rename-package
;;; ──────────────────────────────────────────────────────────────────────

;;; ──────────────────────────────────────────────────────────────────────
;;; unintern
;;; ──────────────────────────────────────────────────────────────────────

