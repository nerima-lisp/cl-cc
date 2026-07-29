;;;; tests/conformance/package-conformance-tests.lisp
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

(it-sequential "pkg-find-package-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; Bootstrap the registry with CL-USER
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil)))
      (expect pkg :to-be-truthy)
      (expect (cl-cc/vm::vm-symbol-name (cl-cc/vm::vm-package-name pkg)) :to-equal "CL-USER"))))

(it-sequential "pkg-intern-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil))
           (sym (cl-cc/vm::vm-intern-symbol "MY-TEST-SYM" pkg)))
      (expect sym :to-be-truthy)
      (expect (cl-cc/vm::vm-symbol-name sym) :to-equal "MY-TEST-SYM"))))

(it-sequential "pkg-export-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-find-package "CL-USER" nil)))
      (cl-cc/vm::vm-export (list (cl-cc/vm::vm-intern-symbol "EXPORTED-SYM" pkg)) pkg)
      (multiple-value-bind (sym status)
          (cl-cc/vm::vm-find-symbol "EXPORTED-SYM" pkg)
        (expect sym :to-be-truthy)
        (expect status :to-be :external)))))

(it-sequential "pkg-import-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((src (cl-cc/vm::vm-make-package "SRC-PKG"))
           (dst (cl-cc/vm::vm-make-package "DST-PKG"))
           (sym (cl-cc/vm::vm-intern-symbol "IMPORTED" src)))
      (cl-cc/vm::vm-export (list sym) src)
      (cl-cc/vm::vm-import (list sym) dst)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "IMPORTED" dst)
        (expect found :to-be-truthy)
        (expect status :to-be :internal)))))

(it-sequential "pkg-use-package-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "LIB-PKG"))
           (user (cl-cc/vm::vm-make-package "USER-PKG"))
           (sym (cl-cc/vm::vm-intern-symbol "LIB-FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "LIB-FN" user)
        (expect found :to-be-truthy)
        (expect status :to-be :inherited)))))

(it-sequential "pkg-unuse-package-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "UNUSE-LIB"))
           (user (cl-cc/vm::vm-make-package "UNUSE-USER"))
           (sym (cl-cc/vm::vm-intern-symbol "FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (cl-cc/vm::vm-unuse-package lib user)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "FN" user)
        (expect found :to-be-null)))))

(it-sequential "pkg-shadow-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((lib (cl-cc/vm::vm-make-package "SHADOW-LIB"))
           (user (cl-cc/vm::vm-make-package "SHADOW-USER"))
           (sym (cl-cc/vm::vm-intern-symbol "FN" lib)))
      (cl-cc/vm::vm-export (list sym) lib)
      (cl-cc/vm::vm-use-package lib user)
      (cl-cc/vm::vm-shadow (list "FN") user)
      ;; After shadowing, FN in user should be internal (not inherited)
      (multiple-value-bind (found status)
          (cl-cc/vm::vm-find-symbol "FN" user)
        (expect found :to-be-truthy)
        (expect status :to-be :internal)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Package Name Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-package-name-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "NAME-TEST-PKG")))
      (expect (cl-cc/vm::vm-package-name pkg) :to-equal "NAME-TEST-PKG"))))

(it-sequential "pkg-package-nicknames-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let* ((pkg (cl-cc/vm::vm-make-package "NICK-TEST" :nicknames '("N1" "N2")))
           (nicks (cl-cc/vm::vm-package-nicknames pkg)))
      (expect (member "N1" nicks :test #'equal) :to-be-truthy)
      (expect (member "N2" nicks :test #'equal) :to-be-truthy))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Symbol Operations
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-make-symbol-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (let ((sym (cl-cc/vm::vm-make-symbol "UNINTERNED")))
      (expect sym :to-be-truthy)
      (expect (cl-cc/vm::vm-symbol-name sym) :to-equal "UNINTERNED")
      ;; make-symbol creates uninterned symbols
      (expect (cl-cc/vm::vm-symbol-package sym) :to-be-null))))

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

(it-sequential "pkg-do-symbols-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "ITER-PKG"))
          (syms '()))
      (cl-cc/vm::vm-intern-symbol "A" pkg)
      (cl-cc/vm::vm-intern-symbol "B" pkg)
      (cl-cc/vm::vm-do-symbols (sym pkg)
        (push sym syms))
      (expect (= 2 (length syms)) :to-be-truthy))))

(it-sequential "pkg-list-all-packages-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkgs (cl-cc/vm::vm-list-all-packages)))
      (expect (>= (length pkgs) 1) :to-be-truthy)
      ;; CL-USER should be in the list
      (expect (find "CL-USER" pkgs
                         :test #'equal
                         :key #'cl-cc/vm::vm-package-name) :to-be-truthy))))

;;; ──────────────────────────────────────────────────────────────────────
;;; delete-package / rename-package
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-delete-package-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "TEMP-PKG")))
      (expect (cl-cc/vm::vm-find-package "TEMP-PKG" nil) :to-be-truthy)
      (cl-cc/vm::vm-delete-package pkg)
      (expect (cl-cc/vm::vm-find-package "TEMP-PKG" nil) :to-be-null))))

(it-sequential "pkg-rename-package-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "OLD-NAME")))
      (cl-cc/vm::vm-rename-package pkg "NEW-NAME")
      (expect (cl-cc/vm::vm-find-package "OLD-NAME" nil) :to-be-null)
      (expect (cl-cc/vm::vm-find-package "NEW-NAME" nil) :to-be-truthy))))

;;; ──────────────────────────────────────────────────────────────────────
;;; unintern
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-unintern-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (cl-cc/vm::vm-bootstrap-package-registry)
    (let ((pkg (cl-cc/vm::vm-make-package "UNINTERN-TEST")))
      (cl-cc/vm::vm-intern-symbol "TEMP-SYM" pkg)
      (expect (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg) :to-be-truthy)
      (cl-cc/vm::vm-unintern (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg) pkg)
      (expect (cl-cc/vm::vm-find-symbol "TEMP-SYM" pkg) :to-be-null))))
