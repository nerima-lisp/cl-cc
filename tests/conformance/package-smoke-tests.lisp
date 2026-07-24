;;;; tests/conformance/package-smoke-tests.lisp
;;;; Wave 1 Smoke Tests — verify the package self-hosting pipeline
;;;; These are NOT expected-fail; they should PASS using the runtime registry.

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────
;;; Smoke Test 1: find-package → intern → export → find-symbol pipeline
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "pkg-smoke-find-package"
  (let ((pkg (cl-cc/runtime::rt-find-package "CL-USER")))
    (expect pkg :to-be-truthy)
    (expect (cl-cc/runtime::rt-package-name pkg) :to-equal "CL-USER")))

(it-sequential "pkg-smoke-find-package-cl"
  (let ((pkg (cl-cc/runtime::rt-find-package "CL")))
    (expect pkg :to-be-truthy)
    (expect (cl-cc/runtime::rt-package-name pkg) :to-equal "CL")))

(it-sequential "pkg-smoke-intern"
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-TEST-SYM" pkg)))
    (expect sym :to-be-truthy)
    (expect (cl-cc/runtime::rt-symbol-name sym) :to-equal "SMOKE-TEST-SYM")))

(it-sequential "pkg-smoke-export"
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-EXPORT-SYM" pkg))
         (result (cl-cc/runtime::rt-export (list sym) pkg)))
    (expect result :to-be-truthy)))

(it-sequential "pkg-smoke-find-symbol"
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SMOKE-FIND-SYM" pkg))
         (_ (cl-cc/runtime::rt-export (list sym) pkg)))
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-FIND-SYM" pkg)
      (expect found :to-be-truthy)
      (expect status :to-be :external))))

(it-sequential "pkg-smoke-use-package"
  (let* ((lib (cl-cc/runtime::rt-find-package "CL-USER"))
         (user (or (cl-cc/runtime::rt-find-package "SMOKE-USER-PKG")
                   (cl-cc/runtime::rt-make-package "SMOKE-USER-PKG")))
         (sym (cl-cc/runtime::rt-intern "SMOKE-USE-FN" lib))
         (_ (cl-cc/runtime::rt-export (list sym) lib)))
    ;; use-package
    (cl-cc/runtime::rt-use-package (list lib) user)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-USE-FN" user)
      (expect found :to-be-truthy)
      (expect status :to-be :inherited))
    ;; unuse-package
    (cl-cc/runtime::rt-unuse-package (list lib) user)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-USE-FN" user)
      (expect found :to-be-null))))

(it-sequential "pkg-smoke-list-all-packages"
  (let ((pkgs (cl-cc/runtime::rt-list-all-packages)))
    (expect (>= (length pkgs) 2) :to-be-truthy)
    (expect (find "CL-USER" pkgs
                       :test #'equal
                       :key #'cl-cc/runtime::rt-package-name) :to-be-truthy)
    (expect (find "CL" pkgs
                       :test #'equal
                       :key #'cl-cc/runtime::rt-package-name) :to-be-truthy)))

(it-sequential "pkg-smoke-import"
  (let* ((src (or (cl-cc/runtime::rt-find-package "SMOKE-SRC")
                  (cl-cc/runtime::rt-make-package "SMOKE-SRC")))
         (dst (or (cl-cc/runtime::rt-find-package "SMOKE-DST")
                  (cl-cc/runtime::rt-make-package "SMOKE-DST")))
         (sym (cl-cc/runtime::rt-intern "SMOKE-IMPORT-FN" src))
         (_ (cl-cc/runtime::rt-export (list sym) src)))
    (cl-cc/runtime::rt-import (list sym) dst)
    (multiple-value-bind (found status)
        (cl-cc/runtime::rt-find-symbol "SMOKE-IMPORT-FN" dst)
      (expect found :to-be-truthy)
      (expect status :to-be :internal))))

(it-sequential "pkg-smoke-symbol-name"
  (let* ((pkg (cl-cc/runtime::rt-find-package "CL-USER"))
         (sym (cl-cc/runtime::rt-intern "SYMBOL-NAME-TEST" pkg)))
    (expect (cl-cc/runtime::rt-symbol-name sym) :to-equal "SYMBOL-NAME-TEST")))
