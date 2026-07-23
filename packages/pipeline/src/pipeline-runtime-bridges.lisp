;;;; pipeline-runtime-bridges.lisp - Language runtime bridge registration
(in-package :cl-cc/pipeline)

(defun %register-backend-runtime-bridges ()
  "Install every registered backend's runtime helpers as VM host bridges.

The VM host bridge is a whitelist. Backends (php/js/…) own the knowledge of
which of their functions are callable from compiled code and register a
provider thunk with cl-cc/bootstrap; here we install whatever the registry
yields. The pipeline no longer references any backend package for this — see
each backend's runtime-bridge-provider.lisp. Called for every language so a
program of any backend sees its bridges (registration is idempotent)."
  (dolist (provider (cl-cc/bootstrap:backend-bridge-providers))
    (dolist (entry (funcall provider))
      (cl-cc/vm:vm-register-host-bridge (car entry) (cdr entry)))))

;; JS function bridges (%JS-*) are registered via the backend bridge registry;
;; the JS↔VM closure integration and *JS-* global seeding now live in
;; cl-cc/javascript's vm-integration.lisp and self-register with
;; cl-cc/bootstrap. The pipeline runs every registered backend generically —
;; it no longer references :cl-cc/javascript at all.

(defun %install-backend-vm-integrations ()
  "Run each registered backend's VM-integration installer (idempotent)."
  (dolist (fn (cl-cc/bootstrap:backend-vm-integration-installers))
    (funcall fn)))

(defun seed-js-runtime-globals (state)
  "Seed every registered backend's runtime globals into STATE's VM globals.

Kept under this (historically JS-specific) exported name for callers
(packages/cli, js e2e tests); it now delegates to the backend global-seeder
registry, so a backend's prelude globals resolve without the pipeline naming
the backend. The JS prelude binds standard globals (Symbol, Infinity, error
classes, …) to *JS-* specials that compile to vm-get-global, so without seeding
every JS program would fail with 'Unbound global variable: *JS-...*'."
  (when state
    (dolist (fn (cl-cc/bootstrap:backend-global-seeders))
      (funcall fn state))))
