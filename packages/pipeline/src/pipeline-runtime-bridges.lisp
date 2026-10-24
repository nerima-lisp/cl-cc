;;;; pipeline-runtime-bridges.lisp - Language runtime bridge registration
(in-package :cl-cc/pipeline)

(defun %register-backend-protocol-bridges ()
  "Register every registered backend's bridge symbols with the VM.

The pipeline used to name cl-cc/php and cl-cc/javascript directly and scan them
for their %PHP-/%JS- helpers, which made the orchestrator depend on each
backend's package name and internal naming convention -- and made either
backend impossible to move to its own repository, since a dependent cannot name
an external package's internal symbols. Each backend now answers for itself
through cl-cc/backend-protocol, and the only thing that stays here is the part
that needs the VM: calling VM-REGISTER-HOST-BRIDGE.

Returns the symbols registered, which is what the migration was checked
against: the set has to be identical to what the old package scan produced."
  (let ((registered '()))
    ;; Two registries, because §5-1 was implemented twice. The one in
    ;; cl-cc/bootstrap is the older and wider of the pair -- it also carries
    ;; parser registration -- and the standalone cl-cc-php and
    ;; cl-cc-javascript repositories have already migrated to it. Draining it
    ;; here is what lets those repositories be used unchanged, without a
    ;; backend having to register through both.
    (dolist (entry (cl-cc/bootstrap:backend-bridge-providers))
      (let ((sym (car entry))
            (fn (cdr entry)))
        (when (and (symbolp sym) (functionp fn))
          (cl-cc/vm:vm-register-host-bridge sym fn)
          (push sym registered))))
    (dolist (entry cl-cc/backend-protocol:*registered-backends* (nreverse registered))
      (dolist (sym (cl-cc/backend-protocol:backend-bridge-symbols (cdr entry)))
        (when (and (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym)))
          (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym))
          (push sym registered))))))

(defun %register-logical-pathname-runtime-bridges ()
  (cl-cc/vm:vm-register-host-bridge
   'cl:logical-pathname-translations
   #'cl:logical-pathname-translations)
  (cl-cc/vm:vm-register-host-bridge
   (intern "%SET-LOGICAL-PATHNAME-TRANSLATIONS" :cl-cc)
   (lambda (host translations)
     (setf (cl:logical-pathname-translations host) translations)
     translations)))
(defun %register-package-runtime-bridges ()
  (cl-cc/vm:vm-register-host-bridge
   (quote cl:make-package)
   (lambda (name &key nicknames (use (quote (:cl))))
     (cl-cc/runtime:rt-make-package name :nicknames nicknames :use use)))
  (cl-cc/vm:vm-register-host-bridge
   (quote cl:symbol-name)
   (lambda (symbol)
     (let ((guest-symbol (if (cl-cc/vm::vm-immediate-symbol-p symbol)
                             (cl-cc/vm::vm-decode-symbol symbol)
                             symbol)))
       (cl-cc/runtime:rt-symbol-name guest-symbol))))
  (cl-cc/vm:vm-register-host-bridge
   (quote cl:intern)
   (lambda (name &optional package)
     (let* ((state cl-cc/vm:*vm-state*)
            (globals (and state (cl-cc/vm:vm-global-vars state)))
            (guest-name (if (cl-cc/vm::vm-immediate-symbol-p name)
                            (cl-cc/vm::vm-decode-symbol name)
                            name))
            (guest-package (or package
                               (and globals
                                    (gethash (quote cl:*package*) globals))))
            (guest-package (if (cl-cc/vm::vm-immediate-symbol-p guest-package)
                               (cl-cc/vm::vm-decode-symbol guest-package)
                               guest-package)))
       (cl-cc/runtime:rt-intern
        (cl-cc/vm::%vm-host-string guest-name)
        (cl-cc/vm::%vm-host-string guest-package))))))
(defun %register-backend-runtime-bridges ()
  "Register built-in package/pathname and loaded backend runtime bridges.

Called by both source compilation entry points. Backend bridge discovery belongs
to backend registries; the built-in Common Lisp bridges are registered directly."
  (%register-logical-pathname-runtime-bridges)
  (%register-package-runtime-bridges)
  (%register-backend-protocol-bridges))

(defun %register-php-runtime-bridges ()
  "Register PHP runtime helpers as VM host bridge functions.

Kept as a name because the pipeline calls it at a specific point in start-up;
the work is now the backend protocol's, and this registers whatever backends
have registered themselves, PHP among them."
  (%register-backend-protocol-bridges))
(defun %backend-vm-integration ()
  "Build the VM capabilities handed to every registered backend.

Every closure reads CL-CC/VM:*VM-STATE* when called rather than closing over
one, so a single install stays correct across VM invocations -- the JS `this'
binding in particular runs inside whichever state is current at call time."
  (flet ((globals ()
           (let ((state cl-cc/vm:*vm-state*))
             (and state (cl-cc/vm:vm-global-vars state)))))
    (cl-cc/backend-protocol:make-vm-integration
     :closure-p (lambda (value) (cl-cc/vm::%vm-closure-object-p value))
     :call-closure (lambda (closure args)
                     (cl-cc/vm::%vm-call-closure-sync closure cl-cc/vm:*vm-state* args))
     :global-bound-p (lambda (symbol)
                       (let ((gv (globals)))
                         (and gv (nth-value 1 (gethash symbol gv)))))
     :global-value (lambda (symbol)
                     (let ((gv (globals)))
                       (and gv (gethash symbol gv))))
     :set-global (lambda (symbol value)
                   (let ((gv (globals)))
                     (when gv (setf (gethash symbol gv) value))))
     :remove-global (lambda (symbol)
                      (let ((gv (globals)))
                        (when gv (remhash symbol gv)))))))

(defun %install-backend-vm-integrations ()
  "Offer the VM capabilities to every registered backend.

Backends whose host runtime never re-enters the VM inherit the protocol's no-op
method and are unaffected; JavaScript's runtime does, because a host array
method can be handed a compiled-JS callback."
  (let ((integration (%backend-vm-integration)))
    ;; Installers registered through cl-cc/bootstrap take no argument: a
    ;; backend that registers there installs its own integration and does not
    ;; need capabilities handed to it.
    (dolist (thunk (cl-cc/bootstrap:backend-vm-integration-installers))
      (funcall thunk))
    (dolist (entry cl-cc/backend-protocol:*registered-backends*)
      (cl-cc/backend-protocol:install-backend-vm-integration (cdr entry) integration))))

(defun %register-js-runtime-bridges ()
  "Register JavaScript runtime helpers and install its VM integration.

Both halves used to live here and both named cl-cc/javascript internals: the
outbound scan for %JS-* helpers, and the inbound SETF of that package's
*JS-APPLY-FN* / *JS-CALLABLE-P* / *JS-APPLY-WITH-THIS-FN*. The backend answers
for both now; what stays is the part that needs the VM."
  (%register-backend-protocol-bridges)
  (%install-backend-vm-integrations))

(defun seed-js-runtime-globals (state)
  "Seed backend runtime specials into STATE's VM globals.

The JS prelude binds standard globals to the VALUES of host specials -- Symbol
to *js-symbol-global*, Infinity to *js-inf-float*, the error classes and so on.
Each such reference compiles to a VM-GET-GLOBAL, so without seeding every JS
program fails at run time with an unbound global. Which specials those are is
the backend's answer, through BACKEND-GLOBAL-SYMBOLS."
  (when state
    (dolist (seeder (cl-cc/bootstrap:backend-global-seeders))
      (funcall seeder state))
    (dolist (entry cl-cc/backend-protocol:*registered-backends*)
      (dolist (sym (cl-cc/backend-protocol:backend-global-symbols (cdr entry)))
        (when (boundp sym)
          (setf (gethash sym (cl-cc/vm:vm-global-vars state))
                (symbol-value sym)))))))
