;;;; backend-protocol.lisp — language backend registration
;;;;
;;;; Inverts the direction between the pipeline and the language backends.
;;;;
;;;; The pipeline used to reach into cl-cc/php and cl-cc/javascript directly:
;;;; it knew their package names and their %PHP-/%JS- naming conventions, and
;;;; scanned those packages itself to decide what to register as VM host
;;;; bridges. That is a compile-time dependency from the orchestrator onto each
;;;; backend's internals, and it is what stopped either backend from moving into
;;;; its own repository -- an external package's internal symbols are not
;;;; something a dependent can name.
;;;;
;;;; Here the dependency runs the other way. A backend registers itself and
;;;; answers which of its own symbols it wants bridged; the pipeline asks the
;;;; registry and never names a backend package. The graph becomes a Y:
;;;;
;;;;     cl-cc/php ─┐
;;;;                ├─> cl-cc/backend-protocol <─ cl-cc/pipeline
;;;;  cl-cc/js ─────┘
;;;;
;;;; This lives in bootstrap, the deepest system both backends and the pipeline
;;;; already depend on, and deliberately holds no VM code: it is a registry and
;;;; two generic functions. Actually calling VM-REGISTER-HOST-BRIDGE stays in
;;;; the pipeline, which is the only participant that depends on cl-cc/vm.

(defpackage :cl-cc/backend-protocol
  (:use :cl)
  (:export #:*registered-backends*
           #:register-backend
           #:registered-backend
           #:backend-bridge-symbols
           #:backend-global-symbols))

(in-package :cl-cc/backend-protocol)

(defvar *registered-backends* '()
  "Alist of (LANGUAGE . BACKEND), most recently registered first.")

(defun register-backend (language backend)
  "Register BACKEND under LANGUAGE, replacing any previous registration.

Backends register themselves at load time, so reloading a backend system must
not leave two entries for one language behind."
  (setf *registered-backends*
        (cons (cons language backend)
              (remove language *registered-backends* :key #'car)))
  backend)

(defun registered-backend (language)
  "Return the backend registered under LANGUAGE, or NIL."
  (cdr (assoc language *registered-backends*)))

(defgeneric backend-bridge-symbols (backend)
  (:documentation
   "Return the fbound symbols BACKEND wants callable from compiled code.

The VM host bridge is a whitelist, so a symbol the backend lowers calls to but
does not list here is not callable. Each backend decides this for itself --
knowing its own naming convention is the one thing it is certain to know.")
  (:method (backend) (declare (ignore backend)) '()))

(defgeneric backend-global-symbols (backend)
  (:documentation
   "Return the bound special variables BACKEND wants seeded into VM globals.

Only backends whose prelude reads host specials through VM-GET-GLOBAL need
this; the default is none.")
  (:method (backend) (declare (ignore backend)) '()))
