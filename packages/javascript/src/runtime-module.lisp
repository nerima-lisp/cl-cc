;;;; packages/javascript/src/runtime-module.lisp — JS module import/export bridge

(in-package :cl-cc/javascript)

(defun %js-module-namespace (module-name &optional with-opts)
  "Create the host module namespace object used by dynamic import."
  (let ((specifier (%js-to-string module-name)))
    (%js-make-object
     "default" +js-undefined+
     "__moduleName__" specifier
     "url" specifier
     "with" (or with-opts +js-undefined+)
     "resolve" (lambda (next-specifier)
                 (%js-to-string next-specifier)))))

(defun %js-import (module-name &optional with-opts)
  "Dynamic import host hook returning a resolved module namespace promise."
  (%js-promise-resolve
   (%js-module-namespace module-name with-opts)))

(defun %js-import-meta ()
  "Return host metadata for import.meta."
  (%js-make-object
   "url" "file://cl-cc/javascript/module"
   "resolve" (lambda (specifier)
               (%js-to-string specifier))))

(defvar *js-module-exports* (%js-make-object)
  "Current module export namespace accumulated while evaluating a module.")

(defun %js-reset-module-exports ()
  "Reset and return the current module export namespace."
  (setf *js-module-exports* (%js-make-object)))

(defun %js-current-module-exports ()
  "Return the current module export namespace object."
  *js-module-exports*)

(defun %js-export-reexports ()
  "Return the mutable re-export metadata array for the current module."
  (let ((existing (%js-get-prop *js-module-exports* "__reexports__")))
    (if (%js-vec-p existing)
        existing
        (let ((fresh (%js-make-array)))
          (%js-set-prop *js-module-exports* "__reexports__" fresh)
          fresh))))

(defun %js-export-specifier-object (spec)
  "Convert a parsed export specifier plist into a JS metadata object."
  (%js-make-object
   "local" (getf spec :local)
   "exported" (getf spec :exported)))

(defun %js-export-kind-name (kind)
  "Return a stable metadata name for export KIND."
  (if (keywordp kind)
      (string-downcase (symbol-name kind))
      (%js-to-string kind)))

(defun %js-record-reexport (kind value from-module)
  "Record unresolved re-export metadata for the host module loader."
  (%js-array-push
   (%js-export-reexports)
   (%js-make-object
    "kind" (%js-export-kind-name kind)
    "from" (or from-module +js-undefined+)
    "value" value)))

(defun %js-export (kind value &optional from-module declaration-names)
  "Record a module export and return VALUE.
Declaration exports receive DECLARATION-NAMES from the parser so named function,
class, and lexical exports are visible in the module namespace object."
  (case kind
    (:default
     (%js-set-prop *js-module-exports* "default" value))
    (:declaration
     (dolist (name declaration-names)
       (%js-set-prop *js-module-exports* name value)))
    (:named
     (dolist (spec value)
       (let ((exported (getf spec :exported)))
         (%js-set-prop *js-module-exports* exported +js-undefined+)))
     (%js-set-prop *js-module-exports*
                   "__namedExports__"
                   (apply #'%js-make-array
                          (mapcar #'%js-export-specifier-object value))))
    (:re-export
     (%js-record-reexport :named
                          (apply #'%js-make-array
                                 (mapcar #'%js-export-specifier-object value))
                          from-module))
    (:star
     (%js-record-reexport :star value from-module)))
  value)
