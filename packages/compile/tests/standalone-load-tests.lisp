;;;; tests/integration/standalone-load-tests.lisp
;;;;
;;;; Verifies each extracted sibling ASDF system from the package-by-feature
;;;; migration is loaded and its feature package is populated. This codifies
;;;; the in-process guarantee that downstream consumers can load the sibling
;;;; systems (e.g. :cl-cc-type) without depending on the umbrella :cl-cc
;;;; system's eval-when asdf:load-asd bootstrap pulling them in.
;;;;
;;;; For each extracted system we check:
;;;;   1. (asdf:find-system :cl-cc-XXX) returns non-nil
;;;;   2. The corresponding feature package exists
;;;;   3. A representative symbol is present in the package namespace
;;;;
;;;; A subprocess-based true-standalone load test is a future enhancement.

(in-package :cl-cc/test)


(defun %standalone-find-system (system-name)
  "Return the ASDF system object for SYSTEM-NAME, or NIL on failure."
  (handler-case (asdf:find-system system-name nil)
    (error () nil)))

(defun %standalone-symbol-present-p (symbol-name package-name)
  "Return non-NIL when SYMBOL-NAME is interned in PACKAGE-NAME."
  (let ((pkg (find-package package-name)))
    (when pkg
      (multiple-value-bind (sym status) (find-symbol symbol-name pkg)
        (declare (ignore status))
        sym))))

(it-sequential "standalone-sibling-systems-loaded cl-cc-ast"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-ast :cl-cc/ast "AST-CHILDREN")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-parse"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-parse :cl-cc/parse "PARSE-CL-SOURCE")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-binary"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-binary :cl-cc/binary "MACH-O-BUILDER")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-runtime"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-runtime :cl-cc/runtime "RT-CONS")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-bytecode"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-bytecode :cl-cc/bytecode "ENCODE-ADD")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-ir"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-ir :cl-cc/ir "IR-MAKE-FUNCTION")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-mir"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-mir :cl-cc/mir "MIR-MAKE-FUNCTION")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-type"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-type :cl-cc/type "TYPE-TO-STRING")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-optimize"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-optimize :cl-cc/optimize "OPTIMIZE-INSTRUCTIONS")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-emit"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-emit :cl-cc/emit "ALLOCATE-REGISTERS")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-expand"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-expand :cl-cc/expand "OUR-MACROEXPAND-1")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-compile"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-compile :cl-cc/compile "COMPILE-EXPRESSION")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(it-sequential "standalone-sibling-systems-loaded cl-cc-vm"
  (destructuring-bind (system-name package-name representative-symbol) (list :cl-cc-vm :cl-cc/vm "VM-STATE")
    (expect (%standalone-find-system system-name) :to-be-truthy) (expect (find-package package-name) :to-be-truthy) (expect (%standalone-symbol-present-p representative-symbol package-name) :to-be-truthy)))

(defun %bridge-registered-p (symbol-name)
  "Return T when SYMBOL-NAME resolved via :cl-cc (as the VM does) is in the bridge table.
The VM looks up function names via (find-symbol name :cl-cc), so this mirrors that path."
  (let ((sym (find-symbol symbol-name :cl-cc)))
    (and sym (gethash sym cl-cc/vm::*vm-host-bridge-functions*))))

(it-sequential "bridge-cross-package-symbols-registered run-string"
  (destructuring-bind (expected symbol-name) (list t "RUN-STRING")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered run-string-repl"
  (destructuring-bind (expected symbol-name) (list t "RUN-STRING-REPL")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered our-load"
  (destructuring-bind (expected symbol-name) (list t "OUR-LOAD")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered compile-expression"
  (destructuring-bind (expected symbol-name) (list t "COMPILE-EXPRESSION")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered compile-string"
  (destructuring-bind (expected symbol-name) (list t "COMPILE-STRING")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered our-eval"
  (destructuring-bind (expected symbol-name) (list t "OUR-EVAL")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered parse-all-forms"
  (destructuring-bind (expected symbol-name) (list t "PARSE-ALL-FORMS")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered generate-lambda-bindings"
  (destructuring-bind (expected symbol-name) (list t "GENERATE-LAMBDA-BINDINGS")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "bridge-cross-package-symbols-registered register-macro-absent"
  (destructuring-bind (expected symbol-name) (list nil "REGISTER-MACRO")
    (expect (not (null (%bridge-registered-p symbol-name))) :to-equal expected)))

(it-sequential "vm-eval-hooks-wired-after-load"
  (expect cl-cc/vm::*vm-eval-hook* :to-be-truthy)
  (expect cl-cc/vm::*vm-compile-string-hook* :to-be-truthy))
