;;;; tests/unit/compile/builtin-registry-data-tests.lisp — Builtin Registry Data tests
(in-package :cl-cc/test)

(it-sequential "builtin-registry-data-table-sizes unary"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-unary-entries* 100)
    (expect (> (length table) min-size) :to-be-truthy)))

(it-sequential "builtin-registry-data-table-sizes binary"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-binary-entries* 20)
    (expect (> (length table) min-size) :to-be-truthy)))

(it-sequential "builtin-registry-data-table-sizes string-cmp"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-string-cmp-entries* 10)
    (expect (> (length table) min-size) :to-be-truthy)))

;; "car" and "mod" ctors are covered by builtin-registry-constructor-symbols
;; in builtin-registry-tests.lisp. "set" and "string=" are not covered there.
(it-sequential "builtin-registry-data-representative-mappings set"
  (destructuring-bind (sym table expected-ctor) (list 'set cl-cc/compile::*builtin-binary-entries* 'cl-cc::make-vm-set-symbol-value)
    (expect (cdr (assoc sym table)) :to-equal expected-ctor)))

(it-sequential "builtin-registry-data-representative-mappings string="
  (destructuring-bind (sym table expected-ctor) (list 'string= cl-cc/compile::*builtin-string-cmp-entries* 'cl-cc::make-vm-string=)
    (expect (cdr (assoc sym table)) :to-equal expected-ctor)))

(it-sequential "builtin-registry-data-representative-mappings find-package"
  (destructuring-bind (sym table expected-ctor) (list 'find-package cl-cc/compile::*builtin-unary-entries* 'cl-cc::make-vm-find-package)
    (expect (cdr (assoc sym table)) :to-equal expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings float"
  (destructuring-bind (sym expected-ctor) (list 'float 'cl-cc::make-vm-float-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings float-precision"
  (destructuring-bind (sym expected-ctor) (list 'float-precision 'cl-cc::make-vm-float-precision)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings float-radix"
  (destructuring-bind (sym expected-ctor) (list 'float-radix 'cl-cc::make-vm-float-radix)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings float-sign"
  (destructuring-bind (sym expected-ctor) (list 'float-sign 'cl-cc::make-vm-float-sign)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings float-digits"
  (destructuring-bind (sym expected-ctor) (list 'float-digits 'cl-cc::make-vm-float-digits)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings decode-float"
  (destructuring-bind (sym expected-ctor) (list 'decode-float 'cl-cc::make-vm-decode-float)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "builtin-registry-data-float-mappings integer-decode-float"
  (destructuring-bind (sym expected-ctor) (list 'integer-decode-float 'cl-cc::make-vm-integer-decode-float)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*)) :to-be expected-ctor)))

(it-sequential "fr-183-known-function-property-db-classifies-representatives"
  (expect (cl-cc/optimize:known-function-property-p '+ :pure) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-property-p '+ :foldable) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-property-p 'length :read-only) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-property-p 'length :nonneg-result) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-property-p 'eq :always-returns) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-property-p 'eq :no-escape) :to-be-truthy)
  (expect (cl-cc/optimize:known-function-effect-kind '+) :to-be :pure)
  (expect (cl-cc/optimize:known-function-effect-kind 'length) :to-be :read-only)
  (expect (cl-cc/optimize:known-function-effect-kind 'cons) :to-be :alloc)
  (expect (cl-cc/optimize:known-function-effect-kind 'princ) :to-be :io)
  (expect (cl-cc/optimize:known-function-effect-kind 'rplaca) :to-be :write-global)
  (expect (cl-cc/optimize:known-function-effect-kind 'error) :to-be :control))

(it-sequential "fr-183-known-function-property-db-stays-conservative"
  (expect (cl-cc/optimize:known-function-property-p 'car :pure) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p 'car :no-escape) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p 'cons :no-escape) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p 'append :no-escape) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p 'complement :pure) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p 'complement :no-escape) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-effect-kind 'complement) :to-be :alloc)
  (expect (cl-cc/optimize:known-function-property-p 'mod :always-returns) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-property-p '/ :always-returns) :to-be-falsy)
  (expect (cl-cc/optimize:known-function-properties 'not-a-known-function) :to-be-null)
  (expect (cl-cc/optimize:known-function-effect-kind 'not-a-known-function) :to-be :unknown))

(it-sequential "fr-183-builtin-registry-stores-properties-on-every-entry"
  (let ((missing '()))
    (maphash (lambda (name entry)
               (unless (and (listp (cl-cc/compile::be-properties entry))
                            (member :registered-builtin
                                    (cl-cc/compile::be-properties entry)
                                    :test #'eq))
                 (push name missing)))
             cl-cc/compile::*builtin-registry*)
    (expect (nreverse missing) :to-be-null)))

(it-sequential "fr-183-representative-registry-properties"
  (dolist (case '( ("LENGTH" (:read-only :nonneg-result))
                  ("CAR" (:read-only))
                  ("ABS" (:pure :foldable :no-escape))
                  ("PRINC" (:io))
                  ("RPLACA" (:write-global :always-returns))
                  ("ERROR" (:control))))
    (destructuring-bind (name-str expected-properties) case
      (let ((entry (gethash name-str cl-cc/compile::*builtin-registry*)))
        (expect entry :to-be-truthy)
        (dolist (property expected-properties)
          (expect (member property (cl-cc/compile::be-properties entry) :test #'eq) :to-be-truthy))))))
