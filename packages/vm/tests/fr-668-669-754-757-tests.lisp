;;;; packages/vm/tests/fr-668-669-754-757-tests.lisp

(in-package :cl-cc/test)


(defun %fr-vm-unary (ctor value)
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 value)
    (exec1 (funcall ctor :dst 0 :src 1) state)
    (values (cl-cc:vm-reg-get state 0)
            (cl-cc:vm-values-list state)
            state)))

(defun %fr-vm-binary (ctor lhs rhs)
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 lhs)
    (cl-cc:vm-reg-set state 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) state)
    (values (cl-cc:vm-reg-get state 0)
            (cl-cc:vm-values-list state)
            state)))
(defun %fr-vm-constructor (name feature)
  (multiple-value-bind (symbol status)
      (find-symbol name :cl-cc)
    (unless (and symbol (eq status :external) (fboundp symbol))
      (skip (format nil "~A constructor ~A is not available yet" feature name)))
    (symbol-function symbol)))
(it-sequential "fr-754-coerce-runtime-cases integer-to-double"
  (destructuring-bind (value type expected) (list 3 'double-float 3.0d0)
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases ratio-to-single"
  (destructuring-bind (value type expected) (list 1/2 'single-float 0.5f0)
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases float-to-integer"
  (destructuring-bind (value type expected) (list 3.9d0 'integer 3)
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases list-to-vector"
  (destructuring-bind (value type expected) (list '(a b) 'vector #(a b))
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases vector-to-list"
  (destructuring-bind (value type expected) (list #(1 2 3) 'list '(1 2 3))
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases char-to-string"
  (destructuring-bind (value type expected) (list #\x 'string "x")
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-754-coerce-runtime-cases string-to-character"
  (destructuring-bind (value type expected) (list "Z" 'character #\Z)
    (multiple-value-bind (actual) (%fr-vm-binary (%fr-vm-constructor "MAKE-VM-COERCE" "FR-754") value type)
    (if (vectorp expected)
        (expect (coerce actual 'list) :to-equal (coerce expected 'list))
        (expect actual :to-equal expected)))))

(it-sequential "fr-757-function-cell-management-round-trip"
  (let ((state (make-test-vm))
        (fn #'identity))
    (cl-cc:vm-reg-set state 1 'fr-757-fn)
    (cl-cc:vm-reg-set state 2 fn)
    (exec1 (funcall (%fr-vm-constructor "MAKE-VM-SET-FDEFINITION" "FR-757") :dst 0 :lhs 1 :rhs 2) state)
    (expect (cl-cc:vm-reg-get state 0) :to-be fn)
    (exec1 (cl-cc:make-vm-fboundp :dst 3 :src 1) state)
    (expect (cl-cc:vm-reg-get state 3) :to-be-truthy)
    (exec1 (cl-cc:make-vm-fdefinition :dst 4 :src 1) state)
    (expect (cl-cc:vm-reg-get state 4) :to-be fn)
    (exec1 (funcall (%fr-vm-constructor "MAKE-VM-FMAKUNBOUND" "FR-757") :dst 5 :src 1) state)
    (expect (cl-cc:vm-reg-get state 5) :to-be 'fr-757-fn)
    (exec1 (cl-cc:make-vm-fboundp :dst 6 :src 1) state)
    (expect (cl-cc:vm-reg-get state 6) :to-be-falsy)))
