;;;; tests/unit/expand/expander-defclass-tests.lisp — Defclass expander tests

(in-package :cl-cc/test)



(it-sequential "expand-defclass-slot-spec-bare-symbol"
  (expect (cl-cc/expand::expand-defclass-slot-spec 'foo) :to-be 'foo))

(it-sequential "expand-defclass-slot-spec-no-initform-preserved"
  (let ((result (cl-cc/expand::expand-defclass-slot-spec
                 '(x :initarg :x :accessor x-accessor))))
    (expect (first result) :to-be 'x)
    (expect (second result) :to-be :initarg)
    (expect (third result) :to-be :x)
    (expect (fourth result) :to-be :accessor)
    (expect (fifth result) :to-be 'x-accessor)))

(it-sequential "expand-defclass-slot-spec-expands-initform"
  (let ((result (cl-cc/expand::expand-defclass-slot-spec
                 '(x :initarg :x :initform (+ 1 2)))))
    (expect (member :initform result) :to-be-truthy)))

(it-sequential "expand-defclass-slot-spec-non-initform-keys-untouched"
  (let* ((spec '(x :type integer :accessor get-x :initform 0))
         (result (cl-cc/expand::expand-defclass-slot-spec spec)))
    (expect (getf (rest result) :type) :to-be 'integer)))
