;;; runtime-stdlib-3-expander-tests.lisp — FR-935 proclamation verification

(in-package :cl-cc/test)



(defun %runtime-stdlib-3-proclamation (kind name)
  (let ((bucket (gethash kind cl-cc/expand:*global-proclamations*)))
    (and bucket (gethash name bucket))))

(it-sequential "runtime-stdlib-3-declaim-records-type-ftype-special"
  (let ((cl-cc/expand:*global-proclamations* (make-hash-table :test #'eq)))
    (expect (cl-cc/expand:our-macroexpand-1
                  '(declaim (type fixnum *counter*)
                            (ftype (function (integer) integer) inc-counter)
                            (special *counter*))) :to-be-null)
    (expect (%runtime-stdlib-3-proclamation 'type '*counter*) :to-equal 'fixnum)
    (expect (%runtime-stdlib-3-proclamation 'ftype 'inc-counter) :to-equal '(function (integer) integer))
    (expect (%runtime-stdlib-3-proclamation 'special '*counter*) :to-be-truthy)))
