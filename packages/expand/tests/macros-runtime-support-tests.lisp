;;;; tests/unit/expand/macros-runtime-support-tests.lisp
;;;; Coverage tests for src/expand/macros-runtime-support.lisp and macros-package-system.lisp

(in-package :cl-cc/test)



(it-sequential "in-package-expansion"
  (let ((result (our-macroexpand-1 '(in-package :cl-cc))))
    (expect (car result) :to-be 'progn)
    (expect (car (second result)) :to-be 'setq)
    (expect (third result) :to-equal '(quote :cl-cc))))

(it-sequential "declare-expansion-preserves-form"
  (expect (our-macroexpand-1 '(declare (special x))) :to-equal '(declare (special x))))

(it-sequential "declaim-non-inline-forms-still-expand-to-nil"
  (expect (our-macroexpand-1 '(declaim (special x))) :to-equal nil))

(it-sequential "declaim-inline-updates-registry"
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1
                   '(declaim (inline fast slow)
                             (notinline slow)
                             (special x))) :to-equal nil)
    (expect (gethash 'fast cl-cc/expand:*declaim-inline-registry*) :to-be :inline)
    (expect (gethash 'slow cl-cc/expand:*declaim-inline-registry*) :to-be :notinline)
    (expect (gethash 'x cl-cc/expand:*declaim-inline-registry*) :to-be-falsy)))

(it-sequential "declaim-optimize-updates-registry"
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1
                   '(declaim (optimize speed
                                       (safety 0)
                                       (debug 3)
                                       (space 2)
                                       (compilation-speed 1)
                                       (speed 4)
                                       (unknown-quality 2))
                             (special x))) :to-equal nil)
    (expect (= 3 (gethash 'speed cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 0 (gethash 'safety cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 3 (gethash 'debug cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 2 (gethash 'space cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 1 (gethash 'compilation-speed cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (gethash 'unknown-quality cl-cc/expand:*declaim-optimize-registry*) :to-be-falsy)))

(it-sequential "declaim-optimize-updates-fresh-registry-after-repeat-expansion"
  (let ((form '(declaim (optimize (safety 0)))))
    (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
      (expect (our-macroexpand-1 form) :to-equal nil)
      (expect (= 0 (gethash 'safety cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy))
    (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
      (expect (our-macroexpand-1 form) :to-equal nil)
      (expect (= 0 (gethash 'safety cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy))))

(it-sequential "locally-preserves-declarations"
  (let ((result (our-macroexpand-1 '(locally (declare (special x)) x))))
    (expect (car result) :to-be 'let)
    (expect (car (caddr result)) :to-be 'declare)))

(it-sequential "progv-expands-with-unwind-protect"
  (let ((result (our-macroexpand-1 '(progv syms vals (foo)))))
    (expect (car result) :to-be 'let*)
    (expect (car (caddr result)) :to-be 'unwind-protect)))

(it-sequential "defpackage-creates-package"
  (let* ((result (our-macroexpand-1 '(defpackage :foo (:use :cl) (:export foo))))
         (result-str (format nil "~S" result)))
    (expect 'progn :to-be (car result))
    (expect (search "RT-FIND-PACKAGE" result-str) :to-be-truthy)
    (expect (search "RT-MAKE-PACKAGE" result-str) :to-be-truthy)
    (expect (search "RT-EXPORT" result-str) :to-be-truthy)
    (expect (search "ADD-PACKAGE-LOCAL-NICKNAME" result-str) :to-be-falsy)))

(it-sequential "package-iteration-expands-to-runtime-package-lookup"
  (let* ((result (our-macroexpand-1 '(do-symbols (s :cl-user) s)))
         (result-str (format nil "~S" result)))
    (expect (search "RT-FIND-PACKAGE" result-str) :to-be-truthy)
    (expect (search "(FIND-PACKAGE :CL-USER)" result-str) :to-be-falsy)))

(it-sequential "foreign-funcall-expands-to-vm-bridge"
  (dolist (head (list 'foreign-funcall
                      (intern "FOREIGN-FUNCALL" (find-package :cffi))))
    (let ((result (our-macroexpand-1 `(,head "strlen" :string "abcd" :int))))
      (expect (symbol-name (car result)) :to-equal "%FOREIGN-FUNCALL")
      (expect (package-name (symbol-package (car result))) :to-equal "CL-CC/VM"))))

(it-sequential "foreign-funcall-string-encoded-cffi-aliases-are-not-registered"
  (dolist (name '("CFFI:FOREIGN-FUNCALL" "CFFI::FOREIGN-FUNCALL"))
    (expect (cl-cc/expand::lookup-macro (intern name :cl-cc/expand)) :to-be-falsy)))
