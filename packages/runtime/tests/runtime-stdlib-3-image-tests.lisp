;;;; packages/runtime/tests/runtime-stdlib-3-image-tests.lisp — FR-1002/FR-1003

(in-package :cl-cc/test)


(defparameter *rt-core-test-root* nil)
(defparameter *rt-core-test-closure* nil)
(defparameter *rt-core-test-instance* nil)
(defparameter *rt-core-test-cycle* nil)
(defparameter *rt-core-test-string* nil)

(defclass rt-core-test-node ()
  ((name :initarg :name :accessor rt-core-test-node-name)
   (next :initarg :next :accessor rt-core-test-node-next)))

(defun %rt-core-test-path (name)
  (namestring (merge-pathnames (make-pathname :name name :type "core")
                               (uiop:temporary-directory))))

(defun %rt-core-file-size (path)
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun %rt-core-register-test-roots ()
  (mapc #'cl-cc/runtime:rt-image-register-global
        '(*rt-core-test-root*
          *rt-core-test-closure*
          *rt-core-test-instance*
          *rt-core-test-cycle*
          *rt-core-test-string*)))

(it-sequential "runtime-core-roundtrip-full-heap-roots"
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-roundtrip")))
    (setf *rt-core-test-root* (list :numbers #(1 2 3) :table (make-hash-table :test 'equal)))
    (setf (gethash "answer" (getf *rt-core-test-root* :table)) '(4 2))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-root* :mutated)
    (cl-cc/runtime:rt-load-core path)
    (expect (first *rt-core-test-root*) :to-equal :numbers)
    (expect (equalp #(1 2 3) (second *rt-core-test-root*)) :to-be-truthy)
    (expect (gethash "answer" (getf *rt-core-test-root* :table)) :to-equal '(4 2))))

(it-sequential "runtime-core-closure-serialization"
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-closure")))
    (setf *rt-core-test-closure* (let ((n 40)) (lambda (x) (+ n x))))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-closure* (lambda (x) x))
    (cl-cc/runtime:rt-load-core path)
    (expect (funcall *rt-core-test-closure* 2) :to-equal 42)))

(it-sequential "runtime-core-clos-instance-serialization"
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-clos"))
        (node (make-instance 'rt-core-test-node :name "root")))
    (setf (rt-core-test-node-next node) node
          *rt-core-test-instance* node)
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-instance* nil)
    (cl-cc/runtime:rt-load-core path)
    (expect (rt-core-test-node-name *rt-core-test-instance*) :to-equal "root")
    (expect (rt-core-test-node-next *rt-core-test-instance*) :to-be *rt-core-test-instance*)))

(it-sequential "runtime-core-circular-structure-serialization"
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-cycle"))
        (cell (cons :head nil)))
    (setf (cdr cell) cell
          *rt-core-test-cycle* cell)
    (cl-cc/runtime:rt-save-core path :compression :none)
    (setf *rt-core-test-cycle* nil)
    (cl-cc/runtime:rt-load-core path)
    (expect (car *rt-core-test-cycle*) :to-be :head)
    (expect (cdr *rt-core-test-cycle*) :to-be *rt-core-test-cycle*)))

(it-sequential "runtime-core-loading-with-mmap"
  (%rt-core-register-test-roots)
  (let ((path (%rt-core-test-path "clcc-mmap")))
    (setf *rt-core-test-root* '(:mmap t))
    (cl-cc/runtime:rt-save-core path :compression :none)
    (let ((core (cl-cc/runtime:rt-load-core path)))
      (expect cl-cc/runtime:*saved-core-pathname* :to-equal (pathname path))
      (expect (getf core :lazy-loading) :to-be-truthy)
      (expect (getf core :aslr) :to-equal :offset-relative)
      (expect *rt-core-test-root* :to-equal '(:mmap t)))))
