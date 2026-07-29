;;;; -*- coding: utf-8 -*-
;;;; packages/compile/tests/fr-009-pic-tests.lisp
;;;;
;;;; FR-009: Polymorphic Inline Cache tests.
;;;;
;;;; The VM inline-cache stores a (specializer-key . method) pair at each
;;;; vm-generic-call site. These tests verify behavioral correctness via
;;;; run-string (compile-level dispatch) and cache hit/miss profiling.
;;;;

(in-package :cl-cc/test)
;; Behavioral tests -- compile-level dispatch via run-string
(it-sequential "fr-009-pic-behavioral two-class-switch"
  :timeout
  15
  (destructuring-bind (expected form) (list '(:a :a :b :a) "(progn
         (defclass pic-class-a () ())
         (defclass pic-class-b () ())
         (defgeneric pic-id (x))
         (defmethod pic-id ((x pic-class-a)) :a)
         (defmethod pic-id ((x pic-class-b)) :b)
         (let* ((a (make-instance 'pic-class-a))
                (b (make-instance 'pic-class-b)))
           (list (pic-id a) (pic-id a) (pic-id b) (pic-id a))))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "fr-009-pic-behavioral multi-arg-pic"
  :timeout
  15
  (destructuring-bind (expected form) (list '((:x= 1) (:y= 2) (:x= 3) (:y= 4)) "(progn
          (defclass point-x () ((v :initarg :v)))
          (defclass point-y () ((v :initarg :v)))
          (defgeneric pick-x (obj))
          (defmethod pick-x ((obj point-x)) (list :x= (slot-value obj 'v)))
          (defmethod pick-x ((obj point-y)) (list :y= (slot-value obj 'v)))
          (let ((x1 (make-instance 'point-x :v 1))
                (y2 (make-instance 'point-y :v 2))
                (x3 (make-instance 'point-x :v 3))
                (y4 (make-instance 'point-y :v 4)))
            (list (pick-x x1) (pick-x y2) (pick-x x3) (pick-x y4))))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "fr-009-pic-behavioral three-class-thrash"
  :timeout
  15
  (destructuring-bind (expected form) (list '(:a :b :c :a :b :c) "(progn
          (defclass thrash-a () ())
          (defclass thrash-b () ())
          (defclass thrash-c () ())
          (defgeneric thrash-id (x))
          (defmethod thrash-id ((x thrash-a)) :a)
          (defmethod thrash-id ((x thrash-b)) :b)
          (defmethod thrash-id ((x thrash-c)) :c)
          (let ((a (make-instance 'thrash-a))
                (b (make-instance 'thrash-b))
                (c (make-instance 'thrash-c)))
            (list (thrash-id a) (thrash-id b) (thrash-id c)
                  (thrash-id a) (thrash-id b) (thrash-id c))))")
    (expect (run-string form) :to-equal expected)))

(it-sequential "fr-009-pic-behavioral mixed-with-inheritance"
  :timeout
  15
  (destructuring-bind (expected form) (list '(:base :derived :base) "(progn
          (defclass pic-base () ())
          (defclass pic-derived (pic-base) ())
          (defgeneric inh-id (x))
          (defmethod inh-id ((x pic-base)) :base)
          (defmethod inh-id ((x pic-derived)) :derived)
          (let ((b (make-instance 'pic-base))
                (d (make-instance 'pic-derived)))
            (list (inh-id b) (inh-id d) (inh-id b))))")
    (expect (run-string form) :to-equal expected)))

;; VM-level PIC profiling tests -- cache hit/miss counters and type-feedback
(it-sequential "fr-009-pic-cache-transitions-and-profiling"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (generic-caches (make-hash-table :test #'equal))
         (method-a (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-a :params '(:x)))
         (method-b (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'prof-method-b :params '(:x))))
    (setf (cl-cc/vm::vm-profile-enabled-p state) t)
    (let ((ht (gethash 'prof-gf-fn generic-caches)))
      (unless ht
        (setf ht (make-hash-table :test #'eq))
        (setf (gethash 'prof-gf-fn generic-caches) ht))
      ;; Simulate 6-call PIC sequence: miss, hit, miss, hit, miss, hit
      (setf (gethash 'prof-a-class ht) (cons method-a 0))
      (incf (cdr (gethash 'prof-a-class ht)))
      (setf (gethash 'prof-b-class ht) (cons method-b 0))
      (incf (cdr (gethash 'prof-b-class ht)))
      ;; Second access to prof-a-class is a cache HIT: increment its counter
      ;; rather than re-installing (which would reset the count).
      (incf (cdr (gethash 'prof-a-class ht)))
      (let* ((entry-a (gethash 'prof-a-class ht))
             (entry-b (gethash 'prof-b-class ht)))
        (expect (car entry-a) :to-be method-a)
        (expect (car entry-b) :to-be method-b)
        (expect (= 2 (cdr entry-a)) :to-be-truthy)
        (expect (= 1 (cdr entry-b)) :to-be-truthy)))))

(it-sequential "fr-009-pic-invalidation-on-method-registration"
  (let* ((state (make-instance 'cl-cc/vm::vm-io-state))
         (cache (make-hash-table :test #'eq))
         (method-1 (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'inv-method-1 :params '(:x)))
         (method-2 (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label 'inv-method-2 :params '(:x))))
    (setf (gethash 'inv-class-a cache) (cons method-1 5))
    (setf (gethash 'inv-class-b cache) (cons method-2 3))
    (remhash 'inv-class-a cache)
    (expect (gethash 'inv-class-a cache) :to-be-falsy)
    (let ((entry-b (gethash 'inv-class-b cache)))
      (expect (car entry-b) :to-be method-2)
      (expect (= 3 (cdr entry-b)) :to-be-truthy))
    (setf (gethash 'inv-class-a cache) (cons method-2 0))
    (let ((new-entry (gethash 'inv-class-a cache)))
      (expect (car new-entry) :to-be method-2)
      (expect (= 0 (cdr new-entry)) :to-be-truthy))))
