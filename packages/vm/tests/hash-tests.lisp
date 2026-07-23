;;;; tests/unit/vm/hash-tests.lisp — VM Hash Table Operations Unit Tests
;;;;
;;;; Tests for hash table instruction execution (create, get, set, remove,
;;;; count, keys, values, test, clear, predicate) via the VM.

(in-package :cl-cc/test)



;;; ─── resolve-hash-test ────────────────────────────────────────────────────

(it-sequential "resolve-hash-test-cases eq"
  (destructuring-bind (test-sym expected-designator) (list 'eq 'eq)
    (expect (cl-cc/vm::resolve-hash-test test-sym) :to-equal expected-designator)))

(it-sequential "resolve-hash-test-cases eql"
  (destructuring-bind (test-sym expected-designator) (list 'eql 'eql)
    (expect (cl-cc/vm::resolve-hash-test test-sym) :to-equal expected-designator)))

(it-sequential "resolve-hash-test-cases equal"
  (destructuring-bind (test-sym expected-designator) (list 'equal 'equal)
    (expect (cl-cc/vm::resolve-hash-test test-sym) :to-equal expected-designator)))

(it-sequential "resolve-hash-test-cases equalp"
  (destructuring-bind (test-sym expected-designator) (list 'equalp 'equalp)
    (expect (cl-cc/vm::resolve-hash-test test-sym) :to-equal expected-designator)))

(it-sequential "resolve-hash-test-cases nil-defaults-eql"
  (destructuring-bind (test-sym expected-designator) (list nil 'eql)
    (expect (cl-cc/vm::resolve-hash-test test-sym) :to-equal expected-designator)))

(it-sequential "resolve-hash-test-unknown-errors"
  (expect (handler-case
       (progn (cl-cc/vm::resolve-hash-test 'bogus) nil)
     (error () t)) :to-be-truthy))

;;; ─── Hash Table Create ────────────────────────────────────────────────────

(it-sequential "make-hash-table-default-test"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (let ((obj (cl-cc/vm::vm-reg-get state :R0)))
      (expect (typep obj 'cl-cc/vm::vm-hash-table-object) :to-be-truthy))))

(it-sequential "make-hash-table-size-and-rehash-options"
  (let ((state (make-test-vm)))
    (cl-cc/vm::vm-reg-set state :SIZE 32)
    (cl-cc/vm::vm-reg-set state :REHASH-SIZE 2)
    (cl-cc/vm::vm-reg-set state :REHASH-THRESHOLD 0.75)
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0
                                            :size :SIZE
                                            :rehash-size :REHASH-SIZE
                                            :rehash-threshold :REHASH-THRESHOLD)
             state)
    (vm-exec (cl-cc:make-vm-hash-table-size :dst :R1 :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-rehash-size :dst :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-rehash-threshold :dst :R3 :table :R0) state)
    (expect (>= (cl-cc/vm::vm-reg-get state :R1) 32) :to-be-truthy)
    (expect (cl-cc/vm::vm-reg-get state :R2) :to-equal 2)
    (expect (= 0.75 (cl-cc/vm::vm-reg-get state :R3)) :to-be-truthy)))

;;; ─── Hash Table Set/Get Round-Trip ────────────────────────────────────────

(it-sequential "sethash-gethash-roundtrip"
  (let ((state (make-test-vm)))
    ;; Create table in :R0
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    ;; Put key=42 in :R1, value=99 in :R2
    (cl-cc/vm::vm-reg-set state :R1 42)
    (cl-cc/vm::vm-reg-set state :R2 99)
    ;; sethash :R1 :R2 :R0
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    ;; gethash :R3 :R1 :R0
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 99)))

(it-sequential "gethash-missing-key"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'nonexistent)
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal nil)))

(it-sequential "specialized-gethash-roundtrip eq"
  (destructuring-bind (ctor test-sym key) (list #'cl-cc:make-vm-gethash-eq 'eq 'key)
    (let ((state (make-test-vm)))
    (cl-cc/vm::vm-reg-set state :TEST test-sym)
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test :TEST) state)
    (cl-cc/vm::vm-reg-set state :R1 key)
    (cl-cc/vm::vm-reg-set state :R2 99)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (funcall ctor :dst :R3 :found-dst :R4 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 99)
    (expect (cl-cc/vm::vm-reg-get state :R4) :to-equal 1)
    (expect (cl-cc/vm::vm-values-list state) :to-equal (list 99 1)))))

(it-sequential "specialized-gethash-roundtrip eql"
  (destructuring-bind (ctor test-sym key) (list #'cl-cc:make-vm-gethash-eql 'eql 42)
    (let ((state (make-test-vm)))
    (cl-cc/vm::vm-reg-set state :TEST test-sym)
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test :TEST) state)
    (cl-cc/vm::vm-reg-set state :R1 key)
    (cl-cc/vm::vm-reg-set state :R2 99)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (funcall ctor :dst :R3 :found-dst :R4 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 99)
    (expect (cl-cc/vm::vm-reg-get state :R4) :to-equal 1)
    (expect (cl-cc/vm::vm-values-list state) :to-equal (list 99 1)))))

(it-sequential "specialized-gethash-roundtrip equal"
  (destructuring-bind (ctor test-sym key) (list #'cl-cc:make-vm-gethash-equal 'equal "key")
    (let ((state (make-test-vm)))
    (cl-cc/vm::vm-reg-set state :TEST test-sym)
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test :TEST) state)
    (cl-cc/vm::vm-reg-set state :R1 key)
    (cl-cc/vm::vm-reg-set state :R2 99)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (funcall ctor :dst :R3 :found-dst :R4 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 99)
    (expect (cl-cc/vm::vm-reg-get state :R4) :to-equal 1)
    (expect (cl-cc/vm::vm-values-list state) :to-equal (list 99 1)))))

;;; ─── Hash Table Remove ────────────────────────────────────────────────────

(it-sequential "remhash-removes-entry"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'key)
    (cl-cc/vm::vm-reg-set state :R2 'val)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-remhash :key :R1 :table :R0) state)
    (vm-exec (cl-cc:make-vm-gethash :dst :R3 :key :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal nil)))

;;; ─── Hash Table Count ─────────────────────────────────────────────────────

(it-sequential "hash-table-count-behavior"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R1) :to-equal 0))
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'a)
    (cl-cc/vm::vm-reg-set state :R2 1)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (cl-cc/vm::vm-reg-set state :R1 'b)
    (cl-cc/vm::vm-reg-set state :R2 2)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R3 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 2)))

;;; ─── Hash Table Clear ─────────────────────────────────────────────────────

(it-sequential "clrhash-empties-table"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'key)
    (cl-cc/vm::vm-reg-set state :R2 'val)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (vm-exec (cl-cc:make-vm-clrhash :table :R0) state)
    (vm-exec (cl-cc:make-vm-hash-table-count :dst :R3 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R3) :to-equal 0)))

;;; ─── Hash Table Predicate ─────────────────────────────────────────────────

(it-sequential "hash-table-p true"
  (destructuring-bind (is-ht) (list t)
    (let ((state (make-test-vm)))
    (if is-ht
        (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
        (cl-cc/vm::vm-reg-set state :R0 42))
    (vm-exec (cl-cc:make-vm-hash-table-p :dst :R1 :src :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R1) :to-equal (if is-ht 1 0)))))

(it-sequential "hash-table-p false"
  (destructuring-bind (is-ht) (list nil)
    (let ((state (make-test-vm)))
    (if is-ht
        (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
        (cl-cc/vm::vm-reg-set state :R0 42))
    (vm-exec (cl-cc:make-vm-hash-table-p :dst :R1 :src :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R1) :to-equal (if is-ht 1 0)))))

;;; ─── Hash Table Keys / Values ─────────────────────────────────────────────

(it-sequential "hash-table-keys-and-values"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (cl-cc/vm::vm-reg-set state :R1 'x) (cl-cc/vm::vm-reg-set state :R2 10)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    (cl-cc/vm::vm-reg-set state :R1 'y) (cl-cc/vm::vm-reg-set state :R2 20)
    (vm-exec (cl-cc:make-vm-sethash :key :R1 :value :R2 :table :R0) state)
    ;; keys
    (vm-exec (cl-cc:make-vm-hash-table-keys :dst :R3 :table :R0) state)
    (let ((keys (cl-cc/vm::vm-reg-get state :R3)))
      (expect (length keys) :to-equal 2)
      (expect (member 'x keys) :to-be-truthy)
      (expect (member 'y keys) :to-be-truthy))
    ;; values
    (vm-exec (cl-cc:make-vm-hash-table-values :dst :R4 :table :R0) state)
    (let ((vals (cl-cc/vm::vm-reg-get state :R4)))
      (expect (length vals) :to-equal 2)
      (expect (member 10 vals) :to-be-truthy)
      (expect (member 20 vals) :to-be-truthy))))

;;; ─── Hash Table Test ──────────────────────────────────────────────────────

(it-sequential "hash-table-test-returns-symbol"
  (let ((state (make-test-vm)))
    (vm-exec (cl-cc:make-vm-make-hash-table :dst :R0 :test nil) state)
    (vm-exec (cl-cc:make-vm-hash-table-test :dst :R1 :table :R0) state)
    (expect (cl-cc/vm::vm-reg-get state :R1) :to-equal 'eql)))

(it-sequential "hash-lock-elision-wrapper-falls-back-after-abort"
  (let* ((table (make-hash-table :test 'eql))
         (obj (make-instance 'cl-cc/vm::vm-hash-table-object
                             :table table
                             :lock #+sb-thread (sb-thread:make-mutex :name "vm-hash-test-lock")
                                   #-sb-thread nil))
         (attempts 0)
         (cl-cc/vm::*vm-hash-enable-lock-elision-p* t)
         (cl-cc/vm::*vm-hash-htm-supported-p* t)
         (cl-cc/vm::*vm-hash-low-contention-p* t))
    (cl-cc/vm::vm-hash-with-lock-fallback
     obj
     (lambda ()
       (incf attempts)
       (when (= attempts 1)
         (error "simulated-htm-abort"))
       (setf (gethash 'k table) 99)))
    (expect (= 2 attempts) :to-be-truthy)
    (expect (= 1 (cl-cc/vm::vm-hash-table-htm-abort-count obj)) :to-be-truthy)
    (expect (gethash 'k table) :to-equal 99)))

(it-sequential "hash-lock-elision-disabled-after-abort-threshold"
  (let* ((table (make-hash-table :test 'eql))
         (obj (make-instance 'cl-cc/vm::vm-hash-table-object
                             :table table
                             :lock #+sb-thread (sb-thread:make-mutex :name "vm-hash-test-lock")
                                   #-sb-thread nil))
         (cl-cc/vm::*vm-hash-enable-lock-elision-p* t)
         (cl-cc/vm::*vm-hash-htm-supported-p* t)
         (cl-cc/vm::*vm-hash-low-contention-p* t)
         (cl-cc/vm::*vm-hash-htm-abort-threshold* 2))
    (setf (cl-cc/vm::vm-hash-table-htm-abort-count obj) 2)
    (expect (cl-cc/vm::vm-hash-htm-eligible-p obj) :to-be-falsy)))
