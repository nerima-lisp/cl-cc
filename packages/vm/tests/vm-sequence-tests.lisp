;;;; packages/vm/tests/vm-sequence-tests.lisp
;;;;
;;;; Dedicated unit tests for vm-sequence.lisp:
;;;;   - define-builtin-sequence-methods macro expansion sanity
;;;;   - copy-instance: hash-table path, missing __class__ error, standard-object path
;;;;   - copy-structure: all dispatch branches
;;;;   - deep-copy: cycle detection via seen table, function/character immediates
;;;;   - sequence-protocol-p: true/false cases

(in-package :cl-cc/test)



;;; ── 1. sequence-protocol-p ────────────────────────────────────────────────

(it-sequential "sequence-protocol-p-builtin-types-true list-nonempty"
  (destructuring-bind (object) (list (list 1 2 3))
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-truthy)))

(it-sequential "sequence-protocol-p-builtin-types-true list-empty"
  (destructuring-bind (object) (list nil)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-truthy)))

(it-sequential "sequence-protocol-p-builtin-types-true vector"
  (destructuring-bind (object) (list #(10 20))
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-truthy)))

(it-sequential "sequence-protocol-p-builtin-types-true bit-vector"
  (destructuring-bind (object) (list #*1010)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-truthy)))

(it-sequential "sequence-protocol-p-builtin-types-true string"
  (destructuring-bind (object) (list "hello")
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-truthy)))

(it-sequential "sequence-protocol-p-non-sequences-false integer"
  (destructuring-bind (object) (list 42)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

(it-sequential "sequence-protocol-p-non-sequences-false float"
  (destructuring-bind (object) (list 3.14)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

(it-sequential "sequence-protocol-p-non-sequences-false symbol"
  (destructuring-bind (object) (list 'foo)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

(it-sequential "sequence-protocol-p-non-sequences-false keyword"
  (destructuring-bind (object) (list :bar)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

(it-sequential "sequence-protocol-p-non-sequences-false character"
  (destructuring-bind (object) (list #\a)
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

(it-sequential "sequence-protocol-p-non-sequences-false hash-table"
  (destructuring-bind (object) (list (make-hash-table))
    (expect (cl-cc/vm::sequence-protocol-p object) :to-be-falsy)))

;;; ── 2. length / elt / (setf elt) / subseq for each built-in type ─────────

(it-sequential "length-by-type list-3"
  (destructuring-bind (seq expected) (list (list 1 2 3) 3)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "length-by-type list-0"
  (destructuring-bind (seq expected) (list nil 0)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "length-by-type vector-2"
  (destructuring-bind (seq expected) (list #(10 20) 2)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "length-by-type bit-vector-4"
  (destructuring-bind (seq expected) (list #*1010 4)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "length-by-type string-5"
  (destructuring-bind (seq expected) (list "hello" 5)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "length-by-type string-0"
  (destructuring-bind (seq expected) (list "" 0)
    (expect (= expected (cl-cc/vm::length seq)) :to-be-truthy)))

(it-sequential "elt-by-type list-0"
  (destructuring-bind (seq index expected) (list (list :a :b :c) 0 :a)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "elt-by-type list-2"
  (destructuring-bind (seq index expected) (list (list :a :b :c) 2 :c)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "elt-by-type vector-1"
  (destructuring-bind (seq index expected) (list #(10 20 30) 1 20)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "elt-by-type bit-vector-2"
  (destructuring-bind (seq index expected) (list #*101 2 1)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "elt-by-type string-0"
  (destructuring-bind (seq index expected) (list "xyz" 0 #\x)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "elt-by-type string-2"
  (destructuring-bind (seq index expected) (list "xyz" 2 #\z)
    (expect (cl-cc/vm::elt seq index) :to-equal expected)))

(it-sequential "setf-elt-list-mutates"
  (let ((lst (list 1 2 3)))
    (setf (cl-cc/vm::elt lst 1) 99)
    (expect (= 99 (second lst)) :to-be-truthy)
    (expect (= 1 (first lst)) :to-be-truthy)
    (expect (= 3 (third lst)) :to-be-truthy)))

(it-sequential "setf-elt-vector-mutates"
  (let ((vec (vector 10 20 30)))
    (setf (cl-cc/vm::elt vec 0) 77)
    (expect (= 77 (cl:aref vec 0)) :to-be-truthy)
    (expect (= 20 (cl:aref vec 1)) :to-be-truthy)))

(it-sequential "setf-elt-bit-vector-mutates"
  (let ((vec (make-array 3 :element-type 'bit :initial-contents '(1 0 1))))
    (setf (cl-cc/vm::elt vec 1) 1)
    (expect (= 1 (cl:aref vec 0)) :to-be-truthy)
    (expect (= 1 (cl:aref vec 1)) :to-be-truthy)
    (expect (= 1 (cl:aref vec 2)) :to-be-truthy)))

(it-sequential "setf-elt-string-mutates"
  (let ((str (copy-seq "abc")))
    (setf (cl-cc/vm::elt str 2) #\Z)
    (expect (cl:char str 2) :to-equal #\Z)
    (expect (cl:char str 0) :to-equal #\a)))

(it-sequential "subseq-by-type list-1-3"
  (destructuring-bind (seq start end expected) (list (list :a :b :c :d) 1 3 (list :b :c))
    (expect (equalp expected (cl-cc/vm::subseq seq start end)) :to-be-truthy)))

(it-sequential "subseq-by-type vector-0-2"
  (destructuring-bind (seq start end expected) (list #(10 20 30) 0 2 #(10 20))
    (expect (equalp expected (cl-cc/vm::subseq seq start end)) :to-be-truthy)))

(it-sequential "subseq-by-type string-1-3"
  (destructuring-bind (seq start end expected) (list "abcd" 1 3 "bc")
    (expect (equalp expected (cl-cc/vm::subseq seq start end)) :to-be-truthy)))

(it-sequential "subseq-bit-vector-preserves-bit-vector"
  (let ((result (cl-cc/vm::subseq #*10110 1 4)))
    (expect (typep result 'bit-vector) :to-be-truthy)
    (expect (equalp #*011 result) :to-be-truthy)))

;;; ── 3. make-sequence-like ─────────────────────────────────────────────────

(it-sequential "make-sequence-like-list-default-nil"
  (let ((result (cl-cc/vm::make-sequence-like '(1 2) 3)))
    (expect (listp result) :to-be-truthy)
    (expect (= 3 (cl:length result)) :to-be-truthy)
    (expect (every #'null result) :to-be-truthy)))

(it-sequential "make-sequence-like-string-default-space"
  (let ((result (cl-cc/vm::make-sequence-like "hi" 4)))
    (expect (stringp result) :to-be-truthy)
    (expect (= 4 (cl:length result)) :to-be-truthy)
    (expect result :to-equal "    ")))

(it-sequential "make-sequence-like-vector-explicit-element"
  (let ((result (cl-cc/vm::make-sequence-like #(0) 3 :initial-element :x)))
    (expect (vectorp result) :to-be-truthy)
    (expect (= 3 (cl:length result)) :to-be-truthy)
    (expect (every (lambda (element) (eq :x element)) result) :to-be-truthy)))

(it-sequential "make-sequence-like-bit-vector-preserves-element-type"
  (let ((defaulted (cl-cc/vm::make-sequence-like #*1 3))
        (ones (cl-cc/vm::make-sequence-like #*0 3 :initial-element 1)))
    (expect (typep defaulted 'bit-vector) :to-be-truthy)
    (expect (equalp #*000 defaulted) :to-be-truthy)
    (expect (typep ones 'bit-vector) :to-be-truthy)
    (expect (equalp #*111 ones) :to-be-truthy)))

;;; ── 4. copy-instance paths ────────────────────────────────────────────────

(it-sequential "copy-instance-hash-table-shallow-copy"
  (let* ((original (make-hash-table :test #'equal))
         (copy nil))
    (setf (gethash :__class__ original) :my-class
          (gethash :slot-a   original) 42)
    (setf copy (cl-cc/vm::copy-instance original))
    (expect (eq original copy) :to-be-falsy)
    (expect (gethash :__class__ copy) :to-be :my-class)
    (expect (= 42 (gethash :slot-a copy)) :to-be-truthy)
    ;; shallow: mutation to original does not affect copy
    (setf (gethash :slot-a original) 999)
    (expect (= 42 (gethash :slot-a copy)) :to-be-truthy)))

(it-sequential "copy-instance-missing-class-key-signals-error"
  (let ((bare (make-hash-table)))
    (setf (gethash :slot-x bare) 1)
    (signals error (cl-cc/vm::copy-instance bare))))

(it-sequential "copy-instance-vector-with-hash-header-uses-copy-seq"
  (let* ((header (make-hash-table))
         (original (vector header 10 20))
         (copy (cl-cc/vm::copy-instance original)))
    (expect (vectorp copy) :to-be-truthy)
    (expect (eq original copy) :to-be-falsy)
    (expect (= 10 (cl:aref copy 1)) :to-be-truthy)))

(it-sequential "copy-instance-standard-object-is-shallow"
  (let* ((object (make-instance 'cl-cc/vm::sequence))
         (copy nil))
    ;; sequence has no slots — just verify it round-trips without error
    (setf copy (cl-cc/vm::copy-instance object))
    (expect (eq object copy) :to-be-falsy)
    (expect (typep copy 'cl-cc/vm::sequence) :to-be-truthy)))

;;; ── 5. copy-structure dispatch branches ──────────────────────────────────

(it-sequential "copy-structure-list-is-fresh-and-equal"
  (let* ((original (list :a :b :c))
         (copy (cl-cc/vm::copy-structure original)))
    (expect copy :to-equal original)
    (expect (eq original copy) :to-be-falsy)))

(it-sequential "copy-structure-vector-is-fresh-and-equal"
  (let* ((original (vector 1 2 3))
         (copy (cl-cc/vm::copy-structure original)))
    (expect (equalp original copy) :to-be-truthy)
    (expect (eq original copy) :to-be-falsy)))

(it-sequential "copy-structure-string-is-fresh-and-equal"
  (let* ((original "test-string")
         (copy (cl-cc/vm::copy-structure original)))
    (expect copy :to-equal original)
    (expect (eq original copy) :to-be-falsy)))

(it-sequential "copy-structure-standard-object-delegates-to-copy-instance"
  (let* ((object (make-instance 'cl-cc/vm::sequence))
         (copy (cl-cc/vm::copy-structure object)))
    (expect (eq object copy) :to-be-falsy)
    (expect (typep copy 'cl-cc/vm::sequence) :to-be-truthy)))

(it-sequential "copy-structure-hash-table-vm-instance-delegates-to-copy-instance"
  (let* ((original (make-hash-table :test #'equal)))
    (setf (gethash :__class__ original) :point
          (gethash :x         original) 3
          (gethash :y         original) 4)
    (let ((copy (cl-cc/vm::copy-structure original)))
      (expect (eq original copy) :to-be-falsy)
      (expect (= 3 (gethash :x copy)) :to-be-truthy)
      (expect (= 4 (gethash :y copy)) :to-be-truthy))))

(it-sequential "copy-structure-unsupported-signals-error"
  (signals error (cl-cc/vm::copy-structure 99)))

;;; ── 6. deep-copy: immediate values and cycle detection ───────────────────

(it-sequential "deep-copy-immediates-return-same-object nil"
  (destructuring-bind (value) (list nil)
    (expect (cl-cc/vm::deep-copy value) :to-be value)))

(it-sequential "deep-copy-immediates-return-same-object integer"
  (destructuring-bind (value) (list 42)
    (expect (cl-cc/vm::deep-copy value) :to-be value)))

(it-sequential "deep-copy-immediates-return-same-object float"
  (destructuring-bind (value) (list 1.5)
    (expect (cl-cc/vm::deep-copy value) :to-be value)))

(it-sequential "deep-copy-immediates-return-same-object symbol"
  (destructuring-bind (value) (list 'foo)
    (expect (cl-cc/vm::deep-copy value) :to-be value)))

(it-sequential "deep-copy-immediates-return-same-object character"
  (destructuring-bind (value) (list #\a)
    (expect (cl-cc/vm::deep-copy value) :to-be value)))

(it-sequential "deep-copy-function-returns-same-object"
  (let ((fn #'cl:+))
    (expect (cl-cc/vm::deep-copy fn) :to-be fn)))

(it-sequential "deep-copy-cycle-in-cons-is-preserved"
  (let* ((pair (cons :head nil))
         (seen (make-hash-table :test #'eq)))
    ;; register pair in seen before calling deep-copy to simulate a cycle break
    (setf (gethash pair seen) pair)
    ;; deep-copy should find it in seen and return the registered value
    (expect (cl-cc/vm::deep-copy pair seen) :to-be pair)))

(it-sequential "deep-copy-hash-table-cycle-via-seen"
  (let* ((original (make-hash-table))
         (sentinel :already-copied)
         (seen (make-hash-table :test #'eq)))
    (setf (gethash original seen) sentinel)
    (expect (cl-cc/vm::deep-copy original seen) :to-be sentinel)))

(it-sequential "deep-copy-nested-hash-table-copies-recursively"
  (let* ((inner (list 10 20))
         (outer (make-hash-table :test #'equal)))
    (setf (gethash "key" outer) inner)
    (let ((copy (cl-cc/vm::deep-copy outer)))
      (expect (eq outer copy) :to-be-falsy)
      (expect (eq inner (gethash "key" copy)) :to-be-falsy)
      (expect (gethash "key" copy) :to-equal inner))))

(it-sequential "deep-copy-string-produces-fresh-equal-copy"
  (let* ((original "mutable")
         (copy (cl-cc/vm::deep-copy original)))
    (expect copy :to-equal original)
    (expect (eq original copy) :to-be-falsy)))

(it-sequential "deep-copy-vector-elements-are-recursively-copied"
  (let* ((inner (list :a))
         (original (vector inner 99))
         (copy (cl-cc/vm::deep-copy original)))
    (expect (eq original copy) :to-be-falsy)
    (expect (eq inner (cl:aref copy 0)) :to-be-falsy)
    (expect (cl:aref copy 0) :to-equal inner)
    (expect (= 99 (cl:aref copy 1)) :to-be-truthy)))
