;;;; tests/unit/runtime/heap-trace-tests.lisp — Card Table + Address Predicate Tests
;;;
;;; Tests for src/runtime/heap-trace.lisp:
;;;   - Card table helpers: rt-card-index, rt-card-dirty-p,
;;;     rt-card-mark-dirty, rt-card-clear, rt-card-clear-all
;;;   - Address predicates: rt-young-addr-p, rt-old-addr-p, rt-heap-addr-p
;;;   - Pointer slot resolver: rt-object-pointer-slots

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun %make-trace-heap ()
  "Create a minimal heap for tracing tests.
   young-size=32 → semi-size=16, young-from-base=0, young addresses 0..15.
   old-size=32  → old-base=32, old addresses 32..63, num-cards=ceiling(32/64)=1.
   large-obj-size=old-size → large-obj addresses 64..95."
  (cl-cc/runtime:make-rt-heap :young-size 32 :old-size 32))

(defun %write-obj-header (heap addr size tag)
  "Write an object header at ADDR with SIZE words and TYPE-TAG."
  (cl-cc/runtime:rt-heap-set-header
   heap addr
   (cl-cc/runtime:make-rt-header size tag :gc-bits 0)))

;;; ------------------------------------------------------------
;;; Card Table: rt-card-index
;;; ------------------------------------------------------------

(it-sequential "heap-trace-card-index-at-old-base-is-zero"
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (expect (= 0 (cl-cc/runtime:rt-card-index heap old-base)) :to-be-truthy)))

(it-sequential "heap-trace-card-index-within-first-card-is-zero"
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (expect (= 0 (cl-cc/runtime:rt-card-index heap (+ old-base 10))) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Card Table: mark / clear / dirty-p
;;; ------------------------------------------------------------

(it-sequential "heap-trace-card-mark-clear-lifecycle"
  (let* ((heap     (%make-trace-heap))
         (old-base (cl-cc/runtime:rt-heap-old-base heap)))
    (expect (cl-cc/runtime:rt-card-dirty-p heap old-base) :to-be-falsy)
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (expect (cl-cc/runtime:rt-card-dirty-p heap old-base) :to-be-truthy)
    (cl-cc/runtime:rt-card-clear heap old-base)
    (expect (cl-cc/runtime:rt-card-dirty-p heap old-base) :to-be-falsy)
    (cl-cc/runtime:rt-card-mark-dirty heap old-base)
    (cl-cc/runtime:rt-card-clear-all heap)
    (expect (cl-cc/runtime:rt-card-dirty-p heap old-base) :to-be-falsy)))

;;; ------------------------------------------------------------
;;; Address Predicates
;;; ------------------------------------------------------------

(it-sequential "heap-trace-young-addr-p-cases in-range-0"
  (destructuring-bind (addr expected) (list 0 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-young-addr-p-cases in-range-10"
  (destructuring-bind (addr expected) (list 10 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-young-addr-p-cases in-range-15"
  (destructuring-bind (addr expected) (list 15 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-young-addr-p-cases boundary-16"
  (destructuring-bind (addr expected) (list 16 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-young-addr-p-cases old-base-32"
  (destructuring-bind (addr expected) (list 32 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-young-addr-p-cases negative"
  (destructuring-bind (addr expected) (list -1 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-young-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases below-old-31"
  (destructuring-bind (addr expected) (list 31 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases old-base-32"
  (destructuring-bind (addr expected) (list 32 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases old-mid-40"
  (destructuring-bind (addr expected) (list 40 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases old-end-63"
  (destructuring-bind (addr expected) (list 63 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases beyond-64"
  (destructuring-bind (addr expected) (list 64 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-old-addr-p-cases young-0"
  (destructuring-bind (addr expected) (list 0 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-old-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases young-0"
  (destructuring-bind (addr expected) (list 0 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases young-15"
  (destructuring-bind (addr expected) (list 15 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases gap-16"
  (destructuring-bind (addr expected) (list 16 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases gap-31"
  (destructuring-bind (addr expected) (list 31 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases old-32"
  (destructuring-bind (addr expected) (list 32 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases old-63"
  (destructuring-bind (addr expected) (list 63 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases large-obj-64"
  (destructuring-bind (addr expected) (list 64 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases large-obj-95"
  (destructuring-bind (addr expected) (list 95 t)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

(it-sequential "heap-trace-heap-addr-p-cases beyond-96"
  (destructuring-bind (addr expected) (list 96 nil)
    (let ((heap (%make-trace-heap)))
    (expect (not (not (cl-cc/runtime:rt-heap-addr-p heap addr))) :to-equal expected))))

;;; ------------------------------------------------------------
;;; rt-object-pointer-slots
;;; ------------------------------------------------------------

(it-sequential "heap-trace-pointer-slots-cons-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 1)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '(1 2))))

(it-sequential "heap-trace-pointer-slots-symbol-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 2)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '(1 2 3))))

(it-sequential "heap-trace-pointer-slots-closure-4-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 3)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '(2 3))))

(it-sequential "heap-trace-pointer-slots-closure-2-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 3)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '())))

(it-sequential "heap-trace-pointer-slots-array-4-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 4 5)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '(2 3))))

(it-sequential "heap-trace-pointer-slots-other-3-tag"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 3 7)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal '(1 2))))

(it-sequential "heap-trace-pointer-slots-string-tag-returns-nil"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 5 6)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal nil)))

(it-sequential "heap-trace-pointer-slots-unknown-tag-returns-nil"
  (let ((heap (%make-trace-heap)))
    (%write-obj-header heap 0 2 0)
    (expect (cl-cc/runtime:rt-object-pointer-slots heap 0) :to-equal nil)))
