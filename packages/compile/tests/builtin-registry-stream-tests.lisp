;;;; tests/unit/compile/builtin-registry-stream-tests.lisp
;;;; Continuation of builtin-registry-data-ext-tests.lisp.
;;;; Tests for stream/zero-compare/ternary-custom calling-convention tables.

(in-package :cl-cc/test)

;;; ─── *builtin-zero-compare-entries* ─────────────────────────────────────

(it-sequential "builtin-zero-compare-zerop-plusp-minusp zerop"
  (destructuring-bind (sym expected-ctor) (list 'zerop 'cl-cc::make-vm-num-eq)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-zero-compare-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-zero-compare-zerop-plusp-minusp plusp"
  (destructuring-bind (sym expected-ctor) (list 'plusp 'cl-cc::make-vm-gt)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-zero-compare-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-zero-compare-zerop-plusp-minusp minusp"
  (destructuring-bind (sym expected-ctor) (list 'minusp 'cl-cc::make-vm-lt)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-zero-compare-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-stream-input-opt-entries* ──────────────────────────────────

(it-sequential "builtin-stream-input-opt-representative read-char"
  (destructuring-bind (sym expected-ctor default-handle) (list 'read-char 'cl-cc::make-vm-read-char 0)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-input-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

(it-sequential "builtin-stream-input-opt-representative read-line"
  (destructuring-bind (sym expected-ctor default-handle) (list 'read-line 'cl-cc::make-vm-read-line 0)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-input-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

(it-sequential "builtin-stream-input-opt-representative peek-char"
  (destructuring-bind (sym expected-ctor default-handle) (list 'peek-char 'cl-cc::make-vm-peek-char 0)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-input-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

;;; ─── *builtin-stream-void-opt-entries* ───────────────────────────────────

(it-sequential "builtin-stream-void-opt-representative force-output"
  (destructuring-bind (sym expected-ctor default-handle) (list 'force-output 'cl-cc::make-vm-force-output 1)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-void-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

(it-sequential "builtin-stream-void-opt-representative finish-output"
  (destructuring-bind (sym expected-ctor default-handle) (list 'finish-output 'cl-cc::make-vm-finish-output 1)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-void-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

(it-sequential "builtin-stream-void-opt-representative clear-input"
  (destructuring-bind (sym expected-ctor default-handle) (list 'clear-input 'cl-cc::make-vm-clear-input 0)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-void-opt-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (= default-handle (third entry)) :to-be-truthy))))

;;; ─── *builtin-stream-write-val-entries* ──────────────────────────────────

(it-sequential "builtin-stream-write-val-representative write-char"
  (destructuring-bind (sym expected-ctor value-slot default-handle) (list 'write-char 'cl-cc::make-vm-write-char :char 1)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-write-val-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be value-slot)
    (expect (= default-handle (fourth entry)) :to-be-truthy))))

(it-sequential "builtin-stream-write-val-representative write-byte"
  (destructuring-bind (sym expected-ctor value-slot default-handle) (list 'write-byte 'cl-cc::make-vm-write-byte :byte-val 1)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-write-val-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be value-slot)
    (expect (= default-handle (fourth entry)) :to-be-truthy))))

(it-sequential "builtin-stream-write-val-representative write-line"
  (destructuring-bind (sym expected-ctor value-slot default-handle) (list 'write-line 'cl-cc::make-vm-write-line :str 1)
    (let ((entry (find sym cl-cc/compile::*builtin-stream-write-val-entries* :key #'first)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be value-slot)
    (expect (= default-handle (fourth entry)) :to-be-truthy))))

;;; ─── *builtin-ternary-custom-entries* ───────────────────────────────────

(it-sequential "builtin-ternary-custom-representative acons"
  (destructuring-bind (sym expected-ctor slot1 slot2 slot3 return-style) (list 'acons 'cl-cc/vm:make-vm-acons :key :value :alist :dst)
    (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2)
    (expect (fifth entry) :to-be slot3)
    (expect (sixth entry) :to-be return-style))))

(it-sequential "builtin-ternary-custom-representative subst"
  (destructuring-bind (sym expected-ctor slot1 slot2 slot3 return-style) (list 'subst 'cl-cc/vm:make-vm-subst :new-val :old-val :tree :dst)
    (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2)
    (expect (fifth entry) :to-be slot3)
    (expect (sixth entry) :to-be return-style))))

(it-sequential "builtin-ternary-custom-representative aset"
  (destructuring-bind (sym expected-ctor slot1 slot2 slot3 return-style) (list 'cl-cc/compile::aset 'cl-cc/vm:make-vm-aset :array-reg :index-reg :val-reg :move-third)
    (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2)
    (expect (fifth entry) :to-be slot3)
    (expect (sixth entry) :to-be return-style))))
