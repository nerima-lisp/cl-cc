;;;; tests/unit/compile/builtin-registry-data-ext-tests.lisp
;;;; Unit tests for src/compile/builtin-registry-data-ext.lisp
;;;;
;;;; Validates all 17 extended calling-convention data tables:
;;;; table-query, handle-input, side-effect, void-side-effect, nullary,
;;;; string-trim, handle-effect, binary-custom, binary-move-first,
;;;; binary-void, unary-custom-void, unary-opt-nil, binary-opt-one,
;;;; binary-opt-nil-slot, ternary-opt-nil-custom, binary-synth-zero,
;;;; unary-custom, zero-compare, stream-input-opt, stream-void-opt,
;;;; stream-write-val, ternary-custom.

(in-package :cl-cc/test)

;;; ─── Helper ──────────────────────────────────────────────────────────────

(defun %table-ctor-sym (entry)
  "Extract the constructor symbol from a data table entry.
   Handles both (sym . ctor) cons cells and (sym ctor ...) lists."
  (if (consp (cdr entry))
      (second entry)   ; list form: (sym ctor slot1 ...)
      (cdr entry)))    ; cons form: (sym . ctor)

;;; ─── Table size sanity ───────────────────────────────────────────────────

(it-sequential "builtin-ext-table-non-empty table-query"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-table-query-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty handle-input"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-handle-input-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty side-effect"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-side-effect-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty void-side-effect"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-void-side-effect-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty nullary"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-nullary-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty string-trim"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-string-trim-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty handle-effect"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-handle-effect-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-custom"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-custom-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-move-first"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-move-first-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-void"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-void-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty unary-custom-void"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-unary-custom-void-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty unary-opt-nil"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-unary-opt-nil-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-opt-one"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-opt-one-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-opt-nil-slot"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-opt-nil-slot-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty ternary-opt-nil"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-ternary-opt-nil-custom-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty binary-synth-zero"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-binary-synth-zero-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty unary-custom"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-unary-custom-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty zero-compare"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-zero-compare-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty stream-input-opt"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-stream-input-opt-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty stream-void-opt"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-stream-void-opt-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty stream-write-val"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-stream-write-val-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-ext-table-non-empty ternary-custom"
  (destructuring-bind (table) (list cl-cc/compile::*builtin-ternary-custom-entries*)
    (expect (> (length table) 0) :to-be-truthy)))

(it-sequential "builtin-binary-opt-nil-slot-find-symbol-entry"
  (let ((entry (assoc 'find-symbol cl-cc/compile::*builtin-binary-opt-nil-slot-entries*)))
    (expect entry :to-equal '(find-symbol cl-cc::make-vm-find-symbol :src :pkg))))

(it-sequential "builtin-table-minimum-sizes nullary"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-nullary-entries* 10)
    (expect (>= (length table) min-size) :to-be-truthy)))

(it-sequential "builtin-table-minimum-sizes binary-custom"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-binary-custom-entries* 10)
    (expect (>= (length table) min-size) :to-be-truthy)))

(it-sequential "builtin-table-minimum-sizes ternary-custom"
  (destructuring-bind (table min-size) (list cl-cc/compile::*builtin-ternary-custom-entries* 5)
    (expect (>= (length table) min-size) :to-be-truthy)))

;;; ─── *builtin-string-cmp-entries* (extended case-insensitive family) ──────

(it-sequential "builtin-string-cmp-extended-representative-entries string-equal"
  (destructuring-bind (sym expected-ctor) (list 'string-equal 'cl-cc::make-vm-string-equal)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-cmp-extended-representative-entries string-lessp"
  (destructuring-bind (sym expected-ctor) (list 'string-lessp 'cl-cc::make-vm-string-lessp)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-cmp-extended-representative-entries string-greaterp"
  (destructuring-bind (sym expected-ctor) (list 'string-greaterp 'cl-cc::make-vm-string-greaterp)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-cmp-extended-representative-entries string-not-equal"
  (destructuring-bind (sym expected-ctor) (list 'string-not-equal 'cl-cc::make-vm-string-not-equal)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-cmp-extended-representative-entries string-not-greaterp"
  (destructuring-bind (sym expected-ctor) (list 'string-not-greaterp 'cl-cc::make-vm-string-not-greaterp)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-cmp-extended-representative-entries string-not-lessp"
  (destructuring-bind (sym expected-ctor) (list 'string-not-lessp 'cl-cc::make-vm-string-not-lessp)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-cmp-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-table-query-entries* ──────────────────────────────────────

(it-sequential "builtin-table-query-representative-entries hash-table-count"
  (destructuring-bind (sym expected-ctor) (list 'hash-table-count 'cl-cc/vm:make-vm-hash-table-count)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-table-query-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-table-query-representative-entries hash-table-keys"
  (destructuring-bind (sym expected-ctor) (list 'cl-cc/compile::hash-table-keys 'cl-cc/vm:make-vm-hash-table-keys)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-table-query-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-table-query-representative-entries hash-table-values"
  (destructuring-bind (sym expected-ctor) (list 'cl-cc/vm:hash-table-values 'cl-cc/vm:make-vm-hash-table-values)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-table-query-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-table-query-representative-entries hash-table-test"
  (destructuring-bind (sym expected-ctor) (list 'hash-table-test 'cl-cc/vm:make-vm-hash-table-test)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-table-query-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-handle-input-entries* ─────────────────────────────────────

(it-sequential "builtin-handle-input-representative-entries file-position"
  (destructuring-bind (sym expected-ctor) (list 'file-position 'cl-cc::make-vm-file-position)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-handle-input-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-handle-input-representative-entries file-length"
  (destructuring-bind (sym expected-ctor) (list 'file-length 'cl-cc::make-vm-file-length)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-handle-input-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-handle-input-representative-entries read-byte"
  (destructuring-bind (sym expected-ctor) (list 'read-byte 'cl-cc::make-vm-read-byte)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-handle-input-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-handle-input-representative-entries listen"
  (destructuring-bind (sym expected-ctor) (list 'listen 'cl-cc::make-vm-listen-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-handle-input-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-side-effect-entries* ──────────────────────────────────────

(it-sequential "builtin-side-effect-representative-entries princ"
  (destructuring-bind (sym expected-ctor) (list 'princ 'cl-cc::make-vm-princ)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-side-effect-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-side-effect-representative-entries prin1"
  (destructuring-bind (sym expected-ctor) (list 'prin1 'cl-cc::make-vm-prin1)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-side-effect-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-side-effect-representative-entries print"
  (destructuring-bind (sym expected-ctor) (list 'print 'cl-cc::make-vm-print-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-side-effect-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-void-side-effect-entries* ─────────────────────────────────

(it-sequential "builtin-void-side-effect-entries terpri"
  (destructuring-bind (sym expected-ctor) (list 'terpri 'cl-cc::make-vm-terpri-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-void-side-effect-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-void-side-effect-entries fresh-line"
  (destructuring-bind (sym expected-ctor) (list 'fresh-line 'cl-cc::make-vm-fresh-line-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-void-side-effect-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-nullary-entries* ───────────────────────────────────────────

(it-sequential "builtin-nullary-representative-entries gensym"
  (destructuring-bind (sym expected-ctor) (list 'gensym 'cl-cc::make-vm-gensym-inst)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-nullary-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-nullary-representative-entries get-universal-time"
  (destructuring-bind (sym expected-ctor) (list 'cl-cc/vm:get-universal-time 'cl-cc::make-vm-get-universal-time)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-nullary-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-nullary-representative-entries next-method-p"
  (destructuring-bind (sym expected-ctor) (list 'next-method-p 'cl-cc::make-vm-next-method-p)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-nullary-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-nullary-representative-entries lisp-implementation-type"
  (destructuring-bind (sym expected-ctor) (list 'cl-cc/vm:lisp-implementation-type 'cl-cc::make-vm-lisp-implementation-type)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-nullary-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-string-trim-entries* ──────────────────────────────────────

(it-sequential "builtin-string-trim-all-variants string-trim"
  (destructuring-bind (sym expected-ctor) (list 'string-trim 'cl-cc::make-vm-string-trim)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-trim-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-trim-all-variants string-left-trim"
  (destructuring-bind (sym expected-ctor) (list 'string-left-trim 'cl-cc::make-vm-string-left-trim)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-trim-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-string-trim-all-variants string-right-trim"
  (destructuring-bind (sym expected-ctor) (list 'string-right-trim 'cl-cc::make-vm-string-right-trim)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-string-trim-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-binary-custom-entries* ────────────────────────────────────

(it-sequential "builtin-binary-custom-representative-entries nth"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'nth 'cl-cc::make-vm-nth :index :list)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-custom-representative-entries cons"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'cons 'cl-cc::make-vm-cons :car-src :cdr-src)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-custom-representative-entries hash-cons"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'cl-cc/compile::hash-cons 'cl-cc::make-vm-hash-cons :car-src :cdr-src)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-custom-representative-entries append"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'append 'cl-cc::make-vm-append :src1 :src2)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-custom-representative-entries aref"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'aref 'cl-cc::make-vm-aref :array-reg :index-reg)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

;;; ─── *builtin-binary-move-first-entries* ────────────────────────────────

(it-sequential "builtin-binary-move-first-rplaca-rplacd rplaca"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'rplaca 'cl-cc::make-vm-rplaca :cons :val)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-move-first-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-move-first-rplaca-rplacd rplacd"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'rplacd 'cl-cc::make-vm-rplacd :cons :val)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-move-first-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

;;; ─── *builtin-binary-void-entries* ──────────────────────────────────────

(it-sequential "builtin-binary-void-representative remhash"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'remhash 'cl-cc::make-vm-remhash :key :table)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-void-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

(it-sequential "builtin-binary-void-representative unread-char"
  (destructuring-bind (sym expected-ctor slot1 slot2) (list 'unread-char 'cl-cc::make-vm-unread-char :char :handle)
    (let ((entry (assoc sym cl-cc/compile::*builtin-binary-void-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2))))

;;; ─── *builtin-unary-custom-void-entries* ────────────────────────────────

(it-sequential "builtin-unary-custom-void-representative error"
  (destructuring-bind (sym expected-ctor slot) (list 'error 'cl-cc/vm:make-vm-signal-error :error-reg)
    (let ((entry (assoc sym cl-cc/compile::*builtin-unary-custom-void-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot))))

(it-sequential "builtin-unary-custom-void-representative signal"
  (destructuring-bind (sym expected-ctor slot) (list 'signal 'cl-cc/compile::make-vm-signal :condition-reg)
    (let ((entry (assoc sym cl-cc/compile::*builtin-unary-custom-void-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot))))

(it-sequential "builtin-unary-custom-void-representative clrhash"
  (destructuring-bind (sym expected-ctor slot) (list 'clrhash 'cl-cc/vm:make-vm-clrhash :table)
    (let ((entry (assoc sym cl-cc/compile::*builtin-unary-custom-void-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot))))

;;; ─── *builtin-binary-opt-one-entries* ───────────────────────────────────

(it-sequential "builtin-binary-opt-one-ffloor-family ffloor"
  (destructuring-bind (sym expected-ctor) (list 'ffloor 'cl-cc::make-vm-ffloor)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-binary-opt-one-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-binary-opt-one-ffloor-family fceiling"
  (destructuring-bind (sym expected-ctor) (list 'fceiling 'cl-cc::make-vm-fceiling)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-binary-opt-one-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-binary-opt-one-ffloor-family ftruncate"
  (destructuring-bind (sym expected-ctor) (list 'ftruncate 'cl-cc::make-vm-ftruncate)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-binary-opt-one-entries*)) :to-equal expected-ctor)))

(it-sequential "builtin-binary-opt-one-ffloor-family fround"
  (destructuring-bind (sym expected-ctor) (list 'fround 'cl-cc::make-vm-fround)
    (expect (cdr (assoc sym cl-cc/compile::*builtin-binary-opt-one-entries*)) :to-equal expected-ctor)))

;;; ─── *builtin-ternary-opt-nil-custom-entries* ────────────────────────────

(it-sequential "builtin-ternary-opt-nil-custom-representative get"
  (destructuring-bind (sym expected-ctor slot1 slot2 slot3) (list 'get 'cl-cc::make-vm-symbol-get :sym :indicator :default)
    (let ((entry (assoc sym cl-cc/compile::*builtin-ternary-opt-nil-custom-entries*)))
    (expect entry :to-be-truthy)
    (expect (second entry) :to-equal expected-ctor)
    (expect (third entry) :to-be slot1)
    (expect (fourth entry) :to-be slot2)
    (expect (fifth entry) :to-be slot3))))
