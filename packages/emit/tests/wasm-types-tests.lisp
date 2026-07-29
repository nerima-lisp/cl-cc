;;;; tests/unit/emit/wasm-types-tests.lisp — WASM Type Constants Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; Section IDs, value type encodings, GC proposal types, heap types,
;;;; type definition encodings, opcodes, named type indices, and
;;;; uniqueness invariants.

(in-package :cl-cc/test)



;;; ─── Section IDs ───────────────────────────────────────────────────────────

(it-sequential "wasm-section-ids-range custom"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-custom+ 0)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range type"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-type+ 1)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range import"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-import+ 2)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range function"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-function+ 3)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range table"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-table+ 4)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range memory"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-memory+ 5)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range global"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-global+ 6)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range export"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-export+ 7)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range start"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-start+ 8)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range element"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-element+ 9)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range code"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-code+ 10)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range data"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-data+ 11)
    (expect const :to-equal expected)))

(it-sequential "wasm-section-ids-range data-count"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-section-data-count+ 12)
    (expect const :to-equal expected)))

;;; ─── Primitive value types ─────────────────────────────────────────────────

(it-sequential "wasm-primitive-value-types i32"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32+ #x7f)
    (expect const :to-equal expected)))

(it-sequential "wasm-primitive-value-types i64"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64+ #x7e)
    (expect const :to-equal expected)))

(it-sequential "wasm-primitive-value-types f32"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f32+ #x7d)
    (expect const :to-equal expected)))

(it-sequential "wasm-primitive-value-types f64"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64+ #x7c)
    (expect const :to-equal expected)))

(it-sequential "wasm-primitive-value-types funcref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-funcref+ #x70)
    (expect const :to-equal expected)))

(it-sequential "wasm-primitive-value-types externref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-externref+ #x6f)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types anyref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-anyref+ #x6e)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types eqref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-eqref+ #x6d)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types i31ref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i31ref+ #x6c)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types structref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-structref+ #x6b)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types arrayref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-arrayref+ #x6a)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-reference-types nullref"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-nullref+ #x69)
    (expect const :to-equal expected)))

;;; ─── Heap types mirror reference types ─────────────────────────────────────

(it-sequential "wasm-heap-types-mirror-ref-types func"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-funcref+ cl-cc/codegen::+heap-func+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types extern"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-externref+ cl-cc/codegen::+heap-extern+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types any"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-anyref+ cl-cc/codegen::+heap-any+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types eq"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-eqref+ cl-cc/codegen::+heap-eq+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types i31"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-i31ref+ cl-cc/codegen::+heap-i31+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types struct"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-structref+ cl-cc/codegen::+heap-struct+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types array"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-arrayref+ cl-cc/codegen::+heap-array+)
    (expect heap-type :to-equal ref-type)))

(it-sequential "wasm-heap-types-mirror-ref-types none"
  (destructuring-bind (ref-type heap-type) (list cl-cc/codegen::+wasm-nullref+ cl-cc/codegen::+heap-none+)
    (expect heap-type :to-equal ref-type)))

;;; ─── GC type definition encodings ──────────────────────────────────────────

(it-sequential "wasm-type-definition-encodings func"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-func+ #x60)
    (expect const :to-equal expected)))

(it-sequential "wasm-type-definition-encodings struct"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-struct+ #x5f)
    (expect const :to-equal expected)))

(it-sequential "wasm-type-definition-encodings array"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-array+ #x5e)
    (expect const :to-equal expected)))

(it-sequential "wasm-type-definition-encodings sub"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-sub+ #x50)
    (expect const :to-equal expected)))

(it-sequential "wasm-type-definition-encodings sub-final"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-sub-final+ #x4f)
    (expect const :to-equal expected)))

(it-sequential "wasm-type-definition-encodings rec"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-type-rec+ #x4e)
    (expect const :to-equal expected)))

;;; ─── Mutability ────────────────────────────────────────────────────────────

(it-sequential "wasm-mutability-values immutable"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-immutable+ 0)
    (expect const :to-equal expected)))

(it-sequential "wasm-mutability-values mutable"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-mutable+ 1)
    (expect const :to-equal expected)))

;;; ─── Export/import descriptors ─────────────────────────────────────────────

(it-sequential "wasm-export-descriptor-values func"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-export-func+ 0)
    (expect const :to-equal expected)))

(it-sequential "wasm-export-descriptor-values table"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-export-table+ 1)
    (expect const :to-equal expected)))

(it-sequential "wasm-export-descriptor-values memory"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-export-memory+ 2)
    (expect const :to-equal expected)))

(it-sequential "wasm-export-descriptor-values global"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-export-global+ 3)
    (expect const :to-equal expected)))

(it-sequential "wasm-import-export-descriptors-equal func"
  (destructuring-bind (export-val import-val) (list cl-cc/codegen::+wasm-export-func+ cl-cc/codegen::+wasm-import-func+)
    (expect import-val :to-equal export-val)))

(it-sequential "wasm-import-export-descriptors-equal table"
  (destructuring-bind (export-val import-val) (list cl-cc/codegen::+wasm-export-table+ cl-cc/codegen::+wasm-import-table+)
    (expect import-val :to-equal export-val)))

(it-sequential "wasm-import-export-descriptors-equal memory"
  (destructuring-bind (export-val import-val) (list cl-cc/codegen::+wasm-export-memory+ cl-cc/codegen::+wasm-import-memory+)
    (expect import-val :to-equal export-val)))

(it-sequential "wasm-import-export-descriptors-equal global"
  (destructuring-bind (export-val import-val) (list cl-cc/codegen::+wasm-export-global+ cl-cc/codegen::+wasm-import-global+)
    (expect import-val :to-equal export-val)))

;;; ─── Core opcodes ──────────────────────────────────────────────────────────

(it-sequential "wasm-control-flow-opcodes unreachable"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-unreachable+ #x00)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes nop"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-nop+ #x01)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes block"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-block+ #x02)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes loop"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-loop+ #x03)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes if"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-if+ #x04)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes else"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-else+ #x05)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes end"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-end+ #x0b)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes return"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-return+ #x0f)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes br"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-br+ #x0c)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes br-if"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-br-if+ #x0d)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes br-table"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-br-table+ #x0e)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes call"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-call+ #x10)
    (expect const :to-equal expected)))

(it-sequential "wasm-control-flow-opcodes call-indirect"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-call-indirect+ #x11)
    (expect const :to-equal expected)))

(it-sequential "wasm-local-global-opcodes local-get"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-local-get+ #x20)
    (expect const :to-equal expected)))

(it-sequential "wasm-local-global-opcodes local-set"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-local-set+ #x21)
    (expect const :to-equal expected)))

(it-sequential "wasm-local-global-opcodes local-tee"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-local-tee+ #x22)
    (expect const :to-equal expected)))

(it-sequential "wasm-local-global-opcodes global-get"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-global-get+ #x23)
    (expect const :to-equal expected)))

(it-sequential "wasm-local-global-opcodes global-set"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-global-set+ #x24)
    (expect const :to-equal expected)))

(it-sequential "wasm-const-opcodes i32-const"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-const+ #x41)
    (expect const :to-equal expected)))

(it-sequential "wasm-const-opcodes i64-const"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-const+ #x42)
    (expect const :to-equal expected)))

(it-sequential "wasm-const-opcodes f64-const"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-const+ #x44)
    (expect const :to-equal expected)))

