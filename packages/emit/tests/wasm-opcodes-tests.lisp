;;;; tests/unit/emit/wasm-opcodes-tests.lisp — WASM Opcode Tests
;;;;
;;;; Tests for src/emit/wasm-types.lisp:
;;;; i32/i64/f64 arithmetic and comparison opcodes, conversion opcodes,
;;;; reference opcodes, GC opcodes, named type indices, void sentinel,
;;;; and uniqueness invariants.

(in-package :cl-cc/test)


;;; ─── i32 comparison opcodes ────────────────────────────────────────────────

(it-sequential "wasm-i32-comparison-opcodes eqz"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-eqz+ #x45)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes eq"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-eq+ #x46)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes ne"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-ne+ #x47)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes lt-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-lt-s+ #x48)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes gt-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-gt-s+ #x4a)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes le-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-le-s+ #x4c)
    (expect const :to-equal expected)))

(it-sequential "wasm-i32-comparison-opcodes ge-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-ge-s+ #x4e)
    (expect const :to-equal expected)))

;;; ─── i64 opcodes ───────────────────────────────────────────────────────────

(it-sequential "wasm-i64-comparison-opcodes eqz"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-eqz+ #x50)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes eq"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-eq+ #x51)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes ne"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-ne+ #x52)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes lt-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-lt-s+ #x53)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes gt-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-gt-s+ #x55)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes le-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-le-s+ #x57)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-comparison-opcodes ge-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-ge-s+ #x59)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes add"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-add+ #x7c)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes sub"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-sub+ #x7d)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes mul"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-mul+ #x7e)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes div-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-div-s+ #x7f)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes rem-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-rem-s+ #x81)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes and"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-and+ #x83)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes or"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-or+ #x84)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes xor"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-xor+ #x85)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes shl"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-shl+ #x86)
    (expect const :to-equal expected)))

(it-sequential "wasm-i64-arithmetic-opcodes shr-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-shr-s+ #x87)
    (expect const :to-equal expected)))

;;; ─── f64 opcodes ───────────────────────────────────────────────────────────

(it-sequential "wasm-f64-opcodes add"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-add+ #xa0)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes sub"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-sub+ #xa1)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes mul"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-mul+ #xa2)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes div"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-div+ #xa3)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes neg"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-neg+ #x9a)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes abs"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-abs+ #x99)
    (expect const :to-equal expected)))

(it-sequential "wasm-f64-opcodes sqrt"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-sqrt+ #x9f)
    (expect const :to-equal expected)))

;;; ─── Conversion opcodes ────────────────────────────────────────────────────

(it-sequential "wasm-conversion-opcodes i32-wrap-i64"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i32-wrap-i64+ #xa7)
    (expect const :to-equal expected)))

(it-sequential "wasm-conversion-opcodes i64-extend-i32-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-extend-i32-s+ #xac)
    (expect const :to-equal expected)))

(it-sequential "wasm-conversion-opcodes f64-convert-i64-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-f64-convert-i64-s+ #xb9)
    (expect const :to-equal expected)))

(it-sequential "wasm-conversion-opcodes i64-trunc-f64-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-i64-trunc-f64-s+ #xb0)
    (expect const :to-equal expected)))

;;; ─── Reference opcodes ─────────────────────────────────────────────────────

(it-sequential "wasm-reference-opcodes ref-null"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-ref-null+ #xd0)
    (expect const :to-equal expected)))

(it-sequential "wasm-reference-opcodes ref-is-null"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-ref-is-null+ #xd1)
    (expect const :to-equal expected)))

(it-sequential "wasm-reference-opcodes ref-func"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-ref-func+ #xd2)
    (expect const :to-equal expected)))

(it-sequential "wasm-reference-opcodes ref-eq"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-ref-eq+ #xd3)
    (expect const :to-equal expected)))

;;; ─── GC opcodes ────────────────────────────────────────────────────────────

(it-sequential "wasm-gc-prefix"
  (expect cl-cc/codegen::+wasm-gc-prefix+ :to-equal #xfb))

(it-sequential "wasm-relaxed-simd-opcodes swizzle"
  (destructuring-bind (opcode expected-bytes) (list cl-cc/codegen::+wasm-i8x16-relaxed-swizzle+ '(#xfd #x80 #x01))
    (expect (coerce (cl-cc/codegen::wasm-encode-simd-op opcode) 'list) :to-equal expected-bytes)))

(it-sequential "wasm-relaxed-simd-opcodes f32x4-madd"
  (destructuring-bind (opcode expected-bytes) (list cl-cc/codegen::+wasm-f32x4-relaxed-madd+ '(#xfd #x85 #x01))
    (expect (coerce (cl-cc/codegen::wasm-encode-simd-op opcode) 'list) :to-equal expected-bytes)))

(it-sequential "wasm-relaxed-simd-opcodes i32x4-select"
  (destructuring-bind (opcode expected-bytes) (list cl-cc/codegen::+wasm-i32x4-relaxed-lane-select+ '(#xfd #x8b #x01))
    (expect (coerce (cl-cc/codegen::wasm-encode-simd-op opcode) 'list) :to-equal expected-bytes)))

(it-sequential "wasm-relaxed-simd-opcodes f64x2-max"
  (destructuring-bind (opcode expected-bytes) (list cl-cc/codegen::+wasm-f64x2-relaxed-max+ '(#xfd #x90 #x01))
    (expect (coerce (cl-cc/codegen::wasm-encode-simd-op opcode) 'list) :to-equal expected-bytes)))

(it-sequential "wasm-relaxed-simd-opcodes q15mulr"
  (destructuring-bind (opcode expected-bytes) (list cl-cc/codegen::+wasm-i16x8-relaxed-q15mulr-s+ '(#xfd #x91 #x01))
    (expect (coerce (cl-cc/codegen::wasm-encode-simd-op opcode) 'list) :to-equal expected-bytes)))

(it-sequential "wasm-gc-struct-opcodes new"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-new+ 0)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-struct-opcodes new-default"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-new-default+ 1)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-struct-opcodes get"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-get+ 2)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-struct-opcodes get-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-get-s+ 3)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-struct-opcodes get-u"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-get-u+ 4)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-struct-opcodes set"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-struct-set+ 5)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes new"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-new+ 6)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes new-default"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-new-default+ 7)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes new-fixed"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-new-fixed+ 8)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes get"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-get+ 11)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes set"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-set+ 14)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-array-opcodes len"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-array-len+ 15)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-ref-opcodes ref-test"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-ref-test+ 20)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-ref-opcodes ref-cast"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-ref-cast+ 22)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-ref-opcodes ref-i31"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-ref-i31+ 28)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-ref-opcodes i31-get-s"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-i31-get-s+ 29)
    (expect const :to-equal expected)))

(it-sequential "wasm-gc-ref-opcodes i31-get-u"
  (destructuring-bind (const expected) (list cl-cc/codegen::+wasm-gc-i31-get-u+ 30)
    (expect const :to-equal expected)))

;;; ─── Named type indices ────────────────────────────────────────────────────

(it-sequential "wasm-predefined-type-indices main-func"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-main-func+ 0)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices bytes-array"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-bytes-array+ 1)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices string"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-string+ 2)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices symbol"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-symbol+ 3)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices cons"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-cons+ 4)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices eqref-array"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-eqref-array+ 5)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices env"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-env+ 6)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices closure"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-closure+ 7)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices class-meta"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-class-meta+ 8)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices instance"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-instance+ 9)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices htable"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-htable+ 10)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices float"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-float+ 11)
    (expect const :to-equal expected)))

(it-sequential "wasm-predefined-type-indices char"
  (destructuring-bind (const expected) (list cl-cc/codegen::+type-idx-char+ 12)
    (expect const :to-equal expected)))

(it-sequential "wasm-num-predefined-types"
  (expect cl-cc/codegen::+num-predefined-types+ :to-equal 13)
  (expect cl-cc/codegen::+num-predefined-types+ :to-equal (1+ cl-cc/codegen::+type-idx-char+)))

;;; ─── Void sentinel ─────────────────────────────────────────────────────────

(it-sequential "wasm-void-sentinel"
  (expect cl-cc/codegen::+wasm-void+ :to-equal #x40))

;;; ─── Uniqueness invariants ─────────────────────────────────────────────────

(it-sequential "wasm-uniqueness-invariants"
  (let ((ids (list cl-cc/codegen::+wasm-section-custom+ cl-cc/codegen::+wasm-section-type+
                   cl-cc/codegen::+wasm-section-import+ cl-cc/codegen::+wasm-section-function+
                   cl-cc/codegen::+wasm-section-table+ cl-cc/codegen::+wasm-section-memory+
                   cl-cc/codegen::+wasm-section-global+ cl-cc/codegen::+wasm-section-export+
                   cl-cc/codegen::+wasm-section-start+ cl-cc/codegen::+wasm-section-element+
                   cl-cc/codegen::+wasm-section-code+ cl-cc/codegen::+wasm-section-data+
                   cl-cc/codegen::+wasm-section-data-count+)))
    (expect (length (remove-duplicates ids)) :to-equal (length ids)))
  (let ((indices (list cl-cc/codegen::+type-idx-main-func+ cl-cc/codegen::+type-idx-bytes-array+
                       cl-cc/codegen::+type-idx-string+ cl-cc/codegen::+type-idx-symbol+
                       cl-cc/codegen::+type-idx-cons+ cl-cc/codegen::+type-idx-eqref-array+
                       cl-cc/codegen::+type-idx-env+ cl-cc/codegen::+type-idx-closure+
                       cl-cc/codegen::+type-idx-class-meta+ cl-cc/codegen::+type-idx-instance+
                       cl-cc/codegen::+type-idx-htable+ cl-cc/codegen::+type-idx-float+
                       cl-cc/codegen::+type-idx-char+)))
    (expect (length (remove-duplicates indices)) :to-equal (length indices)))
  (let ((ops (list cl-cc/codegen::+wasm-gc-struct-new+ cl-cc/codegen::+wasm-gc-struct-new-default+
                   cl-cc/codegen::+wasm-gc-struct-get+ cl-cc/codegen::+wasm-gc-struct-get-s+
                   cl-cc/codegen::+wasm-gc-struct-get-u+ cl-cc/codegen::+wasm-gc-struct-set+)))
    (expect (length (remove-duplicates ops)) :to-equal 6)
    (expect (first ops) :to-equal 0)
    (expect (car (last ops)) :to-equal 5)))
