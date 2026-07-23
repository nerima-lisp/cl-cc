;;;; tests/unit/emit/wasm-trampoline-tests.lisp — WASM Trampoline Builder Tests
;;;;
;;;; Tests for src/emit/wasm-trampoline.lisp:
;;;; group-into-basic-blocks, build-label-pc-map, wasm-basic-block struct

(in-package :cl-cc/test)



;;; ─── group-into-basic-blocks ──────────────────────────────────────────────────

(it-sequential "trampoline-bb-empty"
  (expect (cl-cc/codegen::group-into-basic-blocks nil) :to-be-null))

(it-sequential "trampoline-bb-no-labels"
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (length bbs) :to-equal 1)
    (expect (cl-cc/codegen::wasm-bb-label (first bbs)) :to-be-null)
    (expect (cl-cc/codegen::wasm-bb-pc-index (first bbs)) :to-equal 0)
    (expect (length (cl-cc/codegen::wasm-bb-instructions (first bbs))) :to-equal 2)))

(it-sequential "trampoline-bb-label-grouping"
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 42)
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (length bbs) :to-equal 1)
    (expect (cl-cc/codegen::wasm-bb-label (first bbs)) :to-equal "entry")
    (expect (cl-cc/codegen::wasm-bb-pc-index (first bbs)) :to-equal 0)
    (expect (length (cl-cc/codegen::wasm-bb-instructions (first bbs))) :to-equal 2))
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (length bbs) :to-equal 2)
    (expect (cl-cc/codegen::wasm-bb-label (first bbs)) :to-equal "a")
    (expect (cl-cc/codegen::wasm-bb-pc-index (first bbs)) :to-equal 0)
    (expect (cl-cc/codegen::wasm-bb-label (second bbs)) :to-equal "b")
    (expect (cl-cc/codegen::wasm-bb-pc-index (second bbs)) :to-equal 1))
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (length bbs) :to-equal 2)
    (expect (cl-cc/codegen::wasm-bb-label (first bbs)) :to-be-null)
    (expect (length (cl-cc/codegen::wasm-bb-instructions (first bbs))) :to-equal 1)
    (expect (cl-cc/codegen::wasm-bb-label (second bbs)) :to-equal "loop"))
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-label :name "b")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (length bbs) :to-equal 2)
    (expect (cl-cc/codegen::wasm-bb-label (first bbs)) :to-equal "a")
    (expect (length (cl-cc/codegen::wasm-bb-instructions (first bbs))) :to-equal 0)
    (expect (cl-cc/codegen::wasm-bb-label (second bbs)) :to-equal "b")))

(it-sequential "trampoline-bb-struct-predicate"
  (let* ((instrs (list (make-vm-label :name "x")))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs)))
    (expect (cl-cc/codegen::wasm-basic-block-p (first bbs)) :to-be-truthy)))

;;; ─── build-label-pc-map ───────────────────────────────────────────────────────

(it-sequential "trampoline-pc-map-zero-entries"
  (expect (hash-table-p (cl-cc/codegen::build-label-pc-map nil)) :to-be-truthy)
  (expect (hash-table-count (cl-cc/codegen::build-label-pc-map nil)) :to-equal 0)
  (let* ((instrs (list (make-vm-const :dst :r0 :value 1)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs))
         (m (cl-cc/codegen::build-label-pc-map bbs)))
    (expect (hash-table-count m) :to-equal 0)))

(it-sequential "trampoline-pc-map-labels"
  (let* ((instrs (list (make-vm-label :name "main")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs))
         (m (cl-cc/codegen::build-label-pc-map bbs)))
    (expect (hash-table-count m) :to-equal 1)
    (expect (gethash "main" m) :to-equal 0))
  (let* ((instrs (list (make-vm-label :name "a")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-label :name "b")
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-label :name "c")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs))
         (m (cl-cc/codegen::build-label-pc-map bbs)))
    (expect (hash-table-count m) :to-equal 3)
    (expect (gethash "a" m) :to-equal 0)
    (expect (gethash "b" m) :to-equal 1)
    (expect (gethash "c" m) :to-equal 2)))

(it-sequential "trampoline-pc-map-with-implicit-entry"
  (let* ((instrs (list (make-vm-const :dst :r0 :value 0)
                       (make-vm-label :name "loop")
                       (make-vm-ret :reg :r0)))
         (bbs (cl-cc/codegen::group-into-basic-blocks instrs))
         (m (cl-cc/codegen::build-label-pc-map bbs)))
    ;; Only the labeled block appears in the map
    (expect (hash-table-count m) :to-equal 1)
    (expect (gethash "loop" m) :to-equal 1)))

;;; ─── %make-eq-hash-table ────────────────────────────────────────────────────

(it-sequential "trampoline-make-eq-hash-table dotted-pair"
  (destructuring-bind (alist key expected) (list '((a . 1) (b . 2)) 'a 1)
    (let ((ht (cl-cc/codegen::%make-eq-hash-table alist)))
    (expect (gethash key ht) :to-equal expected))))

(it-sequential "trampoline-make-eq-hash-table list-style"
  (destructuring-bind (alist key expected) (list '((x 10)) 'x 10)
    (let ((ht (cl-cc/codegen::%make-eq-hash-table alist)))
    (expect (gethash key ht) :to-equal expected))))

(it-sequential "trampoline-make-eq-hash-table two-keys"
  (destructuring-bind (alist key expected) (list '((m . "v1") (n . "v2")) 'n "v2")
    (let ((ht (cl-cc/codegen::%make-eq-hash-table alist)))
    (expect (gethash key ht) :to-equal expected))))

(it-sequential "trampoline-make-eq-hash-table-size"
  (let ((ht (cl-cc/codegen::%make-eq-hash-table '((a . 1) (b . 2) (c . 3)))))
    (expect (hash-table-count ht) :to-equal 3)))

;;; ─── WASM dispatch tables ───────────────────────────────────────────────────

(it-sequential "trampoline-binop-table-lookups vm-add"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-add "i64.add")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-binop-table-lookups vm-sub"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-sub "i64.sub")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-binop-table-lookups vm-mul"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-mul "i64.mul")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-binop-table-lookups vm-logand"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-logand "i64.and")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-binop-table-lookups vm-logior"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-logior "i64.or")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-binop-table-lookups vm-logxor"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-logxor "i64.xor")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-binop-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-cmp-table-lookups vm-lt"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-lt "i64.lt_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-cmp-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-cmp-table-lookups vm-gt"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-gt "i64.gt_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-cmp-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-cmp-table-lookups vm-le"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-le "i64.le_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-cmp-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-cmp-table-lookups vm-ge"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-ge "i64.ge_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-cmp-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-cmp-table-lookups vm-eq"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-eq "i64.eq")
    (expect (gethash type-sym cl-cc/codegen::*wasm-i64-cmp-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-unary-table-lookups vm-inc"
  (destructuring-bind (type-sym expected-fmt) (list 'vm-inc "(i64.add ~A (i64.const 1))")
    (expect (gethash type-sym cl-cc/codegen::*wasm-unary-fixnum-table*) :to-equal expected-fmt)))

(it-sequential "trampoline-unary-table-lookups vm-dec"
  (destructuring-bind (type-sym expected-fmt) (list 'vm-dec "(i64.sub ~A (i64.const 1))")
    (expect (gethash type-sym cl-cc/codegen::*wasm-unary-fixnum-table*) :to-equal expected-fmt)))

(it-sequential "trampoline-unary-table-lookups vm-neg"
  (destructuring-bind (type-sym expected-fmt) (list 'vm-neg "(i64.sub (i64.const 0) ~A)")
    (expect (gethash type-sym cl-cc/codegen::*wasm-unary-fixnum-table*) :to-equal expected-fmt)))

(it-sequential "trampoline-unary-table-lookups vm-lognot"
  (destructuring-bind (type-sym expected-fmt) (list 'vm-lognot "(i64.xor ~A (i64.const -1))")
    (expect (gethash type-sym cl-cc/codegen::*wasm-unary-fixnum-table*) :to-equal expected-fmt)))

(it-sequential "trampoline-unary-table-lookups vm-logcount"
  (destructuring-bind (type-sym expected-fmt) (list 'vm-logcount "(i64.popcnt ~A)")
    (expect (gethash type-sym cl-cc/codegen::*wasm-unary-fixnum-table*) :to-equal expected-fmt)))

(it-sequential "trampoline-minmax-table-lookups vm-min"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-min "i64.le_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-minmax-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-minmax-table-lookups vm-max"
  (destructuring-bind (type-sym expected-opcode) (list 'vm-max "i64.ge_s")
    (expect (gethash type-sym cl-cc/codegen::*wasm-minmax-table*) :to-equal expected-opcode)))

(it-sequential "trampoline-struct-get-table-entries"
  (expect (search "struct.get $cons_t 0"
                       (gethash 'vm-car cl-cc/codegen::*wasm-struct-get-table*)) :to-be-truthy)
  (expect (search "struct.get $cons_t 1"
                       (gethash 'vm-cdr cl-cc/codegen::*wasm-struct-get-table*)) :to-be-truthy))

;;; ─── %wasm-const-value-to-wat ─────────────────────────────────────────────

(it-sequential "wasm-const-value-to-wat-cases integer-42"
  (destructuring-bind (val expected-prefix) (list 42 "(ref.i31")
    (let ((result (cl-cc/codegen::%wasm-const-value-to-wat val)))
    (expect (stringp result) :to-be-truthy)
    (expect (or (string= result expected-prefix)
                     (and (>= (length result) (length expected-prefix))
                          (string= expected-prefix result :end2 (length expected-prefix)))) :to-be-truthy))))

(it-sequential "wasm-const-value-to-wat-cases nil"
  (destructuring-bind (val expected-prefix) (list nil "(ref.null eq)")
    (let ((result (cl-cc/codegen::%wasm-const-value-to-wat val)))
    (expect (stringp result) :to-be-truthy)
    (expect (or (string= result expected-prefix)
                     (and (>= (length result) (length expected-prefix))
                          (string= expected-prefix result :end2 (length expected-prefix)))) :to-be-truthy))))

(it-sequential "wasm-const-value-to-wat-cases true"
  (destructuring-bind (val expected-prefix) (list t "(ref.i31 (i32.const 1))")
    (let ((result (cl-cc/codegen::%wasm-const-value-to-wat val)))
    (expect (stringp result) :to-be-truthy)
    (expect (or (string= result expected-prefix)
                     (and (>= (length result) (length expected-prefix))
                          (string= expected-prefix result :end2 (length expected-prefix)))) :to-be-truthy))))

(it-sequential "wasm-const-value-to-wat-string-produces-utf8-array"
  ;; WASM GC string support (docs/wasm.md FRs) lowers Lisp strings to a
  ;; string.new_utf8_array constructor; strings are no longer "unsupported".
  (let ((wat (cl-cc/codegen::%wasm-const-value-to-wat "unsupported")))
    (expect (search "string.new_utf8_array" wat) :to-be-truthy)))

;;; ─── %wasm-if-eqref ────────────────────────────────────────────────────────

(it-sequential "wasm-if-eqref-structure basic"
  (destructuring-bind (cond-wat then-wat else-wat) (list "(i64.ge_s x (i64.const 0))" "then-wat" "else-wat")
    (let ((result (cl-cc/codegen::%wasm-if-eqref cond-wat then-wat else-wat)))
    (expect (search "(if (result eqref)" result) :to-be-truthy)
    (expect (search cond-wat result) :to-be-truthy)
    (expect (search "(then" result) :to-be-truthy)
    (expect (search "(else" result) :to-be-truthy))))

;;; ─── User-level dense CASE br_table lowering ──────────────────────────────

(defun %test-label-pc-map (&rest entries)
  "Build an EQUAL label->pc map for trampoline case-lowering tests."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (entry entries map)
      (setf (gethash (car entry) map) (cdr entry)))))

(it-sequential "wasm-dense-case-targets-detects-density"
  (expect (cl-cc/codegen::wasm-dense-integer-case-targets-p
    '((10 . "case10") (11 . "case11") (12 . "case12") (13 . "case13"))) :to-be-truthy))

(it-sequential "wasm-sparse-case-targets-do-not-use-br-table"
  (expect (cl-cc/codegen::wasm-dense-integer-case-targets-p
    '((1 . "case1") (100 . "case100") (200 . "case200") (300 . "case300"))) :to-be-falsy))

(it-sequential "wasm-dense-case-emits-user-br-table"
  (let* ((reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))
         (label-pc-map (%test-label-pc-map '("case10" . 10)
                                           '("case11" . 11)
                                           '("case12" . 12)
                                           '("case13" . 13)
                                           '("otherwise" . 99)))
         (out (make-string-output-stream)))
    (cl-cc/codegen::wasm-reg-to-local reg-map :R0)
    (expect (cl-cc/codegen::maybe-emit-wasm-dense-case-br-table
      :R0
      '((10 . "case10") (11 . "case11") (12 . "case12") (13 . "case13"))
      "otherwise"
      label-pc-map reg-map out) :to-be-truthy)
    (let ((wat (get-output-stream-string out)))
      (assert-output-contains wat "dense integer CASE lowered to WASM br_table")
      (assert-output-contains wat "br_table")
      (assert-output-contains wat "$case_default")
      (assert-output-contains wat "(i64.const 10)")
      (assert-output-contains wat "(i32.const 99)"))))

(it-sequential "wasm-sparse-case-emits-no-user-br-table"
  (let* ((reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))
         (label-pc-map (%test-label-pc-map '("case1" . 1)
                                           '("case100" . 100)
                                           '("case200" . 200)
                                           '("case300" . 300)
                                           '("otherwise" . 99)))
         (out (make-string-output-stream)))
    (cl-cc/codegen::wasm-reg-to-local reg-map :R0)
    (expect (cl-cc/codegen::maybe-emit-wasm-dense-case-br-table
      :R0
      '((1 . "case1") (100 . "case100") (200 . "case200") (300 . "case300"))
      "otherwise"
      label-pc-map reg-map out) :to-be-falsy)
    (expect (search "br_table" (get-output-stream-string out) :test #'char=) :to-be-falsy)))
