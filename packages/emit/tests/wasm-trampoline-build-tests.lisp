;;;; tests/unit/emit/wasm-trampoline-build-tests.lisp
;;;; Coverage for src/emit/wasm-trampoline-build.lisp:
;;;;   collect-registers-from-instructions,
;;;;   build-trampoline-body (empty + single-block),
;;;;   build-all-wasm-functions (label-to-table mapping).

(in-package :cl-cc/test)

(defsuite wasm-trampoline-build-suite
  :description "Tests for wasm-trampoline-build.lisp: body builder + module assembler"
  :parent cl-cc-unit-suite)

(in-suite wasm-trampoline-build-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-test-wasm-reg-map ()
  "Fresh register map with 0 params."
  (cl-cc/codegen::make-wasm-reg-map-for-function 0))

(defun %wat-string (emit-fn)
  "Collect WAT output from EMIT-FN into a string."
  (let ((s (make-string-output-stream)))
    (funcall emit-fn s)
    (get-output-stream-string s)))

;;; ─── collect-registers-from-instructions ─────────────────────────────────

(deftest wasm-tb-collect-regs-empty-instructions
  "collect-registers-from-instructions on nil leaves reg-map unchanged."
  (let ((reg-map (make-test-wasm-reg-map)))
    (let ((before-count (hash-table-count (cl-cc/codegen::wasm-reg-map-table reg-map))))
      (cl-cc/codegen::collect-registers-from-instructions nil reg-map)
      (assert-= before-count (hash-table-count (cl-cc/codegen::wasm-reg-map-table reg-map))))))

(deftest wasm-tb-collect-regs-vm-const-touches-dst
  "collect-registers-from-instructions allocates a local for vm-const dst."
  (let ((reg-map (make-test-wasm-reg-map))
        (insts (list (cl-cc:make-vm-const :dst :R0 :value 42))))
    (cl-cc/codegen::collect-registers-from-instructions insts reg-map)
    ;; :R0 should now be in the table
    (assert-true (gethash :R0 (cl-cc/codegen::wasm-reg-map-table reg-map)))))

(deftest wasm-tb-collect-regs-vm-move-touches-src-and-dst
  "collect-registers-from-instructions allocates locals for both src and dst of vm-move."
  (let ((reg-map (make-test-wasm-reg-map))
        (insts (list (cl-cc:make-vm-move :dst :R1 :src :R2))))
    (cl-cc/codegen::collect-registers-from-instructions insts reg-map)
    (assert-true (gethash :R1 (cl-cc/codegen::wasm-reg-map-table reg-map)))
    (assert-true (gethash :R2 (cl-cc/codegen::wasm-reg-map-table reg-map)))))

(deftest wasm-tb-collect-regs-assigns-unique-indices
  "collect-registers-from-instructions gives each register a unique local index."
  (let ((reg-map (make-test-wasm-reg-map))
        (insts (list (cl-cc:make-vm-move :dst :R0 :src :R1)
                     (cl-cc:make-vm-move :dst :R2 :src :R3))))
    (cl-cc/codegen::collect-registers-from-instructions insts reg-map)
    (let ((tbl (cl-cc/codegen::wasm-reg-map-table reg-map)))
      (let ((indices (mapcar (lambda (k) (gethash k tbl)) '(:R0 :R1 :R2 :R3))))
        (assert-= 4 (length (remove-duplicates indices)))))))

(deftest wasm-tb-collect-regs-idempotent-for-same-register
  "collect-registers-from-instructions reuses the same index for repeated register."
  (let ((reg-map (make-test-wasm-reg-map))
        (insts (list (cl-cc:make-vm-const :dst :R0 :value 1)
                     (cl-cc:make-vm-const :dst :R0 :value 2))))
    (cl-cc/codegen::collect-registers-from-instructions insts reg-map)
    ;; :R0 appears twice but should have exactly one entry
    (assert-= 1 (hash-table-count (cl-cc/codegen::wasm-reg-map-table reg-map)))))

;;; ─── build-trampoline-body ───────────────────────────────────────────────

(deftest wasm-tb-build-body-empty-blocks-emits-comment
  "build-trampoline-body with zero basic-blocks emits a comment and nothing else."
  (let ((reg-map (make-test-wasm-reg-map))
        (label-pc-map (make-hash-table :test #'equal)))
    (let ((result (%wat-string
                   (lambda (s)
                     (cl-cc/codegen::build-trampoline-body nil label-pc-map reg-map nil s)))))
      (assert-true (search "empty" result)))))

(deftest wasm-tb-build-body-single-block-has-block-exit
  "build-trampoline-body with one basic-block emits $exit and $dispatch markers."
  (let* ((reg-map (make-test-wasm-reg-map))
         (label-pc-map (make-hash-table :test #'equal))
         ;; A minimal basic-block with no instructions
         (bb (cl-cc/codegen::make-wasm-basic-block :label "entry" :instructions nil))
         (result (%wat-string
                  (lambda (s)
                    (cl-cc/codegen::build-trampoline-body (list bb) label-pc-map reg-map nil s)))))
    (assert-true (search "$exit" result))
    (assert-true (search "$dispatch" result))
    (assert-true (search "br_table" result))))

(deftest wasm-tb-build-body-single-block-has-blk-0
  "build-trampoline-body emits $blk_0 for the first basic-block."
  (let* ((reg-map (make-test-wasm-reg-map))
         (label-pc-map (make-hash-table :test #'equal))
         (bb (cl-cc/codegen::make-wasm-basic-block :label "start" :instructions nil))
         (result (%wat-string
                  (lambda (s)
                    (cl-cc/codegen::build-trampoline-body (list bb) label-pc-map reg-map nil s)))))
    (assert-true (search "$blk_0" result))))

(deftest wasm-tb-build-body-two-blocks-has-blk-0-and-1
  "build-trampoline-body emits $blk_0 and $blk_1 for two basic-blocks."
  (let* ((reg-map (make-test-wasm-reg-map))
         (label-pc-map (make-hash-table :test #'equal))
         (bb0 (cl-cc/codegen::make-wasm-basic-block :label "a" :instructions nil))
         (bb1 (cl-cc/codegen::make-wasm-basic-block :label "b" :instructions nil))
         (result (%wat-string
                  (lambda (s)
                    (cl-cc/codegen::build-trampoline-body (list bb0 bb1) label-pc-map reg-map nil s)))))
    (assert-true (search "$blk_0" result))
    (assert-true (search "$blk_1" result))))

(deftest wasm-tb-build-function-loads-closure-params
  "build-wasm-function-wat loads closure params from the $cl_arg globals."
  (let* ((func (cl-cc/codegen::make-wasm-function-def
                :index 0
                :wat-name "$fn_with_params"
                :params '(:R0 :R1)
                :source-instructions (list (cl-cc:make-vm-ret :reg :R0))))
         (result (cl-cc/codegen::build-wasm-function-wat func)))
    (assert-true (search "(local.set 0 (global.get $cl_arg0))" result))
    (assert-true (search "(local.set 1 (global.get $cl_arg1))" result))))

(deftest wasm-tb-emit-function-reserves-closure-param-locals
  "emit-wat-function uses the same inferred param local layout as the trampoline body."
  (let* ((func (cl-cc/codegen::make-wasm-function-def
                :index 0
                :wat-name "$fn_with_param_locals"
                :params '(:R0 :R1)
                :source-instructions (list (cl-cc:make-vm-ret :reg :R1)))))
    (cl-cc/codegen::build-wasm-function-wat func)
    (let ((result (%wat-string
                   (lambda (s)
                     (cl-cc/codegen::emit-wat-function func s)))))
      (assert-true (search "closure parameter local 0" result))
      (assert-true (search "closure parameter local 1" result))
      (assert-true (search "$pc at index 2" result)))))

;;; ─── FR-228 bulk-memory helpers ─────────────────────────────────────────

(deftest wasm-tb-array-fill-emits-specialized-array-fill
  "wasm-array-fill-wat emits the specialized array.fill form for typed GC arrays."
  (let ((cl-cc/codegen::*wasm-gc-array-types-enabled* t))
    (let* ((reg-map (make-test-wasm-reg-map))
           (array-reg :R0)
           (value-reg :R1)
           (array-local (cl-cc/codegen::wasm-reg-to-local reg-map array-reg))
           (value-local (cl-cc/codegen::wasm-reg-to-local reg-map value-reg))
           (_ (cl-cc/codegen::wasm-array-reg-record-kind reg-map array-reg :fixnum))
           (result (cl-cc/codegen::wasm-array-fill-wat
                    reg-map array-reg value-reg "(i32.const 3)" "(i32.const 5)")))
      (assert-true (search "(array.fill $fixnum_array_t" result))
      (assert-true (search "(ref.cast (ref $fixnum_array_t)" result))
      (assert-true (search (format nil "(local.get ~D)" array-local) result))
      (assert-true (search (format nil "(i64.extend_i32_s (i31.get_s (local.get ~D)))" value-local)
                           result)))))

(deftest wasm-tb-array-copy-emits-specialized-array-copy
  "wasm-array-copy-wat emits the specialized array.copy form with source and destination types."
  (let ((cl-cc/codegen::*wasm-gc-array-types-enabled* t))
    (let* ((reg-map (make-test-wasm-reg-map))
           (dst-reg :R0)
           (src-reg :R1)
           (len-reg :R2)
           (dst-local (cl-cc/codegen::wasm-reg-to-local reg-map dst-reg))
           (src-local (cl-cc/codegen::wasm-reg-to-local reg-map src-reg))
           (len-local (cl-cc/codegen::wasm-reg-to-local reg-map len-reg))
           (_dst (cl-cc/codegen::wasm-array-reg-record-kind reg-map dst-reg :char))
           (_src (cl-cc/codegen::wasm-array-reg-record-kind reg-map src-reg :fixnum))
           (result (cl-cc/codegen::wasm-array-copy-wat reg-map dst-reg src-reg len-reg)))
      (assert-true (search "(array.copy $char_array_t $fixnum_array_t" result))
      (assert-true (search "(ref.cast (ref $char_array_t)" result))
      (assert-true (search "(ref.cast (ref $fixnum_array_t)" result))
      (assert-true (search (format nil "(local.get ~D)" dst-local) result))
      (assert-true (search (format nil "(local.get ~D)" src-local) result))
      (assert-true (search (format nil "(i31.get_s (local.get ~D))" len-local)
                           result)))))

(deftest wasm-tb-memory-copy-honors-multi-memory-gating
  "wasm-memory-copy-wat* switches between plain and explicit-memory forms based on the multi-memory gate."
  (let ((plain (let ((cl-cc/codegen::*wasm-multiple-memories-enabled* nil))
                 (cl-cc/codegen::wasm-memory-copy-wat* "(i32.const 4)" "(i32.const 8)" "(i32.const 16)"
                                                      :dst-memory 0 :src-memory 1)))
        (multi (let ((cl-cc/codegen::*wasm-multiple-memories-enabled* t))
                 (cl-cc/codegen::wasm-memory-copy-wat* "(i32.const 4)" "(i32.const 8)" "(i32.const 16)"
                                                      :dst-memory 0 :src-memory 1))))
    (assert-true (search "(memory.copy (i32.const 4) (i32.const 8) (i32.const 16))" plain))
    (assert-true (search "(memory.copy (memory 0) (i32.const 4) (memory 1) (i32.const 8) (i32.const 16))" multi))))

;;; ─── build-all-wasm-functions (module table setup) ───────────────────────

(deftest wasm-tb-build-all-returns-module
  "build-all-wasm-functions returns the module it was given."
  (let* ((module (cl-cc/codegen::make-wasm-module-ir
                  :functions nil
                  :imports nil
                  :exports nil
                  :table-size 0)))
    (let ((result (cl-cc/codegen::build-all-wasm-functions module)))
      (assert-eq module result))))

(deftest wasm-tb-build-all-sets-table-size
  "build-all-wasm-functions sets module table-size to function count."
  (let* ((f0 (cl-cc/codegen::make-wasm-function-def
              :index 0 :wat-name "$f0"
              :source-instructions nil :params nil :body nil))
         (f1 (cl-cc/codegen::make-wasm-function-def
              :index 1 :wat-name "$f1"
              :source-instructions nil :params nil :body nil))
         (module (cl-cc/codegen::make-wasm-module-ir
                  :functions (list f0 f1)
                  :imports nil
                  :exports nil
                  :table-size 0)))
    (cl-cc/codegen::build-all-wasm-functions module)
    (assert-= 2 (cl-cc/codegen::wasm-module-table-size module))))
