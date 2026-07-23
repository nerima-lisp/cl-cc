;;;; packages/emit/tests/native-advanced-bridge-tests.lisp

(in-package :cl-cc/test)



(it-sequential "fr-534-speculative-execution-mitigation-lfence"
  (let ((bytes (cl-cc/codegen::with-output-to-vector (out)
                 (cl-cc/codegen:emit-x86-64-speculation-barrier out))))
    (expect (coerce bytes 'list) :to-equal '(#x0F #xAE #xE8)))
  (let ((cl-cc/codegen:*x86-64-spectre-mitigations-enabled* t))
    (expect (cl-cc/codegen:x86-64-speculative-execution-mitigation-enabled-p) :to-be-truthy)))

(it-sequential "fr-690-llvm-ir-backend-lowering-emits-module"
  (let ((ir (cl-cc/emit:emit-llvm-ir nil :name "fr690" :target-triple "x86_64-apple-darwin")))
    (expect (search "ModuleID" ir) :to-be-truthy)
    (expect (search "target datalayout" ir) :to-be-truthy)
    (expect (search "TODO" ir) :to-be-falsy)
    (expect (search "bridge-only" ir) :to-be-falsy)
    (expect (member :fr-690 (cl-cc/emit:llvm-ir-bridge-capabilities)) :to-be-truthy)))

(it-sequential "fr-712-mlir-integration-bridge-emits-module"
  (let ((mlir (cl-cc/emit:emit-mlir nil :name "fr712")))
    (expect (search "module @fr712" mlir) :to-be-truthy)
    (expect (search "func.func @clcc_entry() -> i64" mlir) :to-be-truthy)
    (expect (search "arith.constant 0 : i64" mlir) :to-be-truthy)
    (expect (search "func.return %reg0 : i64" mlir) :to-be-truthy)
    (expect (search "TODO" mlir) :to-be-falsy)
    (expect (search "bridge-only" mlir) :to-be-falsy)
    (expect (member :fr-712 (cl-cc/emit:mlir-bridge-capabilities)) :to-be-truthy)))

(it-sequential "fr-721-macro-fusion-awareness-preserves-cmp-jcc"
  (let* ((cmp (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2))
         (br  (cl-cc:make-vm-jump-zero :reg :R0 :label "cold")))
    (expect (cl-cc/codegen:x86-64-macro-fusion-candidate-p cmp br) :to-be-truthy)
    (expect (cl-cc/codegen:x86-64-preserve-macro-fusion (list cmp br)) :to-equal (list cmp br))))
