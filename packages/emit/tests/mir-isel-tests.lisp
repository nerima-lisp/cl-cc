;;;; packages/emit/tests/mir-isel-tests.lisp — MIR pipeline / ISel smoke tests

(in-package :cl-cc/test)


(it-sequential "mir-isel-maximal-munch-prefers-largest-x86-tile"
  (let* ((tree '(:add (:reg r1) (:add (:mul (:reg r2) (:const 4)) (:const 8))))
         (tiles (cl-cc/codegen:isel-maximal-munch tree :x86-64))
         (root-rule (car (last tiles))))
    (expect (cl-cc/codegen:isel-rule-name (car root-rule)) :to-be :x86-lea-address)))

(it-sequential "mir-isel-maximal-munch-prefers-aarch64-scaled-address"
  (let* ((tree '(:add (:reg x1) (:mul (:reg x2) (:const 8))))
         (tiles (cl-cc/codegen:isel-maximal-munch tree :aarch64))
         (root-rule (car (last tiles))))
    (expect (cl-cc/codegen:isel-rule-name (car root-rule)) :to-be :a64-add-scaled-address)))

(it-sequential "mir-vm-program-roundtrip-preserves-unsupported-instructions"
  (let* ((instructions (list (make-vm-const :dst 0 :value 1)
                             (make-vm-print :reg 0)
                             (make-vm-halt :reg 0)))
         (program (make-vm-program :instructions instructions :result-register 0))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64)))
    (expect (= (length instructions) (length (vm-program-instructions selected))) :to-be-truthy)
    (expect (typep (second (vm-program-instructions selected)) 'vm-print) :to-be-truthy)))

(it-sequential "mir-vm-program-constant-folds-before-isel"
  (let* ((program (make-vm-program
                   :instructions (list (make-vm-const :dst 0 :value 2)
                                       (make-vm-const :dst 1 :value 3)
                                       (make-vm-add :dst 2 :lhs 0 :rhs 1)
                                       (make-vm-halt :reg 2))
                   :result-register 2))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64))
         (insts (vm-program-instructions selected)))
    (expect (typep (third insts) 'vm-const) :to-be-truthy)
    (expect (vm-value (third insts)) :to-equal 5)))

(it-sequential "mir-pipeline-flag-compilation-produces-same-vm-stream-shape"
  (let* ((result (cl-cc/compile:compile-toplevel-forms '((+ 1 2)) :target :x86_64))
         (program (cl-cc/compile:compilation-result-program result))
         (selected (cl-cc/codegen:isel-vm-program program :target :x86-64)))
    (expect (typep selected 'vm-program) :to-be-truthy)
    (expect (every (lambda (inst) (typep inst 'vm-instruction))
                        (vm-program-instructions selected)) :to-be-truthy)))
