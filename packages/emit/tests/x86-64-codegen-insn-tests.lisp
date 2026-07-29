;;;; tests/unit/emit/x86-64-codegen-insn-tests.lisp — x86-64 Individual Instruction Emitter Tests
;;;;
;;;; Tests for individual x86-64 instruction emitters in src/emit/x86-64-codegen.lisp:
;;;; emit-vm-bswap, emit-vm-add, emit-vm-select, emit-vm-jump-zero-inst,
;;;; emit-vm-logcount, emit-vm-integer-length, emit-vm-call-like-inst,
;;;; emit-vm-tail-call-inst
;;;;
;;;; Depends on %x86-collect-bytes helper defined in x86-64-codegen-tests.lisp
;;;; (loaded before this file via :serial t ASDF).

(in-package :cl-cc/test)


(it-sequential "x86-64-bswap-emitter-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-bswap (cl-cc:make-vm-bswap :dst :R0 :src :R1) s)))))
    (expect (= 5 (length bytes)) :to-be-truthy)
    (expect (= #x48 (first bytes)) :to-be-truthy)
    (expect (= #x89 (second bytes)) :to-be-truthy)
    (expect (= #xC8 (third bytes)) :to-be-truthy)
    (expect (= #x0F (fourth bytes)) :to-be-truthy)
    (expect (= #xC8 (fifth bytes)) :to-be-truthy)))

(it-sequential "x86-64-add-emitter-two-address-lowering"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-add
                   (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    ;; MOV rax,rcx / ADD rax,rdx
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x89 (nth 1 bytes)) :to-be-truthy)
    (expect (= #xC8 (nth 2 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 3 bytes)) :to-be-truthy)
    (expect (= #x01 (nth 4 bytes)) :to-be-truthy)
    (expect (= #xD0 (nth 5 bytes)) :to-be-truthy)))

(it-sequential "x86-64-select-emitter-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-select
                   (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (expect (gethash 'cl-cc/vm::vm-select cl-cc/codegen::*x86-64-emitter-table*) :to-be #'cl-cc/codegen::emit-vm-select)
    (expect (= 10 (length bytes)) :to-be-truthy)
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x89 (nth 1 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 3 bytes)) :to-be-truthy)
    (expect (= #x85 (nth 4 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 6 bytes)) :to-be-truthy)
    (expect (= #x0F (nth 7 bytes)) :to-be-truthy)
    (expect (= #x45 (nth 8 bytes)) :to-be-truthy)
    (dolist (branch-opcode '(#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77
                             #x78 #x79 #x7A #x7B #x7C #x7D #x7E #x7F
                             #xE9 #xEB))
      (expect (member branch-opcode bytes :test #'=) :to-be-falsy))))

(it-sequential "x86-64-jump-zero-test-je-adjacent"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-jump-zero-inst
                   (cl-cc:make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 9)
                         ht))))))
    (expect (= 5 (length bytes)) :to-be-truthy)
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x85 (nth 1 bytes)) :to-be-truthy)
    (expect (= #x74 (nth 3 bytes)) :to-be-truthy)))

(it-sequential "x86-64-logcount-emitter-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-logcount
                   (cl-cc:make-vm-logcount :dst :R0 :src :R1) s)))))
    (expect (= 5 (length bytes)) :to-be-truthy)
    (expect (= #xF3 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 1 bytes)) :to-be-truthy)
    (expect (= #x0F (nth 2 bytes)) :to-be-truthy)
    (expect (= #xB8 (nth 3 bytes)) :to-be-truthy)))

(it-sequential "x86-64-integer-length-emitter-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-integer-length
                   (cl-cc:make-vm-integer-length :dst :R0 :src :R1) s)))))
    (expect (= 22 (length bytes)) :to-be-truthy)
    ;; xor rax,rax / test rcx,rcx / je rel8 / lzcnt rax,rcx / neg rax / add rax,64
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x31 (nth 1 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 3 bytes)) :to-be-truthy)
    (expect (= #x85 (nth 4 bytes)) :to-be-truthy)
    (expect (= #x74 (nth 6 bytes)) :to-be-truthy)
    (expect (= #xF3 (nth 8 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 9 bytes)) :to-be-truthy)
    (expect (= #x0F (nth 10 bytes)) :to-be-truthy)
    (expect (= #xBD (nth 11 bytes)) :to-be-truthy)))

(it-sequential "x86-64-call-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-call-like-inst
                   (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s)))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #xFF (nth 1 bytes)) :to-be-truthy)
    (expect (= #xD1 (nth 2 bytes)) :to-be-truthy)
    (expect (= #x48 (nth 3 bytes)) :to-be-truthy)
    (expect (= #x89 (nth 4 bytes)) :to-be-truthy)
    (expect (= #xC0 (nth 5 bytes)) :to-be-truthy)))

(it-sequential "x86-64-tail-call-encoding"
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-vm-tail-call-inst
                   (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil) s)))))
    (expect (= 3 (length bytes)) :to-be-truthy)
    (expect (= #x48 (nth 0 bytes)) :to-be-truthy)
    (expect (= #xFF (nth 1 bytes)) :to-be-truthy)
    (expect (= #xE1 (nth 2 bytes)) :to-be-truthy)))

(it-sequential "x86-64-call-encoding-retpoline"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-use-retpoline* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-call-like-inst
                     (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s))))))
    (expect (= 44 (length bytes)) :to-be-truthy)
    ;; starts with CALL rel32
    (expect (= #xE8 (nth 0 bytes)) :to-be-truthy)
    ;; includes PAUSE + LFENCE in thunk capture loop
    (expect (find #xF3 bytes) :to-be-truthy)
    (expect (find #xAE bytes) :to-be-truthy)))

(it-sequential "x86-64-tail-call-encoding-retpoline"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-use-retpoline* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-tail-call-inst
                     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil) s))))))
    (expect (= 20 (length bytes)) :to-be-truthy)
    (expect (= #xE8 (nth 0 bytes)) :to-be-truthy)
    (expect (find #xF3 bytes) :to-be-truthy)
    (expect (= #xC3 (car (last bytes))) :to-be-truthy)))

(it-sequential "x86-64-call-cfi-guard-avoids-clobbering-rax-target"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-cfi-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-call-like-inst
                     (cl-cc:make-vm-call :dst :R1 :func :R0 :args nil) s))))))
    ;; CALL RAX must remain at end of non-retpoline path.
    (expect (subseq bytes (- (length bytes) 6) (- (length bytes) 3)) :to-equal '(#x48 #xFF #xD0))
    ;; Guard must not clobber target via `mov rax, [rax]`.
    (expect (search '(#x48 #x8B #x00) bytes :test #'eql) :to-be-falsy)))

(it-sequential "x86-64-shadow-stack-control-inst-emits-nop-when-shadow-stack-disabled"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* nil))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-push-handler :handler-type :error :handler-label "Lh" :result-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 2 (length bytes)) :to-be-truthy)
    (expect (= #x66 (nth 0 bytes)) :to-be-truthy)
    (expect (= #x90 (nth 1 bytes)) :to-be-truthy)))

(it-sequential "x86-64-shadow-stack-control-inst-emits-saveprevssp-when-enabled"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-push-handler :handler-type :error :handler-label "Lh" :result-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal '(#xF3 #x0F #x01 #xEA #x66 #x90))))

(it-sequential "x86-64-shadow-stack-control-inst-uses-distinct-restore-marker"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-pop-handler)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal '(#xF3 #x0F #x01 #x28 #x66 #x90))))

(it-sequential "x86-64-shadow-stack-control-inst-uses-incsspq-for-adjust-paths"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-signal :condition-reg :R0)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal '(#xF3 #x48 #x0F #xAE #xE8 #x90))))

(it-sequential "x86-64-shadow-stack-control-inst-covers-vm-establish-catch"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-establish-catch
                      :tag-reg :R0 :handler-label "Lc" :result-reg :R1)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal '(#xF3 #x0F #x01 #xEA #x66 #x90))))

(it-sequential "x86-64-shadow-stack-control-inst-covers-vm-handler-ops remove-handler"
  (destructuring-bind (make-fn expected-bytes) (list #'cl-cc/vm::make-vm-remove-handler '(#xF3 #x0F #x01 #x28 #x66 #x90))
    (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (funcall make-fn)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal expected-bytes))))

(it-sequential "x86-64-shadow-stack-control-inst-covers-vm-handler-ops sync-handler-regs"
  (destructuring-bind (make-fn expected-bytes) (list #'cl-cc/vm::make-vm-sync-handler-regs '(#xF3 #x48 #x0F #xAE #xE8 #x90))
    (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (funcall make-fn)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal expected-bytes))))

(it-sequential "x86-64-shadow-stack-control-inst-covers-vm-throw"
  (let ((bytes (let ((cl-cc/codegen::*x86-64-shadow-stack-enabled* t))
                 (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc/codegen::emit-vm-instruction-with-labels
                     (cl-cc/vm::make-vm-throw :tag-reg :R0 :value-reg :R1)
                     s
                     0
                     (make-hash-table :test #'equal)))))))
    (expect (= 6 (length bytes)) :to-be-truthy)
    (expect bytes :to-equal '(#xF3 #x48 #x0F #xAE #xE8 #x90))))
