(in-package :cl-cc/vm)

;;; VM instruction exports grouped by category.
;;;
;;; The long constructor lists live in adjacent helper files so this top-level
;;; export surface keeps the high-signal symbols (instruction names, predicates,
;;; accessors, and defining macros) easy to scan.

#-cl-cc-self-hosting
(export
 '(vm-cons-cell-car
   vm-cons-cell-cdr
   vm-closure-object
    vm-closure-entry-label
    vm-closure-params
     vm-closure-captured-regs
     vm-closure-captured-vals
     vm-closure-invocation-count
     vm-closure-compilation-tier
     vm-closure-program-flat
     vm-closure-label-table
   vm-instruction
    vm-const
    vm-move
    vm-type-check
    vm-deopt
    vm-osr-entry
    vm-type-check-deopt-label
    vm-type-check-deopt-id
    vm-deopt-label
    vm-deopt-id
     vm-deopt-reason
     vm-osr-label
     vm-osr-id
     vm-deopt-info
     make-vm-deopt-info
     vm-deopt-info-pc
     vm-deopt-info-label
     vm-deopt-info-live-regs
     vm-deopt-info-vreg->preg
     vm-deopt-info-inline-stack
     vm-deopt-info-env
     vm-deopt-info-description
     vm-deopt-frame
     make-vm-deopt-frame
     vm-deopt-frame-pc
     vm-deopt-frame-reason
     vm-deopt-frame-registers
     vm-deopt-frame-physical-registers
     vm-deopt-frame-call-stack
     vm-deopt-frame-closure-env
     vm-deopt-frame-values-list
     vm-deopt-frame-inline-stack
     vm-deopt-frame-info
     vm-binop
   vm-add
   vm-integer-add
   vm-integer-sub
   vm-integer-mul
   vm-integer-mul-high-u
    vm-integer-mul-high-s
    vm-add-checked
    vm-sub-checked
    vm-mul-checked
     vm-float-add
     vm-float-sub
     vm-float-mul
     vm-fma
     vm-float-div
     vm-sqrt
     vm-sin-inst
     vm-cos-inst
     vm-exp-inst
     vm-log-inst
     vm-tan-inst
     vm-asin-inst
     vm-acos-inst
     vm-atan-inst
     vm-sub
   vm-mul
   vm-label
   vm-jump
   vm-jump-zero
   vm-print
   vm-halt
   vm-closure
    vm-call
     vm-tail-call
     vm-call/cc
     vm-call-with-prompt
     vm-abort-to-prompt
      vm-trampoline
     vm-recompile
     vm-ret
    vm-func-ref
    vm-push
    vm-pop
     vm-values
     vm-values-regs
     vm-mv-bind-regs
     vm-vr0
     vm-vr1
     vm-vr2
     vm-vr-count
     vm-mv-bind-regs-count
      vm-values-typep
      vm-nth-value
      vm-load-time-value
      vm-mv-buffer
      vm-mv-count
      vm-load-time-values
      vm-load-time-value-cell
      make-vm-load-time-value-cell
      vm-load-time-value-cell-id
      vm-load-time-value-cell-form
      vm-load-time-value-cell-read-only-p
      vm-load-time-value-cell-resolved-p
      vm-load-time-value-cell-value
      +maximum-multiple-values+
     vm-spread-values
   vm-mv-bind
   vm-dst-regs
   vm-lcm
   vm-gcd
   vm-ash
   vm-rotate
   vm-bswap
   vm-logand
   vm-logior
   vm-logxor
   vm-logeqv
   vm-logtest
   vm-logbitp
   vm-logcount
   vm-integer-length
   vm-min
   vm-max
   vm-rem
   vm-mod
   vm-div
   vm-truncate
   vm-floor-inst
   vm-ceiling-inst
   vm-round-inst
   vm-lognot
    vm-rational
    vm-rationalize
    vm-numerator
    vm-bignump
    vm-floatp
    vm-rationalp
    vm-realp
    vm-complexp
     vm-denominator
     vm-make-array
     vm-make-specialized-array
     vm-specialized-array
     vm-specialized-array-p
     vm-specialized-array-header
     vm-specialized-array-element-type
     vm-specialized-array-length
     vm-specialized-array-storage
     vm-specialized-array-gc-skip-p
     vm-specialized-array-ref
     vm-specialized-array-element-pointer-free-p
     vm-bit-vector-p
     vm-bit-vector-ref
     vm-array-length
    vm-array-reg
    vm-index-reg
    vm-val-reg
     vm-aref
      vm-aset
      vm-prefetch
      vm-prefetch-base-reg
      vm-prefetch-index-reg
       vm-prefetch-scale
       vm-prefetch-offset
       vm-prefetch-locality
       vm-prefetch-kind
       vm-simd-vector-op
       make-vm-simd-vector-op
       vm-simd-vector-op-p
       vm-simd-vector-op-op
       vm-simd-vector-op-dst-array
       vm-simd-vector-op-lhs-array
       vm-simd-vector-op-rhs-array
       vm-simd-vector-op-index-reg
       vm-simd-vector-op-lanes
       vm-simd-vector-op-element-type
       vm-fill
      vm-copy-vector
      vm-vector
    vm-eq
   vm-num-eq
   vm-lt
   vm-gt
   vm-le
   vm-ge
   vm-and
   vm-or
   vm-abs
   vm-inc
   vm-dec
   vm-not
   vm-concatenate
   vm-select
   vm-cons-p
   vm-hash-cons-p
   vm-symbol-p
   vm-number-p
   vm-integer-p
   vm-function-p
   ;; The optimizer's rule tables name instruction types as bare symbols read in
   ;; :CL-CC/OPTIMIZE and compare them with EQ against TYPE-OF. An unexported
   ;; type is therefore a *different* symbol there and can never match, which
   ;; silently disabled the FR-344 producer rules for LISTP/STRINGP/VECTORP and
   ;; the COERCE-TO-* producers. Any instruction type a pass names must be
   ;; exported.
   vm-listp
   vm-stringp
   vm-vectorp
   vm-simple-vector-p
   vm-string-coerce
   vm-coerce-to-string
   vm-coerce-to-vector
   vm-coerce-to-list
    vm-apply
     vm-add-package-local-nickname
     vm-remove-package-local-nickname
     vm-register-function
     vm-declare-forward-reference
     vm-forward-reference-name
    vm-function-registry
   vm-resolve-function
   vm-register-host-bridge
   *vm-host-bridge-functions*
   vm-generic-function-p
   vm-resolve-gf-method
    vm-set-global
    vm-get-global
    vm-set-global-p
    vm-get-global-p
    vm-global-name
    vm-global-vars
   vm-values-list
    vm-establish-handler
    vm-remove-handler
    vm-signal-error
    vm-type-error-condition
    vm-sync-handler-regs
    vm-handler-label
    vm-handler-result-reg
    vm-error-type
    vm-error-reg
    vm-expected-type
    vm-datum-reg
    vm-type-error-values-p
    vm-establish-catch
    vm-throw
   vm-catch-tag-reg
   vm-catch-handler-label
   vm-catch-result-reg
   vm-throw-tag-reg
    vm-throw-value-reg
    vm-prompt-reg
    vm-value-reg
    vm-continuation
    vm-continuation-p
    vm-capture-continuation
    vm-invoke-continuation
    vm-handler-stack
   vm-closure-p
   vm-const-p
   vm-jump-p
   vm-label-p
   vm-ret-p
   vm-add-p
   vm-call-p
   vm-move-p
   vm-neg-p
   vm-array-adjustable-p
   vm-array-has-fill-pointer-p
   vm-input-stream-p
   vm-interactive-stream-p
   vm-next-method-p
   vm-open-stream-p
   vm-output-stream-p
   vm-alpha-char-p
   vm-digit-char-p
   vm-graphic-char-p
   vm-standard-char-p
   vm-lower-case-p
   vm-upper-case-p
   vm-both-case-p
    vm-sub-p
    vm-a
    vm-b
    vm-c
    vm-const-value
   vm-move-dst
   vm-move-src
   vm-lbl-name
    vm-gethash-eq
    vm-gethash-eql
    vm-gethash-equal
    vm-gethash-default
    vm-make-array-fill-pointer
    vm-make-hash-table-test
    vm-typep-type-name
   vm-call-next-method-args-reg
   vm-make-string-char
   vm-closure-rest-stack-alloc-p
   vm-closure-inline-policy
   vm-func-name
    vm-parts
    vm-dst-array-reg
    vm-src-array-reg
    vm-len-reg
   vm-size-reg
    vm-initial-element
    vm-fill-pointer
    vm-fill-pointer-reg
    vm-adjustable
    vm-adjustable-reg
    vm-element-type
    vm-element-type-reg
    vm-displaced-to-reg
    vm-make-array-adjustable
   define-vm-instruction
   define-simple-instruction
   define-vm-unary-instruction
   define-vm-binary-instruction
   define-vm-char-comparison
   rotate-right
   bswap
     vm-intern-pkg
     vm-local-nickname-pkg
     vm-local-nickname-nick
     vm-local-nickname-target
     vm-make-closure-p
    vm-closure-optional-params
    vm-closure-rest-param
    vm-closure-key-params
    vm-closure-inst-dispatch-tag
    vm-func-ref-dispatch-tag

    ;; ── The IR contract for out-of-tree passes (§5-2) ──────────────────────
    ;;
    ;; An optimizer pass or a codegen backend has to be able to name an
    ;; instruction and read its slots -- that is what an IR is for. While these
    ;; were internal, every such pass reached them through CL-CC/VM::, which is
    ;; not something a separate repository can do, and which is why §5-2 makes
    ;; this surface the precondition for extracting optimize and native codegen.
    ;;
    ;; Instruction types and slot accessors only. The VM's own helpers stay
    ;; internal: anything %-prefixed, the CLOS and package emulation, and the
    ;; bare slot names.

    ;; Non-local control and conditions
    vm-push-handler vm-push-handler-type
    vm-pop-handler
    vm-bind-restart vm-restart-name-inst vm-restart-label
    vm-signal vm-error-instruction vm-cerror vm-warn

    ;; Exception tables (zero-cost handler-case)
    make-vm-exception-entry
    vm-exception-entry-start-pc vm-exception-entry-end-pc
    vm-exception-entry-handler-pc vm-exception-entry-condition-type
    vm-exception-entry-result-reg
    vm-register-program-exception-table *vm-program-exception-tables*

    ;; Atomics and fences
    vm-atomic-load vm-aload-addr
    vm-atomic-store vm-astore-addr vm-astore-val
    vm-atomic-cas vm-acas-addr vm-acas-expected vm-acas-newval
    vm-atomic-incf vm-aincf-addr vm-aincf-delta
    vm-atomic-swap
    vm-memory-barrier vm-load-fence vm-store-fence

    ;; Globals
    vm-get-global-dst vm-get-global-name
    vm-set-global-name vm-set-global-src

    ;; CLOS slot access
    vm-slot-read-dst vm-slot-read-obj-reg vm-slot-read-slot-name
    vm-slot-write-obj-reg vm-slot-write-slot-name vm-slot-write-value-reg

    ;; Speculation metadata the optimizer reads and rewrites
    vm-generic-call-metadata vm-ic-type-counters vm-pgo-specializer

    ;; Misc instruction surface
    vm-svref vm-float-sign vm-type-name
    copy-vm-program))
