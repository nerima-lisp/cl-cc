;;;; packages/vm/tests/runtime-stdlib-2-vm-tests.lisp
;;;; FR-787–FR-931: Symbol-existence smoke tests for runtime-stdlib-2 features.
;;;;
;;;; Each test asserts that the listed symbol is fboundp / boundp / a loadable package.
;;;; Groups are by check type; FR labels are encoded in the case label strings.

(in-package :cl-cc/test)



(it-sequential "runtime-stdlib-2-vm-system-loads"
  :timeout
  5
  (expect (asdf:find-system :cl-cc-vm nil) :to-be-truthy))

;;; ─── Multi-symbol FR groups (cannot collapse to single-entry rows) ─────────

(it-sequential "fr-787-string-builder-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/vm::make-string-builder) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::string-builder-append!) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::string-builder-finish) :to-be-truthy))

(it-sequential "fr-788-rope-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/vm::rope) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::rope-concat) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::rope-split) :to-be-truthy)
  (expect (fboundp 'cl-cc/vm::rope-to-string) :to-be-truthy))

(it-sequential "fr-791-logging-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/runtime::log-error) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::log-warn) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::log-info) :to-be-truthy))

(it-sequential "fr-792-metrics-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/runtime::rt-make-counter) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::rt-counter-increment!) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::rt-make-histogram) :to-be-truthy))

(it-sequential "fr-793-perf-counters-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/runtime::rt-perf-init) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::rt-with-perf-counters) :to-be-truthy))

(it-sequential "fr-804-syntax-rules-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/expand::define-syntax) :to-be-truthy)
  (expect (fboundp 'cl-cc/expand::with-gensyms) :to-be-truthy))

(it-sequential "fr-812-c-embedding-exists"
  :timeout
  5
  (expect (fboundp 'cl-cc/runtime::cl-cc-init) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::cl-cc-eval) :to-be-truthy)
  (expect (fboundp 'cl-cc/runtime::cl-cc-call) :to-be-truthy))

;;; ─── Package existence ───────────────────────────────────────────────────────

(it-sequential "fr-package-exists fr-796-lsp"
  (destructuring-bind (pkg) (list :cl-cc/tools/lsp)
    (expect (find-package pkg) :to-be-truthy)))

(it-sequential "fr-package-exists fr-797-dap"
  (destructuring-bind (pkg) (list :cl-cc/tools/dap)
    (expect (find-package pkg) :to-be-truthy)))

(it-sequential "fr-package-exists fr-908-expand"
  (destructuring-bind (pkg) (list :cl-cc/expand)
    (expect (find-package pkg) :to-be-truthy)))

;;; ─── Single-function fboundp checks ─────────────────────────────────────────

(it-sequential "fr-function-exists fr-800-callcc"
  (destructuring-bind (sym) (list 'cl-cc/vm::call-with-current-continuation)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-801-escape"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-call/cc)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-805-once-only"
  (destructuring-bind (sym) (list 'cl-cc/expand::once-only)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-808-shebang"
  (destructuring-bind (sym) (list 'cl-cc/cli::parse-cli-args)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-809-argv"
  (destructuring-bind (sym) (list 'cl-cc/cli::cl-cc-argv)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-813-multi-vm"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-vm-instance)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-816-arena"
  (destructuring-bind (sym) (list 'cl-cc/runtime::make-arena)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-817-object-pool"
  (destructuring-bind (sym) (list 'cl-cc/runtime::make-object-pool)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-821-copy-structure"
  (destructuring-bind (sym) (list 'cl-cc/vm::copy-structure)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-824-transients"
  (destructuring-bind (sym) (list 'cl-cc/vm::transient)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-828-stack-canary"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-stack-canary-check)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-829-overflow"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-check-fixnum-overflow)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-830-taint"
  (destructuring-bind (sym) (list 'cl-cc/vm::taint-mark)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-835-adaptive-runtime"
  (destructuring-bind (sym) (list 'cl-cc/vm::runtime-tuning-report)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-838-sequence-proto"
  (destructuring-bind (sym) (list 'cl-cc/vm::sequence-protocol-p)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-839-iterator"
  (destructuring-bind (sym) (list 'cl-cc/expand::make-iterator)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-842-kahan"
  (destructuring-bind (sym) (list 'cl-cc/vm::kahan-sum)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-843-float-traps"
  (destructuring-bind (sym) (list 'cl-cc/vm::with-float-traps-masked)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-844-extended-prec"
  (destructuring-bind (sym) (list 'cl-cc/vm::dd+)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-847-mutex"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-mutex)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-848-rwlock"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-rwlock)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-851-socket"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-tcp-socket)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-852-dns"
  (destructuring-bind (sym) (list 'cl-cc/vm::dns-resolve)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-853-tls"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-tls-context)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-856-delay"
  (destructuring-bind (sym) (list 'cl-cc/expand::delay)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-857-memoize"
  (destructuring-bind (sym) (list 'cl-cc/expand::memoize)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-865-mv-apply"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-nth-value)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-868-file-position"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-file-position)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-869-mmap"
  (destructuring-bind (sym) (list 'cl-cc/runtime::mmap-file)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-872-cow-string"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-string-copy)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-873-cow-array"
  (destructuring-bind (sym) (list 'cl-cc/vm::copy-on-write-array-p)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-876-segmented-stack"
  (destructuring-bind (sym) (list 'cl-cc/runtime::grow-stack-segment)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-877-copying-stack"
  (destructuring-bind (sym) (list 'cl-cc/runtime::relocate-stack-pointers)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-880-custom-hash"
  (destructuring-bind (sym) (list 'cl-cc/vm::define-hash-table-test)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-881-rehash"
  (destructuring-bind (sym) (list 'cl-cc/vm::hash-table-rehash-size)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-884-floor"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-floor)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-885-ffloor"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-ffloor)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-888-allocate-fast"
  (destructuring-bind (sym) (list 'cl-cc/vm::allocate-instance-vector)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-889-initargs-cache"
  (destructuring-bind (sym) (list 'cl-cc/vm::class-slot-vector-index)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-892-load-time-value"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-load-time-value)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-895-symbol-table"
  (destructuring-bind (sym) (list 'cl-cc/vm::freeze-symbol-table)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-896-package-lock"
  (destructuring-bind (sym) (list 'cl-cc/vm::lock-package)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-902-pgo"
  (destructuring-bind (sym) (list 'cl-cc/vm::save-pgo-data)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-905-tco-unwind"
  (destructuring-bind (sym) (list 'cl-cc/vm::vm-check-dynamic-extent)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-911-riscv"
  (destructuring-bind (sym) (list 'cl-cc/emit::riscv64-emit-function)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-914-delimited-cont"
  (destructuring-bind (sym) (list 'cl-cc/vm::call-with-continuation-prompt)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-917-reproducible"
  (destructuring-bind (sym) (list 'cl-cc/cli::cl-cc-deterministic-build-p)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-923-io-buffering"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-buffered-stream)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-924-special-streams"
  (destructuring-bind (sym) (list 'cl-cc/vm::make-string-input-stream)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-927-pathname"
  (destructuring-bind (sym) (list 'cl-cc/vm::wild-pathname-p)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-930-mop"
  (destructuring-bind (sym) (list 'cl-cc/vm::class-slots)
    (expect (fboundp sym) :to-be-truthy)))

(it-sequential "fr-function-exists fr-931-camuc"
  (destructuring-bind (sym) (list 'cl-cc/vm::compute-applicable-methods-using-classes)
    (expect (fboundp sym) :to-be-truthy)))

;;; ─── Single-variable boundp checks ──────────────────────────────────────────

(it-sequential "fr-variable-exists fr-820-print-circle"
  (destructuring-bind (sym) (list 'cl-cc/vm::*print-circle*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-827-bounds-check"
  (destructuring-bind (sym) (list 'cl-cc/vm::*safety-level*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-833-gc-tuning"
  (destructuring-bind (sym) (list 'cl-cc/runtime::*gc-nursery-size*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-834-jit-thresholds"
  (destructuring-bind (sym) (list 'cl-cc/vm::*jit-tier1-threshold*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-860-numeric-contagion"
  (destructuring-bind (sym) (list 'cl-cc/vm::*numeric-contagion-table*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-861-inline-dispatch"
  (destructuring-bind (sym) (list 'cl-cc/vm::*arith-dispatch-table*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-864-mv-frame"
  (destructuring-bind (sym) (list 'cl-cc/vm::+maximum-multiple-values+)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-899-fasl-demand"
  (destructuring-bind (sym) (list 'cl-cc/vm::*fasl-toc-enabled*)
    (expect (boundp sym) :to-be-truthy)))

(it-sequential "fr-variable-exists fr-920-forward-ref"
  (destructuring-bind (sym) (list 'cl-cc/compile::*forward-reference-patch-table*)
    (expect (boundp sym) :to-be-truthy)))
