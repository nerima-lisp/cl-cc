# Architecture

cl-cc is a self-hosting compiler implemented in pure Common Lisp. It compiles
ANSI Common Lisp to a register-based bytecode VM, and generates native code
for x86-64, AArch64, and WebAssembly from there.

## The pipeline

```text
Source (.lisp, .php, .js)
    │
    ▼
Lexer + CST Parser                                        packages/parse
    │   hand-written, incremental
    ▼
Macro Expander                                            packages/expand
    │   defstruct -> defclass, defconstant -> defparameter,
    │   standard macro expansion
    ▼
AST (CLOS defstructs)                                     packages/ast
    │   ast-defun, ast-let, ast-defclass, ...
    │
    ├──► CPS Transform                                    packages/cps
    │      the preferred lowering path for supported forms
    ▼
Codegen -> VM Bytecode                                    packages/codegen
    │   register-based, ~220 instruction types            packages/vm
    │
    ├──► VM Interpreter                                   packages/vm
    │      SBCL-hosted; meta-circular eval
    │
    ├──► MIR / SSA  (Braun et al. 2013)                   packages/mir
    │       │                                             packages/regalloc
    │       ├──► x86-64 assembly text / Mach-O binary     packages/target
    │       ├──► AArch64 assembly text                    packages/emit
    │       └──► WebAssembly (wasm32)
    │
    └──► Bytecode encoder/decoder                         packages/binary
           portable format
```

The PHP and JavaScript frontends join at the AST stage: they produce the same
`ast-*` nodes, so every stage below the parser is shared. See
[Recipes](recipes.md#the-php-frontend).

## Packages

| Package | Role |
| ----------------- | ------------------------------- |
| `cl-cc-bootstrap` | bootstrap foundation |
| `cl-cc-parse`     | lexer and CST parser |
| `cl-cc-expand`    | macro expansion |
| `cl-cc-ast`       | AST definitions |
| `cl-cc-cps`       | CPS transformation |
| `cl-cc-type`      | Hindley–Milner type inference |
| `cl-cc-optimize`  | optimization passes |
| `cl-cc-prolog`    | Prolog optimization backend |
| `cl-cc-mir`       | MIR / SSA representation |
| `cl-cc-regalloc`  | register allocation |
| `cl-cc-codegen`   | code generation |
| `cl-cc-emit`      | native code emission |
| `cl-cc-target`    | target architectures (x86-64, AArch64, Wasm) |
| `cl-cc-binary`    | binary format |
| `cl-cc-vm`        | VM interpreter |
| `cl-cc-runtime`   | runtime (GC, FFI, concurrency) |
| `cl-cc-stdlib`    | standard library |
| `cl-cc-compile`   | compilation pipeline integration |
| `cl-cc-pipeline`  | high-level pipeline |

`packages/sb-mop/`, `packages/sb-pcl/`, and `packages/closer-mop/` provide the
MOP and PCL backends and a Closer to MOP compatibility layer.
`packages/repl/` and `packages/cli/` implement the user-facing entry points.

## Repository layout

```text
cl-cc/
├── flake.nix         Nix flake entry point
├── cl-cc.asd         umbrella ASDF system definition
├── run-tests.lisp    Lisp-level test entry point
├── nix/              Nix modules (build, test, devshell)
├── packages/         compiler subsystems
├── src/              umbrella source, plus the JIT and FFI tooling
├── t/                test suite
└── docs/             this site (src/) and the specification trail (notes/)
```

## Type system

Type inference is Hindley–Milner (Algorithm W) with error-sentinel recovery
paths, so an ill-typed subexpression does not abort inference for the rest of
the program. Union types are narrowed across conditionals.

| Feature | Status |
|---|---|
| Algorithm W inference with error-sentinel recovery | implemented |
| Union type narrowing on conditionals | implemented |
| Parametric types: `(List T)`, `(Option T)` | implemented |
| Typeclasses | partial |

Type checking runs independently of execution via `cl-cc check <file>`.

## Optimization passes

The optimizer is multi-stage, and each stage works on a different
representation.

| Stage | Passes | Package |
| ---- | ---------------------------------------------------------- | ----------------------------- |
| 1 | CSE, constant folding, copy propagation | `cl-cc-optimize` |
| 2 | dead code elimination, strength reduction, jump threading | `cl-cc-optimize` |
| 3 | Prolog rule base: peephole optimization + e-graph rule discovery | `cl-cc-prolog` |
| 4 | e-graph equality saturation | `cl-cc-optimize` |
| 5 | CFG construction, SSA conversion, register allocation | `cl-cc-mir`, `cl-cc-regalloc` |

Conventional bytecode-level optimization runs first. Declarative,
Prolog-driven peephole rules and e-graph equality saturation follow. The final
stage builds a CFG, converts to SSA form (Braun et al. 2013), performs
ML-based register allocation, and emits native code.

## Runtime

### Memory management

- Two-generation GC: a young generation (Cheney semi-space) and an old
  generation (tri-color mark-sweep)
- SATB write barrier, supporting concurrent collection
- Thread-Local Allocation Buffers (TLAB) for fast allocation
- GC safepoints with precise stack maps
- `storage-condition` raised at 80%, 90%, and 95% heap pressure
- Heap-allocated closures, cons cells, and CLOS instances
- Stack overflow guard (`*max-call-stack-depth*`)

### Execution model

- **Inline caches** — monomorphic, polymorphic, and megamorphic call site
  dispatch
- **Type Feedback Vector (TFV)** — runtime type profiling that feeds Tier-1
  compilation

### Subsystems

- **Concurrency** — green threads on a work-stealing scheduler, CSP channels,
  the actor model, STM, futures and promises, structured task groups, fibers
- **Memory reclamation** — Epoch-Based Reclamation (EBR), hazard pointers,
  RCU, Quiescent-State-Based Reclamation (QSBR), MVCC
- **Lock-free structures** — stack, queue, hash map, SPSC ring buffer
- **Synchronization** — mutex, RWLock, semaphore, condition variable,
  barrier, once-call
- **OS layer** — file I/O, process control, signal handling, mmap,
  socket/network (TCP/UDP), io_uring stubs, event loop
- **FFI** — foreign function calling, callback trampolines, native struct
  layout, inline assembly stubs. The VM interpreter provides a
  CFFI-compatible shim backed by the SBCL host.
- **Image** — heap snapshot save and restore, with magic, version, and CRC32
  verification
- **Distributed** — Raft consensus (leader election, log replication), CRDTs
  (GCounter, PNCounter, LWWRegister), cluster membership
- **Observability** — OpenTelemetry spans (JSON export), performance
  counters, structured logging, vector clocks, deadlock detector
- **Atomic operations** — CAS, swap, load, store, incf, memory barriers,
  load/store fences

## Self-hosting

cl-cc is self-hosting in the meta-circular sense. Five properties together
make that true:

1. **Meta-circular `eval`** — `(eval '...)` inside a running program calls
   back into the cl-cc compiler pipeline, not the host SBCL.
2. **Macro expansion via `our-eval`** — `defmacro` and
   `define-compiler-macro` expansion is handled by cl-cc compiling and running
   its own bytecode.
3. **Compiler-in-the-compiler** — cl-cc can compile programs that themselves
   implement parsers, compilers, and evaluators, including CPS transformers
   and stack-machine compilers that mirror cl-cc's own internals.
4. **REPL state persistence** — function, class, and accessor definitions
   persist across `run-string-repl` calls, which is what makes incremental
   REPL-driven development work.
5. **Quasiquote in compiled code** — cl-cc compiles `` ` `` and `,` correctly
   inside `defun` bodies, so it can run its own macro-generating functions —
   the same pattern used throughout `packages/cps/src/cps.lisp`.

The examples below all run under `cl-cc run`.

### A miniature AST compiler

```lisp
(defstruct ast-node kind value children)

(defun parse-expr (sexp)
  (cond
    ((integerp sexp)
     (make-ast-node :kind :lit :value sexp :children nil))
    ((and (consp sexp) (eq (car sexp) '+))
     (make-ast-node :kind :add :value nil
       :children (mapcar #'parse-expr (cdr sexp))))))

(defun eval-ast (node)
  (case (ast-node-kind node)
    (:lit  (ast-node-value node))
    (:add  (apply #'+ (mapcar #'eval-ast (ast-node-children node))))))

(eval-ast (parse-expr '(+ 1 (+ 2 (+ 3 4)))))
;; => 10
```

### A CPS transformer using quasiquotes

This is the actual code from `packages/cps/src/cps.lisp`, compiled by cl-cc
itself:

```lisp
(defun %cps-sexp-binop (op a b k)
  (let ((va (gensym "A")) (vb (gensym "B")))
    (%cps-sexp-node a
      `(lambda (,va)
         ,(%cps-sexp-node b `(lambda (,vb) (funcall ,k (,op ,va ,vb))))))))

(defun %cps-sexp-node (node k)
  (cond
    ((integerp node) `(funcall ,k ,node))
    ((symbolp  node) `(funcall ,k ,node))
    ((consp node)
     (case (car node)
       ((+ - *) (%cps-sexp-binop (car node) (second node) (third node) k))
       (otherwise (error "Unsupported"))))))

(defun cps-transform (expr) `(lambda (k) ,(%cps-sexp-node expr 'k)))

;; The CPS form for (+ 1 2) — each subexpression gets its own continuation:
(cps-transform '(+ 1 2))
;; => (LAMBDA (K)
;;      (FUNCALL (LAMBDA (#:A1) (FUNCALL (LAMBDA (#:B2) (FUNCALL K (+ #:A1 #:B2))) 2)) 1))

(eval (list (cps-transform '(* 6 7)) '(lambda (result) result)))
;; => 42
```

### A stack-machine compiler

cl-cc compiles a program that itself compiles and executes expressions:

```lisp
(defun compile-expr (expr)
  (cond
    ((numberp expr) (list (list 'push expr)))
    ((eq (car expr) '+)
     (append (compile-expr (cadr expr))
             (compile-expr (caddr expr))
             (list '(add))))
    ((eq (car expr) '*)
     (append (compile-expr (cadr expr))
             (compile-expr (caddr expr))
             (list '(mul))))))

(defun execute (program)
  (let ((stack nil))
    (dolist (i program (car stack))
      (case (car i)
        (push (push (cadr i) stack))
        (add  (push (+ (pop stack) (pop stack)) stack))
        (mul  (push (* (pop stack) (pop stack)) stack))))))

(execute (compile-expr '(+ (* 2 3) (* 4 5))))
;; => 26
```

### CPS inside a compiled program

```lisp
(defun fib-cps (n k)
  (if (<= n 1)
      (funcall k n)
      (fib-cps (- n 1)
               (lambda (v1)
                 (fib-cps (- n 2)
                          (lambda (v2)
                            (funcall k (+ v1 v2))))))))

(defun identity (x) x)
(fib-cps 15 #'identity)
;; => 610
```

The self-host pipeline itself lives in
`packages/selfhost/src/pipeline-selfhost.lisp`. For what remains before
cl-cc is free of a host CL entirely, see
[Compatibility](compatibility.md#standalone-binaries).
