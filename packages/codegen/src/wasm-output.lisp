;;;; packages/codegen/src/wasm-output.lisp - WAT module serialization
;;;
;;; emit-wasm-module: serialize a wasm-module-ir to WAT text.
;;; Binary encoding lives in wasm-binary.lisp.
;;; AOT compilation lives in wasm-aot.lisp.
;;; Debug/DevTools custom sections live in wasm-binary-debug.lisp.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; emit-wasm-module: serialize a wasm-module-ir to WAT text
;;; ─────────────────────────────────────────────────────────────────────────────

(defun emit-wasm-annotation-custom-section (stream key value)
  "Emit a clcc.annotations custom section entry for staged Wasm metadata."
  (%emit-wasm-custom-string stream "clcc.annotations"
                            (format nil "~A=~A" key value)))

(defun emit-wat-low-priority-proposal-helpers (stream)
  "Emit Wave 11-15 feature-gated helper comments/custom sections that do not alter core lowering."
  (when (wasm-wide-arithmetic-feature-enabled-p)
    (format stream "~%  ;; FR-238 helpers: ~A | ~A | ~A | ~A"
            (wasm-i64-add128-wat "(local.get 0)" "(local.get 1)" "(local.get 2)" "(local.get 3)")
            (wasm-i64-sub128-wat "(local.get 0)" "(local.get 1)" "(local.get 2)" "(local.get 3)")
            (wasm-i64-mul-wide-s-wat "(local.get 0)" "(local.get 1)")
            (wasm-i64-mul-wide-u-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-compact-import-section-enabled*
    (emit-wasm-annotation-custom-section stream "FR-240" "compact-import-section metadata enabled; import name trie/compression staged for binary writer"))
  (when *wasm-custom-descriptors-enabled*
    (emit-wasm-annotation-custom-section stream "FR-241" "descriptor generation maps externref slots to WebAssembly.Descriptor metadata"))
  (when *wasm-memory-control-enabled*
    (format stream "~%  ;; FR-243 helper: ~A" (wasm-memory-discard-wat (wasm-memory-const-wat 0) (wasm-memory-const-wat 0))))
  (when *wasm-jit-interface-enabled*
    (format stream "~%  ;; FR-245: JIT interface feedback hook custom section follows")
    (%emit-wasm-custom-string stream "clcc.jit-interface" "{\"feedback\":[\"inline-cache\",\"hot-calls\",\"monomorphic-stubs\"]}"))
  (when *wasm-flexible-vectors-enabled*
    (format stream "~%  ;; FR-246 helpers: ~A | ~A"
            (wasm-flexible-vector-op-wat "add" "(local.get 0)" "(local.get 1)" :width :v128x2)
            (wasm-flexible-vector-op-wat "add" "(local.get 0)" "(local.get 1)" :width :v512)))
  (when (wasm-half-precision-feature-enabled-p)
    (format stream "~%  ;; FR-248 helpers: ~A | ~A | ~A"
            (wasm-f16-binop-wat "add" "(local.get 0)" "(local.get 1)")
            (wasm-f16-load-wat "(local.get 0)")
            (wasm-f16-store-wat "(local.get 0)" "(local.get 1)")))
  (when (wasm-reference-typed-strings-feature-enabled-p)
    (format stream "~%  ;; FR-251 helpers: ~A | ~A"
            (wasm-stringref-length-wat "(local.get 0)")
            (wasm-stringref-get-codeunit-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-startup-snapshots-enabled*
    (format stream "~%  ~A" (wasm-startup-snapshot-comment-wat)))
  (when (wasm-func-bind-feature-enabled-p)
    (format stream "~%  ;; FR-290 helper: ~A" (wasm-func-bind-wat "$main_func_t" "(ref.func $main)" "(ref.null eq)")))
  (when *wasm-wasi-extended-worlds-enabled*
    (emit-wasm-annotation-custom-section stream "FR-296" "WASI extended worlds: wasi:keyvalue, wasi:messaging, wasi:sql"))
  (when (wasm-cfi-feature-enabled-p)
    (emit-wasm-annotation-custom-section stream "FR-261.cfi" "typed call_ref/call_indirect signatures are emitted for indirect calls"))
  (when *wasm-csp-compliant-enabled*
    (emit-wasm-annotation-custom-section stream "FR-261.csp" "no dynamic wasm-unsafe-eval path required for AOT output"))
  (when *wasm-constant-time-enabled*
    (emit-wasm-annotation-custom-section stream "FR-261.constant-time" "constant-time lowering prefers select over data-dependent branches"))
  (when (wasm-coop-coep-feature-enabled-p)
    (emit-wasm-annotation-custom-section stream "FR-297" "deploy with COOP=same-origin and COEP=require-corp for SharedArrayBuffer"))
  (when *wasm-wasi-p2-enabled*
    (format stream "~%  ;; FR-207: WASI Preview 2 worlds enabled: filesystem, sockets, clocks"))
  (when *wasm-wasi-p3-enabled*
    (format stream "~%  ;; FR-257: WASI 0.3 async I/O stubs use suspend/resume around wasi:io/streams"))
  (when *wasm-wasi-worlds-full-enabled*
    (format stream "~%  ;; FR-274: WASI world definitions enabled: wasi:nn, wasi:http, wasi:cli"))
  (when *wasm-stack-switching-enabled*
    (format stream "~%  ;; FR-205 helpers: ~A | ~A | ~A"
            (wasm-cont-new-wat "$main_func_t" "(ref.func $main)")
            (wasm-suspend-wat "$cl_suspend_tag" "(ref.null eq)")
            (wasm-resume-wat "(local.get 0)" "(ref.null eq)")))
  (when *wasm-effect-handlers-enabled*
    (format stream "~%  ;; FR-272 helper: ~A" (wasm-effect-perform-wat "$restart_handler" "(ref.null eq)")))
  (when *wasm-cont-throw-enabled*
    (format stream "~%  ;; FR-301 helper: ~A" (wasm-cont-throw-wat "(local.get 0)" "(local.get 1)")))
  (when *wasm-component-model-enabled*
    (format stream "~%  ;; FR-206: Component Model enabled; WIT type infrastructure custom section follows")
    (%emit-wasm-custom-string stream "clcc.component.wit" "package clcc:runtime; world clcc { export main: func() -> string; }"))
  (when *wasm-component-model-tests-enabled*
    (format stream "~%  ;; FR-319: Component Model test metadata enabled for WIT interface verification")))

(defun emit-wat-deployment-js-glue (stream)
  "Emit browser/deployment JS glue custom sections for low-priority wasm features."
  (when *wasm-service-worker-enabled*
    (%emit-wasm-custom-string
     stream "clcc.service-worker.js"
     "self.addEventListener('install', e => e.waitUntil(caches.open('clcc-wasm').then(c => c.addAll(['./module.wasm']))));\nself.addEventListener('fetch', e => e.respondWith(caches.match(e.request).then(r => r || fetch(e.request))));"))
  (when *wasm-runtime-feature-detection-enabled*
    (%emit-wasm-custom-string
     stream "clcc.feature-detect.js"
     "export async function detectClccWasmFeatures(bytes){ const ok=WebAssembly.validate(bytes); return { mvp: ok, gc: typeof WebAssembly.Global === 'function', threads: typeof SharedArrayBuffer !== 'undefined', exceptions: typeof WebAssembly.Exception === 'function', componentModel: false }; }")))

(defun emit-wasm-module (module stream)
  "Serialize a wasm-module-ir to WAT text format on STREAM.
   Assumes build-all-wasm-functions has already been called so every
   wasm-function-def has a populated :body slot."
  (format stream "(module")
  (format stream "~%  ;; cl-cc generated WASM module (GC proposal)")
  ;; FR-258: Wasm Profiles — declare required features
  (when (wasm-profiles-feature-enabled-p)
    (emit-wasm-profiles-section stream))
  ;; Type section
  (emit-wat-type-section stream)
  ;; Imports
  (let ((*wasm-aot-current-used-imports* (wasm-module-used-host-imports module)))
    (emit-wat-imports stream))
  ;; Exception tags for CL conditions and catch/throw payloads
  (emit-wat-tags module stream)
  (emit-wat-exception-helper stream)
  ;; FR-252: EH v2 support — emit JS exception bridge helpers
  (when (wasm-js-exception-bridge-feature-enabled-p)
    (emit-wat-js-conversion-helpers stream))
  (when (wasm-eh-v2-feature-enabled-p)
    (emit-wat-eh-v2-helper stream))
  ;; Table (size updated by build-all-wasm-functions)
  (emit-wat-table module stream)
  (emit-wat-table64-helpers stream)
  ;; User-defined global variables (from defvar/setq)
  (emit-wat-globals module stream)
  ;; Argument-passing calling convention globals ($cl_arg0..$cl_arg15)
  (emit-wat-call-globals stream)
  ;; Memories (linear memory declarations)
  (emit-wat-memories module stream)
  ;; Low-priority proposal helpers from Waves 11-15.
  (emit-wat-low-priority-proposal-helpers stream)
  (emit-wat-deployment-js-glue stream)
  ;; JS/FFI helper functions and JS glue snippets.
  (emit-wat-js-ffi-helpers stream)
  ;; Host-side Worker bootstrap guidance for SharedArrayBuffer-backed memory.
  (emit-wat-worker-bootstrap stream)
  ;; Functions
  (dolist (func (wasm-module-functions module))
    (emit-wat-function func stream))
  (emit-wat-bigint-wrappers module stream)
  (emit-wat-bigint-js-wrapper-code stream)
  ;; Elem segment: populate funcref table so call_indirect can dispatch
  (emit-wat-elem module stream)
  ;; FR-216: Branch Hinting custom section
  (when (wasm-branch-hints-feature-enabled-p)
    (emit-wasm-branch-hints-section module stream))
  ;; FR-222: DWARF debug info custom sections
  (when (wasm-dwarf-feature-enabled-p)
    (emit-wasm-dwarf-sections module stream))
  ;; FR-223: Source Map reference
  (when (wasm-source-map-enabled-p)
    (emit-wasm-source-map-reference stream))
  ;; FR-242: Extended Name Section for readable DevTools symbols.
  (when (wasm-extended-names-feature-enabled-p)
    (emit-wasm-name-section module stream))
  ;; FR-263/269/318/317/288: browser developer tooling JS helpers.
  (emit-wasm-developer-tooling-sections module stream)
  (format stream "~%) ;; end module~%"))

;;; (Binary encoding is in wasm-binary.lisp.)
;;; (AOT compilation is in wasm-aot.lisp.)
;;; (Debug/DevTools custom sections are in wasm-binary-debug.lisp.)
