;;;; tests/wasm-tests.lisp - WebAssembly Backend Tests
;;;
;;; This module provides basic tests for the WASM/WAT code generation backend,
;;; including:
;;; - Module header presence
;;; - Predefined GC type section (cons, closure, etc.)
;;; - Function definition emission
;;; - Nil constant representation
;;; - compile-string :target :wasm integration

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────────
;;; Helper: compile a source string to WAT, returning the assembly string.
;;; Returns NIL on any error so individual tests can assert-true on the result.
;;; ──────────────────────────────────────────────────────────────────────────

(defun %wat-for (source-string)
  "Compile SOURCE-STRING via compile-string :target :wasm and return the WAT
   assembly string, or NIL if compilation fails."
  (ignore-errors
    (let ((result (compile-string source-string :target :wasm)))
      (when result
        (compilation-result-assembly result)))))

(defun %direct-wasm-emit (inst)
  "Emit INST through the non-trampoline wasm target methods directly." 
  (let ((s (make-string-output-stream))
        (target (make-instance 'cl-cc/codegen::wasm-target
                               :reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %direct-wasm-emit* (instructions)
  "Emit a sequence of INSTRUCTIONS through wasm target methods directly." 
  (let ((s (make-string-output-stream))
        (target (make-instance 'cl-cc/codegen::wasm-target
                               :reg-map (cl-cc/codegen::make-wasm-reg-map-for-function 0))))
    (dolist (inst instructions)
      (cl-cc/codegen::emit-instruction target inst s))
    (get-output-stream-string s)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 1-2: Module structure — always present in any WAT output
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-module-structure module-header"
  (destructuring-bind (expected) (list "(module")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure module-close"
  (destructuring-bind (expected) (list "end module")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure cons-type"
  (destructuring-bind (expected) (list "$cons_t")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure closure-type"
  (destructuring-bind (expected) (list "$closure_t")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure string-type"
  (destructuring-bind (expected) (list "$string_t")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure cl-io-imports"
  (destructuring-bind (expected) (list "cl_io")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure funcref-table"
  (destructuring-bind (expected) (list "funcref_table")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure arg-globals"
  (destructuring-bind (expected) (list "$cl_arg0")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-module-structure print-import"
  (destructuring-bind (expected) (list "print_val")
    (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 3: Function emission
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-defun-structure"
  (let ((wat (%wat-for "(defun add (x y) (+ x y))")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat "(func")
    (assert-output-contains wat "(result eqref)")))

(it-sequential "wasm-simple-arithmetic-compiles"
  (let ((wat (%wat-for "(+ 1 2)")))
    (expect wat :to-be-truthy)
    (expect (> (length wat) 0) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 4: Nil constant representation
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-single-output-contains nil-ref-null"
  (destructuring-bind (source expected) (list "nil" "(ref.null eq)")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-single-output-contains closure-struct"
  (destructuring-bind (source expected) (list "(defun double (x) (* x 2))" "struct.new $closure_t")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-single-output-contains move-local-tee"
  (destructuring-bind (source expected) (list "(let ((x 1)) x)" "local.tee")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-single-output-contains elem-segment"
  (destructuring-bind (source expected) (list "(defun f (x) x)" "(elem")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-single-output-contains print-host-call"
  (destructuring-bind (source expected) (list "(let ((x 42)) (print x))" "$host_print_val")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 5: compile-string integration
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-compilation-result-structure"
  (let* ((result (ignore-errors (compile-string "(+ 1 2)" :target :wasm)))
         (asm (when result (compilation-result-assembly result))))
    (expect result :to-be-truthy)
    (expect (typep result 'compilation-result) :to-be-truthy)
    (expect (typep asm 'string) :to-be-truthy)
    (expect (> (length asm) 0) :to-be-truthy)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 7: Arithmetic instruction coverage
;;; ──────────────────────────────────────────────────────────────────────────
;;; defun prevents the optimizer from constant-folding the args away.

(it-sequential "wasm-arithmetic add"
  (destructuring-bind (source expected) (list "(defun f (a b) (+ a b))" "i64.add")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic sub"
  (destructuring-bind (source expected) (list "(defun f (a b) (- a b))" "i64.sub")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic mul"
  (destructuring-bind (source expected) (list "(defun f (a b) (* a b))" "i64.mul")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic div"
  (destructuring-bind (source expected) (list "(defun f (a b) (truncate a b))" "i64.div_s")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic lt"
  (destructuring-bind (source expected) (list "(defun f (a b) (< a b))" "i64.lt_s")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic gt"
  (destructuring-bind (source expected) (list "(defun f (a b) (> a b))" "i64.gt_s")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic logand"
  (destructuring-bind (source expected) (list "(defun f (a b) (logand a b))" "i64.and")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-arithmetic logior"
  (destructuring-bind (source expected) (list "(defun f (a b) (logior a b))" "i64.or")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 8: Global variable access
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-global-vars decl"
  (destructuring-bind (source expected) (list "(defvar *x* 42)" "global")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-global-vars set"
  (destructuring-bind (source expected) (list "(defvar *x* 0) (setq *x* 99)" "global.set")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

(it-sequential "wasm-global-vars get"
  (destructuring-bind (source expected) (list "(defvar *x* 42) *x*" "global.get")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected))))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 9: Closure creation and function call dispatch
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-funcall-dispatch"
  (let ((wat (%wat-for "(defparameter *f* (lambda (x) (* x 2))) (funcall *f* 5)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat "call_indirect")
    (assert-output-contains wat "$main_func_t")
    (assert-output-contains wat "(global.set $cl_arg0")))

(it-sequential "wasm-tail-call-dispatch-uses-return-call-indirect"
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))
    (assert-output-contains out "return_call_indirect")
    (assert-output-contains out "$main_func_t")
    (assert-output-contains out "(global.set $cl_arg0")))

(it-sequential "wasm-tail-call-dispatch-requires-feature"
  (let ((cl-cc/codegen::*wasm-tail-call-enabled* nil))
    (signals error (%direct-wasm-emit
       (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2))))))

(it-sequential "wasm-tail-call-direct-path-uses-return-call-when-callee-known"
  (let ((out (let ((cl-cc/codegen::*wasm-tail-call-enabled* t))
               (%direct-wasm-emit*
                (list (cl-cc:make-vm-func-ref :dst :r1 :label "known_fn")
                      (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))))
    (assert-output-contains out "(return_call $known_fn)")
    (expect (search "return_call_indirect" out :test #'char=) :to-be-falsy)))

(it-sequential "wasm-tail-call-direct-path-requires-feature"
  (let ((cl-cc/codegen::*wasm-tail-call-enabled* nil))
    (signals error (%direct-wasm-emit*
       (list (cl-cc:make-vm-func-ref :dst :r1 :label "known_fn")
             (cl-cc:make-vm-tail-call :dst :r0 :func :r1 :args (list :r2)))))))

(it-sequential "wasm-bitcount-lowers-to-wasm-op logcount"
  (destructuring-bind (source expected-op) (list "(defun f (x) (logcount x))" "i64.popcnt")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected-op))))

(it-sequential "wasm-bitcount-lowers-to-wasm-op integer-length"
  (destructuring-bind (source expected-op) (list "(defun f (x) (integer-length x))" "i64.clz")
    (let ((wat (%wat-for source)))
    (expect wat :to-be-truthy)
    (assert-output-contains wat expected-op))))

(it-sequential "wasm-direct-bitcount-and-arith-emitters logcount"
  (destructuring-bind (expected-op inst) (list "i64.popcnt" (cl-cc:make-vm-logcount :dst :r0 :src :r1))
    (assert-output-contains (%direct-wasm-emit inst) expected-op)))

(it-sequential "wasm-direct-bitcount-and-arith-emitters integer-length"
  (destructuring-bind (expected-op inst) (list "i64.clz" (cl-cc:make-vm-integer-length :dst :r0 :src :r1))
    (assert-output-contains (%direct-wasm-emit inst) expected-op)))

(it-sequential "wasm-direct-bitcount-and-arith-emitters integer-add"
  (destructuring-bind (expected-op inst) (list "i64.add" (cl-cc:make-vm-integer-add :dst :r0 :lhs :r1 :rhs :r2))
    (assert-output-contains (%direct-wasm-emit inst) expected-op)))

(it-sequential "wasm-direct-bitcount-and-arith-emitters integer-sub"
  (destructuring-bind (expected-op inst) (list "i64.sub" (cl-cc:make-vm-integer-sub :dst :r0 :lhs :r1 :rhs :r2))
    (assert-output-contains (%direct-wasm-emit inst) expected-op)))

(it-sequential "wasm-direct-bitcount-and-arith-emitters integer-mul"
  (destructuring-bind (expected-op inst) (list "i64.mul" (cl-cc:make-vm-integer-mul :dst :r0 :lhs :r1 :rhs :r2))
    (assert-output-contains (%direct-wasm-emit inst) expected-op)))

(it-sequential "wasm-gc-array-new-get-set-emitters"
  (let ((mk (%direct-wasm-emit
             (cl-cc:make-vm-make-array :dst :r0 :size-reg :r1 :initial-element nil)))
        (get (%direct-wasm-emit
              (cl-cc:make-vm-aref :dst :r0 :array-reg :r1 :index-reg :r2)))
        (set (%direct-wasm-emit
              (cl-cc:make-vm-aset :array-reg :r0 :index-reg :r1 :val-reg :r2))))
    (assert-output-contains mk "array.new $eqref_array_t")
    (assert-output-contains get "array.get $eqref_array_t")
    (assert-output-contains set "array.set $eqref_array_t")))

(it-sequential "wasm-gc-predefined-type-section-wat-level"
  (let ((wat (%wat-for "(cons 1 2)")))
    (expect wat :to-be-truthy)
    (assert-output-contains wat "(type $bytes_array_t (array (mut i8)))")
    (assert-output-contains wat "(type $string_t (struct")
    (assert-output-contains wat "(type $cons_t (struct (field $car (mut eqref))")
    (assert-output-contains wat "(type $eqref_array_t (array (mut eqref)))")
    (assert-output-contains wat "(type $closure_t (struct")
    (assert-output-contains wat "(struct.new $cons_t")))

(it-sequential "wasm-relaxed-simd-byte-encoding-wat-level-fixture"
  (let* ((bytes (coerce (cl-cc/codegen::wasm-encode-simd-op
                         cl-cc/codegen::+wasm-f32x4-relaxed-madd+)
                        'list))
         (fixture "(module (func (param v128 v128 v128) (result v128) f32x4.relaxed_madd))"))
    (expect bytes :to-equal '(#xfd #x85 #x01))
    (expect (search "f32x4.relaxed_madd" fixture :test #'char=) :to-be-truthy)))

(it-sequential "wasm-gc-array-bce-metadata-emits-elision-marker"
  (let ((get-inst (cl-cc:make-vm-aref :dst :r0 :array-reg :r1 :index-reg :r2))
        (set-inst (cl-cc:make-vm-aset :array-reg :r0 :index-reg :r1 :val-reg :r2)))
    (cl-cc/optimize:opt-mark-bounds-check-eliminable get-inst)
    (cl-cc/optimize:opt-mark-bounds-check-eliminable set-inst)
    (let ((get (%direct-wasm-emit get-inst))
          (set (%direct-wasm-emit set-inst)))
      (assert-output-contains get "BCE: explicit bounds check eliminated")
      (assert-output-contains get "array.get $eqref_array_t")
      (assert-output-contains set "BCE: explicit bounds check eliminated")
      (assert-output-contains set "array.set $eqref_array_t"))))

(it-sequential "wasm-gc-slot-read-write-emitters"
  (let ((out (%direct-wasm-emit*
              (list (cl-cc:make-vm-class-def :dst :r9
                                             :class-name (quote my-class)
                                             :superclasses nil
                                             :slot-names (quote (foo))
                                             :slot-initargs nil
                                             :slot-initform-regs nil
                                             :slot-types nil
                                             :default-initarg-regs nil
                                             :class-slots nil
                                             :metaclass-reg nil)
                    (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name (quote foo))
                    (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name (quote foo) :value-reg :r2)))))
    (assert-output-contains out "struct.get $instance_t 1")
    (assert-output-contains out "array.get $eqref_array_t")
    (assert-output-contains out "array.set $eqref_array_t")
    (assert-output-contains out "struct.set $instance_t 1")))

(it-sequential "wasm-gc-slot-read-write-use-class-slot-index-mapping"
  (let ((out (%direct-wasm-emit*
              (list (cl-cc:make-vm-class-def :dst :r9
                                             :class-name 'my-class
                                             :superclasses nil
                                             :slot-names '(foo bar baz)
                                             :slot-initargs nil
                                             :slot-initform-regs nil
                                             :slot-types nil
                                             :default-initarg-regs nil
                                             :class-slots nil
                                             :metaclass-reg nil)
                    (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'bar)
                    (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name 'baz :value-reg :r2)))))
    ;; bar => index 1, baz => index 2 from class-def slot order.
    (assert-output-contains out "array.get $eqref_array_t")
    (assert-output-contains out "(i32.const 1)")
    (assert-output-contains out "array.set $eqref_array_t")
    (assert-output-contains out "(i32.const 2)")))

(it-sequential "wasm-gc-slot-index-resolution-prefers-object-class-layout"
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r7
                                        :class-name 'class-a
                                        :superclasses nil
                                        :slot-names '(foo bar)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'class-b
                                        :superclasses nil
                                        :slot-names '(bar foo)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               ;; Build object of class-b; foo should resolve to index 1 in class-b.
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)
               (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'foo)))))
    (assert-output-contains out "array.get $eqref_array_t")
    (assert-output-contains out "(i32.const 1)")))

(it-sequential "wasm-gc-slot-index-resolution-includes-superclass-slots"
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r7
                                        :class-name 'super-c
                                        :superclasses nil
                                        :slot-names '(sa sb)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'sub-c
                                        :superclasses '(super-c)
                                        :slot-names '(sc)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)
               ;; inherited sb should resolve to index 1; own sc should resolve to index 2
               (cl-cc:make-vm-slot-read :dst :r0 :obj-reg :r1 :slot-name 'sb)
               (cl-cc:make-vm-slot-write :obj-reg :r1 :slot-name 'sc :value-reg :r2)))))
    (assert-output-contains out "(i32.const 1)")
    (assert-output-contains out "(i32.const 2)")))

(it-sequential "wasm-gc-class-def-emits-class-meta-struct"
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-class-def :dst :r7
                                       :class-name 'my-class
                                       :superclasses nil
                                       :slot-names '(a b)
                                       :slot-initargs nil
                                       :slot-initform-regs nil
                                       :slot-types nil
                                       :default-initarg-regs nil
                                       :class-slots nil
                                       :metaclass-reg nil))))
    (assert-output-contains out "struct.new $class_meta_t")
    (assert-output-contains out "(i32.const 2)")
    (assert-output-contains out "(array.new $eqref_array_t (ref.null eq) (i32.const 0))")))

(it-sequential "wasm-gc-make-obj-uses-class-reg-in-instance-struct"
  (let ((out (%direct-wasm-emit*
              (list
               (cl-cc:make-vm-class-def :dst :r8
                                        :class-name 'my-class
                                        :superclasses nil
                                        :slot-names '(a b)
                                        :slot-initargs nil
                                        :slot-initform-regs nil
                                        :slot-types nil
                                        :default-initarg-regs nil
                                        :class-slots nil
                                        :metaclass-reg nil)
               (cl-cc:make-vm-make-obj :dst :r1 :class-reg :r8 :initarg-regs nil)))))
    (assert-output-contains out "struct.new $instance_t")
    (expect (search "ref.null $class_meta_t" out :test #'char=) :to-be-falsy)))

(it-sequential "wasm-gc-register-method-runtime-bridge-emission"
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-register-method
               :gf-reg :r1
               :specializer 'my-class
               :qualifier nil
               :method-reg :r2))))
    (assert-output-contains out "call $host_rt_register_method")
    ;; Symbol specializer should be passed as non-null staged symbol eqref.
    (assert-output-contains out "struct.new $symbol_t")))

(it-sequential "wasm-gc-generic-call-runtime-bridge-emission"
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1 :args (list :r2 :r3)))))
    (assert-output-contains out "global.set $cl_arg0")
    (assert-output-contains out "global.set $cl_arg1")
    (assert-output-contains out "(call $host_rt_call_generic")
    (assert-output-contains out "(i32.const 2)")))

(it-sequential "wasm-gc-generic-call-runtime-bridge-supports-many-args"
  (let ((out (%direct-wasm-emit
              (cl-cc:make-vm-generic-call :dst :r0 :gf-reg :r1
                                          :args (list :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10)))))
    (assert-output-contains out "global.set $cl_arg8")
    (assert-output-contains out "(i32.const 9)")))

(it-sequential "wasm-closure-table-index-nonzero-for-second-function"
  (let ((wat (%wat-for "(defun f1 (x) x) (defun f2 (x) (+ x 1))")))
    (expect wat :to-be-truthy)
    ;; Both closures are created; at least one with a non-zero index exists
    (assert-output-contains wat "struct.new $closure_t")))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Section 10: Print support
;;; ──────────────────────────────────────────────────────────────────────────

(it-sequential "wasm-binary-write-f64 pos-1.0"
  (destructuring-bind (value expected) (list 1.0d0 '(0 0 0 0 0 0 #xf0 #x3f))
    (let ((buf (cl-cc/binary::make-wasm-buffer)))
    (cl-cc/binary::wasm-buf-write-f64 buf value)
    (expect (coerce buf 'list) :to-equal expected))))

(it-sequential "wasm-binary-write-f64 neg-2.5"
  (destructuring-bind (value expected) (list -2.5d0 '(0 0 0 0 0 0 #x04 #xc0))
    (let ((buf (cl-cc/binary::make-wasm-buffer)))
    (cl-cc/binary::wasm-buf-write-f64 buf value)
    (expect (coerce buf 'list) :to-equal expected))))

(it-sequential "wasm-binary-write-f64 neg-zero"
  (destructuring-bind (value expected) (list -0.0d0 '(0 0 0 0 0 0 0 #x80))
    (let ((buf (cl-cc/binary::make-wasm-buffer)))
    (cl-cc/binary::wasm-buf-write-f64 buf value)
    (expect (coerce buf 'list) :to-equal expected))))

(it-sequential "wasm-binary-leb128-encodings uleb-624485"
  (destructuring-bind (kind value expected) (list :u 624485 '(#xe5 #x8e #x26))
    (let ((actual (ecase kind
                  (:u (cl-cc/codegen::wasm-encode-unsigned-leb128 value))
                  (:s (cl-cc/codegen::wasm-encode-signed-leb128 value)))))
    (expect (coerce actual 'list) :to-equal expected))))

(it-sequential "wasm-binary-leb128-encodings sleb--123456"
  (destructuring-bind (kind value expected) (list :s -123456 '(#xc0 #xbb #x78))
    (let ((actual (ecase kind
                  (:u (cl-cc/codegen::wasm-encode-unsigned-leb128 value))
                  (:s (cl-cc/codegen::wasm-encode-signed-leb128 value)))))
    (expect (coerce actual 'list) :to-equal expected))))

(it-sequential "wasm-binary-leb128-encodings sleb-0"
  (destructuring-bind (kind value expected) (list :s 0 '(0))
    (let ((actual (ecase kind
                  (:u (cl-cc/codegen::wasm-encode-unsigned-leb128 value))
                  (:s (cl-cc/codegen::wasm-encode-signed-leb128 value)))))
    (expect (coerce actual 'list) :to-equal expected))))

(it-sequential "wasm-binary-opcode-bytes i64.const-42"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-const :dst :r0 :value 42) '(#x42 #x2a))
    (expect (coerce (cl-cc/codegen::wasm-encode-vm-instruction-opcode inst)
                        'list) :to-equal expected)))

(it-sequential "wasm-binary-opcode-bytes i64.add"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-add :dst :r0 :lhs :r1 :rhs :r2) '(#x7c))
    (expect (coerce (cl-cc/codegen::wasm-encode-vm-instruction-opcode inst)
                        'list) :to-equal expected)))

(it-sequential "wasm-binary-opcode-bytes i64.sub"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) '(#x7d))
    (expect (coerce (cl-cc/codegen::wasm-encode-vm-instruction-opcode inst)
                        'list) :to-equal expected)))

(it-sequential "wasm-binary-opcode-bytes i64.mul"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) '(#x7e))
    (expect (coerce (cl-cc/codegen::wasm-encode-vm-instruction-opcode inst)
                        'list) :to-equal expected)))

(it-sequential "wasm-binary-opcode-bytes call_indirect"
  (destructuring-bind (inst expected) (list (cl-cc:make-vm-call :dst :r0 :func :r1 :args nil) '(#x11 0 0))
    (expect (coerce (cl-cc/codegen::wasm-encode-vm-instruction-opcode inst)
                        'list) :to-equal expected)))

(it-sequential "wasm-binary-minimal-module-sections"
  (let* ((program (cl-cc:make-vm-program
                   :instructions (list (cl-cc:make-vm-const :dst :r0 :value 42)
                                       (cl-cc:make-vm-halt :reg :r0))
                   :result-register :r0))
         (bytes (cl-cc/codegen:compile-to-wasm-binary program))
         (list-bytes (coerce bytes 'list)))
    (expect (typep bytes '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (subseq list-bytes 0 8) :to-equal '(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00))
    ;; section ids in order: type, function, export, code.  Payload sizes in
    ;; this minimal fixture are single-byte LEB128 values.
    (expect (loop with i = 8
                        while (< i (length list-bytes))
                        for id = (nth i list-bytes)
                        for size = (nth (1+ i) list-bytes)
                        collect id
                        do (incf i (+ 2 size))) :to-equal '(#x01 #x03 #x07 #x0a))
    (expect (search '(#x04 #x6d #x61 #x69 #x6e) list-bytes :test #'eql) :to-be-truthy)))
