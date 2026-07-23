;;;; tests/unit/emit/wasm-ir-tests.lisp — WASM IR Unit Tests
;;;
;;; Tests for wasm-module-ir construction, function/global addition,
;;; register mapping, and WAT name conversion.

(in-package :cl-cc/test)

;;; ─── Struct Construction ──────────────────────────────────────────────────

(it-sequential "wasm-ir-field-default-and-explicit-types"
  (let ((fd (cl-cc/codegen::make-wasm-field))
        (fi (cl-cc/codegen::make-wasm-field :type :i32 :mutability :immutable)))
    (expect (cl-cc/codegen::wasm-field-type fd) :to-be-null)
    (expect (cl-cc/codegen::wasm-field-mutability fd) :to-be :mutable)
    (expect (cl-cc/codegen::wasm-field-type fi) :to-be :i32)
    (expect (cl-cc/codegen::wasm-field-mutability fi) :to-be :immutable)))

(it-sequential "wasm-ir-import-and-export-store-all-fields"
  (let ((imp (cl-cc/codegen::make-wasm-import :module "cl-io" :name "write_char"
                                       :kind :func :type-index 3))
        (exp (cl-cc/codegen::make-wasm-export :name "main" :kind :func :index 5)))
    (expect (cl-cc/codegen::wasm-import-module imp) :to-equal "cl-io")
    (expect (cl-cc/codegen::wasm-import-name imp) :to-equal "write_char")
    (expect (cl-cc/codegen::wasm-import-kind imp) :to-be :func)
    (expect (= 3 (cl-cc/codegen::wasm-import-type-index imp)) :to-be-truthy)
    (expect (cl-cc/codegen::wasm-export-name exp) :to-equal "main")
    (expect (cl-cc/codegen::wasm-export-kind exp) :to-be :func)
    (expect (= 5 (cl-cc/codegen::wasm-export-index exp)) :to-be-truthy)))

(it-sequential "wasm-ir-func-type-stores-params-and-results"
  (let ((ft (cl-cc/codegen::make-wasm-func-type :params '(:i32 :i32) :results '(:i32))))
    (expect (cl-cc/codegen::wasm-func-type-params ft) :to-equal '(:i32 :i32))
    (expect (cl-cc/codegen::wasm-func-type-results ft) :to-equal '(:i32))))

(it-sequential "wasm-ir-struct-type-stores-fields-and-supertype"
  (let* ((f1 (cl-cc/codegen::make-wasm-field :type :i32))
         (f2 (cl-cc/codegen::make-wasm-field :type :eqref))
         (st (cl-cc/codegen::make-wasm-struct-type :fields (list f1 f2) :supertype 0)))
    (expect (= 2 (length (cl-cc/codegen::wasm-struct-type-fields st))) :to-be-truthy)
    (expect (= 0 (cl-cc/codegen::wasm-struct-type-supertype st)) :to-be-truthy)))

(it-sequential "wasm-ir-array-type-stores-element-and-mutability"
  (let ((at (cl-cc/codegen::make-wasm-array-type :element-type :i32 :mutability :immutable)))
    (expect (cl-cc/codegen::wasm-array-type-element-type at) :to-be :i32)
    (expect (cl-cc/codegen::wasm-array-type-mutability at) :to-be :immutable)))

(it-sequential "wasm-ir-type-entry-stores-index-name-and-definition"
  (let* ((ft (cl-cc/codegen::make-wasm-func-type :params '(:i32) :results '(:i32)))
         (te (cl-cc/codegen::make-wasm-type-entry :index 0 :definition ft :wat-name "$add")))
    (expect (= 0 (cl-cc/codegen::wasm-type-entry-index te)) :to-be-truthy)
    (expect (cl-cc/codegen::wasm-type-entry-wat-name te) :to-equal "$add")
    (expect (cl-cc/codegen::wasm-func-type-p (cl-cc/codegen::wasm-type-entry-definition te)) :to-be-truthy)))

;;; ─── Global and Local definitions ────────────────────────────────────────

(it-sequential "wasm-ir-global-def-stores-all-fields"
  (let ((gd (cl-cc/codegen::make-wasm-global-def :index 0 :wat-name "$g_x"
                                          :value-type :i32 :mutability :mutable
                                          :init-value 0)))
    (expect (= 0 (cl-cc/codegen::wasm-global-def-index gd)) :to-be-truthy)
    (expect (cl-cc/codegen::wasm-global-def-wat-name gd) :to-equal "$g_x")
    (expect (cl-cc/codegen::wasm-global-def-value-type gd) :to-be :i32)
    (expect (= 0 (cl-cc/codegen::wasm-global-def-init-value gd)) :to-be-truthy)))

(it-sequential "wasm-ir-local-stores-index-name-and-value-type"
  (let ((loc (cl-cc/codegen::make-wasm-local :index 2 :wat-name "$R0" :value-type :eqref)))
    (expect (= 2 (cl-cc/codegen::wasm-local-index loc)) :to-be-truthy)
    (expect (cl-cc/codegen::wasm-local-wat-name loc) :to-equal "$R0")
    (expect (cl-cc/codegen::wasm-local-value-type loc) :to-be :eqref)))

;;; ─── Module Construction ──────────────────────────────────────────────────

(it-sequential "wasm-ir-empty-module"
  (let ((mod (cl-cc/codegen::make-empty-wasm-module)))
    (expect (cl-cc/codegen::wasm-module-ir-p mod) :to-be-truthy)
    (expect (cl-cc/codegen::wasm-module-types mod) :to-be-null)
    (expect (cl-cc/codegen::wasm-module-functions mod) :to-be-null)
    (expect (cl-cc/codegen::wasm-module-globals mod) :to-be-null)
    (expect (= 0 (cl-cc/codegen::wasm-module-next-func-index mod)) :to-be-truthy)
    (expect (= 0 (cl-cc/codegen::wasm-module-next-global-index mod)) :to-be-truthy)
    (expect (hash-table-p (cl-cc/codegen::wasm-module-global-name-table mod)) :to-be-truthy)))

;;; ─── wasm-module-add-function ─────────────────────────────────────────────

(it-sequential "wasm-ir-add-function-assigns-monotone-indices"
  (let ((mod (cl-cc/codegen::make-empty-wasm-module))
        (fn1 (cl-cc/codegen::make-wasm-function-def :wat-name "$fn_test")))
    (cl-cc/codegen::wasm-module-add-function mod fn1)
    (expect (= 0 (cl-cc/codegen::wasm-func-index fn1)) :to-be-truthy)
    (expect (= 1 (cl-cc/codegen::wasm-module-next-func-index mod)) :to-be-truthy)
    (let ((fn2 (cl-cc/codegen::make-wasm-function-def :wat-name "$fn2")))
      (cl-cc/codegen::wasm-module-add-function mod fn2)
      (expect (= 1 (cl-cc/codegen::wasm-func-index fn2)) :to-be-truthy)
      (expect (= 2 (cl-cc/codegen::wasm-module-next-func-index mod)) :to-be-truthy)
      (expect (= 2 (length (cl-cc/codegen::wasm-module-functions mod))) :to-be-truthy))))

(it-sequential "wasm-ir-add-global-assigns-index-and-registers-name"
  (let ((mod1 (cl-cc/codegen::make-empty-wasm-module))
        (mod2 (cl-cc/codegen::make-empty-wasm-module))
        (mod3 (cl-cc/codegen::make-empty-wasm-module)))
    (let ((gd1 (cl-cc/codegen::make-wasm-global-def :wat-name "$g_x" :value-type :i32)))
      (cl-cc/codegen::wasm-module-add-global mod1 gd1)
      (expect (= 0 (cl-cc/codegen::wasm-global-def-index gd1)) :to-be-truthy)
      (expect (= 1 (cl-cc/codegen::wasm-module-next-global-index mod1)) :to-be-truthy))
    (let ((gd2 (cl-cc/codegen::make-wasm-global-def :wat-name "$g_y" :value-type :i32)))
      (cl-cc/codegen::wasm-module-add-global mod2 gd2)
      (expect (gethash "$g_y" (cl-cc/codegen::wasm-module-global-name-table mod2)) :to-be gd2))
    (let ((gd3 (cl-cc/codegen::make-wasm-global-def :value-type :i32)))
      (cl-cc/codegen::wasm-module-add-global mod3 gd3)
      (expect (= 0 (cl-cc/codegen::wasm-global-def-index gd3)) :to-be-truthy))))

;;; ─── wasm-lisp-name-to-wat-id ────────────────────────────────────────────

(it-sequential "wasm-ir-name-conversion simple-symbol"
  (destructuring-bind (input expected) (list 'foo "foo")
    (expect (cl-cc/codegen::wasm-lisp-name-to-wat-id input) :to-equal expected)))

(it-sequential "wasm-ir-name-conversion with-hyphens"
  (destructuring-bind (input expected) (list 'my-var "my_var")
    (expect (cl-cc/codegen::wasm-lisp-name-to-wat-id input) :to-equal expected)))

(it-sequential "wasm-ir-name-conversion with-asterisks"
  (destructuring-bind (input expected) (list '*features* "_features_")
    (expect (cl-cc/codegen::wasm-lisp-name-to-wat-id input) :to-equal expected)))

(it-sequential "wasm-ir-name-conversion uppercase"
  (destructuring-bind (input expected) (list 'ABC "abc")
    (expect (cl-cc/codegen::wasm-lisp-name-to-wat-id input) :to-equal expected)))

(it-sequential "wasm-ir-name-conversion string-input"
  (destructuring-bind (input expected) (list "Hello" "hello")
    (expect (cl-cc/codegen::wasm-lisp-name-to-wat-id input) :to-equal expected)))

;;; ─── wasm-reg-map ─────────────────────────────────────────────────────────

(it-sequential "wasm-ir-reg-map-param-counts 3-params"
  (destructuring-bind (n-params expected-pc expected-tmp expected-next) (list 3 3 4 5)
    (let ((rm (cl-cc/codegen::make-wasm-reg-map-for-function n-params)))
    (expect (= expected-pc (cl-cc/codegen::wasm-reg-map-pc-index rm)) :to-be-truthy)
    (expect (= expected-tmp (cl-cc/codegen::wasm-reg-map-tmp-index rm)) :to-be-truthy)
    (expect (= expected-next (cl-cc/codegen::wasm-reg-map-next-index rm)) :to-be-truthy))))

(it-sequential "wasm-ir-reg-map-param-counts 0-params"
  (destructuring-bind (n-params expected-pc expected-tmp expected-next) (list 0 0 1 2)
    (let ((rm (cl-cc/codegen::make-wasm-reg-map-for-function n-params)))
    (expect (= expected-pc (cl-cc/codegen::wasm-reg-map-pc-index rm)) :to-be-truthy)
    (expect (= expected-tmp (cl-cc/codegen::wasm-reg-map-tmp-index rm)) :to-be-truthy)
    (expect (= expected-next (cl-cc/codegen::wasm-reg-map-next-index rm)) :to-be-truthy))))

(it-sequential "wasm-ir-global-wat-name-prefixes-correctly"
  (expect (cl-cc/codegen::vm-global-wat-name 'my-var) :to-equal "$g_my_var")
  (expect (cl-cc/codegen::vm-global-wat-name '*features*) :to-equal "$g__features_"))

(it-sequential "wasm-ir-reg-to-local-is-idempotent-and-sequential"
  (let ((rm (cl-cc/codegen::make-wasm-reg-map-for-function 2)))
    (let ((idx (cl-cc/codegen::wasm-reg-to-local rm :R0)))
      (expect (= 4 idx) :to-be-truthy)
      (expect (= 4 (cl-cc/codegen::wasm-reg-to-local rm :R0)) :to-be-truthy)))
  (let ((rm (cl-cc/codegen::make-wasm-reg-map-for-function 0)))
    (expect (= 2 (cl-cc/codegen::wasm-reg-to-local rm :R0)) :to-be-truthy)
    (expect (= 3 (cl-cc/codegen::wasm-reg-to-local rm :R1)) :to-be-truthy)
    (expect (= 4 (cl-cc/codegen::wasm-reg-to-local rm :R2)) :to-be-truthy)
    (expect (= 2 (cl-cc/codegen::wasm-reg-to-local rm :R0)) :to-be-truthy)))
