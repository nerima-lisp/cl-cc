;;;; packages/javascript/tests/js-runtime-misc-tests.lisp
;;;;
;;;; Unit tests for remaining uncovered runtime functions:
;;;; Promise.race/allSettled/finally/withResolvers/try,
;;;; global isNaN/isFinite, structuredClone, queueMicrotask,
;;;; Iterator.from, Map.groupBy, Set-from-iterable, Proxy,
;;;; Math.sumPrecise, Error.isError, AggregateError, AbortController,
;;;; URL, TypedArray constructor factory, Object.defineProperties,
;;;; Reflect.defineProperty / Object.defineProperty.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-suite)

;;; ─── Local test helpers ──────────────────────────────────────────────────────

(defun %jr-assert-string-props (object expected-props)
  (dolist (prop expected-props)
    (destructuring-bind (key expected) prop
      (assert-string= expected (gethash key object)))))

;;; ─── Module exports ─────────────────────────────────────────────────────────

(deftest js-rt-export-default-registers-module-value
  "export default stores the evaluated value on the module namespace."
  (let ((cl-cc/javascript::*js-module-exports* (cl-cc/javascript::%js-make-object)))
    (let* ((result (cl-cc/javascript::%js-export :default 42))
           (exports (cl-cc/javascript::%js-current-module-exports)))
      (assert-= 42 result)
      (assert-= 42 (cl-cc/javascript::%js-get-prop exports "default")))))

(deftest js-rt-export-declaration-registers-named-value
  "export declarations use parser-provided names to populate the namespace."
  (let ((cl-cc/javascript::*js-module-exports* (cl-cc/javascript::%js-make-object)))
    (let* ((fn (lambda () 7))
           (result (cl-cc/javascript::%js-export :declaration fn nil '("add")))
           (exports (cl-cc/javascript::%js-current-module-exports)))
      (assert-eq fn result)
      (assert-eq fn (gethash "add" exports)))))

(deftest js-rt-export-reexport-records-source-metadata
  "export ... from records unresolved re-export metadata for the module loader."
  (let ((cl-cc/javascript::*js-module-exports* (cl-cc/javascript::%js-make-object)))
    (cl-cc/javascript::%js-export
     :re-export
     (list (list :local "foo" :exported "bar"))
     "./dep.js")
    (let* ((exports (cl-cc/javascript::%js-current-module-exports))
           (reexports (cl-cc/javascript::%js-get-prop exports "__reexports__"))
           (entry (aref reexports 0))
           (specs (cl-cc/javascript::%js-get-prop entry "value"))
           (spec (aref specs 0)))
      (assert-= 1 (length reexports))
      (assert-string= "named" (cl-cc/javascript::%js-get-prop entry "kind"))
      (assert-string= "./dep.js" (cl-cc/javascript::%js-get-prop entry "from"))
      (assert-string= "foo" (cl-cc/javascript::%js-get-prop spec "local"))
      (assert-string= "bar" (cl-cc/javascript::%js-get-prop spec "exported")))))

;;; ─── Promise.race ────────────────────────────────────────────────────────────

(deftest js-rt-promise-race-returns-first
  "Promise.race returns the first element of the promise array unchanged."
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve 1))
         (p2  (cl-cc/javascript::%js-promise-resolve 2))
         (arr (%jr-arr p1 p2))
         (winner (cl-cc/javascript::%js-promise-race arr)))
    (assert-eq p1 winner)))

(deftest js-rt-promise-race-empty-pending
  "Promise.race with empty array returns an unsettled promise."
  (let ((result (cl-cc/javascript::%js-promise-race (%jr-arr))))
    (assert-true (cl-cc/javascript::js-promise-p result))
    (assert-false (cl-cc/javascript::js-promise-settled-p result))))

;;; ─── Promise.allSettled ──────────────────────────────────────────────────────

(deftest js-rt-promise-all-settled-mixed
  "Promise.allSettled returns status objects for both fulfilled and rejected."
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve "ok"))
         (p2  (cl-cc/javascript::%js-promise-reject  "err"))
         (arr (%jr-arr p1 p2))
         (settled (cl-cc/javascript::%js-await
                   (cl-cc/javascript::%js-promise-all-settled arr)))
         (r1  (aref settled 0))
         (r2  (aref settled 1)))
    (assert-string= "fulfilled" (gethash "status" r1))
    (assert-string= "ok"        (gethash "value"  r1))
    (assert-string= "rejected"  (gethash "status" r2))
    (assert-string= "err"       (gethash "reason" r2))))

;;; ─── Promise.finally ─────────────────────────────────────────────────────────

(deftest js-rt-promise-finally-calls-cleanup
  "Promise.finally invokes on-finally and returns the original promise."
  (let* ((called (list nil))
         (p      (cl-cc/javascript::%js-promise-resolve 42))
         (result (cl-cc/javascript::%js-promise-finally
                  p (lambda () (setf (car called) t)))))
    (assert-true (car called))
    (assert-eq p result)))

;;; ─── Promise.withResolvers ───────────────────────────────────────────────────

(deftest js-rt-promise-with-resolvers
  "Promise.withResolvers returns an object with promise, resolve, reject."
  (let* ((obj     (cl-cc/javascript::%js-promise-with-resolvers))
         (promise  (gethash "promise" obj))
         (resolve  (gethash "resolve" obj)))
    (assert-true (cl-cc/javascript::js-promise-p promise))
    (assert-true (functionp resolve))
    (funcall resolve 99)
    (assert-= 99 (cl-cc/javascript::%js-await promise))))

;;; ─── Promise.try ─────────────────────────────────────────────────────────────

(deftest js-rt-promise-try-success
  "Promise.try wraps a synchronous result in a fulfilled promise."
  (let* ((result (cl-cc/javascript::%js-promise-try (lambda () 7))))
    (assert-= 7 (cl-cc/javascript::%js-await result))))

;;; ─── Global isNaN / isFinite (with coercion) ─────────────────────────────────

(deftest-each js-rt-global-is-nan
  "Global isNaN coerces its argument before checking (unlike Number.isNaN)."
  :cases (("number-nan"  cl-cc/javascript::*js-nan-float*  t)
          ("string-nan"  "NaN"                              t)
          ("string-num"  "42"                               nil)
          ("integer"     5                                  nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-is-nan val)))

(deftest-each js-rt-global-is-finite
  "Global isFinite coerces its argument (unlike Number.isFinite)."
  :cases (("integer"   42      t)
          ("string-n"  "3.14"  t)
          ("nan"       cl-cc/javascript::*js-nan-float*  nil)
          ("inf"       cl-cc/javascript::*js-inf-float*  nil))
  (val expected)
  (assert-equal expected (cl-cc/javascript::%js-is-finite val)))

;;; ─── structuredClone ─────────────────────────────────────────────────────────

(deftest js-rt-structured-clone
  "%js-structured-clone deep-clones an object (delegates to %js-deep-clone)."
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-structured-clone orig)))
    (assert-false (eq orig clone))
    (assert-= 10 (gethash "x" clone))))

;;; ─── queueMicrotask / browser timers ─────────────────────────────────────────

(deftest js-rt-queue-microtask-runs-fn
  "%js-queue-microtask runs its callback synchronously and returns +js-undefined+."
  (let ((called (list nil)))
    (let ((ret (cl-cc/javascript::%js-queue-microtask
                (lambda () (setf (car called) t)))))
      (assert-true (car called))
      (assert-eq cl-cc/javascript::+js-undefined+ ret))))

(deftest-each js-rt-browser-timer-stubs-absent
  "Browser timer stubs are not exposed as builtins."
  :cases (("setTimeout" "setTimeout")
          ("setInterval" "setInterval")
          ("clearTimeout" "clearTimeout")
          ("clearInterval" "clearInterval"))
  (name)
  (assert-false (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)))
  (assert-false (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=))
  (assert-false (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*))))

;;; ─── Dynamic code stubs ─────────────────────────────────────────────────────

(deftest-each js-rt-dynamic-code-stubs-absent
  "Dynamic code execution stubs are not exposed as builtins."
  :cases (("eval" "eval")
          ("Function" "Function"))
  (name)
  (assert-false (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)))
  (assert-false (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=))
  (assert-false (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*))))

;;; ─── Iterator.from ───────────────────────────────────────────────────────────

(deftest js-rt-iterator-from-array
  "%js-iterator-from-iterable wraps an array in a generator iterator."
  (let* ((arr  (%jr-arr 1 2 3))
         (iter (cl-cc/javascript::%js-iterator-from-iterable arr))
         (acc  nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '(3 2 1) acc)))

(deftest js-rt-iterator-from-set
  "%js-iterator-from-iterable normalizes Set values to an iterator object."
  (let* ((set (cl-cc/javascript::%js-make-set))
         (_   (cl-cc/javascript::%js-set-add set 3))
         (_   (cl-cc/javascript::%js-set-add set 4))
         (iter (cl-cc/javascript::%js-iterator-from-iterable set))
         (acc nil))
    (declare (ignore _))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '(4 3) acc)))

(deftest js-rt-iterator-from-map
  "%js-iterator-from-iterable normalizes Map values to an entries iterator."
  (let* ((map (cl-cc/javascript::%js-make-map))
         (_1  (cl-cc/javascript::%js-map-set map "a" 1))
         (_2  (cl-cc/javascript::%js-map-set map "b" 2))
         (iter (cl-cc/javascript::%js-iterator-from-iterable map))
         (acc nil))
    (declare (ignore _1 _2))
    (cl-cc/javascript::%js-for-of iter
                                  (lambda (entry)
                                    (push (%jr-list entry) acc)))
    (assert-equal '(("b" 2) ("a" 1)) acc)))

(deftest js-rt-iterator-from-plain-next
  "%js-iterator-from-iterable accepts plain iterator objects with next()."
  (let* ((step 0)
         (iterable
           (cl-cc/javascript::%js-make-object
            "next" (lambda ()
                     (prog1
                         (case step
                           (0 (cl-cc/javascript::%js-make-object "value" "first" "done" nil))
                           (1 (cl-cc/javascript::%js-make-object "value" "second" "done" nil))
                           (t (cl-cc/javascript::%js-make-object "value" cl-cc/javascript::+js-undefined+ "done" t)))
                       (incf step)))))
         (iter (cl-cc/javascript::%js-iterator-from-iterable iterable))
         (acc nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '("second" "first") acc)))

(deftest js-rt-iterator-from-@@iterator
  "%js-iterator-from-iterable calls @@iterator when present on a plain object."
  (let* ((iterable
           (cl-cc/javascript::%js-make-object
            "@@iterator" (lambda ()
                           (cl-cc/javascript::%js-vec-to-iter (%jr-arr 8 9)))))
         (iter (cl-cc/javascript::%js-iterator-from-iterable iterable))
         (acc nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '(9 8) acc)))

(deftest js-rt-iterator-from-string
  "%js-iterator-from-iterable wraps strings as iterators over one-char strings."
  (let* ((iter (cl-cc/javascript::%js-iterator-from-iterable "ab"))
         (acc nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (assert-equal '("b" "a") acc)))

(deftest js-rt-iterator-from-non-iterable
  "%js-iterator-from-iterable returns an already-done iterator for non-iterables."
  (let ((iter (cl-cc/javascript::%js-iterator-from-iterable
               (cl-cc/javascript::%js-make-object))))
    (multiple-value-bind (value done) (cl-cc/javascript::%js-iter-next iter)
      (assert-eq cl-cc/javascript::+js-undefined+ value)
      (assert-true done))))

;;; ─── Map.groupBy ─────────────────────────────────────────────────────────────

(deftest js-rt-map-group-by
  "%js-map-group-by groups elements into a Map by key-fn result."
  (let* ((items  (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-map-group-by
                  items (lambda (x) (if (evenp x) "even" "odd"))))
         (evens  (cl-cc/javascript::%js-map-get result "even"))
         (odds   (cl-cc/javascript::%js-map-get result "odd")))
    (assert-= 2 (length evens))
    (assert-= 2 (length odds))))

;;; ─── Set-from-iterable ───────────────────────────────────────────────────────

(deftest js-rt-make-set-from-iterable
  "%js-make-set-from-iterable seeds a new Set from an array."
  (let* ((arr (cl-cc/javascript::%js-make-array 1 2 3 2))
         (s   (cl-cc/javascript::%js-make-set-from-iterable arr)))
    (assert-true  (cl-cc/javascript::%js-set-has s 1))
    (assert-true  (cl-cc/javascript::%js-set-has s 3))
    (assert-false (cl-cc/javascript::%js-set-has s 9))))

;;; ─── Proxy ───────────────────────────────────────────────────────────────────

(deftest js-rt-make-proxy-object
  "%js-make-proxy-object stores target/handler and marks a Proxy wrapper."
  (let* ((target  (cl-cc/javascript::%js-make-object "x" 1))
         (handler (cl-cc/javascript::%js-make-object))
         (proxy   (cl-cc/javascript::%js-make-proxy-object target handler)))
    (assert-eq target  (gethash "__proxy-target__"  proxy))
    (assert-eq handler (gethash "__proxy-handler__" proxy))
    (assert-true (cl-cc/javascript::%js-proxy-object-p proxy))))

(deftest js-rt-proxy-property-traps
  "Proxy get/set/has/deleteProperty traps route ordinary property operations."
  (let* ((target (cl-cc/javascript::%js-make-object "x" 1))
         (deleted nil)
         (handler
           (cl-cc/javascript::%js-make-object
             "get" (lambda (target key receiver)
                     (declare (ignore receiver))
                     (concatenate 'string "trap-" key "-"
                                  (cl-cc/javascript::%js-to-string
                                    (gethash key target cl-cc/javascript::+js-undefined+))))
             "set" (lambda (target key value receiver)
                     (declare (ignore receiver))
                     (setf (gethash key target) value)
                     t)
             "has" (lambda (target key)
                     (declare (ignore target))
                     (string= key "visible"))
             "deleteProperty" (lambda (target key)
                                (setf deleted key)
                                (remhash key target)
                                t)))
         (proxy (cl-cc/javascript::%js-make-proxy-object target handler)))
    (assert-string= "trap-x-1" (cl-cc/javascript::%js-get-prop proxy "x"))
    (assert-= 7 (cl-cc/javascript::%js-set-prop proxy "y" 7))
    (assert-= 7 (gethash "y" target))
    (assert-true (cl-cc/javascript::%js-in "visible" proxy))
    (assert-false (cl-cc/javascript::%js-in "x" proxy))
    (assert-true (cl-cc/javascript::%js-delete proxy "x"))
    (assert-string= "x" deleted)
    (assert-false (nth-value 1 (gethash "x" target)))))

(deftest js-rt-proxy-falls-back-without-traps
  "Proxy operations fall back to the target when a trap is absent."
  (let* ((target (cl-cc/javascript::%js-make-object "x" 1))
         (proxy (cl-cc/javascript::%js-make-proxy-object
                 target (cl-cc/javascript::%js-make-object))))
    (multiple-value-bind (value trapped)
        (cl-cc/javascript::%js-proxy-call-trap proxy "get" target "x" proxy)
      (assert-eq cl-cc/javascript::+js-undefined+ value)
      (assert-false trapped))
    (assert-= 1 (cl-cc/javascript::%js-get-prop proxy "x"))
    (assert-= 2 (cl-cc/javascript::%js-set-prop proxy "y" 2))
    (assert-= 2 (gethash "y" target))
    (assert-true (cl-cc/javascript::%js-in "x" proxy))
    (assert-true (cl-cc/javascript::%js-delete proxy "x"))
    (assert-false (nth-value 1 (gethash "x" target)))))

(deftest js-rt-proxy-reflect-and-object-traps
  "Proxy traps route Reflect and Object descriptor/enumeration helpers."
  (let* ((target (cl-cc/javascript::%js-make-object "a" 1))
         (handler
           (cl-cc/javascript::%js-make-object
             "get" (lambda (target key receiver)
                     (declare (ignore target receiver))
                     (if (string= key "b") 20 cl-cc/javascript::+js-undefined+))
             "set" (lambda (target key value receiver)
                     (declare (ignore receiver))
                     (setf (gethash key target) value)
                     t)
             "ownKeys" (lambda (target)
                         (declare (ignore target))
                         (%jr-arr "b" "c"))
             "getOwnPropertyDescriptor"
             (lambda (target key)
               (declare (ignore target))
               (if (or (string= key "b") (string= key "c"))
                   (cl-cc/javascript::%js-make-object
                     "value" key "writable" t "enumerable" t "configurable" t)
                   cl-cc/javascript::+js-undefined+))
             "defineProperty"
             (lambda (target key descriptor)
               (setf (gethash key target) (gethash "value" descriptor))
               t)))
         (proxy (cl-cc/javascript::%js-make-proxy-object target handler)))
    (assert-= 20 (cl-cc/javascript::%js-reflect-get proxy "b"))
    (assert-true (cl-cc/javascript::%js-reflect-set proxy "z" 99))
    (assert-= 99 (gethash "z" target))
    (assert-true
      (cl-cc/javascript::%js-reflect-define-property
        proxy "d" (cl-cc/javascript::%js-make-object "value" 44)))
    (assert-= 44 (gethash "d" target))
    (let ((keys (cl-cc/javascript::%js-object-keys proxy)))
      (assert-= 2 (length keys))
      (assert-string= "b" (aref keys 0))
      (assert-string= "c" (aref keys 1)))
    (let ((desc (cl-cc/javascript::%js-object-get-own-property-descriptor proxy "b")))
      (assert-string= "b" (gethash "value" desc)))
    (let ((desc (cl-cc/javascript::%js-object-get-own-property-descriptor proxy "c")))
      (assert-string= "c" (gethash "value" desc)))))

;;; ─── Math.sumPrecise (ES2026) ────────────────────────────────────────────────

(deftest js-rt-math-sum-precise
  "%js-math-sum-precise sums a numeric iterable to double-float."
  (let ((result (cl-cc/javascript::%js-math-sum-precise (%jr-arr 1 2 3 4))))
    (assert-= 10.0d0 result)))

;;; ─── Error.isError (ES2026) ──────────────────────────────────────────────────

(deftest js-rt-error-is-error
  "%js-error-is-error returns truthy for Error-like objects, nil for non-errors."
  (let ((with-message (cl-cc/javascript::%js-make-error-instance
                       cl-cc/javascript::*js-error-class* "oops"))
        (plain-obj    (cl-cc/javascript::%js-make-object "x" 1)))
    (assert-true  (cl-cc/javascript::%js-error-is-error with-message))
    (assert-false (cl-cc/javascript::%js-error-is-error plain-obj))
    (assert-false (cl-cc/javascript::%js-error-is-error "error"))))

;;; ─── AggregateError ──────────────────────────────────────────────────────────

(deftest js-rt-make-aggregate-error
  "%js-make-aggregate-error returns a real AggregateError instance."
  (let* ((errors (%jr-arr "e1" "e2"))
         (cause  (cl-cc/javascript::%js-make-object "code" "root"))
         (opts   (cl-cc/javascript::%js-make-object "cause" cause))
         (agg    (cl-cc/javascript::%js-make-aggregate-error errors "multiple" opts)))
    (assert-string= "AggregateError" (gethash "name"    agg))
    (assert-string= "multiple"       (gethash "message" agg))
    (assert-string= "AggregateError: multiple" (gethash "stack" agg))
    (assert-eq errors                (gethash "errors"  agg))
    (assert-eq cause                 (gethash "cause"   agg))
    (assert-true (cl-cc/javascript::%js-instanceof
                  agg cl-cc/javascript::*js-aggregate-error-class*))
    (assert-true (cl-cc/javascript::%js-instanceof
                  agg cl-cc/javascript::*js-error-class*))))

;;; ─── AbortController ─────────────────────────────────────────────────────────

(deftest js-rt-abort-controller
  "%js-make-abort-controller produces a signal/abort pair."
  (let* ((ctrl   (cl-cc/javascript::%js-make-abort-controller))
         (sig    (gethash "signal" ctrl))
         (abort  (gethash "abort"  ctrl)))
    (assert-false (gethash "aborted" sig))
    (funcall abort "reason")
    (assert-true  (gethash "aborted" sig))
    (assert-string= "reason" (gethash "reason" sig))))

(deftest js-rt-abort-controller-dispatches-once
  "AbortController.abort aborts once and synchronously dispatches abort listeners."
  (let* ((ctrl   (cl-cc/javascript::%js-make-abort-controller))
         (sig    (gethash "signal" ctrl))
         (abort  (gethash "abort" ctrl))
         (onabort-calls 0)
         (listener-calls 0))
    (setf (gethash "onabort" sig)
          (lambda (event)
            (incf onabort-calls)
            (assert-string= "abort" (gethash "type" event))
            (assert-eq sig (gethash "target" event))))
    (funcall (gethash "addEventListener" sig)
             "abort"
             (lambda (event)
               (incf listener-calls)
               (assert-string= "abort" (gethash "type" event))
               (assert-eq sig (gethash "currentTarget" event))))
    (funcall abort "first")
    (funcall abort "second")
    (assert-= 1 onabort-calls)
    (assert-= 1 listener-calls)
    (assert-string= "first" (gethash "reason" sig))))

(deftest js-rt-abort-signal-remove-event-listener
  "AbortSignal.removeEventListener prevents a removed abort listener from firing."
  (let* ((ctrl  (cl-cc/javascript::%js-make-abort-controller))
         (sig   (gethash "signal" ctrl))
         (calls 0)
         (listener (lambda (&rest _) (declare (ignore _)) (incf calls))))
    (funcall (gethash "addEventListener" sig) "abort" listener)
    (funcall (gethash "removeEventListener" sig) "abort" listener)
    (funcall (gethash "abort" ctrl) "done")
    (assert-= 0 calls)))

(deftest js-rt-abort-signal-throw-if-aborted
  "AbortSignal.throwIfAborted throws the stored reason."
  (let ((sig (cl-cc/javascript::%js-abort-signal-aborted "boom")))
    (assert-true (gethash "aborted" sig))
    (assert-string= "boom" (gethash "reason" sig))
    (handler-case
        (progn
          (funcall (gethash "throwIfAborted" sig))
          (%fail-test "throwIfAborted did not signal js-exception"))
      (cl-cc/javascript:js-exception (c)
        (assert-string= "boom" (cl-cc/javascript:js-exception-value c))))))

(deftest js-rt-abort-signal-static-helpers
  "AbortSignal static helpers create aborted signals and compose input signals."
  (let* ((ctor    (cl-cc/javascript::%js-make-abort-signal-constructor))
         (aborted (funcall (gethash "abort" ctor) "done"))
         (timeout (funcall (gethash "timeout" ctor) 5))
         (reason  (gethash "reason" timeout))
         (source  (gethash "signal" (cl-cc/javascript::%js-make-abort-controller)))
         (combo   (funcall (gethash "any" ctor) (%jr-arr source))))
    (assert-true (gethash "aborted" aborted))
    (assert-string= "done" (gethash "reason" aborted))
    (assert-true (gethash "aborted" timeout))
    (assert-string= "TimeoutError" (gethash "name" reason))
    (assert-false (gethash "aborted" combo))
    (cl-cc/javascript::%js-abort-signal-abort source "input")
    (assert-true (gethash "aborted" combo))
    (assert-string= "input" (gethash "reason" combo))))

;;; ─── URL ─────────────────────────────────────────────────────────────────────

(deftest js-rt-make-url-parses-components
  "%js-make-url parses common absolute URL components."
  (let ((url (cl-cc/javascript::%js-make-url
              "https://example.com:8443/path/to?q=1#frag")))
    (%jr-assert-string-props
     url
     '(("href" "https://example.com:8443/path/to?q=1#frag")
       ("protocol" "https:")
       ("host" "example.com:8443")
       ("hostname" "example.com")
       ("port" "8443")
       ("origin" "https://example.com:8443")
       ("pathname" "/path/to")
       ("search" "?q=1")
       ("hash" "#frag")))
    (assert-string= "https://example.com:8443/path/to?q=1#frag"
                    (funcall (gethash "toString" url)))))

(deftest js-rt-make-url-resolves-base-relative-path
  "%js-make-url resolves a relative path against a base URL string."
  (let ((url (cl-cc/javascript::%js-make-url
              "child?x=1"
              "https://example.com/a/b/index.html")))
    (%jr-assert-string-props
     url
     '(("href" "https://example.com/a/b/child?x=1")
       ("pathname" "/a/b/child")
       ("search" "?x=1")))))

(deftest js-rt-make-url-normalizes-relative-dot-segments
  "%js-make-url normalizes dot segments when resolving against a base URL."
  (let ((url (cl-cc/javascript::%js-make-url
              "../c/./d/?x=1"
              "https://example.com/a/b/index.html")))
    (%jr-assert-string-props
     url
     '(("href" "https://example.com/a/c/d/?x=1")
       ("pathname" "/a/c/d/")
       ("search" "?x=1")))))

(deftest js-rt-make-url-resolves-query-and-hash-only-relative
  "%js-make-url keeps the base path for query-only and hash-only relative URLs."
  (let ((query-url (cl-cc/javascript::%js-make-url
                    "?q=2"
                    "https://example.com/a/b/index.html?old=1#frag"))
        (hash-url (cl-cc/javascript::%js-make-url
                   "#next"
                   "https://example.com/a/b/index.html?old=1#frag")))
    (%jr-assert-string-props
     query-url
     '(("href" "https://example.com/a/b/index.html?q=2")
       ("pathname" "/a/b/index.html")
       ("search" "?q=2")))
    (%jr-assert-string-props
     hash-url
     '(("href" "https://example.com/a/b/index.html?old=1#next")
       ("pathname" "/a/b/index.html")
       ("search" "?old=1")
       ("hash" "#next")))))

;;; ─── TypedArray constructor factory ─────────────────────────────────────────

(deftest js-rt-typed-array-ctor-factory
  "%js-make-typed-array-ctor returns a lambda that creates a TypedArray."
  (let* ((ctor (cl-cc/javascript::%js-make-typed-array-ctor "Int32Array"))
         (ta   (funcall ctor 3)))
    (assert-true (cl-cc/javascript::js-typed-array-p ta))
    (assert-= 3 (cl-cc/javascript::js-ta-length ta))))

;;; ─── Object.defineProperties ─────────────────────────────────────────────────

(deftest js-rt-object-define-properties
  "%js-object-define-properties sets multiple keys from a descriptor map."
  (let* ((obj   (cl-cc/javascript::%js-make-object))
         (descs (cl-cc/javascript::%js-make-object
                 "a" (cl-cc/javascript::%js-make-object "value" 1)
                 "b" (cl-cc/javascript::%js-make-object "value" 2))))
    (cl-cc/javascript::%js-object-define-properties obj descs)
    (assert-= 1 (gethash "a" obj))
    (assert-= 2 (gethash "b" obj))))

(deftest js-rt-object-define-properties-ignores-non-objects
  "%js-object-define-properties ignores non-object props and non-object descriptors."
  (let* ((obj (cl-cc/javascript::%js-make-object "keep" 1))
         (mixed (cl-cc/javascript::%js-make-object
                 "ok" (cl-cc/javascript::%js-make-object "value" 2)
                 "skip" 9)))
    (cl-cc/javascript::%js-object-define-properties obj (%jr-arr "bad"))
    (cl-cc/javascript::%js-object-define-properties obj mixed)
    (assert-= 1 (gethash "keep" obj))
    (assert-= 2 (gethash "ok" obj))
    (assert-false (nth-value 1 (gethash "skip" obj)))))

;;; ─── Reflect.defineProperty / Object.defineProperty ─────────────────────────

(deftest js-rt-reflect-define-property
  "%js-reflect-define-property writes a value descriptor and returns t."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 42))
         (ret  (cl-cc/javascript::%js-reflect-define-property obj "x" desc)))
    (assert-true ret)
    (assert-= 42 (gethash "x" obj))))

(deftest js-rt-object-define-property-value
  "%js-object-define-property with a value descriptor stores the value."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 99)))
    (let ((ret (cl-cc/javascript::%js-object-define-property obj "n" desc)))
      (assert-eq obj ret)
      (assert-= 99 (gethash "n" obj)))))

(deftest js-rt-object-define-property-getter
  "%js-object-define-property with a get descriptor stores under __get_KEY slot."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (getter (lambda () 7))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "prop" desc)
    (assert-eq getter (gethash "__get_prop" obj))))

(deftest js-rt-object-get-own-property-descriptor-accessor
  "%js-object-get-own-property-descriptor returns accessor descriptor slots."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 7))
         (setter (lambda (value) value))
         (desc (cl-cc/javascript::%js-make-object "get" getter "set" setter)))
    (cl-cc/javascript::%js-object-define-property obj "prop" desc)
    (let ((actual (cl-cc/javascript::%js-object-get-own-property-descriptor obj "prop")))
      (assert-eq getter (gethash "get" actual))
      (assert-eq setter (gethash "set" actual))
      (assert-true (gethash "enumerable" actual))
      (assert-true (gethash "configurable" actual))
      (assert-false (nth-value 1 (gethash "value" actual))))))

(deftest js-rt-object-get-own-property-descriptors-includes-accessor-property
  "%js-object-get-own-property-descriptors exposes accessor slots by property name."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 10))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "computed" desc)
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (actual (gethash "computed" descs)))
      (assert-eq getter (gethash "get" actual))
      (assert-false (nth-value 1 (gethash "__get_computed" descs))))))

(deftest js-rt-object-get-own-property-descriptor-reflects-object-flags
  "%js-object-get-own-property-descriptor reflects simplified seal/freeze flags."
  (let* ((sealed (cl-cc/javascript::%js-make-object "a" 1))
         (frozen (cl-cc/javascript::%js-make-object "b" 2)))
    (cl-cc/javascript::%js-object-seal sealed)
    (cl-cc/javascript::%js-object-freeze frozen)
    (let ((sealed-desc (cl-cc/javascript::%js-object-get-own-property-descriptor sealed "a"))
          (frozen-desc (cl-cc/javascript::%js-object-get-own-property-descriptor frozen "b")))
      (assert-false (gethash "configurable" sealed-desc))
      (assert-true (gethash "writable" sealed-desc))
      (assert-false (gethash "configurable" frozen-desc))
      (assert-false (gethash "writable" frozen-desc)))))

(deftest js-rt-reflect-define-property-respects-prevent-extensions
  "%js-reflect-define-property returns nil when adding to a non-extensible object."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 10)))
    (cl-cc/javascript::%js-object-prevent-extensions obj)
    (assert-false (cl-cc/javascript::%js-reflect-define-property obj "blocked" desc))
    (assert-false (nth-value 1 (gethash "blocked" obj)))))

(deftest js-rt-object-define-property-signals-on-prevent-extensions
  "%js-object-define-property signals when adding to a non-extensible object."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 10)))
    (cl-cc/javascript::%js-object-prevent-extensions obj)
    (assert-signals error
      (cl-cc/javascript::%js-object-define-property obj "blocked" desc))
    (assert-false (nth-value 1 (gethash "blocked" obj)))))

(deftest js-rt-reflect-define-property-rejects-frozen-redefine
  "%js-reflect-define-property returns nil when redefining a frozen object."
  (let* ((obj (cl-cc/javascript::%js-make-object "locked" 1))
         (desc (cl-cc/javascript::%js-make-object "value" 2)))
    (cl-cc/javascript::%js-object-freeze obj)
    (assert-false (cl-cc/javascript::%js-reflect-define-property obj "locked" desc))
    (assert-= 1 (gethash "locked" obj))))

(deftest js-rt-object-define-property-rejects-frozen-redefine
  "%js-object-define-property signals when redefining a frozen object."
  (let* ((obj (cl-cc/javascript::%js-make-object "locked" 1))
         (desc (cl-cc/javascript::%js-make-object "value" 2)))
    (cl-cc/javascript::%js-object-freeze obj)
    (assert-signals error
      (cl-cc/javascript::%js-object-define-property obj "locked" desc))
    (assert-= 1 (gethash "locked" obj))))
