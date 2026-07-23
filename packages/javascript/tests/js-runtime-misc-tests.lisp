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

;;; ─── Local test helpers ──────────────────────────────────────────────────────

(defun %jr-assert-string-props (object expected-props)
  (dolist (prop expected-props)
    (destructuring-bind (key expected) prop
      (assert-string= expected (gethash key object)))))

;;; ─── Module exports ─────────────────────────────────────────────────────────

(it-sequential "js-rt-export-default-registers-module-value"
  (let ((cl-cc/javascript::*js-module-exports* (cl-cc/javascript::%js-make-object)))
    (let* ((result (cl-cc/javascript::%js-export :default 42))
           (exports (cl-cc/javascript::%js-current-module-exports)))
      (expect (= 42 result) :to-be-truthy)
      (expect (= 42 (cl-cc/javascript::%js-get-prop exports "default")) :to-be-truthy))))

(it-sequential "js-rt-export-declaration-registers-named-value"
  (let ((cl-cc/javascript::*js-module-exports* (cl-cc/javascript::%js-make-object)))
    (let* ((fn (lambda () 7))
           (result (cl-cc/javascript::%js-export :declaration fn nil '("add")))
           (exports (cl-cc/javascript::%js-current-module-exports)))
      (expect result :to-be fn)
      (expect (gethash "add" exports) :to-be fn))))

(it-sequential "js-rt-export-reexport-records-source-metadata"
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
      (expect (= 1 (length reexports)) :to-be-truthy)
      (expect (cl-cc/javascript::%js-get-prop entry "kind") :to-equal "named")
      (expect (cl-cc/javascript::%js-get-prop entry "from") :to-equal "./dep.js")
      (expect (cl-cc/javascript::%js-get-prop spec "local") :to-equal "foo")
      (expect (cl-cc/javascript::%js-get-prop spec "exported") :to-equal "bar"))))

;;; ─── Promise.race ────────────────────────────────────────────────────────────

(it-sequential "js-rt-promise-race-returns-first"
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve 1))
         (p2  (cl-cc/javascript::%js-promise-resolve 2))
         (arr (%jr-arr p1 p2))
         (winner (cl-cc/javascript::%js-promise-race arr)))
    (expect winner :to-be p1)))

(it-sequential "js-rt-promise-race-empty-pending"
  (let ((result (cl-cc/javascript::%js-promise-race (%jr-arr))))
    (expect (cl-cc/javascript::js-promise-p result) :to-be-truthy)
    (expect (cl-cc/javascript::js-promise-settled-p result) :to-be-falsy)))

;;; ─── Promise.allSettled ──────────────────────────────────────────────────────

(it-sequential "js-rt-promise-all-settled-mixed"
  (let* ((p1  (cl-cc/javascript::%js-promise-resolve "ok"))
         (p2  (cl-cc/javascript::%js-promise-reject  "err"))
         (arr (%jr-arr p1 p2))
         (settled (cl-cc/javascript::%js-await
                   (cl-cc/javascript::%js-promise-all-settled arr)))
         (r1  (aref settled 0))
         (r2  (aref settled 1)))
    (expect (gethash "status" r1) :to-equal "fulfilled")
    (expect (gethash "value"  r1) :to-equal "ok")
    (expect (gethash "status" r2) :to-equal "rejected")
    (expect (gethash "reason" r2) :to-equal "err")))

;;; ─── Promise.finally ─────────────────────────────────────────────────────────

(it-sequential "js-rt-promise-finally-calls-cleanup"
  (let* ((called (list nil))
         (p      (cl-cc/javascript::%js-promise-resolve 42))
         (result (cl-cc/javascript::%js-promise-finally
                  p (lambda () (setf (car called) t)))))
    (expect (car called) :to-be-truthy)
    (expect result :to-be p)))

;;; ─── Promise.withResolvers ───────────────────────────────────────────────────

(it-sequential "js-rt-promise-with-resolvers"
  (let* ((obj     (cl-cc/javascript::%js-promise-with-resolvers))
         (promise  (gethash "promise" obj))
         (resolve  (gethash "resolve" obj)))
    (expect (cl-cc/javascript::js-promise-p promise) :to-be-truthy)
    (expect (functionp resolve) :to-be-truthy)
    (funcall resolve 99)
    (expect (= 99 (cl-cc/javascript::%js-await promise)) :to-be-truthy)))

;;; ─── Promise.try ─────────────────────────────────────────────────────────────

(it-sequential "js-rt-promise-try-success"
  (let* ((result (cl-cc/javascript::%js-promise-try (lambda () 7))))
    (expect (= 7 (cl-cc/javascript::%js-await result)) :to-be-truthy)))

;;; ─── Global isNaN / isFinite (with coercion) ─────────────────────────────────

(it-sequential "js-rt-global-is-nan number-nan"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-nan-float* t)
    (expect (cl-cc/javascript::%js-is-nan val) :to-equal expected)))

(it-sequential "js-rt-global-is-nan string-nan"
  (destructuring-bind (val expected) (list "NaN" t)
    (expect (cl-cc/javascript::%js-is-nan val) :to-equal expected)))

(it-sequential "js-rt-global-is-nan string-num"
  (destructuring-bind (val expected) (list "42" nil)
    (expect (cl-cc/javascript::%js-is-nan val) :to-equal expected)))

(it-sequential "js-rt-global-is-nan integer"
  (destructuring-bind (val expected) (list 5 nil)
    (expect (cl-cc/javascript::%js-is-nan val) :to-equal expected)))

(it-sequential "js-rt-global-is-finite integer"
  (destructuring-bind (val expected) (list 42 t)
    (expect (cl-cc/javascript::%js-is-finite val) :to-equal expected)))

(it-sequential "js-rt-global-is-finite string-n"
  (destructuring-bind (val expected) (list "3.14" t)
    (expect (cl-cc/javascript::%js-is-finite val) :to-equal expected)))

(it-sequential "js-rt-global-is-finite nan"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-nan-float* nil)
    (expect (cl-cc/javascript::%js-is-finite val) :to-equal expected)))

(it-sequential "js-rt-global-is-finite inf"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-inf-float* nil)
    (expect (cl-cc/javascript::%js-is-finite val) :to-equal expected)))

;;; ─── structuredClone ─────────────────────────────────────────────────────────

(it-sequential "js-rt-structured-clone"
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-structured-clone orig)))
    (expect (eq orig clone) :to-be-falsy)
    (expect (= 10 (gethash "x" clone)) :to-be-truthy)))

;;; ─── queueMicrotask / browser timers ─────────────────────────────────────────

(it-sequential "js-rt-queue-microtask-runs-fn"
  (let ((called (list nil)))
    (let ((ret (cl-cc/javascript::%js-queue-microtask
                (lambda () (setf (car called) t)))))
      (expect (car called) :to-be-truthy)
      (expect ret :to-be cl-cc/javascript::+js-undefined+))))

(it-sequential "js-rt-browser-timer-stubs-absent setTimeout"
  (destructuring-bind (name) (list "setTimeout")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

(it-sequential "js-rt-browser-timer-stubs-absent setInterval"
  (destructuring-bind (name) (list "setInterval")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

(it-sequential "js-rt-browser-timer-stubs-absent clearTimeout"
  (destructuring-bind (name) (list "clearTimeout")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

(it-sequential "js-rt-browser-timer-stubs-absent clearInterval"
  (destructuring-bind (name) (list "clearInterval")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

;;; ─── Dynamic code stubs ─────────────────────────────────────────────────────

(it-sequential "js-rt-dynamic-code-stubs-absent eval"
  (destructuring-bind (name) (list "eval")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

(it-sequential "js-rt-dynamic-code-stubs-absent Function"
  (destructuring-bind (name) (list "Function")
    (expect (nth-value 1 (gethash name cl-cc/javascript::*js-builtin-map*)) :to-be-falsy) (expect (find name cl-cc/javascript::*js-prelude-global-specs*
                      :key #'second
                      :test #'string=) :to-be-falsy) (expect (nth-value 1
                            (gethash (cl-cc/javascript::js-ident-sym name)
                                     cl-cc/javascript::*js-coercion-call-helpers*)) :to-be-falsy)))

;;; ─── Iterator.from ───────────────────────────────────────────────────────────

(it-sequential "js-rt-iterator-from-array"
  (let* ((arr  (%jr-arr 1 2 3))
         (iter (cl-cc/javascript::%js-iterator-from-iterable arr))
         (acc  nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (expect acc :to-equal '(3 2 1))))

(it-sequential "js-rt-iterator-from-set"
  (let* ((set (cl-cc/javascript::%js-make-set))
         (_   (cl-cc/javascript::%js-set-add set 3))
         (_   (cl-cc/javascript::%js-set-add set 4))
         (iter (cl-cc/javascript::%js-iterator-from-iterable set))
         (acc nil))
    (declare (ignore _))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (expect acc :to-equal '(4 3))))

(it-sequential "js-rt-iterator-from-map"
  (let* ((map (cl-cc/javascript::%js-make-map))
         (_1  (cl-cc/javascript::%js-map-set map "a" 1))
         (_2  (cl-cc/javascript::%js-map-set map "b" 2))
         (iter (cl-cc/javascript::%js-iterator-from-iterable map))
         (acc nil))
    (declare (ignore _1 _2))
    (cl-cc/javascript::%js-for-of iter
                                  (lambda (entry)
                                    (push (%jr-list entry) acc)))
    (expect acc :to-equal '(("b" 2) ("a" 1)))))

(it-sequential "js-rt-iterator-from-plain-next"
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
    (expect acc :to-equal '("second" "first"))))

(it-sequential "js-rt-iterator-from-@@iterator"
  (let* ((iterable
           (cl-cc/javascript::%js-make-object
            "@@iterator" (lambda ()
                           (cl-cc/javascript::%js-vec-to-iter (%jr-arr 8 9)))))
         (iter (cl-cc/javascript::%js-iterator-from-iterable iterable))
         (acc nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (expect acc :to-equal '(9 8))))

(it-sequential "js-rt-iterator-from-string"
  (let* ((iter (cl-cc/javascript::%js-iterator-from-iterable "ab"))
         (acc nil))
    (cl-cc/javascript::%js-for-of iter (lambda (v) (push v acc)))
    (expect acc :to-equal '("b" "a"))))

(it-sequential "js-rt-iterator-from-non-iterable"
  (let ((iter (cl-cc/javascript::%js-iterator-from-iterable
               (cl-cc/javascript::%js-make-object))))
    (multiple-value-bind (value done) (cl-cc/javascript::%js-iter-next iter)
      (expect value :to-be cl-cc/javascript::+js-undefined+)
      (expect done :to-be-truthy))))

;;; ─── Map.groupBy ─────────────────────────────────────────────────────────────

(it-sequential "js-rt-map-group-by"
  (let* ((items  (%jr-arr 1 2 3 4))
         (result (cl-cc/javascript::%js-map-group-by
                  items (lambda (x) (if (evenp x) "even" "odd"))))
         (evens  (cl-cc/javascript::%js-map-get result "even"))
         (odds   (cl-cc/javascript::%js-map-get result "odd")))
    (expect (= 2 (length evens)) :to-be-truthy)
    (expect (= 2 (length odds)) :to-be-truthy)))

;;; ─── Set-from-iterable ───────────────────────────────────────────────────────

(it-sequential "js-rt-make-set-from-iterable"
  (let* ((arr (cl-cc/javascript::%js-make-array 1 2 3 2))
         (s   (cl-cc/javascript::%js-make-set-from-iterable arr)))
    (expect (cl-cc/javascript::%js-set-has s 1) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s 3) :to-be-truthy)
    (expect (cl-cc/javascript::%js-set-has s 9) :to-be-falsy)))

;;; ─── Proxy ───────────────────────────────────────────────────────────────────

(it-sequential "js-rt-make-proxy-object"
  (let* ((target  (cl-cc/javascript::%js-make-object "x" 1))
         (handler (cl-cc/javascript::%js-make-object))
         (proxy   (cl-cc/javascript::%js-make-proxy-object target handler)))
    (expect (gethash "__proxy-target__"  proxy) :to-be target)
    (expect (gethash "__proxy-handler__" proxy) :to-be handler)
    (expect (cl-cc/javascript::%js-proxy-object-p proxy) :to-be-truthy)))

(it-sequential "js-rt-proxy-property-traps"
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
    (expect (cl-cc/javascript::%js-get-prop proxy "x") :to-equal "trap-x-1")
    (expect (= 7 (cl-cc/javascript::%js-set-prop proxy "y" 7)) :to-be-truthy)
    (expect (= 7 (gethash "y" target)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-in "visible" proxy) :to-be-truthy)
    (expect (cl-cc/javascript::%js-in "x" proxy) :to-be-falsy)
    (expect (cl-cc/javascript::%js-delete proxy "x") :to-be-truthy)
    (expect deleted :to-equal "x")
    (expect (nth-value 1 (gethash "x" target)) :to-be-falsy)))

(it-sequential "js-rt-proxy-falls-back-without-traps"
  (let* ((target (cl-cc/javascript::%js-make-object "x" 1))
         (proxy (cl-cc/javascript::%js-make-proxy-object
                 target (cl-cc/javascript::%js-make-object))))
    (multiple-value-bind (value trapped)
        (cl-cc/javascript::%js-proxy-call-trap proxy "get" target "x" proxy)
      (expect value :to-be cl-cc/javascript::+js-undefined+)
      (expect trapped :to-be-falsy))
    (expect (= 1 (cl-cc/javascript::%js-get-prop proxy "x")) :to-be-truthy)
    (expect (= 2 (cl-cc/javascript::%js-set-prop proxy "y" 2)) :to-be-truthy)
    (expect (= 2 (gethash "y" target)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-in "x" proxy) :to-be-truthy)
    (expect (cl-cc/javascript::%js-delete proxy "x") :to-be-truthy)
    (expect (nth-value 1 (gethash "x" target)) :to-be-falsy)))

(it-sequential "js-rt-proxy-reflect-and-object-traps"
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
    (expect (= 20 (cl-cc/javascript::%js-reflect-get proxy "b")) :to-be-truthy)
    (expect (cl-cc/javascript::%js-reflect-set proxy "z" 99) :to-be-truthy)
    (expect (= 99 (gethash "z" target)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-reflect-define-property
        proxy "d" (cl-cc/javascript::%js-make-object "value" 44)) :to-be-truthy)
    (expect (= 44 (gethash "d" target)) :to-be-truthy)
    (let ((keys (cl-cc/javascript::%js-object-keys proxy)))
      (expect (= 2 (length keys)) :to-be-truthy)
      (expect (aref keys 0) :to-equal "b")
      (expect (aref keys 1) :to-equal "c"))
    (let ((desc (cl-cc/javascript::%js-object-get-own-property-descriptor proxy "b")))
      (expect (gethash "value" desc) :to-equal "b"))
    (let ((desc (cl-cc/javascript::%js-object-get-own-property-descriptor proxy "c")))
      (expect (gethash "value" desc) :to-equal "c"))))

;;; ─── Math.sumPrecise (ES2026) ────────────────────────────────────────────────

(it-sequential "js-rt-math-sum-precise"
  (let ((result (cl-cc/javascript::%js-math-sum-precise (%jr-arr 1 2 3 4))))
    (expect (= 10.0d0 result) :to-be-truthy)))

;;; ─── Error.isError (ES2026) ──────────────────────────────────────────────────

(it-sequential "js-rt-error-is-error"
  (let ((with-message (cl-cc/javascript::%js-make-error-instance
                       cl-cc/javascript::*js-error-class* "oops"))
        (plain-obj    (cl-cc/javascript::%js-make-object "x" 1)))
    (expect (cl-cc/javascript::%js-error-is-error with-message) :to-be-truthy)
    (expect (cl-cc/javascript::%js-error-is-error plain-obj) :to-be-falsy)
    (expect (cl-cc/javascript::%js-error-is-error "error") :to-be-falsy)))

;;; ─── AggregateError ──────────────────────────────────────────────────────────

(it-sequential "js-rt-make-aggregate-error"
  (let* ((errors (%jr-arr "e1" "e2"))
         (cause  (cl-cc/javascript::%js-make-object "code" "root"))
         (opts   (cl-cc/javascript::%js-make-object "cause" cause))
         (agg    (cl-cc/javascript::%js-make-aggregate-error errors "multiple" opts)))
    (expect (gethash "name"    agg) :to-equal "AggregateError")
    (expect (gethash "message" agg) :to-equal "multiple")
    (expect (gethash "stack" agg) :to-equal "AggregateError: multiple")
    (expect (gethash "errors"  agg) :to-be errors)
    (expect (gethash "cause"   agg) :to-be cause)
    (expect (cl-cc/javascript::%js-instanceof
                  agg cl-cc/javascript::*js-aggregate-error-class*) :to-be-truthy)
    (expect (cl-cc/javascript::%js-instanceof
                  agg cl-cc/javascript::*js-error-class*) :to-be-truthy)))

;;; ─── AbortController ─────────────────────────────────────────────────────────

(it-sequential "js-rt-abort-controller"
  (let* ((ctrl   (cl-cc/javascript::%js-make-abort-controller))
         (sig    (gethash "signal" ctrl))
         (abort  (gethash "abort"  ctrl)))
    (expect (gethash "aborted" sig) :to-be-falsy)
    (funcall abort "reason")
    (expect (gethash "aborted" sig) :to-be-truthy)
    (expect (gethash "reason" sig) :to-equal "reason")))

(it-sequential "js-rt-abort-controller-dispatches-once"
  (let* ((ctrl   (cl-cc/javascript::%js-make-abort-controller))
         (sig    (gethash "signal" ctrl))
         (abort  (gethash "abort" ctrl))
         (onabort-calls 0)
         (listener-calls 0))
    (setf (gethash "onabort" sig)
          (lambda (event)
            (incf onabort-calls)
            (expect (gethash "type" event) :to-equal "abort")
            (expect (gethash "target" event) :to-be sig)))
    (funcall (gethash "addEventListener" sig)
             "abort"
             (lambda (event)
               (incf listener-calls)
               (expect (gethash "type" event) :to-equal "abort")
               (expect (gethash "currentTarget" event) :to-be sig)))
    (funcall abort "first")
    (funcall abort "second")
    (expect (= 1 onabort-calls) :to-be-truthy)
    (expect (= 1 listener-calls) :to-be-truthy)
    (expect (gethash "reason" sig) :to-equal "first")))

(it-sequential "js-rt-abort-signal-remove-event-listener"
  (let* ((ctrl  (cl-cc/javascript::%js-make-abort-controller))
         (sig   (gethash "signal" ctrl))
         (calls 0)
         (listener (lambda (&rest _) (declare (ignore _)) (incf calls))))
    (funcall (gethash "addEventListener" sig) "abort" listener)
    (funcall (gethash "removeEventListener" sig) "abort" listener)
    (funcall (gethash "abort" ctrl) "done")
    (expect (= 0 calls) :to-be-truthy)))

(it-sequential "js-rt-abort-signal-throw-if-aborted"
  (let ((sig (cl-cc/javascript::%js-abort-signal-aborted "boom")))
    (expect (gethash "aborted" sig) :to-be-truthy)
    (expect (gethash "reason" sig) :to-equal "boom")
    (handler-case
        (progn
          (funcall (gethash "throwIfAborted" sig))
          (%fail-test "throwIfAborted did not signal js-exception"))
      (cl-cc/javascript:js-exception (c)
        (expect (cl-cc/javascript:js-exception-value c) :to-equal "boom")))))

(it-sequential "js-rt-abort-signal-static-helpers"
  (let* ((ctor    (cl-cc/javascript::%js-make-abort-signal-constructor))
         (aborted (funcall (gethash "abort" ctor) "done"))
         (timeout (funcall (gethash "timeout" ctor) 5))
         (reason  (gethash "reason" timeout))
         (source  (gethash "signal" (cl-cc/javascript::%js-make-abort-controller)))
         (combo   (funcall (gethash "any" ctor) (%jr-arr source))))
    (expect (gethash "aborted" aborted) :to-be-truthy)
    (expect (gethash "reason" aborted) :to-equal "done")
    (expect (gethash "aborted" timeout) :to-be-truthy)
    (expect (gethash "name" reason) :to-equal "TimeoutError")
    (expect (gethash "aborted" combo) :to-be-falsy)
    (cl-cc/javascript::%js-abort-signal-abort source "input")
    (expect (gethash "aborted" combo) :to-be-truthy)
    (expect (gethash "reason" combo) :to-equal "input")))

;;; ─── URL ─────────────────────────────────────────────────────────────────────

(it-sequential "js-rt-make-url-parses-components"
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
    (expect (funcall (gethash "toString" url)) :to-equal "https://example.com:8443/path/to?q=1#frag")))

(it-sequential "js-rt-make-url-resolves-base-relative-path"
  (let ((url (cl-cc/javascript::%js-make-url
              "child?x=1"
              "https://example.com/a/b/index.html")))
    (%jr-assert-string-props
     url
     '(("href" "https://example.com/a/b/child?x=1")
       ("pathname" "/a/b/child")
       ("search" "?x=1")))))

(it-sequential "js-rt-make-url-normalizes-relative-dot-segments"
  (let ((url (cl-cc/javascript::%js-make-url
              "../c/./d/?x=1"
              "https://example.com/a/b/index.html")))
    (%jr-assert-string-props
     url
     '(("href" "https://example.com/a/c/d/?x=1")
       ("pathname" "/a/c/d/")
       ("search" "?x=1")))))

(it-sequential "js-rt-make-url-resolves-query-and-hash-only-relative"
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

(it-sequential "js-rt-typed-array-ctor-factory"
  (let* ((ctor (cl-cc/javascript::%js-make-typed-array-ctor "Int32Array"))
         (ta   (funcall ctor 3)))
    (expect (cl-cc/javascript::js-typed-array-p ta) :to-be-truthy)
    (expect (= 3 (cl-cc/javascript::js-ta-length ta)) :to-be-truthy)))

;;; ─── Object.defineProperties ─────────────────────────────────────────────────

(it-sequential "js-rt-object-define-properties"
  (let* ((obj   (cl-cc/javascript::%js-make-object))
         (descs (cl-cc/javascript::%js-make-object
                 "a" (cl-cc/javascript::%js-make-object "value" 1)
                 "b" (cl-cc/javascript::%js-make-object "value" 2))))
    (cl-cc/javascript::%js-object-define-properties obj descs)
    (expect (= 1 (gethash "a" obj)) :to-be-truthy)
    (expect (= 2 (gethash "b" obj)) :to-be-truthy)))

(it-sequential "js-rt-object-define-properties-ignores-non-objects"
  (let* ((obj (cl-cc/javascript::%js-make-object "keep" 1))
         (mixed (cl-cc/javascript::%js-make-object
                 "ok" (cl-cc/javascript::%js-make-object "value" 2)
                 "skip" 9)))
    (cl-cc/javascript::%js-object-define-properties obj (%jr-arr "bad"))
    (cl-cc/javascript::%js-object-define-properties obj mixed)
    (expect (= 1 (gethash "keep" obj)) :to-be-truthy)
    (expect (= 2 (gethash "ok" obj)) :to-be-truthy)
    (expect (nth-value 1 (gethash "skip" obj)) :to-be-falsy)))

;;; ─── Reflect.defineProperty / Object.defineProperty ─────────────────────────

(it-sequential "js-rt-reflect-define-property"
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 42))
         (ret  (cl-cc/javascript::%js-reflect-define-property obj "x" desc)))
    (expect ret :to-be-truthy)
    (expect (= 42 (gethash "x" obj)) :to-be-truthy)))

(it-sequential "js-rt-object-define-property-value"
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 99)))
    (let ((ret (cl-cc/javascript::%js-object-define-property obj "n" desc)))
      (expect ret :to-be obj)
      (expect (= 99 (gethash "n" obj)) :to-be-truthy))))

(it-sequential "js-rt-object-define-property-getter"
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (getter (lambda () 7))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "prop" desc)
    (expect (gethash "__get_prop" obj) :to-be getter)))

(it-sequential "js-rt-object-get-own-property-descriptor-accessor"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 7))
         (setter (lambda (value) value))
         (desc (cl-cc/javascript::%js-make-object "get" getter "set" setter)))
    (cl-cc/javascript::%js-object-define-property obj "prop" desc)
    (let ((actual (cl-cc/javascript::%js-object-get-own-property-descriptor obj "prop")))
      (expect (gethash "get" actual) :to-be getter)
      (expect (gethash "set" actual) :to-be setter)
      (expect (gethash "enumerable" actual) :to-be-truthy)
      (expect (gethash "configurable" actual) :to-be-truthy)
      (expect (nth-value 1 (gethash "value" actual)) :to-be-falsy))))

(it-sequential "js-rt-object-get-own-property-descriptors-includes-accessor-property"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 10))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "computed" desc)
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (actual (gethash "computed" descs)))
      (expect (gethash "get" actual) :to-be getter)
      (expect (nth-value 1 (gethash "__get_computed" descs)) :to-be-falsy))))

(it-sequential "js-rt-object-get-own-property-descriptor-reflects-object-flags"
  (let* ((sealed (cl-cc/javascript::%js-make-object "a" 1))
         (frozen (cl-cc/javascript::%js-make-object "b" 2)))
    (cl-cc/javascript::%js-object-seal sealed)
    (cl-cc/javascript::%js-object-freeze frozen)
    (let ((sealed-desc (cl-cc/javascript::%js-object-get-own-property-descriptor sealed "a"))
          (frozen-desc (cl-cc/javascript::%js-object-get-own-property-descriptor frozen "b")))
      (expect (gethash "configurable" sealed-desc) :to-be-falsy)
      (expect (gethash "writable" sealed-desc) :to-be-truthy)
      (expect (gethash "configurable" frozen-desc) :to-be-falsy)
      (expect (gethash "writable" frozen-desc) :to-be-falsy))))

(it-sequential "js-rt-reflect-define-property-respects-prevent-extensions"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 10)))
    (cl-cc/javascript::%js-object-prevent-extensions obj)
    (expect (cl-cc/javascript::%js-reflect-define-property obj "blocked" desc) :to-be-falsy)
    (expect (nth-value 1 (gethash "blocked" obj)) :to-be-falsy)))

(it-sequential "js-rt-object-define-property-signals-on-prevent-extensions"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 10)))
    (cl-cc/javascript::%js-object-prevent-extensions obj)
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/javascript::%js-object-define-property obj "blocked" desc)) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))
    (expect (nth-value 1 (gethash "blocked" obj)) :to-be-falsy)))

(it-sequential "js-rt-reflect-define-property-rejects-frozen-redefine"
  (let* ((obj (cl-cc/javascript::%js-make-object "locked" 1))
         (desc (cl-cc/javascript::%js-make-object "value" 2)))
    (cl-cc/javascript::%js-object-freeze obj)
    (expect (cl-cc/javascript::%js-reflect-define-property obj "locked" desc) :to-be-falsy)
    (expect (= 1 (gethash "locked" obj)) :to-be-truthy)))

(it-sequential "js-rt-object-define-property-rejects-frozen-redefine"
  (let* ((obj (cl-cc/javascript::%js-make-object "locked" 1))
         (desc (cl-cc/javascript::%js-make-object "value" 2)))
    (cl-cc/javascript::%js-object-freeze obj)
    (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::%js-object-define-property obj "locked" desc)) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy))
    (expect (= 1 (gethash "locked" obj)) :to-be-truthy)))
