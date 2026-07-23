;;;; packages/javascript/tests/js-runtime-resolver-tests.lisp
;;;;
;;;; Method dispatch, type resolver coverage (define-js-type-resolver),
;;;; RegExp, Reflect helpers, bound-method, Object property descriptor,
;;;; string char-iter, and Object fallback method table.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)

;;; ─── RegExp public API ───────────────────────────────────────────────────────

(it-sequential "js-rt-regex-test-match"
  (let ((re (cl-cc/javascript::%js-make-regex "hello")))
    (expect (cl-cc/javascript::%js-regex-test re "say hello world") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test re "goodbye") :to-be-falsy)))

(it-sequential "js-rt-regex-exec-returns-match"
  (let* ((re (cl-cc/javascript::%js-make-regex "lo"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (expect (eq m cl-cc/javascript::+js-null+) :to-be-falsy)
    (expect (gethash "0" m) :to-equal "lo")
    (expect (= 3 (truncate (gethash "index" m))) :to-be-truthy)))

(it-sequential "js-rt-regex-exec-no-match"
  (let* ((re (cl-cc/javascript::%js-make-regex "xyz"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (expect m :to-be cl-cc/javascript::+js-null+)))

(it-sequential "js-rt-regex-flags-and-last-index"
  (let ((re (cl-cc/javascript::%js-make-regex "l" "gimy")))
    (expect (cl-cc/javascript::js-regexp-ignore-case-p re) :to-be-truthy)
    (expect (cl-cc/javascript::js-regexp-multiline-p re) :to-be-truthy)
    (expect (cl-cc/javascript::js-regexp-global-p re) :to-be-truthy)
    (expect (cl-cc/javascript::js-regexp-sticky-p re) :to-be-truthy)
    (let* ((exec-re (cl-cc/javascript::make-js-regexp
                     :source "l"
                     :flags "g"
                     :compiled (lambda (str i groups)
                                 (declare (ignore str groups))
                                 (when (and (string= str "hello") (= i 2)) 3))
                     :global-p t
                     :ignore-case-p nil
                     :multiline-p nil
                     :sticky-p nil
                     :last-index 0))
           (m1 (cl-cc/javascript::%js-regex-exec exec-re "hello" 0)))
      (expect (eq m1 cl-cc/javascript::+js-null+) :to-be-falsy)
      (expect (= 3 (cl-cc/javascript::js-regexp-last-index exec-re)) :to-be-truthy)
      (let ((m2 (cl-cc/javascript::%js-regex-exec exec-re "zzzz" 0)))
        (expect m2 :to-be cl-cc/javascript::+js-null+))
      (expect (= 0 (cl-cc/javascript::js-regexp-last-index exec-re)) :to-be-truthy))))

(it-sequential "js-rt-regex-exec-uncompiled-pattern"
  (let* ((re (cl-cc/javascript::make-js-regexp
              :source "("
              :flags ""
              :compiled nil
              :global-p nil
              :ignore-case-p nil
              :multiline-p nil
              :sticky-p nil
              :last-index 0))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (expect m :to-be cl-cc/javascript::+js-null+)))

(it-sequential "js-rt-regex-string-match-global"
  (let ((result (cl-cc/javascript::%js-string-match-regex
                 "hello" (cl-cc/javascript::%js-make-regex "l" "g"))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (aref result 0) :to-equal "l")
    (expect (aref result 1) :to-equal "l")))

(it-sequential "js-rt-regex-string-match-global-empty"
  (let ((result (cl-cc/javascript::%js-string-match-regex
                 "abc" (cl-cc/javascript::%js-make-regex "" "g"))))
    (expect (= 4 (length result)) :to-be-truthy)
    (dotimes (i (length result))
      (expect (aref result i) :to-equal ""))))

(it-sequential "js-rt-regex-string-pattern-branches"
  (let ((replaced (cl-cc/javascript::%js-string-replace-regex "hello" "l" "-")))
    (expect replaced :to-equal "he-lo"))
  (expect (= 3 (cl-cc/javascript::%js-string-search-regex "hello" "lo")) :to-be-truthy)
  (let ((matched (cl-cc/javascript::%js-string-match-regex "hello" "lo")))
    (expect (= 1 (length matched)) :to-be-truthy)
    (expect (aref matched 0) :to-equal "lo"))
  (let ((replaced-all (cl-cc/javascript::%js-string-replace-all-regex "hello" "l" "-")))
    (expect replaced-all :to-equal "he--o"))
  (let ((parts (cl-cc/javascript::%js-string-split-regex "hello" "l")))
    (expect (%jr-list parts) :to-equal '("he" "" "o"))))

(it-sequential "js-rt-regex-string-search"
  (expect (= 3 (cl-cc/javascript::%js-string-search-regex
               "hello" (cl-cc/javascript::%js-make-regex "lo"))) :to-be-truthy))

(it-sequential "js-rt-regex-string-replace"
  (expect (cl-cc/javascript::%js-string-replace-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l") "-") :to-equal "he-lo"))

(it-sequential "js-rt-regex-string-replace-placeholders"
  (expect (cl-cc/javascript::%js-string-replace-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l") "$&-") :to-equal "hel-lo"))

(it-sequential "js-rt-regex-string-replace-fn-and-fallback"
  (let ((re (cl-cc/javascript::make-js-regexp
             :source "he"
             :flags ""
             :compiled (lambda (str i groups)
                         (declare (ignore str groups))
                         (when (= i 0) 2))
             :global-p nil
             :ignore-case-p nil
             :multiline-p nil
             :sticky-p nil
             :last-index 0)))
    (expect (cl-cc/javascript::%js-string-replace-regex
                     "hello"
                     re
                     (lambda (match-str match-start source)
                       (declare (ignore match-str match-start source))
                       "X")) :to-equal "Xllo"))
  (let ((re (cl-cc/javascript::make-js-regexp
             :source "l"
             :flags ""
             :compiled nil
             :global-p nil
             :ignore-case-p nil
             :multiline-p nil
             :sticky-p nil
             :last-index 0)))
    (expect (cl-cc/javascript::%js-string-replace-regex
                     "hello" re "-") :to-equal "he-lo")))

(it-sequential "js-rt-regex-string-replace-all"
  (expect (cl-cc/javascript::%js-string-replace-all-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l" "g") "-") :to-equal "he--o"))

(it-sequential "js-rt-regex-string-split"
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "a,b,c" (cl-cc/javascript::%js-make-regex ","))))
    (expect (= 3 (length parts)) :to-be-truthy)
    (expect (aref parts 0) :to-equal "a")
    (expect (aref parts 1) :to-equal "b")
    (expect (aref parts 2) :to-equal "c")))

(it-sequential "js-rt-regex-string-split-limit-and-empty"
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "ab" (cl-cc/javascript::%js-make-regex "a") 1)))
    (expect (= 1 (length parts)) :to-be-truthy)
    (expect (aref parts 0) :to-equal ""))
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "ab" (cl-cc/javascript::%js-make-regex ""))))
    (expect (= 2 (length parts)) :to-be-truthy)
    (expect (aref parts 0) :to-equal "a")
    (expect (aref parts 1) :to-equal "b")))

(it-sequential "js-rt-regex-string-split-fallback"
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "a(b)c"
                (cl-cc/javascript::make-js-regexp
                 :source "b"
                 :flags ""
                 :compiled nil
                 :global-p nil
                 :ignore-case-p nil
                 :multiline-p nil
                 :sticky-p nil
                 :last-index 0))))
    (expect (%jr-list parts) :to-equal '("a(" ")c"))))

(it-sequential "js-rt-regex-pattern-branches"
  (let ((range-re (cl-cc/javascript::%js-make-regex "^[a-cx]+$")))
    (expect (cl-cc/javascript::%js-regex-test range-re "abcx") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test range-re "abdz") :to-be-falsy))
  (let ((complement-re (cl-cc/javascript::%js-make-regex "[^a-c]+")))
    (expect (cl-cc/javascript::%js-regex-test complement-re "z") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test complement-re "b") :to-be-falsy))
  (let ((class-escape-re (cl-cc/javascript::%js-make-regex "[\\d\\w\\s]+")))
    (expect (cl-cc/javascript::%js-regex-test class-escape-re "a_9 ") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test class-escape-re "!") :to-be-falsy))
  (let ((group-re (cl-cc/javascript::%js-make-regex "^(?:cat|dog)?$")))
    (expect (cl-cc/javascript::%js-regex-test group-re "cat") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test group-re "") :to-be-truthy))
  (let ((literal-escape-re (cl-cc/javascript::%js-make-regex "a\\+b")))
    (expect (cl-cc/javascript::%js-regex-test literal-escape-re "a+b") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test literal-escape-re "ab") :to-be-falsy))
  (let ((dot-re (cl-cc/javascript::%js-make-regex "a.b" "m")))
    (expect (cl-cc/javascript::%js-regex-test dot-re "acb") :to-be-truthy)
    (expect (cl-cc/javascript::%js-regex-test dot-re (format nil "a~%b")) :to-be-falsy))
  (let ((lazy-star-re (cl-cc/javascript::%js-make-regex "a*?")))
    (expect (cl-cc/javascript::%js-regex-test lazy-star-re "aaa") :to-be-truthy)
    (expect (gethash "0" (cl-cc/javascript::%js-regex-exec lazy-star-re "aaa" 0)) :to-equal "")))

;;; ─── Reflect helpers ─────────────────────────────────────────────────────────

(it-sequential "js-rt-reflect-get-set"
  (let ((obj (cl-cc/javascript::%js-make-object "x" 10)))
    (expect (= 10 (cl-cc/javascript::%js-reflect-get obj "x")) :to-be-truthy)
    (cl-cc/javascript::%js-reflect-set obj "x" 42)
    (expect (= 42 (cl-cc/javascript::%js-reflect-get obj "x")) :to-be-truthy)))

(it-sequential "js-rt-reflect-has"
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-reflect-has obj "a") :to-be-truthy)
    (expect (cl-cc/javascript::%js-reflect-has obj "b") :to-be-falsy)))

(it-sequential "js-rt-reflect-delete-property"
  (let ((obj (cl-cc/javascript::%js-make-object "k" 99)))
    (cl-cc/javascript::%js-reflect-delete-property obj "k")
    (expect (cl-cc/javascript::%js-reflect-get obj "k") :to-be cl-cc/javascript::+js-undefined+)))

(it-sequential "js-rt-reflect-apply"
  (let* ((fn     (lambda (a b) (+ a b)))
         (result (cl-cc/javascript::%js-reflect-apply fn nil (%jr-arr 3 4))))
    (expect (= 7 result) :to-be-truthy)))

(it-sequential "js-rt-reflect-construct"
  (let* ((ctor  (lambda (&rest args) (first args)))
         (args  (%jr-arr 77))
         (obj   (cl-cc/javascript::%js-reflect-construct ctor args)))
    (expect (= 77 obj) :to-be-truthy)))

;;; ─── Object property descriptors ────────────────────────────────────────────

(it-sequential "js-rt-object-define-property"
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 77)))
    (cl-cc/javascript::%js-object-define-property obj "y" desc)
    (expect (= 77 (cl-cc/javascript::%js-reflect-get obj "y")) :to-be-truthy)))

(it-sequential "js-rt-object-get-own-property-descriptor"
  (let* ((obj  (cl-cc/javascript::%js-make-object "v" 5))
         (desc (cl-cc/javascript::%js-object-get-own-property-descriptor obj "v")))
    (expect (= 5 (gethash "value" desc)) :to-be-truthy)
    (expect (gethash "writable" desc) :to-be-truthy)))

(it-sequential "js-rt-get-own-property-descriptors-filters-internals visible"
  (destructuring-bind (key should-appear) (list "real" t)
    (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (expect found :to-equal should-appear)))))

(it-sequential "js-rt-get-own-property-descriptors-filters-internals proto"
  (destructuring-bind (key should-appear) (list "__proto__" nil)
    (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (expect found :to-equal should-appear)))))

(it-sequential "js-rt-get-own-property-descriptors-filters-internals getter"
  (destructuring-bind (key should-appear) (list "__get_x" nil)
    (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (expect found :to-equal should-appear)))))

(it-sequential "js-rt-get-own-property-descriptors-filters-internals setter"
  (destructuring-bind (key should-appear) (list "__set_x" nil)
    (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (expect found :to-equal should-appear)))))

;;; ─── bound-method ────────────────────────────────────────────────────────────

(it-sequential "js-rt-bound-method-found"
  (let* ((table (list (cons "double" (lambda (n) (* 2 n)))))
         (bound (cl-cc/javascript::%js-bound-method table 5 "double")))
    (expect (functionp bound) :to-be-truthy)
    (expect (= 10 (funcall bound)) :to-be-truthy)))

(it-sequential "js-rt-bound-method-not-found"
  (let* ((table  (list (cons "existing" #'identity)))
         (result (cl-cc/javascript::%js-bound-method table 5 "missing")))
    (expect result :to-be cl-cc/javascript::+js-undefined+)))

;;; ─── Type resolver coverage (define-js-type-resolver) ────────────────────────

(it-sequential "js-rt-resolve-regexp-props source"
  (destructuring-bind (key expected) (list "source" "hello")
    (let* ((re  (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (expect val :to-equal expected))))

(it-sequential "js-rt-resolve-regexp-props flags"
  (destructuring-bind (key expected) (list "flags" "")
    (let* ((re  (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (expect val :to-equal expected))))

(it-sequential "js-rt-resolve-regexp-props global"
  (destructuring-bind (key expected) (list "global" nil)
    (let* ((re  (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (expect val :to-equal expected))))

(it-sequential "js-rt-resolve-regexp-test-method"
  (let* ((re (cl-cc/javascript::%js-make-regex "hi" ""))
         (fn (cl-cc/javascript::%js-resolve-regexp-method re "test")))
    (expect (funcall fn "say hi there") :to-be-truthy)
    (expect (funcall fn "goodbye") :to-be-falsy)))

(it-sequential "js-rt-resolve-regexp-bool-props ignoreCase"
  (destructuring-bind (key expected) (list "ignoreCase" t)
    (let* ((re (cl-cc/javascript::%js-make-regex "hello" "im"))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (expect val :to-equal expected))))

(it-sequential "js-rt-resolve-regexp-bool-props multiline"
  (destructuring-bind (key expected) (list "multiline" t)
    (let* ((re (cl-cc/javascript::%js-make-regex "hello" "im"))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (expect val :to-equal expected))))

(it-sequential "js-rt-resolve-regexp-last-index"
  (let ((re (cl-cc/javascript::%js-make-regex "hello" "im")))
    (setf (cl-cc/javascript::js-regexp-last-index re) 3)
    (let ((val (cl-cc/javascript::%js-resolve-regexp-method re "lastIndex")))
      (expect (= 3 val) :to-be-truthy))))

(it-sequential "js-rt-resolve-regexp-exec-method"
  (let* ((re (cl-cc/javascript::%js-make-regex "hello" ""))
         (fn (cl-cc/javascript::%js-resolve-regexp-method re "exec"))
         (m  (funcall fn "say hello world")))
    (expect (gethash "0" m) :to-equal "hello")
    (expect (= 4 (truncate (gethash "index" m))) :to-be-truthy)))

(it-sequential "js-rt-resolve-promise-methods"
  (let* ((fulfilled (cl-cc/javascript::%js-promise-resolve 5))
         (rejected   (cl-cc/javascript::%js-promise-reject "boom"))
         (then       (cl-cc/javascript::%js-resolve-promise-method fulfilled "then"))
         (catch      (cl-cc/javascript::%js-resolve-promise-method rejected "catch"))
         (finally    (cl-cc/javascript::%js-resolve-promise-method fulfilled "finally"))
         (called     0)
         (then-value (cl-cc/javascript::%js-await
                      (funcall then (lambda (v) (+ v 1)))))
         (catch-value (cl-cc/javascript::%js-await
                       (funcall catch (lambda (reason)
                                        (if (string= reason "boom") 99 0)))))
         (finally-value (cl-cc/javascript::%js-await
                         (funcall finally (lambda ()
                                           (incf called))))))
    (expect (= 6 then-value) :to-be-truthy)
    (expect (= 99 catch-value) :to-be-truthy)
    (expect (= 1 called) :to-be-truthy)
    (expect (= 5 finally-value) :to-be-truthy)))

(it-sequential "js-rt-resolve-promise-finally-rejects-through"
  (let* ((called 0)
         (rejected (cl-cc/javascript::%js-promise-reject "boom"))
         (finally (cl-cc/javascript::%js-resolve-promise-method rejected "finally"))
         (result (funcall finally (lambda ()
                                    (incf called)))))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/javascript::%js-await result)) (cl-cc/javascript:js-exception () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))
    (expect (= 1 called) :to-be-truthy)))

(it-sequential "js-rt-resolve-function-methods name"
  (destructuring-bind (key expected) (list "name" "")
    (let* ((fn  (lambda (&rest args) args))
         (val (cl-cc/javascript::%js-resolve-function-method fn key))
         ;; toString resolves to a callable method (f.toString() in JS);
         ;; name resolves directly to its value.
         (result (if (functionp val) (funcall val) val)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-resolve-function-methods toString"
  (destructuring-bind (key expected) (list "toString" "function() { [native code] }")
    (let* ((fn  (lambda (&rest args) args))
         (val (cl-cc/javascript::%js-resolve-function-method fn key))
         ;; toString resolves to a callable method (f.toString() in JS);
         ;; name resolves directly to its value.
         (result (if (functionp val) (funcall val) val)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-resolve-function-length"
  (let* ((fn  (lambda (&rest args) args))
         (val (cl-cc/javascript::%js-resolve-function-method fn "length")))
    (expect (= 0 val) :to-be-truthy)))

(it-sequential "js-rt-resolve-function-call-apply-bind"
  (let* ((seen nil)
         (fn   (lambda (&rest args)
                 (setf seen args)
                 args))
         (call (cl-cc/javascript::%js-resolve-function-method fn "call"))
         (apply (cl-cc/javascript::%js-resolve-function-method fn "apply"))
         (bind  (cl-cc/javascript::%js-resolve-function-method fn "bind"))
         (bound (funcall bind "self" 1 2))
         (call-result (funcall call "self" 3 4))
         (apply-result (funcall apply "self" (%jr-arr 5 6)))
         (bind-result (funcall bound 7 8)))
    (expect call-result :to-equal '("self" 3 4))
    (expect apply-result :to-equal '("self" 5 6))
    (expect bind-result :to-equal '("self" 1 2 7 8))
    (expect seen :to-equal '("self" 1 2 7 8))))

(it-sequential "js-rt-resolve-symbol-description"
  (let* ((sym (cl-cc/javascript::%js-make-symbol "label"))
         (val (cl-cc/javascript::%js-resolve-symbol-method sym "description")))
    (expect val :to-equal "label")))

(it-sequential "js-rt-resolve-method-dispatch-fallback"
  (expect (cl-cc/javascript::%js-resolve-method (list 1 2) "anything") :to-be cl-cc/javascript::+js-undefined+))

(it-sequential "js-rt-resolve-typed-array-props int32-length"
  (destructuring-bind (type-name length key expected) (list "Int32Array" 3 "length" 3)
    (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (expect (= expected val) :to-be-truthy))))

(it-sequential "js-rt-resolve-typed-array-props int32-byte-length"
  (destructuring-bind (type-name length key expected) (list "Int32Array" 3 "byteLength" 12)
    (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (expect (= expected val) :to-be-truthy))))

(it-sequential "js-rt-resolve-typed-array-props int32-byte-offset"
  (destructuring-bind (type-name length key expected) (list "Int32Array" 3 "byteOffset" 0)
    (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (expect (= expected val) :to-be-truthy))))

(it-sequential "js-rt-resolve-typed-array-props f16-byte-length"
  (destructuring-bind (type-name length key expected) (list "Float16Array" 3 "byteLength" 6)
    (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (expect (= expected val) :to-be-truthy))))

(it-sequential "js-rt-resolve-bigint-methods toString"
  (destructuring-bind (key expected) (list "toString" "42")
    (let* ((bi     (cl-cc/javascript::%make-js-bigint 42))
         (fn     (cl-cc/javascript::%js-resolve-bigint-method bi key))
         (result (funcall fn cl-cc/javascript::+js-undefined+)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-resolve-bigint-methods toLocaleString"
  (destructuring-bind (key expected) (list "toLocaleString" "42")
    (let* ((bi     (cl-cc/javascript::%make-js-bigint 42))
         (fn     (cl-cc/javascript::%js-resolve-bigint-method bi key))
         (result (funcall fn cl-cc/javascript::+js-undefined+)))
    (expect result :to-equal expected))))

(it-sequential "js-rt-resolve-bigint-value-of"
  (let* ((bi (cl-cc/javascript::%make-js-bigint 7))
         (fn (cl-cc/javascript::%js-resolve-bigint-method bi "valueOf")))
    (expect (funcall fn) :to-be bi)))

;;; ─── String resolver ───────────────────────────────────────────────────────

(it-sequential "js-rt-string-resolver-rejects-deprecated-substr"
  (expect (cl-cc/javascript::%js-resolve-string-method "abcdef" "substr") :to-be cl-cc/javascript::+js-undefined+))

;;; ─── String char-iter via get-prop ──────────────────────────────────────────

(it-sequential "js-rt-string-char-iter-yields-chars"
  (let* ((iter (cl-cc/javascript::%js-string-char-iter "abc"))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (r3   (funcall next))
         (done (funcall next)))
    (expect (gethash "value" r1) :to-equal "a")
    (expect (gethash "value" r2) :to-equal "b")
    (expect (gethash "value" r3) :to-equal "c")
    (expect (gethash "done"  done) :to-be-truthy)))

(it-sequential "js-rt-string-char-iter-via-get-prop"
  (let* ((fn   (cl-cc/javascript::%js-get-prop "xy" "@@iterator"))
         (iter (funcall fn))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (done (funcall next)))
    (expect (gethash "value" r1) :to-equal "x")
    (expect (gethash "value" r2) :to-equal "y")
    (expect (gethash "done"  done) :to-be-truthy)))

;;; ─── Object fallback method table ────────────────────────────────────────────

(it-sequential "js-rt-object-fallback-methods has-own-existing"
  (destructuring-bind (method key expected) (list "hasOwnProperty" "x" t)
    (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (expect (functionp fn) :to-be-truthy)
    (expect (funcall fn key) :to-equal expected))))

(it-sequential "js-rt-object-fallback-methods has-own-missing"
  (destructuring-bind (method key expected) (list "hasOwnProperty" "z" nil)
    (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (expect (functionp fn) :to-be-truthy)
    (expect (funcall fn key) :to-equal expected))))

(it-sequential "js-rt-object-fallback-methods prop-is-enum-yes"
  (destructuring-bind (method key expected) (list "propertyIsEnumerable" "x" t)
    (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (expect (functionp fn) :to-be-truthy)
    (expect (funcall fn key) :to-equal expected))))

(it-sequential "js-rt-object-fallback-methods prop-is-enum-no"
  (destructuring-bind (method key expected) (list "propertyIsEnumerable" "z" nil)
    (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (expect (functionp fn) :to-be-truthy)
    (expect (funcall fn key) :to-equal expected))))

(it-sequential "js-rt-object-fallback-to-string"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (expect (funcall fn) :to-equal "[object Object]")))

(it-sequential "js-rt-object-fallback-value-of"
  (let* ((obj (cl-cc/javascript::%js-make-object "k" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "valueOf")))
    (expect (funcall fn) :to-be obj)))

(it-sequential "js-rt-object-fallback-constructor"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "constructor")))
    (expect (funcall fn) :to-be obj)))

(it-sequential "js-rt-object-fallback-stored-wins"
  (let* ((obj    (cl-cc/javascript::%js-make-object "toString" "custom"))
         (result (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (expect result :to-equal "custom")))

(it-sequential "js-rt-object-fallback-unknown-returns-undefined"
  (let* ((obj    (cl-cc/javascript::%js-make-object))
         (result (cl-cc/javascript::%js-resolve-object-method obj "nonExistent")))
    (expect result :to-be cl-cc/javascript::+js-undefined+)))

;;; ─── Object.is — NaN/zero special cases ─────────────────────────────────────

(it-sequential "js-rt-object-is nan-nan"
  (destructuring-bind (a b expected) (list :js-nan :js-nan t)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is nan-float"
  (destructuring-bind (a b expected) (list :js-nan 1.0d0 nil)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is +0/-0"
  (destructuring-bind (a b expected) (list 0.0d0 -0.0d0 nil)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is -0/+0"
  (destructuring-bind (a b expected) (list -0.0d0 0.0d0 nil)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is same-num"
  (destructuring-bind (a b expected) (list 3.0d0 3.0d0 t)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is same-str"
  (destructuring-bind (a b expected) (list "x" "x" t)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

(it-sequential "js-rt-object-is diff-str"
  (destructuring-bind (a b expected) (list "a" "b" nil)
    (expect (cl-cc/javascript::%js-object-is a b) :to-equal expected)))

;;; ─── ToString: float formatting + Infinity + BigInt ──────────────────────────

(it-sequential "js-rt-to-string-extended int-float"
  (destructuring-bind (value expected) (list 7.0d0 "7")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string-extended float-point"
  (destructuring-bind (value expected) (list 3.14d0 "3.14")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string-extended infinity"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-infinity+ "Infinity")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string-extended neg-inf"
  (destructuring-bind (value expected) (list cl-cc/javascript::+js-neg-infinity+ "-Infinity")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string-extended float-nan"
  (destructuring-bind (value expected) (list cl-cc/javascript::*js-nan-float* "NaN")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

(it-sequential "js-rt-to-string-extended bigint"
  (destructuring-bind (value expected) (list (cl-cc/javascript::%make-js-bigint 99) "99")
    (expect (cl-cc/javascript::%js-to-string value) :to-equal expected)))

;;; ─── Template string joining ─────────────────────────────────────────────────

(it-sequential "js-rt-template-string-join"
  (expect (cl-cc/javascript::%js-template-string '("hello " 42 " world")) :to-equal "hello 42 world")
  (expect (cl-cc/javascript::%js-template-string '(t " and " nil)) :to-equal "true and false"))

;;; ─── structuredClone / deep-clone ────────────────────────────────────────────

(it-sequential "js-rt-deep-clone-array"
  (let* ((orig  (%jr-arr 1 2 3))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (expect (cl-cc/javascript::%js-vec-p clone) :to-be-truthy)
    (expect (= (length orig) (length clone)) :to-be-truthy)
    (expect (= 1 (aref clone 0)) :to-be-truthy)
    (expect (eq orig clone) :to-be-falsy)))

(it-sequential "js-rt-deep-clone-object"
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (expect (cl-cc/javascript::%js-ht-p clone) :to-be-truthy)
    (expect (= 10 (gethash "x" clone)) :to-be-truthy)
    (expect (eq orig clone) :to-be-falsy)))
