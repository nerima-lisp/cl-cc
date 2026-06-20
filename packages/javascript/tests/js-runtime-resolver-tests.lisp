;;;; packages/javascript/tests/js-runtime-resolver-tests.lisp
;;;;
;;;; Method dispatch, type resolver coverage (define-js-type-resolver),
;;;; RegExp, Reflect helpers, bound-method, Object property descriptor,
;;;; string char-iter, and Object fallback method table.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-suite)

;;; ─── RegExp public API ───────────────────────────────────────────────────────

(deftest js-rt-regex-test-match
  "regex-test returns t when the pattern matches."
  (let ((re (cl-cc/javascript::%js-make-regex "hello")))
    (assert-true  (cl-cc/javascript::%js-regex-test re "say hello world"))
    (assert-false (cl-cc/javascript::%js-regex-test re "goodbye"))))

(deftest js-rt-regex-exec-returns-match
  "regex-exec returns match info object with index and '0' key."
  (let* ((re (cl-cc/javascript::%js-make-regex "lo"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-false (eq m cl-cc/javascript::+js-null+))
    (assert-string= "lo" (gethash "0" m))
    (assert-= 3 (truncate (gethash "index" m)))))

(deftest js-rt-regex-exec-no-match
  "regex-exec returns null when there is no match."
  (let* ((re (cl-cc/javascript::%js-make-regex "xyz"))
         (m  (cl-cc/javascript::%js-regex-exec re "hello" 0)))
    (assert-eq cl-cc/javascript::+js-null+ m)))

(deftest js-rt-regex-flags-and-last-index
  "Regexp flag accessors and lastIndex updates/reset correctly."
  (let ((re (cl-cc/javascript::%js-make-regex "l" "gimy")))
    (assert-true  (cl-cc/javascript::js-regexp-ignore-case-p re))
    (assert-true  (cl-cc/javascript::js-regexp-multiline-p re))
    (assert-true  (cl-cc/javascript::js-regexp-global-p re))
    (assert-true  (cl-cc/javascript::js-regexp-sticky-p re))
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
      (assert-false (eq m1 cl-cc/javascript::+js-null+))
      (assert-= 3 (cl-cc/javascript::js-regexp-last-index exec-re))
      (let ((m2 (cl-cc/javascript::%js-regex-exec exec-re "zzzz" 0)))
        (assert-eq cl-cc/javascript::+js-null+ m2))
      (assert-= 0 (cl-cc/javascript::js-regexp-last-index exec-re)))))

(deftest js-rt-regex-exec-uncompiled-pattern
  "regex-exec returns null when pattern compilation fails."
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
    (assert-eq cl-cc/javascript::+js-null+ m)))

(deftest js-rt-regex-string-match-global
  "String.prototype.match() with /g returns all matched substrings."
  (let ((result (cl-cc/javascript::%js-string-match-regex
                 "hello" (cl-cc/javascript::%js-make-regex "l" "g"))))
    (assert-= 3 (length result))
    (assert-string= "l" (aref result 0))
    (assert-string= "l" (aref result 1))
    (assert-string= "l" (aref result 2))))

(deftest js-rt-regex-string-match-global-empty
  "Global empty matches advance by one code unit to avoid infinite loops."
  (let ((result (cl-cc/javascript::%js-string-match-regex
                 "abc" (cl-cc/javascript::%js-make-regex "" "g"))))
    (assert-= 4 (length result))
    (dotimes (i (length result))
      (assert-string= "" (aref result i)))))

(deftest js-rt-regex-string-pattern-branches
  "String pattern inputs exercise the non-RegExp branches for match/search/replace/split."
  (let ((replaced (cl-cc/javascript::%js-string-replace-regex "hello" "l" "-")))
    (assert-string= "he-lo" replaced))
  (assert-= 3 (cl-cc/javascript::%js-string-search-regex "hello" "lo"))
  (let ((matched (cl-cc/javascript::%js-string-match-regex "hello" "lo")))
    (assert-= 1 (length matched))
    (assert-string= "lo" (aref matched 0)))
  (let ((replaced-all (cl-cc/javascript::%js-string-replace-all-regex "hello" "l" "-")))
    (assert-string= "he--o" replaced-all))
  (let ((parts (cl-cc/javascript::%js-string-split-regex "hello" "l")))
    (assert-equal '("he" "" "o") (%jr-list parts))))

(deftest js-rt-regex-string-search
  "String.prototype.search(regexp) returns the first match index."
  (assert-= 3 (cl-cc/javascript::%js-string-search-regex
               "hello" (cl-cc/javascript::%js-make-regex "lo"))))

(deftest js-rt-regex-string-replace
  "String.prototype.replace(regexp, replacement) substitutes the first match."
  (assert-string= "he-lo"
                  (cl-cc/javascript::%js-string-replace-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l") "-")))

(deftest js-rt-regex-string-replace-placeholders
  "Replacement templates expand $& to the matched substring."
  (assert-string= "hel-lo"
                  (cl-cc/javascript::%js-string-replace-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l") "$&-")))

(deftest js-rt-regex-string-replace-fn-and-fallback
  "String.prototype.replace(regexp, fn) uses callback replacement and falls back on uncompiled regex."
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
    (assert-string= "Xllo"
                    (cl-cc/javascript::%js-string-replace-regex
                     "hello"
                     re
                     (lambda (match-str match-start source)
                       (declare (ignore match-str match-start source))
                       "X"))))
  (let ((re (cl-cc/javascript::make-js-regexp
             :source "l"
             :flags ""
             :compiled nil
             :global-p nil
             :ignore-case-p nil
             :multiline-p nil
             :sticky-p nil
             :last-index 0)))
    (assert-string= "he-lo"
                    (cl-cc/javascript::%js-string-replace-regex
                     "hello" re "-"))))

(deftest js-rt-regex-string-replace-all
  "String.prototype.replaceAll(regexp, replacement) substitutes every match."
  (assert-string= "he--o"
                  (cl-cc/javascript::%js-string-replace-all-regex
                   "hello" (cl-cc/javascript::%js-make-regex "l" "g") "-")))

(deftest js-rt-regex-string-split
  "String.prototype.split(regexp) returns pieces around the separator."
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "a,b,c" (cl-cc/javascript::%js-make-regex ","))))
    (assert-= 1 (length parts))
    (assert-string= "a,b,c" (aref parts 0))))

(deftest js-rt-regex-string-split-limit-and-empty
  "Split respects an explicit limit and advances on empty matches."
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "ab" (cl-cc/javascript::%js-make-regex "a") 1)))
    (assert-= 1 (length parts))
    (assert-string= "a" (aref parts 0)))
  (let ((parts (cl-cc/javascript::%js-string-split-regex
                "ab" (cl-cc/javascript::%js-make-regex ""))))
    (assert-= 2 (length parts))
    (assert-string= "" (aref parts 0))
    (assert-string= "" (aref parts 1))))

(deftest js-rt-regex-string-split-fallback
  "String.prototype.split(regexp) falls back to string split when regex compile fails."
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
    (assert-equal '("a(" ")c") (%jr-list parts))))

(deftest js-rt-regex-pattern-branches
  "Regex compilation exercises classes, anchors, alternation, escapes, and lazy quantifiers."
  (let ((range-re (cl-cc/javascript::%js-make-regex "^[a-cx]+$")))
    (assert-true (cl-cc/javascript::%js-regex-test range-re "abcx"))
    (assert-false (cl-cc/javascript::%js-regex-test range-re "abdz")))
  (let ((complement-re (cl-cc/javascript::%js-make-regex "[^a-c]+")))
    (assert-true (cl-cc/javascript::%js-regex-test complement-re "z"))
    (assert-false (cl-cc/javascript::%js-regex-test complement-re "b")))
  (let ((class-escape-re (cl-cc/javascript::%js-make-regex "[\\d\\w\\s]+")))
    (assert-true (cl-cc/javascript::%js-regex-test class-escape-re "a_9 "))
    (assert-false (cl-cc/javascript::%js-regex-test class-escape-re "!")))
  (let ((group-re (cl-cc/javascript::%js-make-regex "^(?:cat|dog)?$")))
    (assert-true (cl-cc/javascript::%js-regex-test group-re "cat"))
    (assert-true (cl-cc/javascript::%js-regex-test group-re "")))
  (let ((literal-escape-re (cl-cc/javascript::%js-make-regex "a\\+b")))
    (assert-true (cl-cc/javascript::%js-regex-test literal-escape-re "a+b"))
    (assert-false (cl-cc/javascript::%js-regex-test literal-escape-re "ab")))
  (let ((dot-re (cl-cc/javascript::%js-make-regex "a.b" "m")))
    (assert-true (cl-cc/javascript::%js-regex-test dot-re "acb"))
    (assert-false (cl-cc/javascript::%js-regex-test dot-re (format nil "a~%b"))))
  (let ((lazy-star-re (cl-cc/javascript::%js-make-regex "a*?")))
    (assert-true (cl-cc/javascript::%js-regex-test lazy-star-re "aaa"))
    (assert-string= "" (gethash "0" (cl-cc/javascript::%js-regex-exec lazy-star-re "aaa" 0)))))

;;; ─── Reflect helpers ─────────────────────────────────────────────────────────

(deftest js-rt-reflect-get-set
  "Reflect.get and Reflect.set wrap %js-get-prop/%js-set-prop."
  (let ((obj (cl-cc/javascript::%js-make-object "x" 10)))
    (assert-= 10 (cl-cc/javascript::%js-reflect-get obj "x"))
    (cl-cc/javascript::%js-reflect-set obj "x" 42)
    (assert-= 42 (cl-cc/javascript::%js-reflect-get obj "x"))))

(deftest js-rt-reflect-has
  "Reflect.has returns true when key exists, nil otherwise."
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (assert-true  (cl-cc/javascript::%js-reflect-has obj "a"))
    (assert-false (cl-cc/javascript::%js-reflect-has obj "b"))))

(deftest js-rt-reflect-delete-property
  "Reflect.deleteProperty removes a key from the object."
  (let ((obj (cl-cc/javascript::%js-make-object "k" 99)))
    (cl-cc/javascript::%js-reflect-delete-property obj "k")
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-reflect-get obj "k"))))

(deftest js-rt-reflect-apply
  "Reflect.apply invokes fn with spread args."
  (let* ((fn     (lambda (a b) (+ a b)))
         (result (cl-cc/javascript::%js-reflect-apply fn nil (%jr-arr 3 4))))
    (assert-= 7 result)))

(deftest js-rt-reflect-construct
  "Reflect.construct invokes a constructor function with an args vector."
  (let* ((ctor  (lambda (&rest args) (first args)))
         (args  (%jr-arr 77))
         (obj   (cl-cc/javascript::%js-reflect-construct ctor args)))
    (assert-= 77 obj)))

;;; ─── Object property descriptors ────────────────────────────────────────────

(deftest js-rt-object-define-property
  "Object.defineProperty sets a value key via descriptor."
  (let* ((obj  (cl-cc/javascript::%js-make-object))
         (desc (cl-cc/javascript::%js-make-object "value" 77)))
    (cl-cc/javascript::%js-object-define-property obj "y" desc)
    (assert-= 77 (cl-cc/javascript::%js-reflect-get obj "y"))))

(deftest js-rt-object-get-own-property-descriptor
  "getOwnPropertyDescriptor returns descriptor object for existing key."
  (let* ((obj  (cl-cc/javascript::%js-make-object "v" 5))
         (desc (cl-cc/javascript::%js-object-get-own-property-descriptor obj "v")))
    (assert-= 5 (gethash "value" desc))
    (assert-true (gethash "writable" desc))))

(deftest-each js-rt-get-own-property-descriptors-filters-internals
  "getOwnPropertyDescriptors excludes __proto__, __get_X, __set_X, __class__ etc."
  :cases (("visible"   "real"      t)
          ("proto"     "__proto__" nil)
          ("getter"    "__get_x"   nil)
          ("setter"    "__set_x"   nil))
  (key should-appear)
  (let* ((obj (cl-cc/javascript::%js-make-object "real" 1)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+
          (gethash "__get_x"   obj) (lambda () 0)
          (gethash "__set_x"   obj) (lambda (v) v))
    (let* ((descs (cl-cc/javascript::%js-object-get-own-property-descriptors obj))
           (found (nth-value 1 (gethash key descs))))
      (assert-equal should-appear found))))

;;; ─── bound-method ────────────────────────────────────────────────────────────

(deftest js-rt-bound-method-found
  "bound-method returns a closure that prepends the receiver."
  (let* ((table (list (cons "double" (lambda (n) (* 2 n)))))
         (bound (cl-cc/javascript::%js-bound-method table 5 "double")))
    (assert-true (functionp bound))
    (assert-= 10 (funcall bound))))

(deftest js-rt-bound-method-not-found
  "bound-method returns +js-undefined+ when the method is not in the table."
  (let* ((table  (list (cons "existing" #'identity)))
         (result (cl-cc/javascript::%js-bound-method table 5 "missing")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

;;; ─── Type resolver coverage (define-js-type-resolver) ────────────────────────

(deftest-each js-rt-resolve-regexp-props
  "Regexp resolver returns struct fields and bound-method closures."
  :cases (("source"  "source"  "hello")
          ("flags"   "flags"   "")
          ("global"  "global"  nil))
  (key expected)
  (let* ((re  (cl-cc/javascript::%js-make-regex "hello" ""))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (assert-equal expected val)))

(deftest js-rt-resolve-regexp-test-method
  "Regexp 'test' property resolves to a closure that tests the pattern."
  (let* ((re (cl-cc/javascript::%js-make-regex "hi" ""))
         (fn (cl-cc/javascript::%js-resolve-regexp-method re "test")))
    (assert-true  (funcall fn "say hi there"))
    (assert-false (funcall fn "goodbye"))))

(deftest-each js-rt-resolve-regexp-bool-props
  "Regexp resolver exposes the ignoreCase and multiline flags."
  :cases (("ignoreCase" "ignoreCase" t)
          ("multiline"  "multiline"  t))
  (key expected)
  (let* ((re (cl-cc/javascript::%js-make-regex "hello" "im"))
         (val (cl-cc/javascript::%js-resolve-regexp-method re key)))
    (assert-equal expected val)))

(deftest js-rt-resolve-regexp-last-index
  "Regexp resolver returns the current lastIndex as a number."
  (let ((re (cl-cc/javascript::%js-make-regex "hello" "im")))
    (setf (cl-cc/javascript::js-regexp-last-index re) 3)
    (let ((val (cl-cc/javascript::%js-resolve-regexp-method re "lastIndex")))
      (assert-= 3 val))))

(deftest js-rt-resolve-regexp-exec-method
  "Regexp exec resolves to a closure that returns match objects."
  (let* ((re (cl-cc/javascript::%js-make-regex "hello" ""))
         (fn (cl-cc/javascript::%js-resolve-regexp-method re "exec"))
         (m  (funcall fn "say hello world")))
    (assert-string= "hello" (gethash "0" m))
    (assert-= 4 (truncate (gethash "index" m)))))

(deftest js-rt-resolve-promise-methods
  "Promise resolver returns closures for then/catch/finally."
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
    (assert-= 6 then-value)
    (assert-= 99 catch-value)
    (assert-= 1 called)
    (assert-= 5 finally-value)))

(deftest js-rt-resolve-promise-finally-rejects-through
  "Promise.finally still rejects when the original promise rejects."
  (let* ((called 0)
         (rejected (cl-cc/javascript::%js-promise-reject "boom"))
         (finally (cl-cc/javascript::%js-resolve-promise-method rejected "finally"))
         (result (funcall finally (lambda ()
                                    (incf called)))))
    (assert-signals cl-cc/javascript:js-exception
      (cl-cc/javascript::%js-await result))
    (assert-= 1 called)))

(deftest-each js-rt-resolve-function-methods
  "Function resolver exposes native metadata and call helpers."
  :cases (("name"     "name"     "")
          ("toString" "toString" "function() { [native code] }"))
  (key expected)
  (let* ((fn  (lambda (&rest args) args))
         (val (cl-cc/javascript::%js-resolve-function-method fn key)))
    (assert-equal expected val)))

(deftest js-rt-resolve-function-length
  "Function resolver returns 0 for the synthetic length property."
  (let* ((fn  (lambda (&rest args) args))
         (val (cl-cc/javascript::%js-resolve-function-method fn "length")))
    (assert-= 0 val)))

(deftest js-rt-resolve-function-call-apply-bind
  "Function resolver returns working call/apply/bind closures."
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
    (assert-equal '("self" 3 4) call-result)
    (assert-equal '("self" 5 6) apply-result)
    (assert-equal '("self" 1 2 7 8) bind-result)
    (assert-equal '("self" 1 2 7 8) seen)))

(deftest js-rt-resolve-symbol-description
  "Symbol resolver exposes the description property."
  (let* ((sym (cl-cc/javascript::%js-make-symbol "label"))
         (val (cl-cc/javascript::%js-resolve-symbol-method sym "description")))
    (assert-string= "label" val)))

(deftest js-rt-resolve-method-dispatch-fallback
  "Method dispatcher returns +js-undefined+ for unsupported values."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-resolve-method (list 1 2) "anything")))

(deftest-each js-rt-resolve-typed-array-props
  "TypedArray resolver returns numeric properties from struct slots."
  :cases (("int32-length"      "Int32Array"   3 "length"     3)
          ("int32-byte-length" "Int32Array"   3 "byteLength" 12)
          ("int32-byte-offset" "Int32Array"   3 "byteOffset" 0)
          ("f16-byte-length"   "Float16Array" 3 "byteLength" 6))
  (type-name length key expected)
  (let* ((ta  (cl-cc/javascript::%js-make-typed-array type-name length))
         (val (cl-cc/javascript::%js-resolve-typed-array-method ta key)))
    (assert-= expected val)))

(deftest-each js-rt-resolve-bigint-methods
  "BigInt resolver returns method closures for toString/toLocaleString."
  :cases (("toString"       "toString"       "42")
          ("toLocaleString" "toLocaleString" "42"))
  (key expected)
  (let* ((bi     (cl-cc/javascript::%make-js-bigint 42))
         (fn     (cl-cc/javascript::%js-resolve-bigint-method bi key))
         (result (funcall fn cl-cc/javascript::+js-undefined+)))
    (assert-string= expected result)))

(deftest js-rt-resolve-bigint-value-of
  "BigInt valueOf returns the BigInt struct itself."
  (let* ((bi (cl-cc/javascript::%make-js-bigint 7))
         (fn (cl-cc/javascript::%js-resolve-bigint-method bi "valueOf")))
    (assert-eq bi (funcall fn))))

;;; ─── String resolver ───────────────────────────────────────────────────────

(deftest js-rt-string-resolver-rejects-deprecated-substr
  "String.prototype.substr is intentionally absent from the method resolver."
  (assert-eq cl-cc/javascript::+js-undefined+
             (cl-cc/javascript::%js-resolve-string-method "abcdef" "substr")))

;;; ─── String char-iter via get-prop ──────────────────────────────────────────

(deftest js-rt-string-char-iter-yields-chars
  "%js-string-char-iter returns an iterator that yields each char as a 1-char string."
  (let* ((iter (cl-cc/javascript::%js-string-char-iter "abc"))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (r3   (funcall next))
         (done (funcall next)))
    (assert-string= "a" (gethash "value" r1))
    (assert-string= "b" (gethash "value" r2))
    (assert-string= "c" (gethash "value" r3))
    (assert-true        (gethash "done"  done))))

(deftest js-rt-string-char-iter-via-get-prop
  "String @@iterator method resolves to an iterator using %js-string-char-iter."
  (let* ((fn   (cl-cc/javascript::%js-get-prop "xy" "@@iterator"))
         (iter (funcall fn))
         (next (gethash "next" iter))
         (r1   (funcall next))
         (r2   (funcall next))
         (done (funcall next)))
    (assert-string= "x" (gethash "value" r1))
    (assert-string= "y" (gethash "value" r2))
    (assert-true        (gethash "done"  done))))

;;; ─── Object fallback method table ────────────────────────────────────────────

(deftest-each js-rt-object-fallback-methods
  "Object fallback methods are available via %js-resolve-object-method."
  :cases (("has-own-existing"  "hasOwnProperty"       "x"   t)
          ("has-own-missing"   "hasOwnProperty"       "z"   nil)
          ("prop-is-enum-yes"  "propertyIsEnumerable" "x"   t)
          ("prop-is-enum-no"   "propertyIsEnumerable" "z"   nil))
  (method key expected)
  (let* ((obj (cl-cc/javascript::%js-make-object "x" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj method)))
    (assert-true  (functionp fn))
    (assert-equal expected (funcall fn key))))

(deftest js-rt-object-fallback-to-string
  "Object fallback toString returns \"[object Object]\"."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "[object Object]" (funcall fn))))

(deftest js-rt-object-fallback-value-of
  "Object fallback valueOf returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object "k" 1))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "valueOf")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-constructor
  "Object fallback constructor returns the object itself."
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (fn  (cl-cc/javascript::%js-resolve-object-method obj "constructor")))
    (assert-eq obj (funcall fn))))

(deftest js-rt-object-fallback-stored-wins
  "When obj has a stored key the stored value is returned, not the fallback."
  (let* ((obj    (cl-cc/javascript::%js-make-object "toString" "custom"))
         (result (cl-cc/javascript::%js-resolve-object-method obj "toString")))
    (assert-string= "custom" result)))

(deftest js-rt-object-fallback-unknown-returns-undefined
  "Unknown method names return +js-undefined+."
  (let* ((obj    (cl-cc/javascript::%js-make-object))
         (result (cl-cc/javascript::%js-resolve-object-method obj "nonExistent")))
    (assert-eq cl-cc/javascript::+js-undefined+ result)))

;;; ─── Object.is — NaN/zero special cases ─────────────────────────────────────

(deftest-each js-rt-object-is
  "Object.is(a, b) treats NaN-equal-NaN as true, +0/-0 as distinct."
  :cases (("nan-nan"   :js-nan        :js-nan        t)   ; NaN is NaN: true
          ("nan-float" :js-nan        1.0d0          nil)  ; NaN is 1.0: false
          ("+0/-0"     0.0d0         -0.0d0          nil)  ; +0 is -0: false
          ("-0/+0"    -0.0d0          0.0d0          nil)  ; -0 is +0: false
          ("same-num"  3.0d0          3.0d0          t)
          ("same-str"  "x"            "x"            t)
          ("diff-str"  "a"            "b"            nil))
  (a b expected)
  (assert-equal expected (cl-cc/javascript::%js-object-is a b)))

;;; ─── ToString: float formatting + Infinity + BigInt ──────────────────────────

(deftest-each js-rt-to-string-extended
  "ToString handles floats (no trailing .0), Infinity, BigInt, and arrays."
  :cases (("int-float"    7.0d0                            "7")
          ("float-point"  3.14d0                           "3.14")
          ("infinity"     cl-cc/javascript::+js-infinity+  "Infinity")
          ("neg-inf"      cl-cc/javascript::+js-neg-infinity+ "-Infinity")
          ("float-nan"    cl-cc/javascript::*js-nan-float* "NaN")
          ("bigint"       (cl-cc/javascript::%make-js-bigint 99) "99"))
  (value expected)
  (assert-string= expected (cl-cc/javascript::%js-to-string value)))

;;; ─── Template string joining ─────────────────────────────────────────────────

(deftest js-rt-template-string-join
  "%js-template-string concatenates parts, coercing each to string."
  (assert-string= "hello 42 world"
                  (cl-cc/javascript::%js-template-string '("hello " 42 " world")))
  (assert-string= "true and false"
                  (cl-cc/javascript::%js-template-string '(t " and " nil))))

;;; ─── structuredClone / deep-clone ────────────────────────────────────────────

(deftest js-rt-deep-clone-array
  "%js-deep-clone copies arrays element-by-element (no shared structure)."
  (let* ((orig  (%jr-arr 1 2 3))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (assert-true (cl-cc/javascript::%js-vec-p clone))
    (assert-= (length orig) (length clone))
    (assert-= 1 (aref clone 0))
    (assert-false (eq orig clone))))

(deftest js-rt-deep-clone-object
  "%js-deep-clone copies hash-table objects (no shared structure)."
  (let* ((orig  (cl-cc/javascript::%js-make-object "x" 10))
         (clone (cl-cc/javascript::%js-deep-clone orig)))
    (assert-true (cl-cc/javascript::%js-ht-p clone))
    (assert-= 10 (gethash "x" clone))
    (assert-false (eq orig clone))))
