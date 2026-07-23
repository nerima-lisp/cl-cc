;;;; packages/javascript/tests/js-runtime-object-ops-tests.lisp
;;;;
;;;; Unit tests for runtime-object.lisp (Object static methods, prototype ops,
;;;; destructuring helpers) and runtime-ops.lisp (bitwise ops, shifts, BigInt
;;;; extras, URI encoding, accessor/misc stubs).
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr, %jr-list)

(in-package :cl-cc/test)

;;; ─── Internal key filter ─────────────────────────────────────────────────────

(it-sequential "js-rt-internal-key-p dunder-proto"
  (destructuring-bind (k expected) (list "__proto__" t)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

(it-sequential "js-rt-internal-key-p dunder-class"
  (destructuring-bind (k expected) (list "__class__" t)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

(it-sequential "js-rt-internal-key-p get-prefix"
  (destructuring-bind (k expected) (list "__get_foo" t)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

(it-sequential "js-rt-internal-key-p set-prefix"
  (destructuring-bind (k expected) (list "__set_foo" t)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

(it-sequential "js-rt-internal-key-p plain-key"
  (destructuring-bind (k expected) (list "name" nil)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

(it-sequential "js-rt-internal-key-p empty"
  (destructuring-bind (k expected) (list "" nil)
    (expect (cl-cc/javascript::%js-internal-key-p k) :to-equal expected)))

;;; ─── Object.keys / values / entries ─────────────────────────────────────────

(it-sequential "js-rt-object-keys-excludes-internals"
  (let* ((obj (cl-cc/javascript::%js-make-object "a" 1 "b" 2)))
    (setf (gethash "__proto__" obj) cl-cc/javascript::+js-null+)
    (let ((keys (sort (coerce (cl-cc/javascript::%js-object-keys obj) 'list) #'string<)))
      (expect keys :to-equal '("a" "b")))))

(it-sequential "js-rt-object-keys-includes-accessor-property-name"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 42))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "answer" desc)
    (let ((keys (coerce (cl-cc/javascript::%js-object-keys obj) 'list)))
      (expect keys :to-equal '("answer")))))

(it-sequential "js-rt-object-values"
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 10 "y" 20))
         (values (sort (coerce (cl-cc/javascript::%js-object-values obj) 'list) #'<)))
    (expect values :to-equal '(10 20))))

(it-sequential "js-rt-object-values-reads-accessor"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 123))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "answer" desc)
    (let ((values (coerce (cl-cc/javascript::%js-object-values obj) 'list)))
      (expect values :to-equal '(123)))))

(it-sequential "js-rt-object-entries"
  (let* ((obj     (cl-cc/javascript::%js-make-object "k" 99))
         (entries (cl-cc/javascript::%js-object-entries obj)))
    (expect (= 1 (length entries)) :to-be-truthy)
    (let ((pair (aref entries 0)))
      (expect (aref pair 0) :to-equal "k")
      (expect (= 99 (aref pair 1)) :to-be-truthy))))

(it-sequential "js-rt-object-own-keys-includes-accessor-property-name"
  (let* ((obj (cl-cc/javascript::%js-make-object "data" 1))
         (getter (lambda () 7))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "computed" desc)
    (let ((keys (sort (coerce (cl-cc/javascript::%js-object-own-keys obj) 'list) #'string<)))
      (expect keys :to-equal '("computed" "data"))
      (expect (member "__get_computed" keys :test #'string=) :to-be-falsy))))

(it-sequential "js-rt-object-symbol-own-property-keys"
  (let* ((obj (cl-cc/javascript::%js-make-object "name" "visible"))
         (sym (cl-cc/javascript::%js-make-symbol "secret")))
    (cl-cc/javascript::%js-set-prop obj sym 99)
    (expect (= 99 (cl-cc/javascript::%js-get-prop obj sym)) :to-be-truthy)
    (expect (coerce (cl-cc/javascript::%js-object-keys obj) 'list) :to-equal '("name"))
    (expect (coerce (cl-cc/javascript::%js-object-get-own-property-names obj) 'list) :to-equal '("name"))
    (let ((symbols (coerce (cl-cc/javascript::%js-object-get-own-property-symbols obj) 'list))
          (own-keys (coerce (cl-cc/javascript::%js-object-own-keys obj) 'list)))
      (expect (length symbols) :to-equal 1)
      (expect (first symbols) :to-be sym)
      (expect (member "name" own-keys :test #'equal) :to-be-truthy)
      (expect (member sym own-keys :test #'eq) :to-be-truthy))))

;;; ─── Object.assign ───────────────────────────────────────────────────────────

(it-sequential "js-rt-object-assign-merges"
  (let* ((target (cl-cc/javascript::%js-make-object "a" 1))
         (src1   (cl-cc/javascript::%js-make-object "b" 2))
         (src2   (cl-cc/javascript::%js-make-object "c" 3))
         (result (cl-cc/javascript::%js-object-assign target src1 src2)))
    (expect result :to-be target)
    (expect (= 1 (gethash "a" target)) :to-be-truthy)
    (expect (= 2 (gethash "b" target)) :to-be-truthy)
    (expect (= 3 (gethash "c" target)) :to-be-truthy)))

(it-sequential "js-rt-object-assign-skips-internal-slots"
  (let* ((proto (cl-cc/javascript::%js-make-object "inherited" 9))
         (src (cl-cc/javascript::%js-object-create proto))
         (target (cl-cc/javascript::%js-make-object)))
    (cl-cc/javascript::%js-set-prop src "a" 10)
    (cl-cc/javascript::%js-object-assign target src)
    (expect (= 10 (cl-cc/javascript::%js-get-prop target "a")) :to-be-truthy)
    (expect (cl-cc/javascript::%js-object-get-prototype-of target) :to-be cl-cc/javascript::+js-null+)
    (expect (nth-value 1 (gethash "__proto__" target)) :to-be-falsy)))

(it-sequential "js-rt-object-assign-copies-symbol-properties"
  (let* ((sym (cl-cc/javascript::%js-make-symbol "copy"))
         (src (cl-cc/javascript::%js-make-object "name" "source"))
         (target (cl-cc/javascript::%js-make-object)))
    (cl-cc/javascript::%js-set-prop src sym 77)
    (cl-cc/javascript::%js-object-assign target src)
    (expect (cl-cc/javascript::%js-get-prop target "name") :to-equal "source")
    (expect (= 77 (cl-cc/javascript::%js-get-prop target sym)) :to-be-truthy)
    (let ((symbols (coerce (cl-cc/javascript::%js-object-get-own-property-symbols target)
                           'list)))
      (expect (length symbols) :to-equal 1)
      (expect (first symbols) :to-be sym))))

(it-sequential "js-rt-object-spread-set-returns-obj"
  (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (let ((ret (cl-cc/javascript::%js-object-spread-set obj "b" 42)))
      (expect ret :to-be obj)
      (expect (= 42 (gethash "b" obj)) :to-be-truthy))))

;;; ─── Object.create / prototype ops ──────────────────────────────────────────

(it-sequential "js-rt-object-create-with-proto"
  (let* ((proto (cl-cc/javascript::%js-make-object "method" t))
         (obj   (cl-cc/javascript::%js-object-create proto)))
    (expect (cl-cc/javascript::%js-object-get-prototype-of obj) :to-be proto)))

(it-sequential "js-rt-object-create-null-proto"
  (let ((obj (cl-cc/javascript::%js-object-create cl-cc/javascript::+js-null+)))
    (expect (cl-cc/javascript::%js-object-get-prototype-of obj) :to-be cl-cc/javascript::+js-null+)))

(it-sequential "js-rt-object-set-prototype-of"
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 1))
         (proto2 (cl-cc/javascript::%js-make-object "tag" "v2")))
    (cl-cc/javascript::%js-object-set-prototype-of obj proto2)
    (expect (cl-cc/javascript::%js-object-get-prototype-of obj) :to-be proto2)))

(it-sequential "js-rt-object-extensibility-seal-freeze"
  (let* ((proto (cl-cc/javascript::%js-make-object "p" 1))
         (obj   (cl-cc/javascript::%js-object-create proto)))
    (expect (cl-cc/javascript::%js-object-extensible-p obj) :to-be-truthy)
    (expect (cl-cc/javascript::%js-object-get-prototype-of obj) :to-be proto)
    (expect (= 1 (cl-cc/javascript::%js-get-prop obj "p")) :to-be-truthy)
    (cl-cc/javascript::%js-object-prevent-extensions obj)
    (expect (cl-cc/javascript::%js-object-extensible-p obj) :to-be-falsy)
    (cl-cc/javascript::%js-set-prop obj "new-key" 2)
    (expect (nth-value 1 (gethash "new-key" obj)) :to-be-falsy)
    (let ((next-proto (cl-cc/javascript::%js-make-object "q" 2)))
      (expect (cl-cc/javascript::%js-reflect-set-prototype-of obj next-proto) :to-be-falsy)
      (expect (cl-cc/javascript::%js-object-get-prototype-of obj) :to-be proto))))

(it-sequential "js-rt-object-seal-and-freeze-mutations"
  (let ((sealed (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-object-seal sealed) :to-be sealed)
    (expect (cl-cc/javascript::%js-object-sealed-p sealed) :to-be-truthy)
    (expect (cl-cc/javascript::%js-delete sealed "a") :to-be-falsy)
    (expect (= 1 (gethash "a" sealed)) :to-be-truthy))
  (let ((frozen (cl-cc/javascript::%js-make-object "x" 10)))
    (expect (cl-cc/javascript::%js-object-freeze frozen) :to-be frozen)
    (expect (cl-cc/javascript::%js-object-frozen-p frozen) :to-be-truthy)
    (cl-cc/javascript::%js-set-prop frozen "x" 99)
    (expect (= 10 (gethash "x" frozen)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-delete frozen "x") :to-be-falsy)
    (expect (= 10 (gethash "x" frozen)) :to-be-truthy)))

;;; ─── Object.hasOwn ───────────────────────────────────────────────────────────

(it-sequential "js-rt-object-has-own present"
  (destructuring-bind (key expected) (list "a" t)
    (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-object-has-own obj key) :to-equal expected))))

(it-sequential "js-rt-object-has-own absent"
  (destructuring-bind (key expected) (list "z" nil)
    (let ((obj (cl-cc/javascript::%js-make-object "a" 1)))
    (expect (cl-cc/javascript::%js-object-has-own obj key) :to-equal expected))))

(it-sequential "js-rt-object-has-own-symbol-key"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (sym (cl-cc/javascript::%js-make-symbol "owned")))
    (cl-cc/javascript::%js-set-prop obj sym 42)
    (expect (cl-cc/javascript::%js-object-has-own obj sym) :to-be-truthy)
    (expect (cl-cc/javascript::%js-object-has-own
                   obj
                   (cl-cc/javascript::%js-make-symbol "owned")) :to-be-falsy)))

(it-sequential "js-rt-object-has-own-accessor-property"
  (let* ((obj (cl-cc/javascript::%js-make-object))
         (getter (lambda () 42))
         (desc (cl-cc/javascript::%js-make-object "get" getter)))
    (cl-cc/javascript::%js-object-define-property obj "answer" desc)
    (expect (cl-cc/javascript::%js-object-has-own obj "answer") :to-be-truthy)))

;;; ─── Object.fromEntries ──────────────────────────────────────────────────────

(it-sequential "js-rt-object-from-entries"
  (let* ((pairs (cl-cc/javascript::%js-make-array (%jr-arr "x" 10)
                                                   (%jr-arr "y" 20)))
         (obj   (cl-cc/javascript::%js-object-from-entries pairs)))
    (expect (= 10 (gethash "x" obj)) :to-be-truthy)
    (expect (= 20 (gethash "y" obj)) :to-be-truthy)))

(it-sequential "js-rt-object-from-entries-preserves-symbol-keys"
  (let* ((sym (cl-cc/javascript::%js-make-symbol "entry"))
         (pairs (cl-cc/javascript::%js-make-array (%jr-arr sym 88)))
         (obj (cl-cc/javascript::%js-object-from-entries pairs)))
    (expect (= 88 (cl-cc/javascript::%js-get-prop obj sym)) :to-be-truthy)
    (let ((symbols (coerce (cl-cc/javascript::%js-object-get-own-property-symbols obj)
                           'list)))
      (expect (length symbols) :to-equal 1)
      (expect (first symbols) :to-be sym))))

;;; ─── Object.withoutKeys ──────────────────────────────────────────────────────

(it-sequential "js-rt-object-without-keys"
  (let* ((obj  (cl-cc/javascript::%js-make-object "a" 1 "b" 2 "c" 3))
         (excl (%jr-arr "b"))
         (copy (cl-cc/javascript::%js-object-without-keys obj excl)))
    (expect (eq obj copy) :to-be-falsy)
    (expect (= 1 (gethash "a" copy)) :to-be-truthy)
    (expect (nth-value 1 (gethash "b" copy)) :to-be-falsy)
    (expect (= 3 (gethash "c" copy)) :to-be-truthy)))

;;; ─── Object.groupBy ──────────────────────────────────────────────────────────

(it-sequential "js-rt-object-group-by"
  (let* ((items  (%jr-arr 1 2 3 4))
         (key-fn (lambda (x index)
                   (declare (ignore index))
                   (if (evenp x) "even" "odd")))
         (grouped (cl-cc/javascript::%js-object-group-by items key-fn)))
    (expect (= 2 (length (gethash "even" grouped))) :to-be-truthy)
    (expect (= 2 (length (gethash "odd"  grouped))) :to-be-truthy)))

(it-sequential "js-rt-object-group-by-passes-index"
  (let* ((seen '())
         (items (%jr-arr "a" "b" "c"))
         (grouped (cl-cc/javascript::%js-object-group-by
                   items
                   (lambda (item index)
                     (push (list item index) seen)
                     (if (evenp index) "even-index" "odd-index")))))
    (expect seen :to-equal '(("c" 2) ("b" 1) ("a" 0)))
    (expect (%jr-list (gethash "even-index" grouped)) :to-equal '("a" "c"))
    (expect (%jr-list (gethash "odd-index" grouped)) :to-equal '("b"))))

(it-sequential "js-rt-object-group-by-null-prototype"
  (let* ((items (%jr-arr 1))
         (grouped (cl-cc/javascript::%js-object-group-by
                   items
                   (lambda (item index)
                     (declare (ignore item index))
                     "all"))))
    (expect (cl-cc/javascript::%js-object-get-prototype-of grouped) :to-be cl-cc/javascript::+js-null+)
    (expect (member "__proto__"
                          (coerce (cl-cc/javascript::%js-object-keys grouped) 'list)
                          :test #'string=) :to-be-falsy)))

;;; ─── Destructuring helpers ───────────────────────────────────────────────────

(it-sequential "js-rt-destructure-array-rest"
  (let* ((arr  (%jr-arr 10 20 30 40))
         (rest (cl-cc/javascript::%js-destructure-array arr 1 :rest)))
    (expect (= 3 (length rest)) :to-be-truthy)
    (expect (= 20 (aref rest 0)) :to-be-truthy)
    (expect (= 40 (aref rest 2)) :to-be-truthy)))

(it-sequential "js-rt-destructure-array-value-mode"
  (let* ((arr (cl-cc/javascript::%js-make-array 10))
         (result (cl-cc/javascript::%js-destructure-array arr 0 99 1 42)))
    (expect (= 10 (first result)) :to-be-truthy)
    (expect (= 42 (second result)) :to-be-truthy)))

(it-sequential "js-rt-destructure-object-rest"
  (let* ((obj    (cl-cc/javascript::%js-make-object "a" 1 "b" 2 "c" 3))
         (others (cl-cc/javascript::%js-destructure-object obj :rest "a")))
    (expect (nth-value 1 (gethash "a" others)) :to-be-falsy)
    (expect (= 2 (gethash "b" others)) :to-be-truthy)
    (expect (= 3 (gethash "c" others)) :to-be-truthy)))

(it-sequential "js-rt-destructure-object-value-mode"
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 7))
         (result (cl-cc/javascript::%js-destructure-object obj "x" 0 "y" 99)))
    (expect (= 7 (first result)) :to-be-truthy)
    (expect (= 99 (second result)) :to-be-truthy)))

;;; ─── 32-bit integer coercion ─────────────────────────────────────────────────

(it-sequential "js-rt-to-int32 small"
  (destructuring-bind (x expected) (list 5 5)
    (expect (= expected (cl-cc/javascript::%js-to-int32 x)) :to-be-truthy)))

(it-sequential "js-rt-to-int32 float"
  (destructuring-bind (x expected) (list 3.7d0 3)
    (expect (= expected (cl-cc/javascript::%js-to-int32 x)) :to-be-truthy)))

(it-sequential "js-rt-to-int32 over-32"
  (destructuring-bind (x expected) (list #x100000001 1)
    (expect (= expected (cl-cc/javascript::%js-to-int32 x)) :to-be-truthy)))

(it-sequential "js-rt-sign-extend32 positive"
  (destructuring-bind (n expected) (list 5 5)
    (expect (= expected (cl-cc/javascript::%js-sign-extend32 n)) :to-be-truthy)))

(it-sequential "js-rt-sign-extend32 max-int32"
  (destructuring-bind (n expected) (list #x7FFFFFFF 2147483647)
    (expect (= expected (cl-cc/javascript::%js-sign-extend32 n)) :to-be-truthy)))

(it-sequential "js-rt-sign-extend32 min-int32"
  (destructuring-bind (n expected) (list #x80000000 -2147483648)
    (expect (= expected (cl-cc/javascript::%js-sign-extend32 n)) :to-be-truthy)))

;;; ─── Bitwise operators ───────────────────────────────────────────────────────

(it-sequential "js-rt-bitwise-binops and"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bitwise-and #b1010 #b1100 #b1000)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-binops or"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bitwise-or #b1010 #b1100 #b1110)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-binops xor"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bitwise-xor #b1010 #b1100 #b0110)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-bitwise-not"
  (expect (= -1 (cl-cc/javascript::%js-bitwise-not 0)) :to-be-truthy)
  (expect (= -6 (cl-cc/javascript::%js-bitwise-not 5)) :to-be-truthy))

;;; ─── Shift operators ─────────────────────────────────────────────────────────

(it-sequential "js-rt-shift-ops shl"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-left 1 4 16)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops shr"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-shift-right -8 1 -4)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

(it-sequential "js-rt-shift-ops ushr"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-unsigned-shift-right -1 28 15)
    (expect (= expected (funcall fn a b)) :to-be-truthy)))

;;; ─── Unary / increment ops ───────────────────────────────────────────────────

(it-sequential "js-rt-unary-plus"
  (expect (= 42 (cl-cc/javascript::%js-unary-plus "42")) :to-be-truthy)
  (expect (= 0 (cl-cc/javascript::%js-unary-plus nil)) :to-be-truthy))

(it-sequential "js-rt-inc-dec-ops prefix-inc"
  (destructuring-bind (fn val expected) (list #'cl-cc/javascript::%js-prefix-inc 5 6)
    (expect (= expected (funcall fn val)) :to-be-truthy)))

(it-sequential "js-rt-inc-dec-ops prefix-dec"
  (destructuring-bind (fn val expected) (list #'cl-cc/javascript::%js-prefix-dec 5 4)
    (expect (= expected (funcall fn val)) :to-be-truthy)))

(it-sequential "js-rt-inc-dec-ops postfix-inc"
  (destructuring-bind (fn val expected) (list #'cl-cc/javascript::%js-postfix-inc 5 5)
    (expect (= expected (funcall fn val)) :to-be-truthy)))

(it-sequential "js-rt-inc-dec-ops postfix-dec"
  (destructuring-bind (fn val expected) (list #'cl-cc/javascript::%js-postfix-dec 5 5)
    (expect (= expected (funcall fn val)) :to-be-truthy)))

;;; ─── BigInt extras ───────────────────────────────────────────────────────────

(it-sequential "js-rt-bigint-constructor integer"
  (destructuring-bind (x expected) (list 42 42)
    (let ((bi (cl-cc/javascript::%js-bigint x)))
    (expect (cl-cc/javascript::js-bigint-p bi) :to-be-truthy)
    (expect (= expected (cl-cc/javascript::js-bigint-value bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-constructor float"
  (destructuring-bind (x expected) (list 3.9d0 3)
    (let ((bi (cl-cc/javascript::%js-bigint x)))
    (expect (cl-cc/javascript::js-bigint-p bi) :to-be-truthy)
    (expect (= expected (cl-cc/javascript::js-bigint-value bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-constructor string"
  (destructuring-bind (x expected) (list "100" 100)
    (let ((bi (cl-cc/javascript::%js-bigint x)))
    (expect (cl-cc/javascript::js-bigint-p bi) :to-be-truthy)
    (expect (= expected (cl-cc/javascript::js-bigint-value bi)) :to-be-truthy))))

(it-sequential "js-rt-bigint-to-string-radix decimal"
  (destructuring-bind (n radix expected) (list 255 10 "255")
    (let ((bi (cl-cc/javascript::%make-js-bigint n)))
    (expect (cl-cc/javascript::%js-bigint-to-string bi radix) :to-equal expected))))

(it-sequential "js-rt-bigint-to-string-radix hex"
  (destructuring-bind (n radix expected) (list 255 16 "ff")
    (let ((bi (cl-cc/javascript::%make-js-bigint n)))
    (expect (cl-cc/javascript::%js-bigint-to-string bi radix) :to-equal expected))))

(it-sequential "js-rt-bigint-to-string-radix binary"
  (destructuring-bind (n radix expected) (list 5 2 "101")
    (let ((bi (cl-cc/javascript::%make-js-bigint n)))
    (expect (cl-cc/javascript::%js-bigint-to-string bi radix) :to-equal expected))))

(it-sequential "js-rt-bigint-div-mod div"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-div 10 3 3)
    (let ((result (funcall fn a b)))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-div-mod mod"
  (destructuring-bind (fn a b expected) (list #'cl-cc/javascript::%js-bigint-mod 10 3 1)
    (let ((result (funcall fn a b)))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-compare lt"
  (destructuring-bind (a b expected) (list 3 5 -1)
    (expect (= expected (cl-cc/javascript::%js-bigint-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-bigint-compare eq"
  (destructuring-bind (a b expected) (list 5 5 0)
    (expect (= expected (cl-cc/javascript::%js-bigint-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-bigint-compare gt"
  (destructuring-bind (a b expected) (list 7 5 1)
    (expect (= expected (cl-cc/javascript::%js-bigint-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-bigint-shift lshift"
  (destructuring-bind (fn a n expected) (list #'cl-cc/javascript::%js-bigint-lshift 1 3 8)
    (let ((result (funcall fn a n)))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-shift rshift"
  (destructuring-bind (fn a n expected) (list #'cl-cc/javascript::%js-bigint-rshift 8 2 2)
    (let ((result (funcall fn a n)))
    (expect (= expected (cl-cc/javascript::js-bigint-value result)) :to-be-truthy))))

(it-sequential "js-rt-bigint-negate"
  (expect (= -42 (cl-cc/javascript::js-bigint-value
                 (cl-cc/javascript::%js-bigint-negate 42))) :to-be-truthy)
  (expect (= 7 (cl-cc/javascript::js-bigint-value
                 (cl-cc/javascript::%js-bigint-negate -7))) :to-be-truthy))

;;; ─── URI encoding ────────────────────────────────────────────────────────────

(it-sequential "js-rt-encode-uri-component"
  (expect (cl-cc/javascript::%js-encode-uri-component "hello world") :to-equal "hello%20world")
  (expect (cl-cc/javascript::%js-encode-uri-component "abc") :to-equal "abc"))

(it-sequential "js-rt-decode-uri-component"
  (expect (cl-cc/javascript::%js-decode-uri-component "hello%20world") :to-equal "hello world"))

;;; ─── Accessor / misc stubs ───────────────────────────────────────────────────

(it-sequential "js-rt-accessor-descriptor"
  (let* ((fn   (lambda () 42))
         (desc (cl-cc/javascript::%js-accessor "get" fn)))
    (expect (gethash "__accessor__" desc) :to-be-truthy)
    (expect (gethash "kind" desc) :to-equal "get")
    (expect (gethash "fn" desc) :to-be fn)))

(it-sequential "js-rt-new-target-returns-undefined"
  (expect (cl-cc/javascript::%js-new-target) :to-be cl-cc/javascript::+js-undefined+))

(it-sequential "js-rt-using-register-identity"
  (let ((r (list 1 2)))
    (expect (cl-cc/javascript::%js-using-register r) :to-be r)))

;;; ─── runtime-property.lisp: accessor-descriptor-p, put-entry, optional ops ──

(it-sequential "js-rt-accessor-descriptor-p get-accessor"
  (destructuring-bind (val expected) (list (cl-cc/javascript::%js-accessor "get" (lambda () 1)) t)
    (expect (cl-cc/javascript::%js-accessor-descriptor-p val) :to-equal expected)))

(it-sequential "js-rt-accessor-descriptor-p set-accessor"
  (destructuring-bind (val expected) (list (cl-cc/javascript::%js-accessor "set" (lambda (v) v)) t)
    (expect (cl-cc/javascript::%js-accessor-descriptor-p val) :to-equal expected)))

(it-sequential "js-rt-accessor-descriptor-p plain-ht"
  (destructuring-bind (val expected) (list (cl-cc/javascript::%js-make-object "x" 1) nil)
    (expect (cl-cc/javascript::%js-accessor-descriptor-p val) :to-equal expected)))

(it-sequential "js-rt-accessor-descriptor-p string"
  (destructuring-bind (val expected) (list "not-an-accessor" nil)
    (expect (cl-cc/javascript::%js-accessor-descriptor-p val) :to-equal expected)))

(it-sequential "js-rt-object-put-entry-accessor-routing"
  (let* ((ht  (cl-cc/javascript::%js-make-ht))
         (fn  (lambda () 42))
         (desc (cl-cc/javascript::%js-accessor "get" fn)))
    (cl-cc/javascript::%js-object-put-entry ht "foo" desc)
    (expect (gethash "__get_foo" ht) :to-be fn)
    (expect (nth-value 1 (gethash "foo" ht)) :to-be-falsy)))

(it-sequential "js-rt-optional-call real-fn"
  (destructuring-bind (func expected) (list (lambda () 99) 99)
    (let ((result (cl-cc/javascript::%js-optional-call func)))
    (if (eq expected :undef)
        (expect result :to-be cl-cc/javascript::+js-undefined+)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-optional-call undefined"
  (destructuring-bind (func expected) (list cl-cc/javascript::+js-undefined+ :undef)
    (let ((result (cl-cc/javascript::%js-optional-call func)))
    (if (eq expected :undef)
        (expect result :to-be cl-cc/javascript::+js-undefined+)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-optional-call null"
  (destructuring-bind (func expected) (list cl-cc/javascript::+js-null+ :undef)
    (let ((result (cl-cc/javascript::%js-optional-call func)))
    (if (eq expected :undef)
        (expect result :to-be cl-cc/javascript::+js-undefined+)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-optional-method-call-present"
  (let* ((obj    (cl-cc/javascript::%js-make-object "double" (lambda (n) (* 2 n))))
         (result (cl-cc/javascript::%js-optional-method-call obj "double" 5)))
    (expect (= 10 result) :to-be-truthy)))

(it-sequential "js-rt-optional-method-call-null"
  (expect (cl-cc/javascript::%js-optional-method-call cl-cc/javascript::+js-null+ "double" 5) :to-be cl-cc/javascript::+js-undefined+))

(it-sequential "js-rt-add-string-coercion"
  (expect (cl-cc/javascript::%js-add 4 "2") :to-equal "42")
  (expect (cl-cc/javascript::%js-add "a" "b") :to-equal "ab")
  (expect (= 6.0d0 (cl-cc/javascript::%js-add 4 2)) :to-be-truthy))
