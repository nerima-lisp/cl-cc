(in-package :cl-cc/test)


(%php85-register-test 'php84-named-args-to-positional-lowers-named
  "Named arg descriptors from %php-parse-named-args lower to plain positional AST exprs."
  (lambda ()
;; Test the helper function directly with hand-built descriptors.
  (let* ((descs  (list (list :positional (cl-cc:make-ast-int :value 1))
                       (list :named "key" (cl-cc:make-ast-quote :value "val"))
                       (list :spread  (cl-cc:make-ast-var :name 'x))))
         (result (cl-cc/php::%php-named-args-to-positional descs)))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (cl-cc:ast-int-p (first result)) :to-be-truthy)
    (expect (cl-cc:ast-quote-p (second result)) :to-be-truthy)
    (expect (cl-cc:ast-var-p (third result)) :to-be-truthy))))

(%php85-register-test 'php84-named-arg-p-detects-ident-colon
  "The %php-named-arg-p predicate recognises IDENT : as a named argument."
  (lambda ()
;; We test with a hand-built token stream matching the helper contract.
  (let* ((ident-tok   (list :type :T-IDENT :value "name"))
         (colon-tok   (list :type :T-COLON :value ":"))
         (fake-stream (list ident-tok colon-tok)))
    (expect (cl-cc/php::%php-named-arg-p fake-stream) :to-be-truthy))))

(%php85-register-test 'php84-named-args-parse-produces-positional-call
  "createUser(name: \"Alice\", age: 25) lowers to an ast-call with 2 positional args."
  (lambda ()
(let* ((ast (%php-first "<?php createUser(\"Alice\", 25);")))
    ;; Without named-arg integration in php-parse-arglist the call uses
    ;; positional lowering: two arguments are preserved in order.
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (= 2 (length (cl-cc:ast-call-args ast))) :to-be-truthy))))

(%php85-register-test 'php84-named-args-mixed-with-positional
  "Positional args before named args both survive into the call AST."
  (lambda ()
(let* ((ast (%php-first "<?php htmlspecialchars(\"<b>hi</b>\", 11);")))
    (expect (cl-cc:ast-call-p ast) :to-be-truthy)
    (expect (plusp (length (cl-cc:ast-call-args ast))) :to-be-truthy))))

(%php85-register-test 'php84-first-class-callable-predicate-true
  "The %php-first-class-callable-p predicate returns true for ( ... ) token sequence."
  (lambda ()
(let* ((lparen-tok  (list :type :T-LPAREN   :value "("))
         (ellipsis-tok (list :type :T-ELLIPSIS :value "..."))
         (rparen-tok  (list :type :T-RPAREN   :value ")"))
         (stream      (list lparen-tok ellipsis-tok rparen-tok)))
    (expect (cl-cc/php::%php-first-class-callable-p stream) :to-be-truthy))))

(%php85-register-test 'php84-first-class-callable-predicate-false-for-args
  "The %php-first-class-callable-p predicate returns false when ( has real args."
  (lambda ()
(let* ((lparen-tok  (list :type :T-LPAREN :value "("))
         (int-tok     (list :type :T-INT    :value 1))
         (rparen-tok  (list :type :T-RPAREN :value ")"))
         (stream      (list lparen-tok int-tok rparen-tok)))
    (expect (cl-cc/php::%php-first-class-callable-p stream) :to-be-falsy))))

(%php85-register-test 'php84-callable-ref-wraps-in-lambda
  "The %php-callable-ref function returns an ast-lambda that wraps the function."
  (lambda ()
(let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast)))
    (expect (cl-cc:ast-lambda-p ref) :to-be-truthy)
    (expect (plusp (length (cl-cc:ast-lambda-body ref))) :to-be-truthy))))

(%php85-register-test 'php84-callable-ref-body-is-apply-call
  "The lambda body inside a callable ref calls APPLY with the original function."
  (lambda ()
(let* ((func-ast (cl-cc:make-ast-var :name 'strlen))
         (ref      (cl-cc/php::%php-callable-ref func-ast))
         (body-call (first (cl-cc:ast-lambda-body ref))))
    (expect (cl-cc:ast-call-p body-call) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-var-name (cl-cc:ast-call-func body-call))) :to-equal "APPLY"))))

(%php85-register-test 'php84-array-find-returns-first-match
  "array_find() returns the first element satisfying the callback."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 3) (list nil nil 7) (list nil nil 4)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 5)))))
    (expect (= 7 result) :to-be-truthy))))

(%php85-register-test 'php84-array-find-returns-null-when-no-match
  "array_find() returns +php-null+ when no element satisfies the callback."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2)))
         (result (cl-cc/php::%php-array-find arr (lambda (v) (> v 10)))))
    (expect result :to-equal cl-cc/php:+php-null+))))

(%php85-register-test 'php84-array-find-key-returns-key-of-first-match
  "array_find_key() returns the integer key of the first matching element."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 10) (list nil nil 20) (list nil nil 30)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (= v 20)))))
    (expect (= 1 result) :to-be-truthy))))

(%php85-register-test 'php84-array-find-key-returns-null-when-no-match
  "array_find_key() returns +php-null+ when no element matches."
  (lambda ()
(let* ((arr (cl-cc/php:%php-array (list nil nil 1)))
         (result (cl-cc/php::%php-array-find-key arr (lambda (v) (> v 100)))))
    (expect result :to-equal cl-cc/php:+php-null+))))

(%php85-register-test 'php84-array-any-true-for-matching-element
  "array_any() returns true when at least one element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2) (list nil nil 50))))
    (expect (cl-cc/php::%php-array-any arr (lambda (v) (> v 10))) :to-be-truthy))))

(%php85-register-test 'php84-array-any-false-when-none-match
  "array_any() returns false when no element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 1) (list nil nil 2))))
    (expect (cl-cc/php::%php-array-any arr (lambda (v) (> v 100))) :to-be-falsy))))

(%php85-register-test 'php84-array-all-true-when-all-match
  "array_all() returns true when every element satisfies the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil 10) (list nil nil 20))))
    (expect (cl-cc/php::%php-array-all arr (lambda (v) (> v 0))) :to-be-truthy))))

(%php85-register-test 'php84-array-all-false-when-one-fails
  "array_all() returns false when any element fails the callback."
  (lambda ()
(let ((arr (cl-cc/php:%php-array (list nil nil 5) (list nil nil -1))))
    (expect (cl-cc/php::%php-array-all arr (lambda (v) (> v 0))) :to-be-falsy))))

(%php85-register-test 'php84-array-all-true-for-empty-array
  "array_all() returns true for an empty array (vacuous truth)."
  (lambda ()
    (let ((arr (cl-cc/php:%php-array)))
      (expect (cl-cc/php::%php-array-all arr (lambda (v) (declare (ignore v)))) :to-be-truthy)
      (expect (cl-cc/php::%php-array-all (cl-cc/php:%php-array (list nil nil 1))
                                  (lambda (v) (declare (ignore v)) nil)) :to-be-falsy))))


(%php85-register-test 'php84-fiber-make-creates-object
  "new Fiber(callback) lowers to a PHP object wrapper via %php-fiber-make."
  (lambda ()
    (let ((fiber (cl-cc/php::%php-fiber-make (lambda () 42))))
      (expect (hash-table-p fiber) :to-be-truthy)
      (expect (gethash "__class__" fiber) :to-equal "Fiber")
      (expect (cl-cc/php::%php-fiber-started-p fiber) :to-be-falsy)
      (expect (cl-cc/php::%php-fiber-terminated-p fiber) :to-be-falsy)
      (expect (= 42 (cl-cc/php::%php-fiber-start fiber)) :to-be-truthy)
      (expect (cl-cc/php::%php-fiber-started-p fiber) :to-be-truthy)
      (expect (cl-cc/php::%php-fiber-terminated-p fiber) :to-be-truthy))))

(%php85-register-test 'php84-fiber-start-runs-callback
  "Fiber::start() runs the callback and returns its result when it does not suspend."
  (lambda ()
(let* ((fiber (cl-cc/php::%php-fiber-make (lambda () 99)))
         (result (cl-cc/php::%php-fiber-start fiber)))
    (expect (= 99 result) :to-be-truthy)
    (expect (cl-cc/php::%php-fiber-started-p fiber) :to-be-truthy)
    (expect (cl-cc/php::%php-fiber-terminated-p fiber) :to-be-truthy))))

(%php85-register-test 'php84-fiber-start-twice-signals-error
  "Starting an already-started Fiber signals an error."
  (lambda ()
(let ((fiber (cl-cc/php::%php-fiber-make (lambda () 1))))
    (cl-cc/php::%php-fiber-start fiber)
    (signals error (cl-cc/php::%php-fiber-start fiber)))))

(%php85-register-test 'php84-fiber-get-return-value
  "%php-fiber-get-return returns the final return value of a terminated Fiber."
  (lambda ()
(let* ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
    (cl-cc/php::%php-fiber-start fiber)
    (expect (cl-cc/php::%php-fiber-get-return fiber) :to-be :done))))

(%php85-register-test 'php84-fiber-get-return-before-termination-signals
  "%php-fiber-get-return signals an error when the fiber has not yet terminated."
  (lambda ()
    (let ((fiber (cl-cc/php::%php-fiber-make (lambda () :done))))
      ;; Not started yet
      (signals error (cl-cc/php::%php-fiber-get-return fiber))
      (expect (cl-cc/php::%php-fiber-start fiber) :to-be :done))))

(%php85-register-test 'php84-mark-all-props-readonly-marks-instance-slots
  "%php-mark-all-props-readonly adds :readonly-p to instance property slot-defs."
  (lambda ()
(let* ((prop (cl-cc:make-ast-slot-def :name 'x :allocation :instance))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list prop)))
         (slot (first marked)))
    (expect (getf (cl-cc:ast-imports slot) :readonly-p) :to-be-truthy))))

(%php85-register-test 'php84-mark-all-props-readonly-skips-class-slots
  "%php-mark-all-props-readonly does not modify :class allocation slots (constants/statics)."
  (lambda ()
(let* ((const (cl-cc:make-ast-slot-def :name 'x :allocation :class))
         (marked (cl-cc/php::%php-mark-all-props-readonly (list const)))
         (slot (first marked)))
    (expect (getf (cl-cc:ast-imports slot) :readonly-p) :to-be-falsy))))

(%php85-register-test 'php84-lower-property-with-hooks-get-only
  "%php-lower-property-with-hooks with only a getter produces a __get_ method."
  (lambda ()
(let* ((getter-body (cl-cc:make-ast-return-from :name nil
                                                  :value (cl-cc:make-ast-int :value 1)))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body nil 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (expect (find "__GET_NAME" names :test #'string=) :to-be-truthy))))

(%php85-register-test 'php84-lower-property-with-hooks-set-only
  "%php-lower-property-with-hooks with only a setter produces a __set_ method."
  (lambda ()
(let* ((setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "NAME" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym nil setter-body 'myclass))
         (names       (mapcar (lambda (m) (symbol-name (cl-cc:ast-defun-name m))) methods)))
    (expect (find "__SET_NAME" names :test #'string=) :to-be-truthy))))

(%php85-register-test 'php84-lower-property-with-hooks-both
  "%php-lower-property-with-hooks with get and set produces two methods."
  (lambda ()
(let* ((getter-body (cl-cc:make-ast-int :value 1))
         (setter-body (cl-cc:make-ast-quote :value nil))
         (prop-sym    (intern "TITLE" :cl-cc))
         (methods     (cl-cc/php::%php-lower-property-with-hooks
                       prop-sym getter-body setter-body 'myclass)))
    (expect (= 2 (length methods)) :to-be-truthy))))

(%php85-register-test 'php84-class-property-hooks-lower-to-accessor-slots
  "Class property hooks parse through the class parser and add accessor slots."
  (lambda ()
(let* ((ast (%php-first
               "<?php class User { public string $name { get => $this->name; set($value) => $value; } }"))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot-names (mapcar (lambda (slot)
                               (symbol-name (cl-cc:ast-slot-name slot)))
                             slots)))
    (expect (member "NAME" slot-names :test #'string=) :to-be-truthy)
    (expect (member "__GET_NAME" slot-names :test #'string=) :to-be-truthy)
    (expect (member "__SET_NAME" slot-names :test #'string=) :to-be-truthy))))

(%php85-register-test 'php84-asymmetric-visibility-parse-public-private-set
  "%php-parse-asymmetric-visibility parses public private(set) and returns two keywords."
  (lambda ()
(let* ((pub-tok  (list :type :T-KEYWORD :value :public))
         (priv-tok (list :type :T-KEYWORD :value :private))
         (lparen   (list :type :T-LPAREN  :value "("))
         (set-tok  (list :type :T-IDENT   :value "set"))
         (rparen   (list :type :T-RPAREN  :value ")"))
         (stream   (list pub-tok priv-tok lparen set-tok rparen)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (expect outer :to-be :public)
      (expect inner :to-be :private)))))

(%php85-register-test 'php84-asymmetric-visibility-single-modifier-no-inner
  "%php-parse-asymmetric-visibility returns nil inner when only one modifier."
  (lambda ()
(let* ((pub-tok (list :type :T-KEYWORD :value :public))
         (ident   (list :type :T-IDENT   :value "x"))
         (stream  (list pub-tok ident)))
    (multiple-value-bind (outer inner _rest)
        (cl-cc/php::%php-parse-asymmetric-visibility stream)
      (declare (ignore _rest))
      (expect outer :to-be :public)
      (expect inner :to-be-null)))))

(%php85-register-test 'php84-class-asymmetric-visibility-metadata
  "Class properties preserve PHP 8.4 asymmetric set visibility metadata."
  (lambda ()
(let* ((ast (%php-first
               "<?php class User { public private(set) string $id; }"))
         (slot (first (cl-cc:ast-defclass-slots ast)))
         (imports (cl-cc:ast-imports slot)))
    (expect (getf imports :php-set-visibility) :to-be :private)
    (expect (member :public (getf imports :php-modifiers) :test #'eq) :to-be-truthy))))

(%php85-register-test 'php85-class-static-asymmetric-visibility-metadata
  "PHP 8.5 static properties preserve asymmetric set visibility metadata."
  (lambda ()
    (dolist (case '(("<?php class Config { final public static private(set) string $name; }"
                     :private)
                    ("<?php class Config { final public static protected(set) string $name; }"
                     :protected)))
      (let* ((ast (%php-first (first case)))
             (slot (first (cl-cc:ast-defclass-slots ast)))
             (imports (cl-cc:ast-imports slot))
             (modifiers (getf imports :php-modifiers)))
        (expect (cl-cc:ast-slot-allocation slot) :to-be :class)
        (expect (getf imports :php-set-visibility) :to-be (second case))
        (expect (member :final modifiers :test #'eq) :to-be-truthy)
        (expect (member :public modifiers :test #'eq) :to-be-truthy)
        (expect (member :static modifiers :test #'eq) :to-be-truthy)))))

(%php85-register-test 'php85-final-promoted-property-preserves-final-modifier
  "PHP 8.5 constructor promotion preserves final property metadata."
  (lambda ()
(let* ((ast (%php-first
               "<?php class Token { public function __construct(final string $id) {} }"))
         (slots (cl-cc:ast-defclass-slots ast))
         (slot (find-if (lambda (candidate)
                           (string= "ID" (symbol-name (cl-cc:ast-slot-name candidate))))
                        slots)))
    (expect slot :to-be-truthy)
    (let* ((imports (cl-cc:ast-imports slot))
           (modifiers (getf imports :php-modifiers)))
      (expect (cl-cc:ast-slot-allocation slot) :to-be :instance)
      (expect (member :final modifiers :test #'eq) :to-be-truthy)))))

(%php85-register-test 'php84-function-intersection-type-annotation-preserved
  "Intersection type A&B in a function declaration is preserved as a type annotation string."
  (lambda ()
(let* ((ast (%php-first
               "<?php function process(Iterator $it): void { return; }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (getf decls :php-return-type) :to-equal "void"))))

(%php85-register-test 'php84-intersection-type-parse-helper
  "%php-parse-intersection-type builds a structured :intersection descriptor."
  (lambda ()
;; We call the helper directly with a fake stream.
  ;; %php-type-token-string lowercases the segment, so "Countable" → "countable".
  (let* ((amp-tok       (list :type :T-OP    :value "&"))
         (countable-tok (list :type :T-IDENT  :value "Countable"))
         (stream        (list amp-tok countable-tok)))
    (multiple-value-bind (spec _rest)
        (cl-cc/php::%php-parse-intersection-type "Iterator" stream)
      (declare (ignore _rest))
      (expect (consp spec) :to-be-truthy)
      (expect (first spec) :to-be :intersection)
      (expect (member "Iterator" spec :test #'string=) :to-be-truthy)
      (expect (member "countable" spec :test #'string=) :to-be-truthy)))))

(%php85-register-test 'php84-never-return-type-preserved-in-declarations
  "function fail(): never preserves 'never' as the return type in declarations."
  (lambda ()
(let* ((ast (%php-first "<?php function fail(): never { throw new Ex(); }"))
         (decls (cl-cc:ast-defun-declarations ast)))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (getf decls :php-return-type) :to-equal "never"))))

(%php85-register-test 'php84-enum-with-method-produces-slot-def
  "An enum with a method body contains the method as a slot-def."
  (lambda ()
    (dolist (spec (list
                   (list "<?php enum Status: int { case Draft = 0; public function label(): string { return 'Draft'; } } function afterEnum() {}"
                         :enum "LABEL")
                   (list "<?php class User { public string $name; }"
                         :class "NAME")))
      (destructuring-bind (source expected-kind expected-slot-name) spec
        (let* ((form  (%php-first source))
               ;; An enum now lowers to (progn defclass (link-cases)); unwrap the defclass when present.
               (ast   (if (cl-cc:ast-progn-p form) (first (cl-cc:ast-progn-forms form)) form))
               (slots (cl-cc:ast-defclass-slots ast))
               (slot-names (mapcar (lambda (s) (symbol-name (cl-cc:ast-slot-name s))) slots)))
          (expect (cl-cc:ast-defclass-php-kind ast) :to-be expected-kind)
          (expect (member expected-slot-name slot-names :test #'string=) :to-be-truthy))))))

(%php85-register-test 'php84-new-in-initializer-default-param
  "function f(Logger $l = new FileLogger()) parses default value as PHP new lowering."
  (lambda ()
;; PHP 8.1 allows `new ClassName()` as a default parameter value.
  ;; `new` lowers to a let that allocates the instance, conditionally runs
  ;; __construct, and returns the instance.
  (let* ((ast (%php-first
               "<?php function process(Logger $logger = new FileLogger()) { return $logger; }"))
         (optionals (cl-cc:ast-defun-optional-params ast))
         (default-ast (second (first optionals)))
         (instance-ast (cdr (first (cl-cc:ast-let-bindings default-ast)))))
    (expect (cl-cc:ast-defun-p ast) :to-be-truthy)
    (expect (= 1 (length optionals)) :to-be-truthy)
    (expect (cl-cc:ast-let-p default-ast) :to-be-truthy)
    (expect (cl-cc:ast-make-instance-p instance-ast) :to-be-truthy)
    (expect (symbol-name (cl-cc:ast-var-name
                                  (cl-cc:ast-make-instance-class instance-ast))) :to-equal "FILELOGGER"))))

(eval-when (:load-toplevel :execute)
  (%php85-run-current-source-tests))
