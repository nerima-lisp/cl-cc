;;;; PHP object-model, type metadata, SPL, and reflection builtins.

(in-package :cl-cc/php)

;;; ─── Type / class helpers ─────────────────────────────────────────────────────

(defparameter *php-runtime-class-tags* (make-hash-table :test #'equal))
(defparameter *php-runtime-interface-tags* (make-hash-table :test #'equal))
(defparameter *php-runtime-class-methods* (make-hash-table :test #'equal))
(defparameter *php-runtime-class-aliases* (make-hash-table :test #'equal))

(defun %php-runtime-type-key (type-name)
  (string-upcase (%php-stringify type-name)))

(defun %php-runtime-class-canonical-key (class-name)
  (let ((current (%php-runtime-type-key class-name))
        (seen (make-hash-table :test #'equal)))
    (loop
      (let ((next (gethash current *php-runtime-class-aliases*)))
        (when (or (null next) (gethash current seen))
          (return current))
        (setf (gethash current seen) t
              current next)))))

(defun %php-defined-class-symbol-p (class-name)
  (let ((sym (find-symbol (%php-runtime-type-key class-name) :cl-cc/php)))
    (and sym (fboundp sym))))

(defun %php-runtime-class-alias-name-forbidden-p (alias)
  (let ((key (%php-runtime-type-key alias)))
    (or (string= key "ARRAY")
        (string= key "CALLABLE"))))

(defun %php-register-runtime-class-tag (class-name)
  (setf (gethash (%php-runtime-type-key class-name) *php-runtime-class-tags*) t))

(defun %php-register-runtime-interface-tag (interface-name)
  (setf (gethash (%php-runtime-type-key interface-name) *php-runtime-interface-tags*) t))

(defun %php-register-runtime-class-methods (class-name methods)
  (setf (gethash (%php-runtime-type-key class-name) *php-runtime-class-methods*)
        (mapcar #'%php-stringify methods)))

(defun %php-runtime-class-tag-exists-p (class-name)
  (gethash (%php-runtime-class-canonical-key class-name) *php-runtime-class-tags*))

(defun %php-runtime-interface-tag-exists-p (interface-name)
  (gethash (%php-runtime-type-key interface-name) *php-runtime-interface-tags*))

(defun %php-runtime-class-methods (class-name)
  (gethash (%php-runtime-class-canonical-key class-name) *php-runtime-class-methods*))

(defun %php-runtime-class-method-exists-p (class-name method)
  (not (null (find (%php-stringify method)
                   (%php-runtime-class-methods class-name)
                   :test #'string-equal))))

(defun %php-register-php85-runtime-types ()
  (dolist (class-name '("NoDiscard"
                        "DelayedTargetValidation"
                        "Closure"
                        "CurlSharePersistentHandle"
                        "Dom\\Element"
                        "Dom\\HTMLCollection"
                        "Dom\\HTMLDocument"
                        "Filter\\FilterException"
                        "Filter\\FilterFailedException"
                        "IntlListFormatter"
                        "Locale"
                        "NumberFormatter"
                        "Pdo\\Sqlite"
                        "SoapClient"
                        "SoapFault"
                        "SoapServer"
                        "ReflectionConstant"
                        "ReflectionProperty"
                        "SQLite3Stmt"
                        "Uri\\UriError"
                        "Uri\\UriException"
                        "Uri\\InvalidUriException"
                        "Uri\\UriComparisonMode"
                        "Uri\\Rfc3986\\Uri"
                        "Uri\\WhatWg\\InvalidUrlException"
                        "Uri\\WhatWg\\UrlValidationErrorType"
                        "Uri\\WhatWg\\UrlValidationError"
                        "Uri\\WhatWg\\Url"
                        "XSLTProcessor"))
    (%php-register-runtime-class-tag class-name))
  (%php-register-runtime-interface-tag "Dom\\ParentNode")
  (%php-register-runtime-class-methods
   "Dom\\Element"
   '("getElementsByClassName" "insertAdjacentHTML"))
  (%php-register-runtime-class-methods
   "Dom\\HTMLDocument"
   '("getElementsByName"))
  (%php-register-runtime-class-methods
   "Closure"
   '("getCurrent"))
  (%php-register-runtime-class-methods
   "Locale"
   '("addLikelySubtags" "isRightToLeft" "minimizeSubtags"))
  (%php-register-runtime-class-methods
   "Pdo\\Sqlite"
   '("setAuthorizer"))
  (%php-register-runtime-class-methods
   "SoapClient"
   '("__construct" "__getTypes"))
  (%php-register-runtime-class-methods
   "SoapFault"
   '("__construct"))
  (%php-register-runtime-class-methods
   "SoapServer"
   '("__construct" "fault"))
  (%php-register-runtime-class-methods
   "XSLTProcessor"
   '("__construct" "getParameter" "setParameter" "removeParameter"))
  (%php-register-runtime-class-methods
   "SQLite3Stmt"
   '("busy"))
  (%php-register-runtime-class-methods
   "ReflectionConstant"
   '("getFileName" "getExtension" "getExtensionName" "getAttributes"
     "isDeprecated"))
  (%php-register-runtime-class-methods
   "ReflectionProperty"
   '("getMangledName"))
  (%php-register-runtime-class-methods
   "Uri\\Rfc3986\\Uri"
   '("__construct" "__debugInfo" "equals" "getFragment" "getHost"
     "getPassword" "getPath" "getPort" "getQuery" "getRawFragment"
     "getRawHost" "getRawPassword" "getRawPath" "getRawQuery"
     "getRawScheme" "getRawUserInfo" "getRawUsername" "getScheme"
     "getUserInfo" "getUsername" "parse" "resolve" "__serialize"
     "toRawString" "toString" "__unserialize" "withFragment" "withHost"
     "withPath" "withPort" "withQuery" "withScheme" "withUserInfo"))
  (%php-register-runtime-class-methods
   "Uri\\WhatWg\\Url"
   '("__construct" "__debugInfo" "equals" "getAsciiHost" "getFragment"
     "getPassword" "getPath" "getPort" "getQuery" "getScheme"
     "getUnicodeHost" "getUsername" "parse" "resolve" "__serialize"
     "toAsciiString" "toUnicodeString" "__unserialize" "withFragment"
     "withHost" "withPassword" "withPath" "withPort" "withQuery"
     "withScheme" "withUsername"))
  (%php-register-runtime-class-methods
   "Uri\\WhatWg\\UrlValidationError"
   '("__construct")))

(%php-register-php85-runtime-types)

(defun %php-class-exists (class-name &optional autoload)
  "PHP class_exists: check if class is defined."
  (declare (ignore autoload))
  (let ((canonical-key (%php-runtime-class-canonical-key class-name)))
    (or (%php-defined-class-symbol-p class-name)
        (%php-defined-class-symbol-p canonical-key)
        (gethash canonical-key *php-runtime-class-tags*))))

(defun %php-class-alias (class-name alias &optional autoload)
  "PHP class_alias: create a runtime-visible class alias."
  (declare (ignore autoload))
  (let* ((alias-string (%php-stringify alias))
         (alias-key (%php-runtime-type-key alias-string))
         (class-key (%php-runtime-class-canonical-key class-name)))
    (when (%php-runtime-class-alias-name-forbidden-p alias-string)
      (%php-throw 'value-error
                  (format nil "class_alias(): Argument #2 ($alias) must not be ~S" alias-string)))
    (cond
      ((not (or (%php-defined-class-symbol-p class-name)
                (%php-defined-class-symbol-p class-key)
                (gethash class-key *php-runtime-class-tags*)))
       nil)
      ((or (%php-defined-class-symbol-p alias-string)
           (gethash alias-key *php-runtime-class-tags*)
           (gethash alias-key *php-runtime-interface-tags*)
           (gethash alias-key *php-runtime-class-aliases*))
       nil)
      (t
       (setf (gethash alias-key *php-runtime-class-aliases*) class-key)
       t))))

(defun %php-interface-exists (interface-name &optional autoload)
  "PHP interface_exists: check if interface is defined."
  (declare (ignore autoload))
  (or (%php-defined-class-symbol-p interface-name)
      (%php-runtime-interface-tag-exists-p interface-name)))

(defun %php-function-exists (function-name)
  "PHP function_exists: check if function is defined."
  (not (null (%php-lookup-builtin (%php-stringify function-name)))))

(defun %php-method-exists (object method)
  "PHP method_exists: check if method exists on object or runtime class name."
  (cond
    ((or (hash-table-p object)
         (typep object 'cl-cc/vm::vm-hash-table-object)
         (and (vectorp object)
              (plusp (length object))
              (hash-table-p (aref object 0))))
     (not (null (%php-object-method object method))))
    ((or (stringp object) (symbolp object))
     (%php-runtime-class-method-exists-p object method))))

(defun %php-property-exists (object property)
  "PHP property_exists: check if property exists."
  (let ((property-name (%php-stringify property)))
    (cond
      ((or (hash-table-p object)
           (typep object 'cl-cc/vm::vm-hash-table-object)
           (and (vectorp object)
                (plusp (length object))
                (hash-table-p (aref object 0))))
       (some (lambda (pair)
               (string= (%php-stringify (car pair)) property-name))
             (%php-object-visible-pairs object)))
      ((or (stringp object) (symbolp object))
       (let ((descriptor (%php-reflection-class-descriptor object)))
         (when descriptor
           (some (lambda (slot)
                   (string= (%php-stringify (cl-cc/vm::slot-definition-name slot))
                            property-name))
                 (cl-cc/vm::class-slots descriptor)))))
       (t nil))))

(defun %php-get-class (object)
  "PHP get_class: return class name of object."
  (%php-object-class-name object))

(defun %php-get-parent-class (object)
  "PHP get_parent_class: return parent class name."
  (let ((storage (%php-object-hashlike-storage object)))
    (cond
      (storage
       (let ((parent (or (gethash "__parent__" storage)
                         (gethash :__parent__ storage))))
         (unless (%php-null-p parent)
           (%php-stringify parent))))
      ((or (stringp object) (symbolp object))
       (let ((descriptor (%php-reflection-class-descriptor object)))
         (when descriptor
           (let ((parent (or (gethash :__parent__ descriptor)
                             (gethash "__parent__" descriptor))))
             (unless (%php-null-p parent)
               (%php-stringify parent))))))
      (t nil))))

(defun %php-is-a (object class-name &optional allow-string)
  "PHP is_a: check if object is an instance of class."
  (declare (ignore allow-string))
  (let ((storage (%php-object-hashlike-storage object)))
    (cond
      (storage
       (let ((cls (or (gethash "__class__" storage)
                      (gethash :__class__ storage))))
         (and (not (%php-null-p cls))
              (string= (%php-runtime-class-canonical-key cls)
                       (%php-runtime-class-canonical-key class-name)))))
      ((or (stringp object) (symbolp object))
       (string= (%php-runtime-class-canonical-key object)
                (%php-runtime-class-canonical-key class-name)))
      (t nil))))

(defun %php-instanceof (object class-name)
  "PHP instanceof: check if object is an instance of class."
  (%php-is-a object class-name))

(defun %php-get-object-vars (object)
  "PHP get_object_vars: get properties of object as array."
  (when (%php-object-class-name object)
    (let ((result (%php-make-array)))
      (dolist (pair (%php-object-visible-pairs object))
        (%php-array-set result (car pair) (cdr pair)))
      result)))

(defun %php-method-table-methods (methods)
  (let ((names nil))
    (when (hash-table-p methods)
      (dolist (pair (%php-array-pairs methods))
        (let ((value (cdr pair)))
          (when (stringp value)
            (push value names)))))
    (nreverse names)))

(defun %php-get-class-methods (class-or-object)
  "PHP get_class_methods: get class method names."
  (let ((object-methods nil)
        (class-name class-or-object))
    (let ((storage (%php-object-hashlike-storage class-or-object)))
      (when storage
        (setf object-methods
              (%php-method-table-methods (gethash "__methods__" storage)))
        (let ((cls (gethash "__class__" storage)))
          (setf class-name (unless (%php-null-p cls) cls)))))
    (let ((result (%php-make-array))
          (seen (make-hash-table :test #'equal)))
      (dolist (method (append object-methods
                              (when class-name
                                (%php-runtime-class-methods class-name)))
               result)
        (let ((name (%php-stringify method)))
          (unless (gethash name seen)
            (setf (gethash name seen) t)
            (%php-array-set result (%php-array-next-auto-index result) name)))))))

(defun %php-reflection-class-descriptor-p (value)
  (and (hash-table-p value)
       (nth-value 1 (gethash :__name__ value))))

(defun %php-reflection-class-symbol-from-name (name)
  (let* ((upcased (string-upcase (%php-stringify name)))
         (found (find-symbol upcased :cl-cc/php)))
    (or found (intern upcased :cl-cc/php))))

(defun %php-reflection-class-symbol (class)
  (cond
    ((symbolp class) class)
    ((%php-reflection-class-descriptor-p class)
     (gethash :__name__ class))
    ((and (cl-cc/vm::%vm-vector-instance-p class)
          (%php-reflection-class-descriptor-p (aref class 0)))
     (gethash :__name__ (aref class 0)))
    ((hash-table-p class)
     (let ((cls (or (gethash :__class__ class)
                    (%php-array-ref class "__class__"))))
       (unless (%php-null-p cls)
         (%php-reflection-class-symbol cls))))
    (t
     (%php-reflection-class-symbol-from-name class))))

(defun %php-reflection-symbol-name (name)
  (cond
    ((symbolp name) (symbol-name name))
    ((stringp name) name)
    (t (%php-stringify name))))

(defun %php-reflection-class-descriptor-from-table (symbol table)
  (when (and symbol (hash-table-p table))
    (let ((target (%php-reflection-symbol-name symbol)))
      (or (gethash symbol table)
          (loop for key being the hash-keys of table
                using (hash-value value)
                when (and (%php-reflection-class-descriptor-p value)
                          (string-equal (%php-reflection-symbol-name key) target))
                  return value)))))

(defun %php-reflection-class-descriptor-from-state (symbol)
  (when (and symbol cl-cc/vm:*vm-current-state*)
    (or (%php-reflection-class-descriptor-from-table
         symbol
         (cl-cc/vm:vm-class-registry cl-cc/vm:*vm-current-state*))
        (%php-reflection-class-descriptor-from-table
         symbol
         (cl-cc/vm:vm-global-vars cl-cc/vm:*vm-current-state*)))))

(defun %php-reflection-class-descriptor (class)
  (cond
    ((%php-reflection-class-descriptor-p class) class)
    ((and (cl-cc/vm::%vm-vector-instance-p class)
          (%php-reflection-class-descriptor-p (aref class 0)))
     (aref class 0))
    ((hash-table-p class)
     (let ((cls (or (gethash :__class__ class)
                    (%php-array-ref class "__class__"))))
       (unless (%php-null-p cls)
         (%php-reflection-class-descriptor cls))))
    (t
     (let ((sym (%php-reflection-class-symbol class)))
       (%php-reflection-class-descriptor-from-state sym)))))

(defun %php-reflection-interface-p (name)
  (not (null (gethash name *php-interface-registry*))))

(defun %php-reflection-unique-symbols (symbols)
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (symbol symbols (nreverse result))
      (let ((name (%php-reflection-symbol-name symbol)))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (push symbol result))))))

(defun %php-reflection-symbols-to-array (symbols)
  (let ((result (%php-make-array)))
    (dolist (symbol symbols result)
      (let ((name (%php-reflection-symbol-name symbol)))
        (%php-array-set result name name)))))

(defun %php-reflection-interface-tree (interface)
  (let ((record (gethash interface *php-interface-registry*)))
    (cons interface
          (loop for parent in (getf record :parents)
                append (%php-reflection-interface-tree parent)))))

(defun %php-reflection-class-parent-symbols (class)
  (labels ((walk (descriptor)
             (loop for super in (and descriptor (gethash :__superclasses__ descriptor))
                   unless (%php-reflection-interface-p super)
                     append (cons super
                                  (walk (%php-reflection-class-descriptor super))))))
    (%php-reflection-unique-symbols
     (walk (%php-reflection-class-descriptor class)))))

(defun %php-reflection-class-interface-symbols (class)
  (labels ((walk (descriptor)
             (loop for super in (and descriptor (gethash :__superclasses__ descriptor))
                   append (if (%php-reflection-interface-p super)
                              (%php-reflection-interface-tree super)
                              (walk (%php-reflection-class-descriptor super))))))
    (%php-reflection-unique-symbols
     (walk (%php-reflection-class-descriptor class)))))

(defun %php-reflection-trait-applications (symbol)
  (when symbol
    (let ((target (%php-reflection-symbol-name symbol)))
      (or (gethash target *php-trait-applications*)
          (loop for key being the hash-keys of *php-trait-applications*
                using (hash-value value)
                when (string-equal (%php-reflection-symbol-name key) target)
                  return value)))))

(defun %php-reflection-class-trait-symbols (class)
  (let* ((symbol (%php-reflection-class-symbol class))
         (applications (%php-reflection-trait-applications symbol)))
    (%php-reflection-unique-symbols
     (loop for application in (reverse applications)
           append (getf application :trait-names)))))

;;; ─── SPL data structures ────────────────────────────────────────────────────

(defun %php-spl-class-name (name)
  (let ((s (%php-stringify name)))
    (cond
      ((string-equal s "SplStack") "SplStack")
      ((string-equal s "SplQueue") "SplQueue")
      ((string-equal s "SplDoublyLinkedList") "SplDoublyLinkedList")
      ((string-equal s "SplMinHeap") "SplMinHeap")
      ((string-equal s "SplMaxHeap") "SplMaxHeap")
      ((string-equal s "SplFixedArray") "SplFixedArray")
      (t s))))

(defun %php-spl-index (value)
  (truncate (if (numberp value)
                value
                (%php-to-number (%php-stringify value)))))

(defun %php-spl-number (value)
  (if (numberp value)
      value
      (%php-to-number (%php-stringify value))))

(defun %php-spl-make-methods (&rest names)
  (let ((methods (%php-make-array)))
    (dolist (name names methods)
      (%php-array-set methods name name))))

(defun %php-spl-method-symbol (name)
  (intern (string-upcase name) :cl-cc/php))

(defun %php-spl-set-method (object name function)
  (setf (gethash name object) function
        (gethash (%php-spl-method-symbol name) object) function)
  function)

(defun %php-spl-install-methods (object specs)
  (dolist (spec specs object)
    (%php-spl-set-method object (first spec) (symbol-function (second spec)))))

(defun %php-spl-object (class-name methods)
  (let ((obj (make-hash-table :test #'equal)))
    (setf (gethash "__class__" obj) class-name)
    (setf (gethash "__methods__" obj) methods)
    obj))

(defun %php-spl-set-property (object name value)
  "Set a PHP-visible property on an SPL-style runtime object."
  (setf (gethash name object) value)
  (let ((fallback-name (string-downcase name)))
    (unless (string= fallback-name name)
      (setf (gethash fallback-name object) value)))
  value)

;;; ─── Reflection runtime objects ─────────────────────────────────────────────

(defun %php-reflection-class-name-value (class)
  (if (hash-table-p class)
      (or (gethash "__class__" class) "")
      (%php-stringify class)))

(defun %php-reflection-constant-get-file-name (self)
  (declare (ignore self))
  +php-null+)

(defun %php-reflection-constant-get-extension (self)
  (declare (ignore self))
  +php-null+)

(defun %php-reflection-constant-get-extension-name (self)
  (declare (ignore self))
  +php-null+)

(defun %php-reflection-constant-get-attributes (self &optional name flags)
  (declare (ignore name flags))
  (or (gethash "__attributes__" self) (%php-array)))

(defun %php-reflection-constant-is-deprecated (self)
  (and (gethash "__deprecated__" self) t))

(defun %php-reflection-constant-new (name)
  (let* ((constant-name (%php-stringify name))
         (obj (%php-spl-object
               "ReflectionConstant"
               (%php-spl-make-methods "getFileName" "getExtension"
                                      "getExtensionName" "getAttributes"
                                      "isDeprecated"))))
    (multiple-value-bind (value foundp) (%php-lookup-constant constant-name)
      (unless foundp
        (error "Undefined PHP constant ~A" constant-name))
      (setf (gethash "__name__" obj) constant-name
            (gethash "__value__" obj) value
            (gethash "__attributes__" obj) (%php-array)
            (gethash "__deprecated__" obj) nil)
      (%php-spl-install-methods
       obj
       '(("getFileName" %php-reflection-constant-get-file-name)
         ("getExtension" %php-reflection-constant-get-extension)
         ("getExtensionName" %php-reflection-constant-get-extension-name)
         ("getAttributes" %php-reflection-constant-get-attributes)
         ("isDeprecated" %php-reflection-constant-is-deprecated)))
      obj)))

(defun %php-reflection-property-get-mangled-name (self)
  (let ((name (gethash "__property__" self))
        (class-name (gethash "__class_name__" self))
        (visibility (gethash "__visibility__" self)))
    (case visibility
      (:private (format nil "~C~A~C~A" #\Null class-name #\Null name))
      (:protected (format nil "~C*~C~A" #\Null #\Null name))
      (t name))))

(defun %php-reflection-property-new (class property)
  (let* ((class-name (%php-reflection-class-name-value class))
         (property-name (%php-stringify property))
         (obj (%php-spl-object
               "ReflectionProperty"
               (%php-spl-make-methods "getMangledName"))))
    (setf (gethash "__class_name__" obj) class-name
          (gethash "__property__" obj) property-name
          (gethash "__visibility__" obj) :public)
    (%php-spl-install-methods
     obj
     '(("getMangledName" %php-reflection-property-get-mangled-name)))
    obj))

;;; ─── PHP 8.5 runtime compatibility objects ────────────────────────────────

(defun %php-dom-html-collection-new (&optional (items '()))
  (let ((obj (%php-spl-object "Dom\\HTMLCollection" (%php-spl-make-methods))))
    (setf (gethash "__items__" obj) items)
    obj))

(defun %php-dom-parent-node-children (owner)
  (let ((collection (%php-dom-html-collection-new)))
    (setf (gethash "__owner__" collection) owner
          (gethash "__property__" collection) "children")
    collection))

(defun %php-dom-element-outer-html (tag-name)
  (if (string= tag-name "")
      ""
      (format nil "<~A></~A>" tag-name tag-name)))

(defun %php-dom-element-get-elements-by-class-name (self class-names)
  (let ((collection (%php-dom-html-collection-new)))
    (setf (gethash "__owner__" collection) self
          (gethash "__class_names__" collection) (%php-stringify class-names))
    collection))

(defun %php-dom-html-document-get-elements-by-name (self element-name)
  (let ((collection (%php-dom-html-collection-new)))
    (setf (gethash "__owner__" collection) self
          (gethash "__name__" collection) (%php-stringify element-name))
    collection))

(defun %php-dom-element-insert-adjacent-html (self where html)
  (let ((entries (or (gethash "__adjacent_html__" self) '())))
    (setf (gethash "__adjacent_html__" self)
          (append entries
                  (list (list :where (%php-stringify where)
                              :html (%php-stringify html))))))
  +php-null+)

(defun %php-dom-element-new (&optional tag-name)
  (let* ((tag-text (if tag-name (%php-stringify tag-name) ""))
         (obj (%php-spl-object
               "Dom\\Element"
               (%php-spl-make-methods "getElementsByClassName"
                                      "insertAdjacentHTML"))))
    (setf (gethash "__tag_name__" obj) tag-text
          (gethash "__adjacent_html__" obj) '())
    (%php-spl-set-property obj "outerHTML" (%php-dom-element-outer-html tag-text))
    (%php-spl-set-property obj "children" (%php-dom-parent-node-children obj))
    (%php-spl-install-methods
     obj
     '(("getElementsByClassName" %php-dom-element-get-elements-by-class-name)
       ("insertAdjacentHTML" %php-dom-element-insert-adjacent-html)))
    obj))

(defun %php-dom-html-document-new (&optional html)
  (let ((obj (%php-spl-object
              "Dom\\HTMLDocument"
              (%php-spl-make-methods "getElementsByName"))))
    (setf (gethash "__html__" obj)
          (if (and html (not (%php-null-p html)))
              (%php-stringify html)
              ""))
    (%php-spl-set-property obj "children" (%php-dom-parent-node-children obj))
    (%php-spl-install-methods
     obj
     '(("getElementsByName" %php-dom-html-document-get-elements-by-name)))
    obj))

(defun %php-soap-client-get-types (self)
  (or (gethash "__types__" self) (%php-array)))

(defun %php-soap-client-new (&rest args)
  (let ((obj (%php-spl-object
              "SoapClient"
              (%php-spl-make-methods "__construct" "__getTypes"))))
    (setf (gethash "__constructor_args__" obj) args
          (gethash "__types__" obj) (%php-array))
    (%php-spl-install-methods
     obj
     '(("__construct" %php-soap-client-construct)
       ("__getTypes" %php-soap-client-get-types)))
    obj))

(defun %php-soap-client-construct (self &rest args)
  (setf (gethash "__constructor_args__" self) args)
  +php-null+)

(defun %php-soap-fault-new (&rest args)
  (let ((obj (%php-spl-object
              "SoapFault"
              (%php-spl-make-methods "__construct"))))
    (setf (gethash "__constructor_args__" obj) args)
    (%php-spl-install-methods
     obj
     '(("__construct" %php-soap-fault-construct)))
    (apply #'%php-soap-fault-construct obj args)
    obj))

(defun %php-soap-fault-construct (self &rest args)
  (setf (gethash "__constructor_args__" self) args)
  (when args
    (setf (gethash "faultcode" self) (first args))
    (when (second args) (setf (gethash "faultstring" self) (second args)))
    (when (third args) (setf (gethash "faultactor" self) (third args)))
    (when (fourth args) (setf (gethash "detail" self) (fourth args)))
    (when (fifth args) (setf (gethash "_name" self) (fifth args)))
    (when (sixth args) (setf (gethash "headerfault" self) (sixth args)))
    (when (seventh args) (setf (gethash "lang" self) (seventh args))))
  +php-null+)

(defun %php-soap-server-fault (self &rest args)
  (setf (gethash "__last_fault__" self) args)
  +php-null+)

(defun %php-soap-server-new (&rest args)
  (let ((obj (%php-spl-object
              "SoapServer"
              (%php-spl-make-methods "__construct" "fault"))))
    (setf (gethash "__constructor_args__" obj) args)
    (%php-spl-install-methods
     obj
     '(("__construct" %php-soap-server-construct)
       ("fault" %php-soap-server-fault)))
    obj))

(defun %php-soap-server-construct (self &rest args)
  (setf (gethash "__constructor_args__" self) args)
  +php-null+)

(defun %php-xslt-processor-construct (self &rest args)
  (setf (gethash "__constructor_args__" self) args)
  +php-null+)

(defun %php-xslt-processor-namespace-table (self namespace &optional createp)
  (let* ((ns (if (or (null namespace) (%php-null-p namespace))
                 ""
                 (%php-stringify namespace)))
         (tables (or (gethash "__parameters__" self)
                     (when createp
                       (setf (gethash "__parameters__" self) (make-hash-table :test #'equal)))))
         (table (and tables (gethash ns tables))))
    (when (and createp (null table))
      (setf table (make-hash-table :test #'equal)
            (gethash ns tables) table))
    table))

(defun %php-xslt-processor-get-parameter (self namespace name)
  (let* ((table (%php-xslt-processor-namespace-table self namespace))
         (key (if (or (null name) (%php-null-p name)) "" (%php-stringify name))))
    (if table
        (gethash key table)
        nil)))

(defun %php-xslt-processor-set-parameter (self namespace name-or-options &optional value)
  (if (and (hash-table-p name-or-options)
           (null value))
      (dolist (pair (%php-array-pairs name-or-options) t)
        (%php-xslt-processor-set-parameter self namespace (car pair) (cdr pair)))
      (let* ((table (%php-xslt-processor-namespace-table self namespace t))
             (key (if (or (null name-or-options) (%php-null-p name-or-options))
                      ""
                      (%php-stringify name-or-options))))
        (setf (gethash key table)
              (if (or (null value) (%php-null-p value))
                  +php-null+
                  (%php-stringify value)))
        t)))

(defun %php-xslt-processor-remove-parameter (self namespace name)
  (let* ((table (%php-xslt-processor-namespace-table self namespace))
         (key (if (or (null name) (%php-null-p name)) "" (%php-stringify name))))
    (when table
      (multiple-value-bind (value present-p) (gethash key table)
        (declare (ignore value))
        (when present-p
          (remhash key table)
          t)))))

(defun %php-xslt-processor-new (&rest args)
  (let ((obj (%php-spl-object
              "XSLTProcessor"
              (%php-spl-make-methods "__construct" "getParameter"
                                     "setParameter" "removeParameter"))))
    (setf (gethash "__constructor_args__" obj) args
          (gethash "__parameters__" obj) (make-hash-table :test #'equal))
    (%php-spl-install-methods
     obj
     '(("__construct" %php-xslt-processor-construct)
       ("getParameter" %php-xslt-processor-get-parameter)
       ("setParameter" %php-xslt-processor-set-parameter)
       ("removeParameter" %php-xslt-processor-remove-parameter)))
    obj))

(defun %php-pdo-sqlite-set-authorizer (self callback)
  (setf (gethash "__authorizer__" self) callback)
  +php-null+)

(defun %php-pdo-sqlite-new (&rest args)
  (let ((obj (%php-spl-object
              "Pdo\\Sqlite"
              (%php-spl-make-methods "setAuthorizer"))))
    (setf (gethash "__constructor_args__" obj) args)
    (%php-spl-install-methods
     obj
     '(("setAuthorizer" %php-pdo-sqlite-set-authorizer)))
    obj))

(defun %php-sqlite3-stmt-busy (self)
  (not (null (gethash "__busy__" self))))

(defun %php-sqlite3-stmt-new (&rest args)
  (declare (ignore args))
  (let ((obj (%php-spl-object
              "SQLite3Stmt"
              (%php-spl-make-methods "busy"))))
    (setf (gethash "__busy__" obj) nil)
    (%php-spl-install-methods
     obj
     '(("busy" %php-sqlite3-stmt-busy)))
    obj))

(defun %php-spl-items (object)
  (or (gethash "__items__" object) '()))

(defun %php-spl-set-items (object items)
  (setf (gethash "__items__" object) items))

(defun %php-spl-list-count (self)
  (length (%php-spl-items self)))

(defun %php-spl-list-empty-p (self)
  (zerop (%php-spl-list-count self)))

(defun %php-spl-list-push (self value)
  (%php-spl-set-items self (append (%php-spl-items self) (list value)))
  +php-null+)

(defun %php-spl-list-unshift (self value)
  (%php-spl-set-items self (cons value (%php-spl-items self)))
  +php-null+)

(defun %php-spl-list-pop (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (car (last items))
          (%php-spl-set-items self (butlast items)))
        +php-null+)))

(defun %php-spl-list-shift (self)
  (let ((items (%php-spl-items self)))
    (if items
        (prog1 (first items)
          (%php-spl-set-items self (rest items)))
        +php-null+)))

(defun %php-spl-list-top (self)
  (let ((items (%php-spl-items self)))
    (if items (car (last items)) +php-null+)))

(defun %php-spl-list-bottom (self)
  (let ((items (%php-spl-items self)))
    (if items (first items) +php-null+)))

(defun %php-spl-list-queue-pop (self)
  (%php-spl-list-shift self))

(defparameter +php-spl-list-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defparameter +php-spl-queue-methods+
  '(("push" %php-spl-list-push)
    ("pop" %php-spl-list-queue-pop)
    ("unshift" %php-spl-list-unshift)
    ("shift" %php-spl-list-shift)
    ("top" %php-spl-list-top)
    ("bottom" %php-spl-list-bottom)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)
    ("enqueue" %php-spl-list-push)
    ("dequeue" %php-spl-list-shift)))

(defun %php-spl-install-list-methods (object &key queue-p)
  (%php-spl-install-methods object
                            (if queue-p
                                +php-spl-queue-methods+
                                +php-spl-list-methods+)))

(defun %php-spl-doubly-linked-list ()
  (let ((object (%php-spl-object
                 "SplDoublyLinkedList"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-stack ()
  (let ((object (%php-spl-object
                 "SplStack"
                 (%php-spl-make-methods "push" "pop" "unshift" "shift"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object)))

(defun %php-spl-queue ()
  (let ((object (%php-spl-object
                 "SplQueue"
                 (%php-spl-make-methods "enqueue" "dequeue" "push" "pop"
                                        "top" "bottom" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (%php-spl-install-list-methods object :queue-p t)))

(defun %php-spl-value< (left right)
  (let ((ln (%php-spl-number left))
        (rn (%php-spl-number right)))
    (cond
      ((or (/= ln 0) (/= rn 0)
           (member left '(0 0.0) :test #'eql)
           (member right '(0 0.0) :test #'eql))
       (< ln rn))
      (t (string< (%php-stringify left) (%php-stringify right))))))

(defun %php-spl-heap-best (items min-p)
  (reduce (lambda (best value)
            (if (if min-p
                    (%php-spl-value< value best)
                    (%php-spl-value< best value))
                value
                best))
          items))

(defun %php-spl-heap-remove-one (items target)
  (let ((removed nil))
    (loop for value in items
          unless (and (not removed) (equal value target))
            collect value
          else do (setf removed t))))

(defun %php-spl-heap-min-p (self)
  (gethash "__min_heap__" self))

(defun %php-spl-heap-insert (self value)
  (%php-spl-list-push self value))

(defun %php-spl-heap-top (self)
  (let ((items (%php-spl-items self)))
    (if items
        (%php-spl-heap-best items (%php-spl-heap-min-p self))
        +php-null+)))

(defun %php-spl-heap-extract (self)
  (let ((items (%php-spl-items self)))
    (if items
        (let ((best (%php-spl-heap-best items (%php-spl-heap-min-p self))))
          (%php-spl-set-items self (%php-spl-heap-remove-one items best))
          best)
        +php-null+)))

(defparameter +php-spl-heap-methods+
  '(("insert" %php-spl-heap-insert)
    ("extract" %php-spl-heap-extract)
    ("top" %php-spl-heap-top)
    ("count" %php-spl-list-count)
    ("isEmpty" %php-spl-list-empty-p)))

(defun %php-spl-heap (class-name min-p)
  (let ((object (%php-spl-object
                 class-name
                 (%php-spl-make-methods "insert" "extract" "top" "count" "isEmpty"))))
    (%php-spl-set-items object '())
    (setf (gethash "__min_heap__" object) min-p)
    (%php-spl-install-methods object +php-spl-heap-methods+)
    object))

(defun %php-spl-min-heap ()
  (%php-spl-heap "SplMinHeap" t))

(defun %php-spl-max-heap ()
  (%php-spl-heap "SplMaxHeap" nil))

(defun %php-spl-fixed-values (self)
  (or (gethash "__values__" self) '()))

(defun %php-spl-fixed-set-values (self values)
  (setf (gethash "__values__" self) values))

(defun %php-spl-fixed-normalize-size (size)
  (max 0 (%php-spl-index size)))

(defun %php-spl-fixed-resize-list (values size)
  (let ((current (length values)))
    (cond
      ((= current size) values)
      ((> current size) (subseq values 0 size))
      (t (append values (make-list (- size current) :initial-element +php-null+))))))

(defun %php-spl-fixed-ref (self index)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (if (and (>= i 0) (< i (length values)))
        (nth i values)
        +php-null+)))

(defun %php-spl-fixed-set (self index value)
  (let* ((values (%php-spl-fixed-values self))
         (i (%php-spl-index index)))
    (when (and (>= i 0) (< i (length values)))
      (setf (nth i values) value)
      (%php-spl-fixed-set-values self values))
    +php-null+))

(defun %php-spl-fixed-size (self)
  (length (%php-spl-fixed-values self)))

(defun %php-spl-fixed-set-size (self new-size)
  (%php-spl-fixed-set-values
   self
   (%php-spl-fixed-resize-list
    (%php-spl-fixed-values self)
    (%php-spl-fixed-normalize-size new-size)))
  +php-null+)

(defun %php-spl-fixed-offset-exists-p (self index)
  (let ((i (%php-spl-index index)))
    (and (>= i 0) (< i (%php-spl-fixed-size self)))))

(defun %php-spl-fixed-unset (self index)
  (%php-spl-fixed-set self index +php-null+))

(defparameter +php-spl-fixed-array-methods+
  '(("getSize" %php-spl-fixed-size)
    ("setSize" %php-spl-fixed-set-size)
    ("offsetGet" %php-spl-fixed-ref)
    ("offsetSet" %php-spl-fixed-set)
    ("offsetExists" %php-spl-fixed-offset-exists-p)
    ("offsetUnset" %php-spl-fixed-unset)
    ("count" %php-spl-fixed-size)))

(defun %php-spl-fixed-array (&optional size)
  (let ((object (%php-spl-object
                 "SplFixedArray"
                 (%php-spl-make-methods "getSize" "setSize" "offsetGet"
                                        "offsetSet" "offsetExists" "offsetUnset"
                                        "count"))))
    (%php-spl-fixed-set-values object
                               (make-list (%php-spl-fixed-normalize-size (or size 0))
                                          :initial-element +php-null+))
    (%php-spl-install-methods object +php-spl-fixed-array-methods+)
    object))

(defun %php-spl-new (class-name &rest args)
  (case (intern (string-upcase (%php-spl-class-name class-name)) :keyword)
    (:SPLSTACK (%php-spl-stack))
    (:SPLQUEUE (%php-spl-queue))
    (:SPLDOUBLYLINKEDLIST (%php-spl-doubly-linked-list))
    (:SPLMINHEAP (%php-spl-min-heap))
    (:SPLMAXHEAP (%php-spl-max-heap))
    (:SPLFIXEDARRAY (apply #'%php-spl-fixed-array args))
    (otherwise (error "Unknown SPL class: ~A" class-name))))
