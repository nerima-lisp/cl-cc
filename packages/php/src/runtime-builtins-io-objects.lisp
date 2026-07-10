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

