;;;; PHP runtime reflection objects.

(in-package :cl-cc/php)

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

