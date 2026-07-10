;;;; PHP runtime compatibility objects.

(in-package :cl-cc/php)

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

