;;;; Core PHP builtin helpers.

(in-package :cl-cc/php)

(defun %php-empty (value)
  "Return true when VALUE is empty according to PHP truthiness rules."
  ;; (%php-empty +php-null+) => T
  ;; (%php-empty "0") => T
  ;; (%php-empty "hello") => NIL
  (not (%php-truthy value)))

(defun %php-is-null (value)
  "Return true when VALUE is PHP null."
  ;; (%php-is-null +php-null+) => T
  ;; (%php-is-null NIL) => NIL
  (%php-null-p value))

(defun %php-unset (value)
  "Return PHP null for unset VALUE."
  ;; The parser lowers array element unsets through `%php-array-unset`; this
  ;; helper exists for builtin registry completeness where an expression-like
  ;; bridge is needed.
  ;; (%php-unset 10) => +PHP-NULL+
  (declare (ignore value))
  +php-null+)

(defun %php-compact (&rest names)
  "Fallback for dynamic compact(); static string names are lowered by parser."
  (declare (ignore names))
  (%php-array))

(defun %php-extract (array &rest options)
  "Fallback for dynamic extract(); static array literals are lowered by parser."
  (declare (ignore array options))
  0)

(defun %php-gc-collect-cycles ()
  "PHP 8.5-compatible gc_collect_cycles() helper.

cl-cc does not model PHP's cyclic GC state, so no collectable PHP cycles are
reported. PHP 8.5 requires an integer result even when collection is inactive."
  0)

(defun %php-opcache-is-script-cached-in-file-cache (filename)
  "PHP 8.5 opcache_is_script_cached_in_file_cache().

cl-cc does not maintain an OPCache file cache, so scripts are never reported as
cached by this runtime helper."
  (declare (ignore filename))
  nil)

(defvar *php-curl-persistent-share-handles* (make-hash-table :test #'equal)
  "Persistent cURL share handles keyed by user-provided identifier.")

(defun %php-curl-share-init-persistent (&optional (id "default"))
  "PHP 8.5 curl_share_init_persistent() helper.

cl-cc models the persistent share handle as a stable PHP object-like
hash-table. Repeated calls with the same ID return the same handle."
  (%php-register-runtime-class-tag "CurlSharePersistentHandle")
  (let ((key (%php-stringify id)))
    (multiple-value-bind (handle foundp)
        (gethash key *php-curl-persistent-share-handles*)
      (if foundp
          handle
          (let ((new-handle (make-hash-table :test #'equal)))
            (setf (gethash "__class__" new-handle) "CurlSharePersistentHandle"
                  (gethash "id" new-handle) key
                  (gethash "persistent" new-handle) t)
            (setf (gethash key *php-curl-persistent-share-handles*) new-handle)
            new-handle)))))

(defun %php-curl-multi-get-handles (multi-handle)
  "PHP 8.5 curl_multi_get_handles() helper."
  (let ((handles (and (hash-table-p multi-handle)
                      (or (gethash "handles" multi-handle)
                          (gethash :handles multi-handle)))))
    (cond
      ((null handles) (%php-array))
      ((and (hash-table-p handles)
            (nth-value 1 (gethash +php-array-order-key+ handles)))
       handles)
      ((listp handles) (%php-list-to-array handles))
      (t (%php-list-to-array (list handles))))))

(defun %php-hash-field (table &rest keys)
  (when (hash-table-p table)
    (loop for key in keys
          do (multiple-value-bind (value foundp) (gethash key table)
               (when foundp
                 (return value))))))

(defun %php-enchant-remove-word (dict word)
  (let* ((needle (%php-stringify word))
         (words (%php-hash-field dict
                                  "session_words" :session-words
                                  "words" :words)))
    (when (hash-table-p words)
      (remhash needle words)
      (remhash word words)))
  t)

(defun %php-enchant-dict-remove-from-session (dict word)
  "PHP 8.5 enchant_dict_remove_from_session() compatibility helper."
  (%php-enchant-remove-word dict word))

(defun %php-enchant-dict-remove (dict word)
  "PHP 8.5 enchant_dict_remove() compatibility helper."
  (%php-enchant-remove-word dict word))

(defun %php-pg-close-stmt (connection statement-name)
  "PHP 8.5 pg_close_stmt() compatibility helper."
  (let ((statements (%php-hash-field connection "statements" :statements)))
    (when (hash-table-p statements)
      (let ((key (%php-stringify statement-name)))
        (remhash key statements)
        (remhash statement-name statements))))
  t)

(defun %php-pg-service (&optional connection)
  "PHP 8.5 pg_service() compatibility helper."
  (or (%php-hash-field connection "service" :service)
      nil))

(defun %php-gettype (value)
  "Return VALUE's PHP type name as a string."
  ;; (%php-gettype 1) => "integer"
  ;; (%php-gettype "x") => "string"
  (case (%php-value-type value)
    (:null "NULL")
    (:bool "boolean")
    (:int "integer")
    (:float "double")
    (:string "string")
    (:array "array")
    (otherwise "object")))

(defun %php-array-ordered-keys (array)
  "Return ARRAY's user keys in PHP insertion order."
  ;; (%php-array-ordered-keys (%php-array (list nil nil "a"))) => (0)
  (check-type array hash-table)
  (copy-list (gethash +php-array-order-key+ array)))

(defun %php-array-pairs (array)
  "Return ARRAY's ordered (KEY . VALUE) pairs."
  ;; (%php-array-pairs (%php-array (list t "a" 1))) => (("a" . 1))
  (check-type array hash-table)
  (loop for key in (%php-array-ordered-keys array)
        collect (cons key (gethash key array))))

(defun %php-array-values-list (array)
  "Return ARRAY's values as a Common Lisp list in PHP insertion order."
  ;; (%php-array-values-list (%php-array (list nil nil "a") (list nil nil "b")))
  ;; => ("a" "b")
  (check-type array hash-table)
  (loop for key in (%php-array-ordered-keys array)
        collect (gethash key array)))

(defvar *php-missing-call-arg* (list :php-missing-call-arg))

(defun %php-call-args-with-named-spread
    (fn-name param-names default-present default-values &rest descriptors)
  "Return a runtime positional argument list for calls that mix ...spread and
named arguments when the spread width is not statically known."
  (let ((positional nil)
        (named (make-hash-table :test #'equal))
        (seen-named nil)
        (highest-named-index -1))
    (labels ((param-index (name)
               (position name param-names :test #'string-equal))
             (push-positional (value)
               (when seen-named
                 (error "PHP positional argument after named argument in call to ~A"
                        fn-name))
               (push value positional)))
      (dolist (descriptor descriptors)
        (destructuring-bind (kind &rest values) descriptor
          (case kind
            (:pos
             (push-positional (first values)))
            (:spread
             (when seen-named
               (error "PHP spread argument after named argument in call to ~A"
                      fn-name))
             (dolist (value (%php-array-values-list (first values)))
               (push value positional)))
            (:named
             (setf seen-named t)
             (destructuring-bind (name value) values
               (let ((index (param-index name)))
                 (unless index
                   (error "Unknown named argument ~A for PHP function ~A"
                          name fn-name))
                 (multiple-value-bind (old present-p) (gethash name named)
                   (declare (ignore old))
                   (when present-p
                     (error "Duplicate named argument ~A for PHP function ~A"
                            name fn-name)))
                 (setf (gethash name named) value
                       highest-named-index (max highest-named-index index)))))
            (otherwise
              (error "Unknown PHP call argument descriptor ~S" kind)))))
      (let* ((positional (nreverse positional))
             (positional-count (length positional))
             (result-size (max positional-count (1+ highest-named-index)))
             (result (make-array result-size
                                 :initial-element *php-missing-call-arg*)))
        (loop for value in positional
              for index from 0
              do (setf (aref result index) value))
        (maphash (lambda (name value)
                   (let ((index (param-index name)))
                     (when (< index positional-count)
                       (error "Duplicate PHP argument for parameter ~A in call to ~A"
                              name fn-name))
                     (setf (aref result index) value)))
                 named)
        (loop for index below result-size
              for value = (aref result index)
              collect (cond
                        ((not (eq value *php-missing-call-arg*)) value)
                        ((nth index default-present)
                         (nth index default-values))
                        (t
                         (error "Missing PHP argument ~A for function ~A"
                                (nth index param-names) fn-name))))))))

(defun %php-list-to-array (values)
  "Return a PHP ordered array containing VALUES at numeric keys."
  ;; (%php-count (%php-list-to-array '("a" "b"))) => 2
  (let ((array (%php-array)))
    (dolist (value values array)
      (%php-array-set array (%php-array-next-auto-index array) value))))

(defun %php-object-hashlike-storage (object)
  "Return OBJECT's underlying storage when OBJECT is a PHP object."
  (cond
    ((hash-table-p object) object)
    ((typep object 'cl-cc/vm::vm-hash-table-object)
     (cl-cc/vm::%vm-hashlike-storage object))
    (t nil)))

(defun %php-object-class-ht (object)
  "Return OBJECT's class descriptor when OBJECT is a PHP object."
  (cond
    ((and (vectorp object)
          (plusp (length object))
          (hash-table-p (aref object 0)))
     (aref object 0))
    (t
     (let ((storage (%php-object-hashlike-storage object)))
       (when storage
         (multiple-value-bind (class-name present-p)
             (gethash "__class__" storage)
           (unless present-p
             (multiple-value-setq (class-name present-p)
               (gethash :__class__ storage)))
           (and present-p class-name)))))))

(defun %php-object-class-name (object)
  "Return OBJECT's PHP class name, or NIL when OBJECT is not a PHP object."
  (let ((class-ht (%php-object-class-ht object)))
    (when class-ht
      (%php-stringify (if (hash-table-p class-ht)
                          (or (gethash :__name__ class-ht)
                              (gethash "__name__" class-ht))
                          class-ht)))))

(defun %php-object-method (object method-name)
  "Return a callable METHOD-NAME from OBJECT's runtime method table."
  (labels ((%method->callable (method)
             (cond
               ((functionp method) method)
               ((hash-table-p method)
                (or (gethash :function method)
                    (gethash "function" method)))
               (t nil)))
           (%lookup-method (table)
             (when table
               (let* ((method-key (%php-stringify method-name))
                      (method-symbol (intern (string-upcase method-key) :cl-cc/php)))
                 (or (%method->callable (gethash method-name table))
                     (%method->callable (gethash method-key table))
                     (%method->callable (gethash method-symbol table))))))
           (%lookup-class-methods (class-ht)
             (let ((storage (%php-object-hashlike-storage class-ht)))
               (when storage
                 (or (%lookup-method (gethash :__methods__ storage))
                     (%lookup-method (gethash "__methods__" storage))
                     (%lookup-method storage)))))
           (%wrap-closure (value)
             ;; A PHP method lives in an instance slot as a vm-closure-object;
             ;; CL:FUNCALL cannot call it, so wrap it in the same host->VM
             ;; trampoline array_map/usort use. The method's first parameter is
             ;; the implicit $this, so callers pass the object as the first arg.
             (cond
               ((functionp value) value)
               ((cl-cc/vm::%vm-closure-object-p value)
                (lambda (&rest args)
                  (cl-cc/vm::%vm-call-closure-sync value cl-cc/vm:*vm-state* args)))
               (t nil)))
           (%lookup-instance-method (obj class-ht)
             ;; PHP compiles methods to per-instance slots (see codegen-clos),
             ;; not the shared :__methods__ table, so magic methods such as
             ;; __serialize/__sleep/__wakeup are found here, by slot.
             (when (hash-table-p class-ht)
               (let* ((method-symbol (intern (string-upcase (%php-stringify method-name))
                                             :cl-cc/php))
                      (index (cl-cc/vm::%vm-slot-vector-index class-ht method-symbol)))
                 (when (and index
                            (vectorp obj)
                            (< index (length obj)))
                   (%wrap-closure (aref obj index)))))))
    (cond
      ((hash-table-p object)
       (%lookup-method object))
      ((typep object 'cl-cc/vm::vm-hash-table-object)
       (%lookup-class-methods (%php-object-hashlike-storage object)))
      ((and (vectorp object)
            (plusp (length object))
            (hash-table-p (aref object 0)))
       (or (%lookup-instance-method object (aref object 0))
           (%lookup-class-methods (aref object 0))))
      (t nil))))

(defun %php-object-visible-pairs (object)
  "Return user-visible property pairs for OBJECT."
  (cond
    ((hash-table-p object)
     (let ((pairs '()))
       (maphash (lambda (key value)
                  (let ((key-string (%php-stringify key)))
                    (unless (or (functionp value)
                                (and (stringp key-string)
                                     (> (length key-string) 2)
                                     (string= key-string "__" :end1 2)))
                      (push (cons key value) pairs))))
                object)
       (sort pairs #'string<
             :key (lambda (pair) (%php-stringify (car pair))))))
    ((typep object 'cl-cc/vm::vm-hash-table-object)
     (let ((storage (%php-object-hashlike-storage object)))
       (when storage
         (let ((pairs '()))
           (maphash (lambda (key value)
                      (let ((key-string (%php-stringify key)))
                        (unless (or (functionp value)
                                    (and (stringp key-string)
                                         (> (length key-string) 2)
                                         (string= key-string "__" :end1 2)))
                          (push (cons key value) pairs))))
                    storage)
           (sort pairs #'string<
                 :key (lambda (pair) (%php-stringify (car pair))))))))
    ((and (vectorp object)
          (plusp (length object))
          (hash-table-p (aref object 0)))
     (let* ((class-ht (aref object 0))
            (pairs nil))
       (dolist (slot (cl-cc/vm::class-slots class-ht))
         (let* ((slot-name (cl-cc/vm::slot-definition-name slot))
                (property-name (string-downcase (symbol-name slot-name)))
                (allocation (cl-cc/vm::slot-definition-allocation slot))
                (index (cl-cc/vm::%vm-slot-vector-index class-ht slot-name)))
           (when (and (eq allocation :instance)
                      (not (and (stringp property-name)
                                (> (length property-name) 2)
                                (string= property-name "__" :end1 2)))
                      index
                      (< index (length object)))
            (let ((value (aref object index)))
               (unless (eq value cl-cc/vm::*unbound-slot-marker*)
                 (push (cons property-name value) pairs))))))
       (sort pairs #'string<
             :key (lambda (pair) (%php-stringify (car pair))))))
    (t nil)))

(defun %php-object-debug-pairs (object)
  "Return property pairs to use for object debug output."
  (let ((debug-info (%php-object-method object "__debugInfo")))
    (if debug-info
        (let ((value (funcall debug-info object)))
          (if (hash-table-p value)
              (%php-array-pairs value)
              (%php-object-visible-pairs object)))
        (%php-object-visible-pairs object))))

(defun %php-object-print-r-string (object)
  "Return a PHP-like print_r representation of OBJECT."
  (let ((class-name (or (%php-object-class-name object) "object"))
        (pairs (%php-object-debug-pairs object)))
    (with-output-to-string (stream)
      (format stream "~A Object (~%" class-name)
      (dolist (pair pairs)
        (format stream "    [~A] => ~A~%"
                (%php-stringify (car pair))
                (%php-value-display-string (cdr pair))))
      (format stream ")"))))

(defun %php-object-var-dump-to-stream (object stream indent)
  "Write a PHP-like var_dump entry for OBJECT to STREAM."
  (let ((pad (make-string indent :initial-element #\Space))
        (class-name (or (%php-object-class-name object) "object"))
        (pairs (%php-object-debug-pairs object)))
    (format stream "~Aobject(~A) (~D) {~%" pad class-name (length pairs))
    (dolist (pair pairs)
      (format stream "~A  [\"~A\"]=>~%" pad (%php-stringify (car pair)))
      (%php-var-dump-to-stream (cdr pair) stream (+ indent 2)))
    (format stream "~A}~%" pad)))

(defun %php-value-display-string (value)
  "Return a readable PHP-like representation of VALUE."
  (cond ((%php-null-p value) "NULL")
        ((eq value t) "true")
        ((null value) "false")
        ((stringp value) value)
        ((numberp value) (princ-to-string value))
        ((%php-object-class-name value)
         (%php-object-print-r-string value))
        ((hash-table-p value)
         (with-output-to-string (stream)
           (format stream "Array(~D) {" (%php-count value))
           (dolist (pair (%php-array-pairs value))
             (format stream " [~A]=>~A"
                     (%php-stringify (car pair))
                     (%php-value-display-string (cdr pair))))
           (format stream " }")))
        (t (princ-to-string value))))

(defun %php-print-r (value &optional return-output)
  "Return or print a PHP-like readable representation of VALUE."
  ;; (%php-print-r (%php-array (list t "a" 1)) T) => "Array(1) { [a]=>1 }"
  (let ((text (%php-value-display-string value)))
    (if (%php-truthy return-output)
        text
        (progn (princ text) t))))

(defun %php-var-dump-to-stream (value stream indent)
  "Write one var_dump entry for VALUE to STREAM at INDENT spaces (recursive for
arrays).  Floats use the PHP number format, not the raw CL form."
  (let ((pad (make-string indent :initial-element #\Space)))
    (case (%php-value-type value)
      (:null   (format stream "~ANULL~%" pad))
      (:bool   (format stream "~Abool(~A)~%" pad (if value "true" "false")))
      (:int    (format stream "~Aint(~D)~%" pad value))
      (:float  (format stream "~Afloat(~A)~%" pad (%php-number-to-string value)))
      (:string (format stream "~Astring(~D) \"~A\"~%" pad (length value) value))
      (:array
       (format stream "~Aarray(~D) {~%" pad (%php-count value))
       (dolist (pair (%php-array-pairs value))
         (let ((k (car pair)))
           (if (integerp k)
               (format stream "~A  [~D]=>~%" pad k)
               (format stream "~A  [\"~A\"]=>~%" pad k)))
         (%php-var-dump-to-stream (cdr pair) stream (+ indent 2)))
       (format stream "~A}~%" pad))
      (:object (%php-object-var-dump-to-stream value stream indent))
      (otherwise (format stream "~Aobject(~A)~%" pad (%php-value-display-string value))))))

(defun %php-var-dump (&rest values)
  "PHP var_dump: WRITE a type-annotated dump of each value to output (it prints;
it does not return a string)."
  (dolist (value values)
    (write-string (with-output-to-string (s)
                    (%php-var-dump-to-stream value s 0))))
  nil)

(defun %php-var-export-string (value indent)
  "Build PHP var_export output for VALUE (a re-parseable representation):
ints/floats bare, strings single-quoted (with ' and \\ escaped), bools
true/false, null NULL, arrays as `array ( k => v, ... )'."
  (let ((pad (make-string indent :initial-element #\Space)))
    (case (%php-value-type value)
      (:null   "NULL")
      (:bool   (if value "true" "false"))
      (:int    (princ-to-string value))
      (:float  (%php-number-to-string value))
      (:string (with-output-to-string (s)
                 (write-char #\' s)
                 (loop for ch across value
                       do (when (or (char= ch #\') (char= ch #\\)) (write-char #\\ s))
                          (write-char ch s))
                 (write-char #\' s)))
      (:array
       (with-output-to-string (s)
         (format s "array (~%")
         (dolist (pair (%php-array-pairs value))
           (let ((k (car pair)))
             (format s "~A  ~A => ~A,~%" pad
                     (if (integerp k) (princ-to-string k)
                         (%php-var-export-string k 0))
                     (%php-var-export-string (cdr pair) (+ indent 2)))))
         (format s "~A)" pad)))
      (otherwise (%php-value-display-string value)))))

(defun %php-var-export (value &optional return-p)
  "PHP var_export: print VALUE's re-parseable representation, or return it as a
string when RETURN-P is truthy."
  (let ((s (%php-var-export-string value 0)))
    (if (%php-truthy return-p) s (progn (write-string s) nil))))
