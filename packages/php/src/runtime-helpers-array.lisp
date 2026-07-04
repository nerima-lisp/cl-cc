;;;; PHP ordered array runtime model.

(in-package :cl-cc/php)

(defun %php-array-empty-p (ht)
  "Return true when PHP ordered array HT contains no user entries."
  (check-type ht hash-table)
  (null (gethash +php-array-order-key+ ht)))

(defun %php-make-array ()
  "Create an empty PHP ordered array."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash +php-array-order-key+ ht) nil
          (gethash +php-array-next-index-key+ ht) 0)
    ht))

(defun %php-array-key-present-p (array key)
  "Return true when ARRAY already contains PHP array KEY."
  (nth-value 1 (gethash key array)))

(defun %php-array-append-order-key (array key)
  "Record KEY at the end of ARRAY's insertion-order list."
  (let ((order (gethash +php-array-order-key+ array)))
    (setf (gethash +php-array-order-key+ array) (append order (list key)))))

(defun %php-array-advance-next-index (array key)
  "Advance ARRAY's next auto index if KEY is a non-negative integer."
  (when (and (integerp key) (>= key 0))
    (let ((candidate (1+ key))
          (next-index (gethash +php-array-next-index-key+ array)))
      (when (> candidate next-index)
        (setf (gethash +php-array-next-index-key+ array) candidate)))))

(defun %php-null-array-key-deprecation-warning ()
  "Emit PHP 8.5's deprecation warning for null array keys."
  (%php-trigger-error
   "PHP 8.5 deprecates implicit conversion from null to array key"
   8192))

(defun %php-array-set (arr key value)
  "Set ARR[KEY] to VALUE, preserving PHP insertion order.

Duplicate keys overwrite their value without changing their original position.
Integer keys greater than or equal to the current auto-index advance the next
auto-increment index to one greater than the key."
  (check-type arr hash-table)
  (when (or (null key) (%php-null-p key))
    (%php-null-array-key-deprecation-warning))
  (unless (%php-array-key-present-p arr key)
    (%php-array-append-order-key arr key))
  (setf (gethash key arr) value)
  (%php-array-advance-next-index arr key)
  value)

(defun %php-array-ref (arr key)
  "Return ARR[KEY] for a PHP ordered array helper hash-table."
  (check-type arr hash-table)
  (when (or (null key) (%php-null-p key))
    (%php-null-array-key-deprecation-warning))
  (multiple-value-bind (value present-p) (gethash key arr)
    (if present-p value +php-null+)))

(defun %php-destructure-ref (value key)
  "Read VALUE[KEY] for list/array destructuring without leaking host type errors."
  (cond
    ((hash-table-p value)
     (%php-array-ref value key))
    ((%php-null-p value)
     +php-null+)
    (t
     (%php-trigger-error
      (format nil "Cannot destructure value of type ~A" (%php-value-type value))
      2)
     +php-null+)))

(defun %php-array-unset (arr key)
  "Delete ARR[KEY] from a PHP ordered array and preserve insertion order."
  (check-type arr hash-table)
  (when (or (null key) (%php-null-p key))
    (%php-null-array-key-deprecation-warning))
  (remhash key arr)
  (setf (gethash +php-array-order-key+ arr)
        (remove key (gethash +php-array-order-key+ arr) :test #'equal))
  +php-null+)

(defun %php-count (arr)
  "Return the number of entries in PHP ordered array ARR."
  (check-type arr hash-table)
  (length (gethash +php-array-order-key+ arr)))

(defun %php-array-key-exists (arr key)
  "Return true when KEY exists in PHP ordered array ARR."
  (check-type arr hash-table)
  (when (or (null key) (%php-null-p key))
    (%php-null-array-key-deprecation-warning))
  ;; PHP coerces a null array key to the empty string.
  (let ((lookup-key (if (or (null key) (%php-null-p key)) "" key)))
    (member lookup-key (gethash +php-array-order-key+ arr) :test #'equal)))

(defun %php-builtin-array-key-exists (key arr)
  "PHP array_key_exists(KEY, ARRAY): the key is the first argument and the array
the second, the reverse of the internal %php-array-key-exists helper."
  (%php-array-key-exists arr key))

(defun %php-array-next-auto-index (array)
  "Return and reserve ARRAY's current PHP auto-increment index."
  (let ((index (gethash +php-array-next-index-key+ array)))
    (setf (gethash +php-array-next-index-key+ array) (1+ index))
    index))

(defun %php-spread (array)
  "Wrap ARRAY in a spread marker for %php-array to splice in.  Lowered from
[...$a] inside an array literal.  (In a CALL, ...$a is rewritten before runtime,
so this is only reached for array-literal spreads.)"
  (cons :__php-spread__ array))

(defun %php-spread-marker-p (x)
  "True when X is a %php-spread marker."
  (and (consp x) (eq (car x) :__php-spread__)))

(defun %php-array (&rest entries)
  "Construct a PHP ordered array from flat entry descriptors.

Each entry descriptor is a list of the form (KEY-PRESENT-P KEY VALUE). When
KEY-PRESENT-P is false, KEY is ignored and VALUE is inserted at the current
auto-increment integer index. Explicit integer keys update the next auto-index
to max(existing-next-index, key + 1), matching PHP array literal semantics.
A VALUE that is a %php-spread marker ([...$a]) splices the wrapped array's
elements in — integer keys re-indexed, string keys preserved (PHP 8.1)."
  (let ((array (%php-make-array)))
    (dolist (entry entries array)
      (destructuring-bind (key-present-p key value) entry
        (cond
          ((%php-spread-marker-p value)
           (let ((src (cdr value)))
             (when (hash-table-p src)
               (dolist (k (gethash +php-array-order-key+ src))
                 (let ((v (gethash k src)))
                   (if (integerp k)
                       (%php-array-set array (%php-array-next-auto-index array) v)
                       (%php-array-set array k v)))))))
          (t
           (%php-array-set array
                           (if key-present-p key (%php-array-next-auto-index array))
                           value)))))))
