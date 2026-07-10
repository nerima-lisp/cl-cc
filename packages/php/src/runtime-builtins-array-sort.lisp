;;;; PHP array sorting builtins.

(in-package :cl-cc/php)

(defun %php-sort-flag-type (sort-flags)
  "Return the comparison mode selected by PHP SORT_* flags."
  (case sort-flags
    (1 :numeric)                         ; SORT_NUMERIC
    (2 :string)                          ; SORT_STRING
    (otherwise :regular)))               ; SORT_REGULAR/default

(defun %php-sort-compare-values (a b sort-type)
  "Return -1, 0, or 1 comparing A and B for PHP array sorting."
  (case sort-type
    (:numeric
     (let ((na (%php-numeric a))
           (nb (%php-numeric b)))
       (cond ((< na nb) -1) ((> na nb) 1) (t 0))))
    (:string
     (let ((sa (%php-stringify a))
           (sb (%php-stringify b)))
       (cond ((string< sa sb) -1) ((string> sa sb) 1) (t 0))))
    (otherwise
     (cond
       ((and (numberp a) (numberp b))
        (cond ((< a b) -1) ((> a b) 1) (t 0)))
       (t (%php-sort-compare-values a b :string))))))

(defun %php-sort-pairs-by (array key-fn &key descending preserve-keys sort-flags)
  "Sort ARRAY by KEY-FN and return T after mutating ARRAY."
  (let* ((sort-type (%php-sort-flag-type sort-flags))
         (pairs (stable-sort
                 (copy-list (%php-array-pairs array))
                 (lambda (left right)
                   (let ((comparison (%php-sort-compare-values
                                      (funcall key-fn left)
                                      (funcall key-fn right)
                                      sort-type)))
                     (if descending
                         (> comparison 0)
                         (< comparison 0)))))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (pair pairs t)
      (%php-array-set array
                      (if preserve-keys (car pair) (%php-array-next-auto-index array))
                      (cdr pair)))))

(defun %php-sort (array &optional sort-flags)
  "Sort ARRAY values ascending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr :sort-flags sort-flags))

(defun %php-rsort (array &optional sort-flags)
  "Sort ARRAY values descending, reindexing numeric keys."
  (%php-sort-pairs-by array #'cdr :descending t :sort-flags sort-flags))

(defun %php-asort (array &optional sort-flags)
  "Sort ARRAY by value ascending, preserving keys."
  (%php-sort-pairs-by array #'cdr :preserve-keys t :sort-flags sort-flags))

(defun %php-ksort (array &optional sort-flags)
  "Sort ARRAY by key ascending, preserving key/value associations."
  (%php-sort-pairs-by array #'car :preserve-keys t :sort-flags sort-flags))

(defun %php-user-sort-in-place (array compare-fn select-fn preserve-keys)
  "Sort ARRAY IN PLACE: PHP's usort/uasort/uksort take the array BY REFERENCE
and mutate it (they do not return a sorted copy).  COMPARE-FN is applied to
(funcall SELECT-FN pair) of each pair - #'cdr for value sorts, #'car for key
sorts.  Numeric keys are re-indexed unless PRESERVE-KEYS.  Uses stable-sort to
match PHP 8.0+ stable-sort semantics.  Returns T (the PHP return value).

The earlier implementations built a fresh result array and returned it, so the
caller's variable - which still held the original hash-table - was never
updated and the sort appeared to do nothing."
  (let ((fn (%php-callable-function compare-fn)))
    (when fn
      (let ((sorted (stable-sort (copy-list (%php-array-pairs array))
                                 (lambda (a b)
                                   (< (%php-numeric
                                       (funcall fn (funcall select-fn a) (funcall select-fn b)))
                                      0)))))
        (clrhash array)
        (setf (gethash +php-array-order-key+ array) nil
              (gethash +php-array-next-index-key+ array) 0)
        (dolist (pair sorted)
          (%php-array-set array
                          (if preserve-keys (car pair) (%php-array-next-auto-index array))
                          (cdr pair)))))
    t))

(defun %php-usort (array compare-fn)
  "PHP usort: sort ARRAY in place by user COMPARE-FN on values, re-indexing keys."
  (%php-user-sort-in-place array compare-fn #'cdr nil))

(defun %php-uasort (array compare-fn)
  "PHP uasort: sort ARRAY in place by user COMPARE-FN on values, preserving keys."
  (%php-user-sort-in-place array compare-fn #'cdr t))

(defun %php-uksort (array compare-fn)
  "PHP uksort: sort ARRAY in place by user COMPARE-FN on keys, preserving keys."
  (%php-user-sort-in-place array compare-fn #'car t))

(defun %php-krsort (array &optional sort-flags)
  "PHP krsort: sort ARRAY by key descending."
  (%php-sort-pairs-by array #'car
                      :preserve-keys t
                      :descending t
                      :sort-flags sort-flags))

(defun %php-arsort (array &optional sort-flags)
  "PHP arsort: sort ARRAY by value descending, preserving keys."
  (%php-sort-pairs-by array #'cdr
                      :preserve-keys t
                      :descending t
                      :sort-flags sort-flags))

(defun %php-array-multisort-spec (array)
  "Build an internal array_multisort spec for ARRAY."
  (vector array nil :regular))

(defun %php-array-multisort-apply-flag (spec flag)
  "Apply an array_multisort sort/order FLAG to SPEC."
  (case flag
    (3 (setf (aref spec 1) t))          ; SORT_DESC
    (4 (setf (aref spec 1) nil))        ; SORT_ASC
    (0 (setf (aref spec 2) :regular))   ; SORT_REGULAR
    (1 (setf (aref spec 2) :numeric))   ; SORT_NUMERIC
    (2 (setf (aref spec 2) :string))    ; SORT_STRING
    (otherwise spec))
  spec)

(defun %php-array-multisort-parse-specs (array rest)
  "Parse PHP's array_multisort argument stream into array specs."
  (let ((specs (list (%php-array-multisort-spec array))))
    (dolist (arg rest (nreverse specs))
      (cond
        ((hash-table-p arg)
         (push (%php-array-multisort-spec arg) specs))
        ((and (integerp arg) specs)
         (%php-array-multisort-apply-flag (first specs) arg))))))

(defun %php-array-multisort-compare-values (a b sort-type)
  "Return -1, 0, or 1 comparing A and B for array_multisort SORT-TYPE."
  (%php-sort-compare-values a b sort-type))

(defun %php-array-multisort-pair-value (pairs index)
  "Return PAIRS[INDEX]'s value, or PHP null when absent."
  (let ((pair (nth index pairs)))
    (if pair (cdr pair) +php-null+)))

(defun %php-array-multisort (array &rest rest)
  "PHP array_multisort: lexicographically sort arrays together in place."
  (let* ((specs (%php-array-multisort-parse-specs array rest))
         (pairs-by-array (mapcar (lambda (spec) (%php-array-pairs (aref spec 0))) specs))
         (lengths (mapcar #'length pairs-by-array)))
    (unless (and specs (every (lambda (len) (= len (first lengths))) lengths))
      (return-from %php-array-multisort nil))
    (labels ((index< (left right)
               (loop for spec in specs
                     for pairs in pairs-by-array
                     for comparison = (%php-array-multisort-compare-values
                                        (%php-array-multisort-pair-value pairs left)
                                        (%php-array-multisort-pair-value pairs right)
                                        (aref spec 2))
                     when (/= comparison 0)
                       return (if (aref spec 1)
                                  (> comparison 0)
                                  (< comparison 0))
                     finally (return nil))))
      (let ((indices (stable-sort (loop for i below (first lengths) collect i)
                                  #'index<)))
        (loop for spec in specs
              for pairs in pairs-by-array
              for target = (aref spec 0)
              do (progn
                   (clrhash target)
                   (setf (gethash +php-array-order-key+ target) nil
                         (gethash +php-array-next-index-key+ target) 0)
                   (dolist (index indices)
                     (let* ((pair (nth index pairs))
                            (key (car pair)))
                       (%php-array-set target
                                       (if (integerp key)
                                           (%php-array-next-auto-index target)
                                           key)
                                       (cdr pair)))))))
        t)))
