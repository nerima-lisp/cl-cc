;;;; packages/php/src/runtime-builtins-regex-array.lisp -- PHP array and natural-sort extra builtins

(in-package :cl-cc/php)

(defun %php-array-walk-recursive (array callback &optional extra-data)
  "PHP array_walk_recursive: apply callback to all leaf values."
  (let ((fn (%php-callable-function callback)))
    (when (and fn (hash-table-p array))
      (labels ((walk (arr)
                 (dolist (pair (%php-array-pairs arr))
                   (if (hash-table-p (cdr pair))
                       (walk (cdr pair))
                       (if extra-data
                           (funcall fn (cdr pair) (car pair) extra-data)
                           (funcall fn (cdr pair) (car pair)))))))
        (walk array))))
  t)

(defun %php-array-splice-in-place (array offset &optional length replacement)
  "PHP array_splice modifying the original array."
  (%php-array-splice array offset length replacement))

(defun %php-strnatcmp (s1 s2 &optional case-insensitive)
  "Natural-order comparison of S1 and S2 (PHP strnatcmp/strnatcasecmp).  Returns
-1, 0, or 1.  A run of digits in BOTH strings is compared by numeric value (so
\"img2\" < \"img10\"); other characters compare by code.  Leading whitespace is
skipped, as PHP does."
  (let* ((a (let ((x (%php-stringify s1))) (if case-insensitive (string-downcase x) x)))
         (b (let ((x (%php-stringify s2))) (if case-insensitive (string-downcase x) x)))
         (la (length a)) (lb (length b))
         (i 0) (j 0))
    (loop
      (loop while (and (< i la) (member (char a i) '(#\Space #\Tab #\Newline #\Return))) do (incf i))
      (loop while (and (< j lb) (member (char b j) '(#\Space #\Tab #\Newline #\Return))) do (incf j))
      (cond
        ((and (>= i la) (>= j lb)) (return 0))
        ((>= i la) (return -1))
        ((>= j lb) (return 1)))
      (let ((ca (char a i)) (cb (char b j)))
        (if (and (digit-char-p ca) (digit-char-p cb))
            (let ((di i) (dj j))
              (loop while (and (< di la) (digit-char-p (char a di))) do (incf di))
              (loop while (and (< dj lb) (digit-char-p (char b dj))) do (incf dj))
              (let ((na (parse-integer a :start i :end di))
                    (nb (parse-integer b :start j :end dj)))
                (cond ((< na nb) (return -1))
                      ((> na nb) (return 1))
                      (t (setf i di j dj)))))
            (cond ((char< ca cb) (return -1))
                  ((char> ca cb) (return 1))
                  (t (incf i) (incf j))))))))

(defun %php-strnatcasecmp (s1 s2)
  "PHP strnatcasecmp: case-insensitive natural-order string comparison."
  (%php-strnatcmp s1 s2 t))

(defun %php-natural-sort-in-place (array case-insensitive)
  "Sort ARRAY's values in natural order IN PLACE, preserving keys. Returns T."
  (let ((pairs (stable-sort (copy-list (%php-array-pairs array))
                            (lambda (p1 p2)
                              (< (%php-strnatcmp (cdr p1) (cdr p2) case-insensitive) 0)))))
    (clrhash array)
    (setf (gethash +php-array-order-key+ array) nil
          (gethash +php-array-next-index-key+ array) 0)
    (dolist (pair pairs t)
      (%php-array-set array (car pair) (cdr pair)))))

(defun %php-natsort (array)
  "PHP natsort: natural-order sort of values, preserving keys."
  (%php-natural-sort-in-place array nil))

(defun %php-natcasesort (array)
  "PHP natcasesort: case-insensitive natural-order sort, preserving keys."
  (%php-natural-sort-in-place array t))

(defun %php-array-map-null (array1 &rest arrays)
  "PHP array_map with null callback: zip arrays into array-of-arrays."
  (let ((result (%php-make-array))
        (lists (mapcar #'%php-array-values-list (cons array1 arrays)))
        (i 0))
    (loop while (every #'consp lists)
          do (let ((row (%php-make-array)))
               (loop for lst in lists for j from 0
                     do (%php-array-set row j (car lst)))
               (%php-array-set result i row)
               (incf i)
               (setf lists (mapcar #'cdr lists))))
    result))
