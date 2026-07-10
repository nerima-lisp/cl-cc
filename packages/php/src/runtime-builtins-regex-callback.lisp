;;;; packages/php/src/runtime-builtins-regex-callback.lisp -- PHP preg_replace_callback builtins

(in-package :cl-cc/php)

(defun %php-preg-replace-callback (pattern callback subject &optional (limit -1) count-var)
  "PHP preg_replace_callback: replace regex matches in SUBJECT using CALLBACK.
The callback receives a PHP array whose [0] element is the full match and
whose [1..N] elements are capture groups, then returns the replacement string.
Scans like %php-preg-replace (strip the /.../ delimiters + flags, compile with
ic/ml, then scan forward).

The previous implementation called the non-existent %php-regex-search and never
stripped the pattern delimiters, so every call raised
`The function %PHP-REGEX-SEARCH is undefined.'"
  (declare (ignore count-var))
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (cb (%php-callable-function callback))
           (max-replacements (if (and (numberp limit) (> limit 0)) limit most-positive-fixnum)))
      (multiple-value-bind (fn group-count)
          (handler-case (%php-compile-regex pat :ic ic :ml ml)
            (error () (values nil 0)))
        (if (and fn cb)
            (with-output-to-string (out)
              (let ((pos 0) (replacements 0))
                (loop while (<= pos (length str))
                      do (if (>= replacements max-replacements)
                             (progn (write-string (subseq str pos) out) (return))
                             (let ((match-start nil) (match-end nil) (match-groups nil))
                               ;; Scan forward for the next match position.
                               (loop for i from pos to (length str)
                                     do (multiple-value-bind (e groups)
                                            (%php-regex-match-at fn group-count str i)
                                          (when e
                                            (setf match-start i
                                                  match-end e
                                                  match-groups groups)
                                            (return))))
                               (if match-start
                                   (let ((match-arr (%php-make-array)))
                                     (write-string (subseq str pos match-start) out)
                                     (loop for k from 0 to group-count
                                           do (%php-array-set match-arr k
                                                              (%php-regex-group-string match-groups k str)))
                                     (write-string (%php-stringify (funcall cb match-arr)) out)
                                     (incf replacements)
                                     (setf pos (max (1+ match-start) match-end)))
                                   (progn (write-string (subseq str pos) out) (return))))))))
            str)))))

(defun %php-preg-replace-callback-array (pattern-map subject &optional (limit -1) count)
  "PHP preg_replace_callback_array: PATTERN-MAP is a single PHP array mapping
each regex pattern (key) to its callback (value); apply them in order to
SUBJECT.  The previous signature took separate patterns/callbacks arrays, so a
normal call preg_replace_callback_array(['/re/' => fn], $s) passed too few
arguments."
  (declare (ignore count))
  (let ((result subject))
    (when (hash-table-p pattern-map)
      (dolist (pair (%php-array-pairs pattern-map))
        (setf result (%php-preg-replace-callback (car pair) (cdr pair) result limit))))
    result))
