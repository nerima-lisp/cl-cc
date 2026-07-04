;;;; packages/php/src/runtime-builtins-string-extra.lisp -- Additional PHP string compatibility operations

(in-package :cl-cc/php)

;;; ─── is_countable (PHP 7.3) ──────────────────────────────────────────────────

(defun %php-is-countable (value)
  "PHP is_countable: check if value can be counted."
  (or (hash-table-p value)
      (and (%php-null-p value) nil)))

;;; ─── quoted_printable functions ──────────────────────────────────────────────

(defun %php-quoted-printable-encode (str)
  "PHP quoted_printable_encode: encode string as quoted-printable."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (let ((code (char-code ch)))
               (if (or (= code 9)
                       (and (>= code 32) (<= code 126) (/= code 61)))
                   (write-char ch out)
                   (format out "=~2,'0X" code))))))

(defun %php-quoted-printable-decode (str)
  "PHP quoted_printable_decode: decode quoted-printable string."
  (let ((s (%php-stringify str))
        (i 0))
    (with-output-to-string (out)
      (loop while (< i (length s))
            do (cond
                 ((and (char= (char s i) #\=)
                       (< (+ i 2) (length s))
                       (digit-char-p (char s (+ i 1)) 16)
                       (digit-char-p (char s (+ i 2)) 16))
                  (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)) out)
                  (incf i 3))
                 (t (write-char (char s i) out) (incf i)))))))

;;; ─── Additional string functions (strtr, strpbrk, strspn, strcspn, etc.) ─────

(defun %php-strtr (str &rest args)
  "PHP strtr: strtr(str, from, to) translates chars; strtr(str, pairs) replaces
substrings from an associative array (longest-match-first)."
  (let ((s (%php-stringify str)))
    (cond
      ;; strtr(str, array) — pairs form
      ((and (= (length args) 1) (hash-table-p (first args)))
       (let* ((pairs (%php-array-pairs (first args)))
              ;; longest keys first (PHP semantics)
              (sorted (sort (copy-list pairs)
                            (lambda (a b) (> (length (%php-stringify (car a)))
                                             (length (%php-stringify (car b))))))))
         (with-output-to-string (out)
           (let ((i 0) (n (length s)))
             (loop while (< i n)
                   do (let ((matched nil))
                        (dolist (pair sorted)
                          (let* ((from (%php-stringify (car pair)))
                                 (flen (length from)))
                            (when (and (> flen 0) (<= (+ i flen) n)
                                       (string= s from :start1 i :end1 (+ i flen)))
                              (write-string (%php-stringify (cdr pair)) out)
                              (incf i flen) (setf matched t) (return))))
                        (unless matched (write-char (char s i) out) (incf i))))))))
      ;; strtr(str, from, to) — per-character translation
      ((= (length args) 2)
       (let* ((from (%php-stringify (first args)))
              (to (%php-stringify (second args)))
              (len (min (length from) (length to))))
         (map 'string
              (lambda (c)
                (let ((pos (position c from :end len)))
                  (if (and pos (< pos len)) (char to pos) c)))
              s)))
      (t s))))

(defun %php-strpbrk (str char-list)
  "PHP strpbrk: return substring from the first occurrence of any char in CHAR-LIST."
  (let* ((s (%php-stringify str))
         (chars (%php-stringify char-list))
         (pos (position-if (lambda (c) (find c chars)) s)))
    (if pos (subseq s pos) nil)))

(defun %php-strspn (subject mask &optional (start 0) length)
  "PHP strspn: length of the initial segment of SUBJECT consisting only of
characters in MASK."
  (let* ((s (%php-stringify subject))
         (m (%php-stringify mask))
         (begin (if (< start 0) (max 0 (+ (length s) start)) (min start (length s))))
         (end (if (and length (not (%php-null-p length)))
                  (min (length s) (+ begin (if (< length 0) (+ (length s) length) length)))
                  (length s)))
         (count 0))
    (loop for i from begin below end
          while (find (char s i) m)
          do (incf count))
    count))

(defun %php-strcspn (subject mask &optional (start 0) length)
  "PHP strcspn: length of the initial segment of SUBJECT NOT containing any
character in MASK."
  (let* ((s (%php-stringify subject))
         (m (%php-stringify mask))
         (begin (if (< start 0) (max 0 (+ (length s) start)) (min start (length s))))
         (end (if (and length (not (%php-null-p length)))
                  (min (length s) (+ begin (if (< length 0) (+ (length s) length) length)))
                  (length s)))
         (count 0))
    (loop for i from begin below end
          until (find (char s i) m)
          do (incf count))
    count))

(defun %php-quotemeta (str)
  "PHP quotemeta: backslash-escape regex metacharacters . \\ + * ? [ ^ ] $ ( )."
  (with-output-to-string (out)
    (loop for c across (%php-stringify str)
          do (when (member c '(#\. #\\ #\+ #\* #\? #\[ #\^ #\] #\$ #\( #\)))
               (write-char #\\ out))
             (write-char c out))))

(defun %php-htmlentities (str &rest args)
  "PHP htmlentities: alias to htmlspecialchars (covers the common &<>\"' entities)."
  (declare (ignore args))
  (%php-htmlspecialchars str))

(defun %php-metaphone (str &optional phonemes)
  "PHP metaphone: simplified phonetic key (uppercase consonant skeleton)."
  (declare (ignore phonemes))
  (let ((s (string-upcase (%php-stringify str))))
    (remove-if-not (lambda (c) (and (alpha-char-p c) (not (find c "AEIOU")))) s)))
