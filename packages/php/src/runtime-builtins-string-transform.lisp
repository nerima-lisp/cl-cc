;;;; packages/php/src/runtime-builtins-string-transform.lisp -- Transform and compare PHP string operations

(in-package :cl-cc/php)

;;; ─── String padding / splitting ──────────────────────────────────────────────

(defun %php-str-pad (string length &optional (pad-string " ") (pad-type +php-str-pad-right+))
  "PHP str_pad: pad STRING to LENGTH using PAD-STRING on the given side."
  (let* ((s (%php-stringify string))
         (p (%php-stringify (or pad-string " ")))
         (target (if (numberp length) length 0))
         (slen (length s))
         (plen (length p)))
    (if (or (<= target slen) (= plen 0))
        s
        (let* ((need (- target slen))
               (full-pads (floor need plen))
               (remainder (- need (* full-pads plen))))
          (flet ((make-pad (n)
                   (let ((r (- n (* (floor n plen) plen))))
                     (with-output-to-string (o)
                       (dotimes (i (floor n plen)) (write-string p o))
                       (when (> r 0) (write-string p o :end r))))))
            (cond ((= pad-type +php-str-pad-left+)
                   (concatenate 'string (make-pad need) s))
                  ((= pad-type +php-str-pad-both+)
                   (let* ((left-need (floor need 2))
                          (right-need (- need left-need)))
                     (concatenate 'string (make-pad left-need) s (make-pad right-need))))
                  (t
                   (concatenate 'string s (make-pad need)))))))))

(defun %php-str-split (string &optional (split-length 1))
  "PHP str_split: split STRING into chunks of SPLIT-LENGTH, returning a PHP array."
  (let* ((s (%php-stringify string))
         (n (length s))
         (cl (max 1 (or split-length 1)))
         (result (%php-make-array)))
    (loop for i from 0 below n by cl
          do (%php-array-set result (%php-array-next-auto-index result)
                             (subseq s i (min n (+ i cl)))))
    result))

(defun %php-substr-count (haystack needle &optional (offset 0) length)
  "PHP substr_count: count non-overlapping occurrences of NEEDLE in HAYSTACK."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (nlen (length n))
         (hlen (length h))
         (start (or offset 0))
         (end (if (and length (not (%php-null-p length))) (+ start length) hlen))
         (count 0))
    (when (> nlen 0)
      (loop for pos = (search n h :start2 start :end2 end)
            while pos
            do (incf count)
               (setf start (+ pos nlen))))
    count))

(defun %php-substr-replace (string replace start &optional length)
  "PHP substr_replace: replace a portion of STRING with REPLACE."
  (let* ((s (%php-stringify string))
         (r (%php-stringify replace))
         (slen (length s))
         (b (if (minusp start) (max 0 (+ slen start)) (min start slen)))
         (e (cond ((or (null length) (%php-null-p length)) slen)
                  ((minusp length) (max b (+ slen length)))
                  (t (min slen (+ b length))))))
    (concatenate 'string (subseq s 0 b) r (subseq s e))))

;;; ─── Case helpers ─────────────────────────────────────────────────────────────

(defun %php-ucfirst (string)
  "PHP ucfirst: uppercase first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) s
        (concatenate 'string (string (char-upcase (char s 0))) (subseq s 1)))))

(defun %php-lcfirst (string)
  "PHP lcfirst: lowercase first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) s
        (concatenate 'string (string (char-downcase (char s 0))) (subseq s 1)))))

(defun %php-ucwords (string &optional delimiters)
  "PHP ucwords: uppercase the first char of each word in STRING.

The default word delimiters are PHP's \" \\t\\r\\n\\f\\v\" — space, tab, CR, LF,
form-feed, vertical-tab.  These MUST be built from actual control characters:
the Common Lisp string literal \" \\t\\r\\n\\f\\v\" does NOT interpret the
escapes (CL has no \\t escape), so it read as the letters \"trnfv\" — which meant
`r' counted as a word boundary and ucwords(\"world\") returned \"WorLd\"."
  (let* ((s (%php-stringify string))
         (delims (if (and delimiters (not (%php-null-p delimiters)))
                     (coerce (%php-stringify delimiters) 'list)
                     (list #\Space #\Tab #\Return #\Newline #\Page (code-char 11))))
         (prev-delim t))
    (coerce (loop for ch across s
                  collect (if prev-delim (char-upcase ch) ch)
                  do (setf prev-delim (member ch delims)))
            'string)))

;;; ─── Char / ordinal ───────────────────────────────────────────────────────────

(defun %php-ord (string)
  "PHP ord: ASCII value of first character of STRING."
  (let ((s (%php-stringify string)))
    (if (= (length s) 0) 0 (char-code (char s 0)))))

(defun %php-chr (codepoint)
  "PHP chr: character from ASCII CODEPOINT."
  (string (code-char (logand codepoint 255))))

;;; ─── Hex / binary conversion ──────────────────────────────────────────────────

(defun %php-bin2hex (string)
  "PHP bin2hex: convert STRING to hex representation."
  (%php-byte-vector-hex (%php-string-bytes string)))

(defun %php-hex2bin (hex)
  "PHP hex2bin: convert HEX string to binary string."
  (let ((h (%php-stringify hex)))
    (with-output-to-string (out)
      (loop for i from 0 below (length h) by 2
            when (< (+ i 1) (length h))
            do (write-char (code-char (parse-integer h :start i :end (+ i 2) :radix 16)) out)))))

;;; ─── String comparison ────────────────────────────────────────────────────────

(defun %php-strcmp (s1 s2)
  "PHP strcmp: binary-safe string comparison."
  (let ((a (%php-stringify s1)) (b (%php-stringify s2)))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strcasecmp (s1 s2)
  "PHP strcasecmp: case-insensitive string comparison."
  (let ((a (string-downcase (%php-stringify s1)))
        (b (string-downcase (%php-stringify s2))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strncmp (s1 s2 n)
  "PHP strncmp: first N chars comparison."
  (let ((a (subseq (%php-stringify s1) 0 (min n (length (%php-stringify s1)))))
        (b (subseq (%php-stringify s2) 0 (min n (length (%php-stringify s2))))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))

(defun %php-strncasecmp (s1 s2 n)
  "PHP strncasecmp: first N chars case-insensitive comparison."
  (let ((a (string-downcase (subseq (%php-stringify s1) 0 (min n (length (%php-stringify s1))))))
        (b (string-downcase (subseq (%php-stringify s2) 0 (min n (length (%php-stringify s2)))))))
    (cond ((string< a b) -1) ((string> a b) 1) (t 0))))
