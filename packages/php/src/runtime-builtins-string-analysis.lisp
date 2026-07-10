;;;; packages/php/src/runtime-builtins-string-analysis.lisp -- Search, word, phonetic, and output PHP string operations

(in-package :cl-cc/php)

;;; ─── String search ────────────────────────────────────────────────────────────

(defun %php-strstr (haystack needle &optional before-needle)
  "PHP strstr: find first occurrence of NEEDLE in HAYSTACK."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (pos (search n h)))
    (when pos
      (if (%php-truthy before-needle)
          (subseq h 0 pos)
          (subseq h pos)))))

(defun %php-stristr (haystack needle &optional before-needle)
  "PHP stristr: case-insensitive strstr."
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (pos (search (string-downcase n) (string-downcase h))))
    (when pos
      (if (%php-truthy before-needle)
          (subseq h 0 pos)
          (subseq h pos)))))

(defun %php-strchr (haystack needle &optional before-needle)
  "PHP strchr: alias for strstr."
  (%php-strstr haystack needle before-needle))

;;; ─── String word operations ───────────────────────────────────────────────────

(defun %php-str-word-count (string &optional (format 0) charlist)
  "PHP str_word_count: count words or return word array."
  (declare (ignore charlist))
  (let* ((s (%php-stringify string))
         (words nil)
         (in-word nil)
         (word-start 0))
    (loop for i from 0 below (length s)
          for ch = (char s i)
          do (cond ((and (alpha-char-p ch) (not in-word))
                    (setf in-word t word-start i))
                   ((and (not (alpha-char-p ch)) in-word)
                    (push (cons word-start (subseq s word-start i)) words)
                    (setf in-word nil)))
          finally (when in-word
                    (push (cons word-start (subseq s word-start)) words)))
    (let ((ordered (reverse words)))
      (cond ((= format 0) (length ordered))
            ((= format 1) (let ((r (%php-make-array)))
                            (loop for i from 0 for (pos . w) in ordered
                                  do (%php-array-set r i w))
                            r))
            ((= format 2) (let ((r (%php-make-array)))
                            (loop for (pos . w) in ordered
                                  do (%php-array-set r pos w))
                            r))
            (t (length ordered))))))

;;; ─── Edit distance / similarity ──────────────────────────────────────────────

(defun %php-edit-distance (a b)
  "Return the Levenshtein distance between two sequences A and B."
  (let* ((m (length a))
         (n (length b))
         (dp (make-array (list (1+ m) (1+ n)) :initial-element 0)))
    (dotimes (i (1+ m)) (setf (aref dp i 0) i))
    (dotimes (j (1+ n)) (setf (aref dp 0 j) j))
    (loop for i from 1 to m do
      (loop for j from 1 to n do
        (setf (aref dp i j)
              (if (equal (elt a (1- i)) (elt b (1- j)))
                  (aref dp (1- i) (1- j))
                  (1+ (min (aref dp (1- i) j)
                           (aref dp i (1- j))
                           (aref dp (1- i) (1- j))))))))
    (aref dp m n)))

(defun %php-levenshtein (s1 s2)
  "PHP levenshtein: edit distance between S1 and S2."
  (%php-edit-distance (%php-stringify s1) (%php-stringify s2)))

(defun %php-combining-mark-p (ch)
  "Return true when CH is in a common Unicode combining mark block."
  (let ((code (char-code ch)))
    (or (<= #x0300 code #x036F)
        (<= #x1AB0 code #x1AFF)
        (<= #x1DC0 code #x1DFF)
        (<= #x20D0 code #x20FF)
        (<= #xFE20 code #xFE2F))))

(defun %php-grapheme-clusters (string)
  "Split STRING into pragmatic Unicode grapheme clusters for Intl helpers."
  (let ((clusters nil)
        (current nil))
    (labels ((flush-current ()
               (when current
                 (push (coerce (nreverse current) 'string) clusters)
                 (setf current nil))))
      (loop for ch across (%php-stringify string) do
        (if (and current (%php-combining-mark-p ch))
            (push ch current)
            (progn
              (flush-current)
              (setf current (list ch)))))
      (flush-current))
    (coerce (nreverse clusters) 'vector)))

(defun %php-grapheme-levenshtein (s1 s2)
  "PHP 8.5 grapheme_levenshtein: edit distance by grapheme clusters."
  (%php-edit-distance (%php-grapheme-clusters s1)
                      (%php-grapheme-clusters s2)))

(defun %php-similar-text (s1 s2 &optional percent-var)
  "PHP similar_text: compute similarity between S1 and S2."
  (declare (ignore percent-var))
  (let* ((a (%php-stringify s1))
         (b (%php-stringify s2))
         (alen (length a))
         (blen (length b)))
    (if (or (= alen 0) (= blen 0))
        0
        (let ((common 0))
          (loop for i from 0 below alen do
            (loop for j from 0 below blen do
              (when (char= (char a i) (char b j))
                (incf common)
                (return))))
          common))))

(defun %php-soundex (string)
  "PHP soundex: Soundex phonetic encoding of STRING."
  (let* ((s (string-upcase (%php-stringify string)))
         (table '((#\B . #\1) (#\F . #\1) (#\P . #\1) (#\V . #\1)
                  (#\C . #\2) (#\G . #\2) (#\J . #\2) (#\K . #\2)
                  (#\Q . #\2) (#\S . #\2) (#\X . #\2) (#\Z . #\2)
                  (#\D . #\3) (#\T . #\3)
                  (#\L . #\4)
                  (#\M . #\5) (#\N . #\5)
                  (#\R . #\6))))
    (if (= (length s) 0) ""
        (let ((result (make-array 4 :element-type 'character :initial-element #\0))
              (idx 1)
              (prev #\Nul))
          (setf (aref result 0) (char s 0))
          (loop for ch across (subseq s 1)
                while (< idx 4)
                do (let ((code (cdr (assoc ch table))))
                     (when (and code (not (eql code prev)))
                       (setf (aref result idx) code
                             prev code)
                       (incf idx))))
          (coerce result 'string)))))

;;; ─── printf family ────────────────────────────────────────────────────────────

(defun %php-printf (format-string &rest args)
  "PHP printf: formatted print, returns length written."
  (let ((out (apply #'%php-sprintf format-string args)))
    (write-string out)
    (length out)))

(defun %php-vsprintf (format-string args-array)
  "PHP vsprintf: sprintf with args from array."
  (apply #'%php-sprintf format-string (%php-array-values-list args-array)))

(defun %php-vprintf (format-string args-array)
  "PHP vprintf: printf with args from array."
  (let ((out (%php-vsprintf format-string args-array)))
    (write-string out)
    (length out)))
