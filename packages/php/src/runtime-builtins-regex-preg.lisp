;;;; packages/php/src/runtime-builtins-regex-preg.lisp -- PHP preg_* regex engine and scalar regex builtins

(in-package :cl-cc/php)

(in-package :cl-cc/php)

;;; -----------------------------------------------------------------------
;;;  PHP pattern parser — strip /delimiters/ and extract flags
;;; -----------------------------------------------------------------------

(defun %php-strip-pattern (pattern)
  "Parse PHP regex /pattern/flags string. Returns (values pattern-str flags-str)."
  (let ((s (%php-stringify pattern)))
    (if (and (>= (length s) 2) (char= (char s 0) #\/))
        ;; Find closing delimiter
        (let* ((last-slash (position #\/ s :from-end t :start 1)))
          (if last-slash
              (values (subseq s 1 last-slash)
                      (subseq s (1+ last-slash)))
              (values s "")))
        (values s ""))))

;;; -----------------------------------------------------------------------
;;;  PHP NFA engine — simplified clone of the JS regex engine
;;; -----------------------------------------------------------------------

(defun %php-compile-char-class (pattern pos)
  "Parse [...] at POS. Returns (values match-fn new-pos)."
  (let ((complement-p (and (< pos (length pattern))
                           (char= (char pattern pos) #\^)))
        (chars nil))
    (when complement-p (incf pos))
    (loop while (and (< pos (length pattern))
                     (not (char= (char pattern pos) #\])))
          do (let ((ch (char pattern pos)))
               (cond
                 ((char= ch #\\)
                  (incf pos)
                  (let ((esc (char pattern pos)))
                    (push (cond ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))))
                                ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return)))))
                                (t esc))
                          chars)
                    (incf pos)))
                 ((and (< (+ pos 2) (length pattern))
                       (char= (char pattern (+ pos 1)) #\-)
                       (not (char= (char pattern (+ pos 2)) #\])))
                  (let ((from ch) (to (char pattern (+ pos 2))))
                    (push (list :range from to) chars)
                    (incf pos 3)))
                 (t (push ch chars) (incf pos)))))
    (when (and (< pos (length pattern)) (char= (char pattern pos) #\]))
      (incf pos))
    (let ((cs (nreverse chars)) (cp complement-p))
      (values (lambda (c)
                (let ((m (loop for item in cs
                               thereis (cond ((characterp item) (char= c item))
                                             ((and (consp item) (eq (car item) :range))
                                              (char<= (cadr item) c (caddr item)))
                                             ((functionp item) (funcall item c))))))
                  (if cp (not m) m)))
              pos))))

(defun %php-compile-regex (pat &key ic ml)
  "Compile PHP regex pattern PAT to a matcher (str pos g) -> end-pos or nil.

Returns (values matcher group-count).  When the matcher is called with a
hash-table G, each capturing group records its matched span as
(start . end) under its 1-based group number — the greedy non-backtracking
engine never has to undo a recorded span, so capture stays simple.  Callers
that don't need groups pass NIL for G."
  (let ((compile-atom nil)
        (compile-seq  nil)
        (compile-alt  nil)
        (group-count  0))
    (setf compile-atom
          (lambda (pos)
            (if (>= pos (length pat))
                (values nil pos)
            (let ((ch (char pat pos)))
              (cond
                ((or (char= ch #\|) (char= ch #\))) (values nil pos))
                ((char= ch #\[)
                 (multiple-value-bind (fn end) (%php-compile-char-class pat (1+ pos))
                   (values (lambda (s i g) (declare (ignore g))
                              (when (and (< i (length s))
                                         (funcall fn (if ic (char-downcase (char s i)) (char s i))))
                                (1+ i)))
                           end)))
                ((and (char= ch #\() (< (1+ pos) (length pat))
                      (char= (char pat (1+ pos)) #\?) (< (+ pos 2) (length pat))
                      (char= (char pat (+ pos 2)) #\:))
                 (multiple-value-bind (f e) (funcall compile-alt (+ pos 3))
                   (values f (if (and (< e (length pat)) (char= (char pat e) #\))) (1+ e) e))))
                ((char= ch #\()
                 ;; Capturing group — allocate its 1-based number BEFORE
                 ;; compiling the inner pattern so nested groups number
                 ;; left-to-right by opening paren, as PHP does.
                 (let ((gnum (incf group-count)))
                   (multiple-value-bind (f e) (funcall compile-alt (1+ pos))
                     (values (lambda (s i g)
                               (let ((end (funcall f s i g)))
                                 (when end
                                   (when g (setf (gethash gnum g) (cons i end)))
                                   end)))
                             (if (and (< e (length pat)) (char= (char pat e) #\))) (1+ e) e)))))
                ((char= ch #\.)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (and (< i (length s))
                                       (or (not ml) (not (char= (char s i) #\Newline))))
                              (1+ i)))
                         (1+ pos)))
                ((char= ch #\^)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (if ml (or (= i 0) (char= (char s (1- i)) #\Newline))
                                       (= i 0)) i))
                         (1+ pos)))
                ((char= ch #\$)
                 (values (lambda (s i g) (declare (ignore g))
                            (when (if ml (or (= i (length s)) (char= (char s i) #\Newline))
                                       (= i (length s))) i))
                         (1+ pos)))
                ((char= ch #\\)
                 (let* ((esc (char pat (1+ pos)))
                        (pred (cond ((char= esc #\d) (lambda (c) (digit-char-p c)))
                                    ((char= esc #\D) (lambda (c) (not (digit-char-p c))))
                                    ((char= esc #\w) (lambda (c) (or (alphanumericp c) (char= c #\_))))
                                    ((char= esc #\W) (lambda (c) (not (or (alphanumericp c) (char= c #\_)))))
                                    ((char= esc #\s) (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))))
                                    ((char= esc #\S) (lambda (c) (not (member c '(#\Space #\Tab #\Newline #\Return)))))
                                    ((char= esc #\n) (lambda (c) (char= c #\Newline)))
                                    ((char= esc #\t) (lambda (c) (char= c #\Tab)))
                                    ((char= esc #\r) (lambda (c) (char= c #\Return)))
                                    (t (let ((lit esc)) (lambda (c) (char= c lit)))))))
                   (values (lambda (s i g) (declare (ignore g))
                              (when (and (< i (length s))
                                         (funcall pred (if ic (char-downcase (char s i)) (char s i))))
                                (1+ i)))
                           (+ pos 2))))
                (t (let ((lit (if ic (char-downcase ch) ch)))
                     (values (lambda (s i g) (declare (ignore g))
                                (when (and (< i (length s))
                                           (char= (if ic (char-downcase (char s i)) (char s i)) lit))
                                  (1+ i)))
                             (1+ pos)))))))))
    (setf compile-seq
          (lambda (pos)
            (let ((fns nil))
              (loop
                (multiple-value-bind (af np) (funcall compile-atom pos)
                  (unless af (return))
                  (setf pos np)
                  (when (< pos (length pat))
                    (let ((q (char pat pos)))
                      (cond
                        ((char= q #\*)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g)
                                      (loop for j = (funcall fn s i g) then (funcall fn s j g)
                                            while j do (setf i j) finally (return i))))))
                        ((char= q #\+)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g)
                                      (let ((j (funcall fn s i g)))
                                        (when j
                                          (loop for k = (funcall fn s j g) then (funcall fn s k g)
                                                while k do (setf j k) finally (return j))))))))
                        ((char= q #\?)
                         (incf pos)
                         (when (and (< pos (length pat)) (char= (char pat pos) #\?)) (incf pos))
                         (let ((fn af))
                           (setf af (lambda (s i g) (or (funcall fn s i g) i))))))))
                  (push af fns)))
              (let ((fs (nreverse fns)))
                (values (lambda (s i g)
                          (let ((p i))
                            (dolist (fn fs p)
                              (let ((r (funcall fn s p g)))
                                (if r (setf p r) (return nil))))))
                        pos)))))
    (setf compile-alt
          (lambda (pos)
            (multiple-value-bind (ff np) (funcall compile-seq pos)
              (setf pos np)
              (if (and (< pos (length pat)) (char= (char pat pos) #\|))
                  (multiple-value-bind (rf rp) (funcall compile-alt (1+ pos))
                    (let ((f ff) (r rf))
                      (values (lambda (s i g) (or (funcall f s i g) (funcall r s i g))) rp)))
                  (values ff pos)))))
    (multiple-value-bind (fn _) (funcall compile-alt 0)
      (declare (ignore _))
      (values fn group-count))))

(defun %php-regex-match-at (fn group-count s i)
  "Run matcher FN (from %php-compile-regex) at position I of S with capture.
Return (values match-end groups) on success, NIL on failure.  GROUPS is a
vector indexed by group number: index 0 = the full match (i . end), 1..N = each
capturing group's (start . end) span or NIL when the group did not participate."
  (let* ((g (make-hash-table))
         (end (funcall fn s i g)))
    (when end
      (let ((groups (make-array (1+ group-count) :initial-element nil)))
        (setf (aref groups 0) (cons i end))
        (loop for k from 1 to group-count
              do (setf (aref groups k) (gethash k g)))
        (values end groups)))))

(defun %php-regex-group-string (groups idx s)
  "Return the substring of S captured by group IDX, or \"\" when absent."
  (let ((span (and (< idx (length groups)) (aref groups idx))))
    (if span (subseq s (car span) (cdr span)) "")))

(defun %php-regex-expand-replacement (repl groups s)
  "Expand $N, ${N} and \\N backreferences in REPL using GROUPS (from
%php-regex-match-at) over subject S.  $0 / \\0 is the whole match.  A reference
to a group that did not match expands to the empty string."
  (with-output-to-string (out)
    (let ((i 0) (n (length repl)))
      (loop while (< i n)
            do (let ((ch (char repl i)))
                 (cond
                   ;; ${N}
                   ((and (char= ch #\$) (< (1+ i) n) (char= (char repl (1+ i)) #\{))
                    (let ((close (position #\} repl :start (+ i 2))))
                      (if close
                          (let ((num (parse-integer repl :start (+ i 2) :end close :junk-allowed t)))
                            (when num (write-string (%php-regex-group-string groups num s) out))
                            (setf i (1+ close)))
                          (progn (write-char ch out) (incf i)))))
                   ;; $N or \N — consume up to two digits, preferring the
                   ;; two-digit group when it exists.
                   ((and (or (char= ch #\$) (char= ch #\\))
                         (< (1+ i) n) (digit-char-p (char repl (1+ i))))
                    (let* ((d1 (digit-char-p (char repl (1+ i))))
                           (d2 (and (< (+ i 2) n) (digit-char-p (char repl (+ i 2)))))
                           (two (and d2 (+ (* 10 d1) d2))))
                      (if (and two (< two (length groups)))
                          (progn (write-string (%php-regex-group-string groups two s) out)
                                 (setf i (+ i 3)))
                          (progn (write-string (%php-regex-group-string groups d1 s) out)
                                 (setf i (+ i 2))))))
                   ;; \\ and other backslash escapes -> the escaped char literally
                   ((and (char= ch #\\) (< (1+ i) n))
                    (write-char (char repl (1+ i)) out) (setf i (+ i 2)))
                   (t (write-char ch out) (incf i))))))))

(defun %php-regex-exec (pattern-str subject &key (start 0) ic ml)
  "Try to match PATTERN-STR in SUBJECT from START. Returns (values match-str match-start) or nil."
  (let ((fn (handler-case (%php-compile-regex pattern-str :ic ic :ml ml) (error () nil)))
        (n (length subject)))
    (when fn
      (loop for i from start to n
            for end = (funcall fn subject i nil)
            when end
              return (values (subseq subject i end) i end)))))

(defun %php-preg-offset (offset subject-length)
  "Normalize preg_* OFFSET into an in-bounds subject index."
  (let ((i (%php-to-integer offset)))
    (cond
      ((minusp i) (max 0 (+ subject-length i)))
      ((> i subject-length) subject-length)
      (t i))))

;;; -----------------------------------------------------------------------
;;;  preg_match
;;; -----------------------------------------------------------------------

(defun %php-preg-match (pattern subject &optional matches flags offset)
  "PHP preg_match: return 1 if pattern matches, 0 if not, store matches."
  (declare (ignore matches flags))  ; matches are handled by lowering when present.
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (start (%php-preg-offset offset (length str))))
      (multiple-value-bind (match-str _start _end) (%php-regex-exec pat str :start start :ic ic :ml ml)
        (declare (ignore _start _end))
        (if match-str 1 0)))))

(defun %php-preg-match-all (pattern subject &optional matches flags offset)
  "PHP preg_match_all: return the number of full matches.

The matcher is anchored at the position it is given, so we must SCAN forward to
the next match rather than only testing the current position — the previous loop
tested fn at successive positions and stopped at the first gap, so it counted
only matches that happened to be consecutive from index 0 (e.g. '1a2b3' / \\d
returned 1 instead of 3)."
  (declare (ignore matches flags))
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (count 0)
           (pos (%php-preg-offset offset (length str))))
      (when fn
        (loop while (<= pos (length str))
              do (let ((match-start nil) (match-end nil))
                   (loop for i from pos to (length str)
                         for e = (funcall fn str i nil)
                         when e do (setf match-start i match-end e) (return))
                   (if match-start
                       (progn (incf count) (setf pos (max (1+ match-start) match-end)))
                       (return)))))
      count)))

(defun %php-preg-match-matches (pattern subject &optional flags offset)
  "Return the $matches array for preg_match($pattern,$subject): index 0 = the
full match, k = capture group k (empty string if the group did not match), or an
empty array when PATTERN does not match.  Returned BY VALUE and assigned to the
caller's $matches at the call site — the VM copies host structs across the
bridge, so a mutable ref box would not propagate."
  (declare (ignore flags))
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (arr (%php-make-array))
           (start (%php-preg-offset offset (length str))))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () (values nil 0)))
        (when fn
          (loop for i from start to (length str)
                do (multiple-value-bind (end groups) (%php-regex-match-at fn gcount str i)
                     (when end
                       (loop for k from 0 to gcount
                             do (%php-array-set arr k (%php-regex-group-string groups k str)))
                       (return))))))
      arr)))

(defun %php-preg-match-all-matches (pattern subject &optional flags offset)
  "Return the $matches array for preg_match_all in PREG_PATTERN_ORDER: index 0 is
an array of every full match, k an array of every capture-group-k match."
  (declare (ignore flags))
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (start (%php-preg-offset offset (length str))))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () (values nil 0)))
        (let ((per-group (make-array (1+ gcount))) (count 0) (pos start))
          (dotimes (k (1+ gcount)) (setf (aref per-group k) (%php-make-array)))
          (when fn
            (loop while (<= pos (length str))
                  do (let ((ms nil) (me nil) (mg nil))
                       (loop for i from pos to (length str)
                             do (multiple-value-bind (e g) (%php-regex-match-at fn gcount str i)
                                  (when e (setf ms i me e mg g) (return))))
                       (if ms
                           (progn
                             (loop for k from 0 to gcount
                                   do (%php-array-set (aref per-group k) count
                                                      (%php-regex-group-string mg k str)))
                             (incf count)
                             (setf pos (max (1+ ms) me)))
                           (return)))))
          (let ((arr (%php-make-array)))
            (loop for k from 0 to gcount do (%php-array-set arr k (aref per-group k)))
            arr))))))

;;; -----------------------------------------------------------------------
;;;  preg_replace
;;; -----------------------------------------------------------------------

(defun %php-preg-replace (pattern replacement subject)
  "PHP preg_replace: replace all matches of PATTERN in SUBJECT with REPLACEMENT."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (repl (%php-stringify replacement)))
      (multiple-value-bind (fn gcount)
          (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length str))
                      do (let ((match-start nil) (match-end nil) (groups nil))
                           ;; Scan forward for the next match, capturing groups.
                           (loop for i from pos to (length str)
                                 do (multiple-value-bind (e g) (%php-regex-match-at fn gcount str i)
                                      (when e (setf match-start i match-end e groups g) (return))))
                           (if match-start
                               (progn
                                 (write-string (subseq str pos match-start) out)
                                 ;; Expand $0/$1/${1}/\1 backreferences in the replacement.
                                 (write-string (%php-regex-expand-replacement repl groups str) out)
                                 (setf pos (max (1+ match-start) match-end)))
                               (progn
                                 (write-string (subseq str pos) out)
                                 (return)))))))
            str)))))

;;; -----------------------------------------------------------------------
;;;  preg_split
;;; -----------------------------------------------------------------------

(defun %php-preg-split (pattern subject &optional (limit -1))
  "PHP preg_split: split SUBJECT by regex PATTERN."
  (multiple-value-bind (pat flags) (%php-strip-pattern pattern)
    (let* ((ic (find #\i flags))
           (ml (find #\m flags))
           (str (%php-stringify subject))
           (fn (handler-case (%php-compile-regex pat :ic ic :ml ml) (error () nil)))
           (result (%php-make-array))
           (pos 0)
           (count 0))
      (if fn
          (loop while (<= pos (length str))
                do (when (and (> limit 0) (>= count (1- limit)))
                     (%php-array-set result count (subseq str pos))
                     (return result))
                   (let ((sep-start nil) (sep-end nil))
                     ;; Scan forward for the separator
                     (loop for i from pos to (length str)
                           for e = (funcall fn str i nil)
                           when e
                             do (setf sep-start i sep-end e)
                                (return))
                     (if sep-start
                         (progn
                           (%php-array-set result count (subseq str pos sep-start))
                           (incf count)
                           (setf pos (max (1+ sep-start) sep-end)))
                         (progn
                           (%php-array-set result count (subseq str pos))
                           (return result))))
                finally (return result))
          (%php-preg-split-literal str pat)))))

(defun %php-preg-split-literal (str sep)
  "Fallback: split STR by literal SEP."
  (let ((result (%php-make-array))
        (count 0)
        (pos 0))
    (loop
      (let ((found (search sep str :start2 pos)))
        (if found
            (progn
              (%php-array-set result count (subseq str pos found))
              (incf count)
              (setf pos (+ found (length sep))))
            (progn
              (%php-array-set result count (subseq str pos))
              (return result)))))
    result))

;;; -----------------------------------------------------------------------
;;;  preg_quote
;;; -----------------------------------------------------------------------

(defun %php-preg-quote (str &optional delimiter)
  "PHP preg_quote: escape special regex characters in STR."
  (declare (ignore delimiter))
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (when (member ch '(#\. #\* #\+ #\? #\[ #\] #\^ #\$ #\( #\) #\{ #\} #\| #\\ #\/))
               (write-char #\\ out))
             (write-char ch out))))
