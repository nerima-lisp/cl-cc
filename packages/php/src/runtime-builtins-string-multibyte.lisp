;;;; packages/php/src/runtime-builtins-string-multibyte.lisp -- Multibyte PHP string operations

(in-package :cl-cc/php)

;;; ─── mb_* multibyte string functions ─────────────────────────────────────────
;;; In CL, strings are Unicode-aware, so mb_* is mostly identical to the regular
;;; string functions for UTF-8 content.

(defun %php-mb-strlen (str &optional encoding)
  (declare (ignore encoding))
  (length (%php-stringify str)))

(defun %php-mb-substr (str start &optional length encoding)
  (declare (ignore encoding))
  (let* ((s (%php-stringify str))
         (n (length s))
         (b (if (< start 0) (max 0 (+ n start)) (min start n)))
         (e (if (null length) n
                (if (< length 0) (max b (+ n length))
                    (min n (+ b length))))))
    (subseq s b e)))

(defun %php-mb-strtolower (str &optional encoding)
  (declare (ignore encoding))
  (string-downcase (%php-stringify str)))

(defun %php-mb-strtoupper (str &optional encoding)
  (declare (ignore encoding))
  (string-upcase (%php-stringify str)))

(defun %php-mb-strpos (haystack needle &optional (offset 0) encoding)
  (declare (ignore encoding))
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (start (if (< offset 0) (max 0 (+ (length h) offset)) offset))
         (found (search n h :start2 start)))
    (if found found nil)))

(defun %php-mb-strrpos (haystack needle &optional (offset 0) encoding)
  (declare (ignore encoding))
  (let* ((h (%php-stringify haystack))
         (n (%php-stringify needle))
         (end (if (< offset 0) (max 0 (+ (length h) offset)) (length h)))
         (found (search n h :from-end t :end2 end)))
    (if found found nil)))

(defun %php-mb-substr-count (haystack needle &optional encoding)
  (declare (ignore encoding))
  (let ((h (%php-stringify haystack))
        (n (%php-stringify needle))
        (count 0) (pos 0))
    (loop (let ((found (search n h :start2 pos)))
            (unless found (return count))
            (incf count)
            (setf pos (+ found (max 1 (length n))))))))

(defun %php-mb-detect-encoding (str &optional encoding-list strict)
  (declare (ignore str encoding-list strict))
  "UTF-8")  ; Simplified: always report UTF-8

(defun %php-mb-internal-encoding (&optional enc)
  (declare (ignore enc))
  "UTF-8")

(defun %php-mb-convert-encoding (str to-encoding &optional from-encoding)
  (declare (ignore to-encoding from-encoding))
  (%php-stringify str))  ; Pass-through: CL handles Unicode natively

(defun %php-mb-strtolower-encoding (str encoding)
  (declare (ignore encoding))
  (string-downcase (%php-stringify str)))

(defun %php-mb-convert-case (str mode &optional encoding)
  "PHP mb_convert_case: convert case of multibyte string."
  (declare (ignore encoding))
  (let ((s (%php-stringify str)))
    (case mode
      (0 (string-upcase s))    ; MB_CASE_UPPER = 0
      (1 (string-downcase s))  ; MB_CASE_LOWER = 1
      (2 (string-capitalize s)); MB_CASE_TITLE = 2
      (t s))))

(defun %php-mb-str-pad (input length &optional (pad-string " ") (pad-type 1) encoding)
  "PHP 8.3 mb_str_pad: pad a multibyte string to LENGTH."
  (declare (ignore encoding))
  (let* ((s (%php-stringify input))
         (p (%php-stringify pad-string))
         (slen (length s))
         (target (max slen length))
         (deficit (- target slen)))
    (if (<= deficit 0) s
        (let* ((full-pads (floor deficit (max 1 (length p))))
               (remainder  (mod   deficit (max 1 (length p))))
               (pad-chunk  (concatenate 'string
                                        (apply #'concatenate 'string (make-list full-pads :initial-element p))
                                        (subseq p 0 remainder))))
          (cond ((= pad-type 1) (concatenate 'string s pad-chunk))          ; STR_PAD_RIGHT
                ((= pad-type 0) (concatenate 'string pad-chunk s))          ; STR_PAD_LEFT
                ((= pad-type 2)                                              ; STR_PAD_BOTH
                 (let* ((left-pad  (floor deficit 2))
                        (right-pad (- deficit left-pad))
                        (mk (lambda (n)
                              (let* ((fp (floor n (max 1 (length p))))
                                     (rm (mod   n (max 1 (length p)))))
                                (concatenate 'string
                                             (apply #'concatenate 'string (make-list fp :initial-element p))
                                             (subseq p 0 rm))))))
                   (concatenate 'string (funcall mk left-pad) s (funcall mk right-pad))))
                (t (concatenate 'string s pad-chunk)))))))

(defun %php-mb-str-split (string &optional (length 1) encoding)
  "PHP mb_str_split: split multibyte string into array of LENGTH-char chunks."
  (declare (ignore encoding))
  (let* ((s (%php-stringify string))
         (n (length s))
         (l (max 1 (if (or (null length) (%php-null-p length)) 1 length)))
         (result (%php-make-array)))
    (loop for i from 0 below n by l
          do (%php-array-set result (%php-array-next-auto-index result)
                             (subseq s i (min n (+ i l)))))
    result))

(defun %php-str-getcsv (string &optional (separator ",") (enclosure "\"") (escape "\\"))
  "PHP str_getcsv: parse a CSV string into an array."
  (declare (ignore escape))
  (let* ((s (%php-stringify string))
         (sep (%php-stringify separator))
         (enc (%php-stringify enclosure))
         (result (%php-make-array))
         (parts (%php-array-values-list (%php-explode sep s))))
    (dolist (field parts)
      (let* ((f (string-trim '(#\Space) field))
             (stripped (if (and (>= (length f) 2)
                                (string= (subseq f 0 1) enc)
                                (string= (subseq f (1- (length f))) enc))
                           (subseq f 1 (1- (length f)))
                           f)))
        (%php-array-set result (%php-array-next-auto-index result) stripped)))
    result))


;;; ─── mb_chr / mb_ord / mb_strimwidth / mb_ereg family ────────────────────────

(defun %php-mb-chr (code &optional encoding)
  "PHP mb_chr: get character by Unicode code point."
  (declare (ignore encoding))
  (handler-case (string (code-char (truncate code))) (error () nil)))

(defun %php-mb-ord (str &optional encoding)
  "PHP mb_ord: get Unicode code point of first character."
  (declare (ignore encoding))
  (let ((s (%php-stringify str)))
    (if (> (length s) 0) (char-code (char s 0)) nil)))

(defun %php-mb-strimwidth (str start width &optional (trim-marker "") encoding)
  "PHP mb_strimwidth: get truncated string to width."
  (declare (ignore encoding))
  (let* ((s (%php-stringify str))
         (n (length s))
         (b (min start n))
         (end (min n (+ b width))))
    (if (< end n)
        (concatenate 'string (subseq s b end) (%php-stringify trim-marker))
        (subseq s b end))))

(defun %php-mb-ereg (pattern str &optional regs)
  "PHP mb_ereg: multibyte extended regex match."
  (declare (ignore regs))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat)))
        (when fn
          (loop for i from 0 to (length s)
                for e = (funcall fn s i nil)
                when e do (return t)
                finally (return nil))))
    (error () nil)))

(defun %php-mb-eregi (pattern str &optional regs)
  "PHP mb_eregi: case-insensitive mb_ereg."
  (declare (ignore regs))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat :ic t)))
        (when fn
          (loop for i from 0 to (length s)
                for e = (funcall fn s i nil)
                when e do (return t)
                finally (return nil))))
    (error () nil)))

(defun %php-mb-ereg-replace (pattern replacement str &optional option)
  "PHP mb_ereg_replace: multibyte regex replace."
  (declare (ignore option))
  (handler-case
      (let* ((pat  (%php-stringify pattern))
             (repl (%php-stringify replacement))
             (s    (%php-stringify str))
             (fn   (%php-compile-regex pat)))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length s))
                      do (let ((ms nil) (me nil))
                           (loop for i from pos to (length s)
                                 for e = (funcall fn s i nil)
                                 when e do (setf ms i me e) (return))
                           (if ms
                               (progn (write-string (subseq s pos ms) out)
                                      (write-string repl out)
                                      (setf pos (max (1+ ms) me)))
                               (progn (write-string (subseq s pos) out) (return)))))))
            s))
    (error () nil)))

(defun %php-mb-eregi-replace (pattern replacement str &optional option)
  "PHP mb_eregi_replace: case-insensitive mb_ereg_replace."
  (declare (ignore option))
  (handler-case
      (let* ((pat  (%php-stringify pattern))
             (repl (%php-stringify replacement))
             (s    (%php-stringify str))
             (fn   (%php-compile-regex pat :ic t)))
        (if fn
            (with-output-to-string (out)
              (let ((pos 0))
                (loop while (<= pos (length s))
                      do (let ((ms nil) (me nil))
                           (loop for i from pos to (length s)
                                 for e = (funcall fn s i nil)
                                 when e do (setf ms i me e) (return))
                           (if ms
                               (progn (write-string (subseq s pos ms) out)
                                      (write-string repl out)
                                      (setf pos (max (1+ ms) me)))
                               (progn (write-string (subseq s pos) out) (return)))))))
            s))
    (error () nil)))

(defun %php-mb-split (pattern str &optional limit)
  "PHP mb_split: split string by regex."
  (declare (ignore limit))
  (handler-case
      (let* ((pat (%php-stringify pattern))
             (s   (%php-stringify str))
             (fn  (%php-compile-regex pat))
             (result nil)
             (pos 0))
        (if fn
            (progn
              (loop while (<= pos (length s))
                    do (let ((ms nil) (me nil))
                         (loop for i from pos to (length s)
                               for e = (funcall fn s i nil)
                               when e do (setf ms i me e) (return))
                         (if ms
                             (progn (push (subseq s pos ms) result)
                                    (setf pos (max (1+ ms) me)))
                             (progn (push (subseq s pos) result) (return)))))
              (%php-list-to-array (nreverse result)))
            (%php-list-to-array (list s))))
    (error () (%php-list-to-array (list (%php-stringify str))))))
