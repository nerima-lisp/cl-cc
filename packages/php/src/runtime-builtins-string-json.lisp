;;;; PHP JSON string builtins.

(in-package :cl-cc/php)

;;; ─── JSON functions ───────────────────────────────────────────────────────────

(defun %php-json-encode (value &optional flags depth)
  "PHP json_encode: encode PHP value to JSON string.  Honours JSON_PRETTY_PRINT
(128): 4-space indentation, newlines, and a space after each object colon."
  (declare (ignore depth))
  (%php-json-encode-value value 0 (if (integerp flags) flags 0)))

(defun %php-json-encode-value (val depth flags)
  (when (> depth 512) (return-from %php-json-encode-value "null"))
  (cond
    ((%php-null-p val) "null")
    ((null val) "false")
    ((eq val t) "true")
    ((integerp val) (format nil "~D" val))
    ((floatp val) (if (= val (floor val))
                      (format nil "~D" (floor val))
                      (format nil "~F" val)))
    ((stringp val) (%php-json-quote-string val))
    ((hash-table-p val)
     (let* ((pairs (%php-array-pairs val))
            (is-array-p (loop for i from 0 for (k . v) in pairs always (eql k i)))
            (pretty (logbitp 7 flags)))           ; JSON_PRETTY_PRINT = 128
       (if (null pairs)
           "[]"                                    ; PHP renders an empty array as []
           (let ((items (mapcar
                         (lambda (p)
                           (let ((v-str (%php-json-encode-value (cdr p) (1+ depth) flags)))
                             (if is-array-p
                                 v-str
                                 (concatenate 'string
                                              (%php-json-quote-string (%php-stringify (car p)))
                                              (if pretty ": " ":")
                                              v-str))))
                         pairs)))
             (let ((o (if is-array-p "[" "{")) (c (if is-array-p "]" "}")))
               (if pretty
                   (let ((ind (make-string (* 4 (1+ depth)) :initial-element #\Space))
                         (ind0 (make-string (* 4 depth) :initial-element #\Space)))
                     (with-output-to-string (s)
                       (write-string o s) (write-char #\Newline s)
                       (loop for (it . rest) on items
                             do (write-string ind s) (write-string it s)
                                (when rest (write-char #\, s))
                                (write-char #\Newline s))
                       (write-string ind0 s) (write-string c s)))
                   (format nil "~A~{~A~^,~}~A" o items c)))))))
    (t "null")))

(defun %php-json-quote-string (s)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for ch across s
          do (case ch
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char ch out))))
    (write-char #\" out)))

(defun %php-json-decode (str &optional assoc depth flags)
  "PHP json_decode: parse JSON STR to a PHP value.  Objects decode to PHP
associative arrays.  (PHP's default object/stdClass mode is not modelled, so the
ASSOC argument is effectively always on — array access $d['k'] works for decoded
objects; $d->k does not.)  Returns null on malformed input."
  (declare (ignore assoc depth flags))
  (handler-case
      (multiple-value-bind (val pos) (%php-json-parse-value (%php-stringify str) 0)
        (declare (ignore pos))
        val)
    (error () +php-null+)))

(defun %php-json-skip-ws (s pos)
  (loop while (and (< pos (length s))
                   (member (char s pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

;;; The JSON parser threads the cursor: every %php-json-parse-* returns
;;; (values value next-pos).  (The previous version returned only the value and
;;; faked the cursor with (+ pos 1) — correct only for single-character values,
;;; so objects, nested structures, and multi-digit numbers all broke; and
;;; parse-string returned an empty fresh stream, so every string decoded to "".)
(defun %php-json-parse-value (s pos)
  "Parse one JSON value at POS. Return (values value next-pos)."
  (setf pos (%php-json-skip-ws s pos))
  (when (>= pos (length s)) (return-from %php-json-parse-value (values +php-null+ pos)))
  (let ((ch (char s pos)))
    (cond
      ((char= ch #\") (%php-json-parse-string s (1+ pos)))
      ((char= ch #\{) (%php-json-parse-object s (1+ pos)))
      ((char= ch #\[) (%php-json-parse-array s (1+ pos)))
      ((and (<= (+ pos 4) (length s)) (string= s "null" :start1 pos :end1 (+ pos 4))) (values +php-null+ (+ pos 4)))
      ((and (<= (+ pos 4) (length s)) (string= s "true" :start1 pos :end1 (+ pos 4))) (values t (+ pos 4)))
      ((and (<= (+ pos 5) (length s)) (string= s "false" :start1 pos :end1 (+ pos 5))) (values nil (+ pos 5)))
      ((or (digit-char-p ch) (char= ch #\-))
       (let ((end pos))
         (when (char= ch #\-) (incf end))
         (loop while (and (< end (length s))
                          (or (digit-char-p (char s end)) (member (char s end) '(#\. #\e #\E #\+ #\-))))
               do (incf end))
         (values (handler-case (let ((*read-default-float-format* 'double-float))
                                 (read-from-string (subseq s pos end)))
                   (error () 0))
                 end)))
      (t (values +php-null+ (1+ pos))))))

(defun %php-json-parse-string (s pos)
  "POS is just after the opening quote. Return (values string next-pos)."
  (let ((buf (make-string-output-stream)))
    (loop while (and (< pos (length s)) (not (char= (char s pos) #\")))
          do (let ((ch (char s pos)))
               (incf pos)
               (if (char= ch #\\)
                   (let ((esc (char s pos)))
                     (incf pos)
                     (case esc
                       (#\" (write-char #\" buf))
                       (#\\ (write-char #\\ buf))
                       (#\/ (write-char #\/ buf))
                       (#\n (write-char #\Newline buf))
                       (#\r (write-char #\Return buf))
                       (#\t (write-char #\Tab buf))
                       (#\b (write-char #\Backspace buf))
                       (#\f (write-char #\Page buf))
                       (#\u (let ((code (parse-integer s :start pos :end (min (length s) (+ pos 4)) :radix 16)))
                              (incf pos 4)
                              (write-char (code-char code) buf)))
                       (t (write-char esc buf))))
                   (write-char ch buf))))
    ;; Return the collected string and skip the closing quote.
    (values (get-output-stream-string buf) (1+ pos))))

(defun %php-json-parse-object (s pos)
  "POS is just after the opening brace. Return (values php-array next-pos)."
  (let ((result (%php-make-array)))
    (setf pos (%php-json-skip-ws s pos))
    (when (and (< pos (length s)) (char= (char s pos) #\}))
      (return-from %php-json-parse-object (values result (1+ pos))))
    (loop
      (setf pos (%php-json-skip-ws s pos))
      (unless (and (< pos (length s)) (char= (char s pos) #\"))
        (return (values result pos)))
      (multiple-value-bind (key kpos) (%php-json-parse-string s (1+ pos))
        (setf pos (%php-json-skip-ws s kpos))
        (when (and (< pos (length s)) (char= (char s pos) #\:)) (incf pos))
        (multiple-value-bind (val vpos) (%php-json-parse-value s pos)
          (%php-array-set result key val)
          (setf pos (%php-json-skip-ws s vpos))
          (cond ((and (< pos (length s)) (char= (char s pos) #\,)) (incf pos))
                ((and (< pos (length s)) (char= (char s pos) #\})) (return (values result (1+ pos))))
                (t (return (values result pos)))))))))

(defun %php-json-parse-array (s pos)
  "POS is just after the opening bracket. Return (values php-array next-pos)."
  (let ((result (%php-make-array)))
    (setf pos (%php-json-skip-ws s pos))
    (when (and (< pos (length s)) (char= (char s pos) #\]))
      (return-from %php-json-parse-array (values result (1+ pos))))
    (loop for i from 0
          do (multiple-value-bind (val vpos) (%php-json-parse-value s pos)
               (%php-array-set result i val)
               (setf pos (%php-json-skip-ws s vpos))
               (cond ((and (< pos (length s)) (char= (char s pos) #\,)) (incf pos))
                     ((and (< pos (length s)) (char= (char s pos) #\])) (return (values result (1+ pos))))
                     (t (return (values result pos))))))))

;;; ─── JSON validation (PHP 8.3) ───────────────────────────────────────────────

(defun %php-json-validate (json &optional (depth 512) (flags 0))
  "PHP 8.3 json_validate: check if string is valid JSON."
  (declare (ignore depth flags))
  (handler-case
      (progn (%php-json-decode json) t)
    (error () nil)))
