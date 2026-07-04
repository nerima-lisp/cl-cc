;;;; packages/php/src/runtime-builtins-string-encoding.lisp -- HTML and byte encoding PHP string operations

(in-package :cl-cc/php)

;;; ─── HTML string functions ────────────────────────────────────────────────────

(defun %php-nl2br (str &optional (is-xhtml t))
  "PHP nl2br: insert <br> before newlines."
  (let ((br (if (%php-truthy is-xhtml) "<br />" "<br>")))
    (with-output-to-string (out)
      (loop for ch across (%php-stringify str)
            do (when (char= ch #\Newline)
                 (write-string br out))
               (write-char ch out)))))

(defun %php-wordwrap (str &optional (width 75) (break-str "
") (cut-long-words nil))
  "PHP wordwrap: wrap STRING to lines of at most WIDTH characters, breaking on
spaces.  When CUT-LONG-WORDS is true, a single word longer than WIDTH is
force-broken into WIDTH-sized pieces (otherwise it overflows its own line)."
  (let ((s (%php-stringify str))
        (w (or width 75))
        (brk (%php-stringify break-str)))
    (with-output-to-string (out)
      (let ((line-len 0)
            (current-word (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
        (labels ((emit-break () (write-string brk out) (setf line-len 0))
                 (flush-word ()
                   (let ((wlen (length current-word)))
                     (when (> wlen 0)
                       (cond
                         ;; Fits on the current line (with a separating space).
                         ((<= (+ line-len (if (> line-len 0) 1 0) wlen) w)
                          (when (> line-len 0) (write-char #\Space out) (incf line-len))
                          (write-string current-word out)
                          (incf line-len wlen))
                         ;; Doesn't fit, but no cut (or word fits a fresh line):
                         ;; move it to a new line as a unit (overflowing if needed).
                         ((or (not (%php-truthy cut-long-words)) (<= wlen w))
                          (when (> line-len 0) (emit-break))
                          (write-string current-word out)
                          (setf line-len wlen))
                         ;; Cut: force-break the over-long word into WIDTH pieces.
                         (t
                          (when (> line-len 0) (emit-break))
                          (let ((start 0))
                            (loop
                              (let ((take (min w (- wlen start))))
                                (write-string (subseq current-word start (+ start take)) out)
                                (setf line-len take)
                                (incf start take)
                                (when (>= start wlen) (return))
                                (emit-break))))))
                       (setf (fill-pointer current-word) 0)))))
          (loop for ch across s
                do (if (char= ch #\Space)
                       (flush-word)
                       (vector-push-extend ch current-word)))
          (flush-word))))))

(defun %php-chunk-split (str &optional (chunklen 76) (end "\r\n"))
  "PHP chunk_split: split string into chunks."
  (let* ((s (%php-stringify str))
         (e (%php-stringify end))
         (n (length s))
         (cl (or chunklen 76)))
    (with-output-to-string (out)
      (loop for i from 0 below n by cl
            do (write-string (subseq s i (min n (+ i cl))) out)
               (write-string e out)))))

(defun %php-html-entity-decode (str &optional flags encoding)
  (declare (ignore flags encoding))
  (let ((s (%php-stringify str)))
    (loop for (ent . ch) in '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                               ("&quot;" . "\"") ("&#039;" . "'") ("&nbsp;" . " "))
          do (setf s (cl:with-output-to-string (out)
                       (let ((pos 0))
                         (loop (let ((found (search ent s :start2 pos)))
                                 (if found
                                     (progn (write-string s out :start pos :end found)
                                            (write-string ch out)
                                            (setf pos (+ found (length ent))))
                                     (progn (write-string s out :start pos) (return)))))))))
    s))

(defun %php-strip-tags (str &optional allowed-tags)
  "PHP strip_tags: remove HTML/PHP tags."
  (declare (ignore allowed-tags))
  (with-output-to-string (out)
    (let ((in-tag nil))
      (loop for ch across (%php-stringify str)
            do (cond ((char= ch #\<) (setf in-tag t))
                     ((char= ch #\>) (setf in-tag nil))
                     ((not in-tag) (write-char ch out)))))))

(defun %php-addslashes (str)
  "PHP addslashes: escape quotes and backslashes."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (when (member ch '(#\\ #\' #\" #\Null))
               (write-char #\\ out))
             (write-char ch out))))

(defun %php-stripslashes (str)
  "PHP stripslashes: remove backslash escapes."
  (with-output-to-string (out)
    (let ((s (%php-stringify str))
          (i 0))
      (loop while (< i (length s))
            do (let ((ch (char s i)))
                 (if (char= ch #\\)
                     (progn (incf i)
                            (when (< i (length s)) (write-char (char s i) out)))
                     (write-char ch out))
                 (incf i))))))

(defun %php-base64-encode (str)
  "PHP base64_encode: base64 encode string."
  ;; Simple base64 implementation
  (let* ((s (%php-stringify str))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (let ((bytes (map 'vector #'char-code s))
            (n (length s)))
        (loop for i from 0 below n by 3
              do (let* ((b0 (aref bytes i))
                        (b1 (if (< (1+ i) n) (aref bytes (1+ i)) 0))
                        (b2 (if (< (+ i 2) n) (aref bytes (+ i 2)) 0))
                        (g0 (ash b0 -2))
                        (g1 (logior (ash (logand b0 3) 4) (ash b1 -4)))
                        (g2 (logior (ash (logand b1 15) 2) (ash b2 -6)))
                        (g3 (logand b2 63)))
                   (write-char (char alphabet g0) out)
                   (write-char (char alphabet g1) out)
                   (write-char (if (< (+ i 1) n) (char alphabet g2) #\=) out)
                   (write-char (if (< (+ i 2) n) (char alphabet g3) #\=) out)))))))

(defun %php-base64-decode (str &optional strict)
  "PHP base64_decode: decode base64 string."
  (declare (ignore strict))
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) (%php-stringify str)))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (flet ((decode-char (ch)
             (let ((pos (position ch alphabet)))
               (or pos 0))))
      (with-output-to-string (out)
        (loop for i from 0 below (length s) by 4
              while (< (+ i 3) (length s))
              do (let* ((g0 (decode-char (char s i)))
                        (g1 (decode-char (char s (1+ i))))
                        (g2 (if (char= (char s (+ i 2)) #\=) 0 (decode-char (char s (+ i 2)))))
                        (g3 (if (char= (char s (+ i 3)) #\=) 0 (decode-char (char s (+ i 3)))))
                        (b0 (logior (ash g0 2) (ash g1 -4)))
                        (b1 (logior (ash (logand g1 15) 4) (ash g2 -2)))
                        (b2 (logior (ash (logand g2 3) 6) g3)))
                   (write-char (code-char b0) out)
                   (when (not (char= (char s (+ i 2)) #\=))
                     (write-char (code-char b1) out))
                   (when (not (char= (char s (+ i 3)) #\=))
                     (write-char (code-char b2) out))))))))

(defun %php-string-bytes (value)
  "Return VALUE as a vector of PHP string bytes."
  (let* ((string (%php-stringify value))
         (bytes (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length string)
          do (setf (aref bytes i) (logand (char-code (char string i)) #xff)))
    bytes))

(defun %php-byte-vector-hex (bytes)
  (let ((alphabet "0123456789abcdef"))
    (with-output-to-string (out)
      (loop for byte across bytes
            for value = (logand byte #xff)
            do (write-char (char alphabet (ash value -4)) out)
               (write-char (char alphabet (logand value #x0f)) out)))))

(defun %php-urlencode (str)
  "PHP urlencode: percent-encode a string."
  (with-output-to-string (out)
    (loop for ch across (%php-stringify str)
          do (cond ((char= ch #\Space) (write-char #\+ out))
                   ((or (alphanumericp ch) (member ch '(#\- #\_ #\. #\~)))
                    (write-char ch out))
                   (t (format out "%~2,'0X" (char-code ch)))))))

(defun %php-urldecode (str)
  "PHP urldecode: decode percent-encoded string."
  (with-output-to-string (out)
    (let ((s (%php-stringify str))
          (i 0))
      (loop while (< i (length s))
            do (let ((ch (char s i)))
                 (cond ((char= ch #\+) (write-char #\Space out) (incf i))
                       ((and (char= ch #\%) (< (+ i 2) (length s)))
                        (write-char (code-char (parse-integer (subseq s (1+ i) (+ i 3)) :radix 16))
                                    out)
                        (incf i 3))
                       (t (write-char ch out) (incf i))))))))

(defun %php-rawurlencode (str) (%php-urlencode str))
(defun %php-rawurldecode (str) (%php-urldecode str))


;;; ─── htmlspecialchars / html_entity_decode additions ────────────────────────

(defun %php-htmlspecialchars-decode (str &optional (flags 11))
  "PHP htmlspecialchars_decode: convert HTML entities to characters."
  (declare (ignore flags))
  (let ((s (%php-stringify str)))
    (cl:with-output-to-string (out)
      (let ((i 0) (n (length s)))
        (loop while (< i n)
              do (cond
                   ((and (< (+ i 3) n) (string= (subseq s i (+ i 4)) "&lt;"))
                    (write-char #\< out) (incf i 4))
                   ((and (< (+ i 3) n) (string= (subseq s i (+ i 4)) "&gt;"))
                    (write-char #\> out) (incf i 4))
                   ((and (< (+ i 4) n) (string= (subseq s i (+ i 5)) "&amp;"))
                    (write-char #\& out) (incf i 5))
                   ((and (< (+ i 5) n) (string= (subseq s i (+ i 6)) "&quot;"))
                    (write-char #\" out) (incf i 6))
                   ((and (< (+ i 5) n) (string= (subseq s i (+ i 6)) "&#039;"))
                    (write-char #\' out) (incf i 6))
                   (t
                    (write-char (char s i) out) (incf i))))))))
