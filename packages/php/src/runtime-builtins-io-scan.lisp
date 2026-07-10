;;;; PHP scanf and sscanf runtime builtins.

(in-package #:cl-cc/php)

(defun %php-scan-whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %php-sscanf-digit-value (ch)
  (cond ((and (char>= ch #\0) (char<= ch #\9))
         (- (char-code ch) (char-code #\0)))
        ((and (char>= ch #\a) (char<= ch #\f))
         (+ 10 (- (char-code ch) (char-code #\a))))
        ((and (char>= ch #\A) (char<= ch #\F))
         (+ 10 (- (char-code ch) (char-code #\A))))
        (t nil)))

(defun %php-sscanf-skip-ws (s pos)
  (loop while (and (< pos (length s))
                   (%php-scan-whitespace-char-p (char s pos)))
        do (incf pos)
        finally (return pos)))

(defun %php-sscanf-scan-int (s pos width radix &key auto-radix unsigned)
  (let* ((start-limit pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (sign 1)
         (base radix))
    (when (and (< pos end-limit)
               (not unsigned)
               (member (char s pos) '(#\+ #\-) :test #'char=))
      (when (char= (char s pos) #\-) (setf sign -1))
      (incf pos))
    (when auto-radix
      (cond
        ((and (<= (+ pos 2) end-limit)
              (char= (char s pos) #\0)
              (member (char s (1+ pos)) '(#\x #\X) :test #'char=))
         (setf base 16)
         (incf pos 2))
        ((and (< pos end-limit) (char= (char s pos) #\0))
         (setf base 8))
        (t (setf base 10))))
    (let ((digits-start pos)
          (value 0))
      (loop while (< pos end-limit)
            for digit = (%php-sscanf-digit-value (char s pos))
            while (and digit (< digit base))
            do (setf value (+ (* value base) digit))
               (incf pos))
      (if (= pos digits-start)
          (values nil start-limit nil)
          (values (* sign value) pos t)))))

(defun %php-sscanf-scan-float (s pos width)
  (let* ((start pos)
         (end-limit (if width (min (length s) (+ pos width)) (length s)))
         (saw-digit nil))
    (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
      (incf pos))
    (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
          do (setf saw-digit t)
             (incf pos))
    (when (and (< pos end-limit) (char= (char s pos) #\.))
      (incf pos)
      (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
            do (setf saw-digit t)
               (incf pos)))
    (when (and saw-digit (< pos end-limit) (member (char s pos) '(#\e #\E) :test #'char=))
      (let ((exp-start pos)
            (exp-digits nil))
        (incf pos)
        (when (and (< pos end-limit) (member (char s pos) '(#\+ #\-) :test #'char=))
          (incf pos))
        (loop while (and (< pos end-limit) (digit-char-p (char s pos)))
              do (setf exp-digits t)
                 (incf pos))
        (unless exp-digits
          (setf pos exp-start))))
    (if saw-digit
        (handler-case
            (let ((*read-default-float-format* 'double-float))
              (values (coerce (read-from-string (subseq s start pos)) 'double-float)
                      pos t))
          (error () (values nil start nil)))
        (values nil start nil))))

(defun %php-sscanf-values (str fmt)
  "Return sscanf parsed values as a PHP array."
  (let* ((input (%php-stringify str))
         (format-string (%php-stringify fmt))
         (i 0)
         (j 0)
         (values nil)
         (failed nil))
    (labels ((emit (value)
               (push value values))
             (parse-width ()
               (let ((start j))
                 (loop while (and (< j (length format-string))
                                  (digit-char-p (char format-string j)))
                       do (incf j))
                 (when (< start j)
                   (parse-integer format-string :start start :end j)))))
      (loop while (and (< j (length format-string)) (not failed))
            for fch = (char format-string j)
            do (cond
                 ((%php-scan-whitespace-char-p fch)
                  (setf i (%php-sscanf-skip-ws input i))
                  (loop while (and (< j (length format-string))
                                   (%php-scan-whitespace-char-p
                                    (char format-string j)))
                        do (incf j)))
                 ((char/= fch #\%)
                  (if (and (< i (length input)) (char= (char input i) fch))
                      (progn (incf i) (incf j))
                      (setf failed t)))
                 (t
                  (incf j)
                  (cond
                    ((>= j (length format-string))
                     (setf failed t))
                    ((char= (char format-string j) #\%)
                     (if (and (< i (length input)) (char= (char input i) #\%))
                         (progn (incf i) (incf j))
                         (setf failed t)))
                    (t
                     (let ((suppress nil))
                       (when (and (< j (length format-string))
                                  (char= (char format-string j) #\*))
                         (setf suppress t)
                         (incf j))
                       (let ((width (parse-width)))
                         (when (>= j (length format-string))
                           (setf failed t))
                         (unless failed
                           (let ((spec (char-downcase (char format-string j))))
                             (incf j)
                             (multiple-value-bind (value new-pos ok)
                                 (case spec
                                   ((#\d #\u)
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :unsigned (char= spec #\u)))
                                   (#\i
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 10
                                     :auto-radix t))
                                   (#\x
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 16
                                     :unsigned t))
                                   (#\o
                                    (%php-sscanf-scan-int
                                     input (%php-sscanf-skip-ws input i) width 8
                                     :unsigned t))
                                   ((#\f #\e #\g)
                                    (%php-sscanf-scan-float
                                     input (%php-sscanf-skip-ws input i) width))
                                   (#\s
                                    (let* ((start (%php-sscanf-skip-ws input i))
                                           (end-limit (if width
                                                          (min (length input)
                                                               (+ start width))
                                                          (length input)))
                                           (end start))
                                      (loop while (and (< end end-limit)
                                                       (not (%php-scan-whitespace-char-p
                                                             (char input end))))
                                            do (incf end))
                                      (if (= start end)
                                          (values nil i nil)
                                          (values (subseq input start end) end t))))
                                   (#\c
                                    (let* ((count (or width 1))
                                           (end (+ i count)))
                                      (if (<= end (length input))
                                          (values (subseq input i end) end t)
                                          (values nil i nil))))
                                   (otherwise
                                    (values nil i nil)))
                               (if ok
                                   (progn
                                     (setf i new-pos)
                                     (unless suppress (emit value)))
                                   (setf failed t))))))))))))
      (%php-list-to-array (nreverse values)))))

(defun %php-sscanf (str fmt &rest ignored)
  "PHP sscanf: parse STR with FMT. Without out-params this returns a PHP array;
the parser lowers out-param calls through `%php-sscanf-values` and returns the
number of assigned values."
  (let ((values (%php-sscanf-values str fmt)))
    (if ignored (%php-count values) values)))

(defun %php-read-standard-input-string ()
  "Read the remaining dynamically bound standard input as a string."
  (with-output-to-string (out)
    (loop for ch = (read-char *standard-input* nil nil)
          while ch
          do (write-char ch out))))

(defun %php-scanf-values (fmt)
  "Return scanf parsed values from standard input as a PHP array."
  (%php-sscanf-values (%php-read-standard-input-string) fmt))

(defun %php-scanf (fmt &rest ignored)
  "PHP scanf: parse the remaining standard input with FMT."
  (let ((values (%php-scanf-values fmt)))
    (if ignored (%php-count values) values)))
