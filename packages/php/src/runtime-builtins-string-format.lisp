;;;; packages/php/src/runtime-builtins-string-format.lisp -- Formatting and escaping PHP string operations

(in-package :cl-cc/php)

(defun %php-sprintf (format-string &rest args)
  "Return a sprintf result supporting %s %d %f %e %o %x %X %b %c %u %% and width/precision."
  ;; (%php-sprintf "Hi %s" "PHP") => "Hi PHP"
  ;; (%php-sprintf "%05d" 42) => "00042"
  (let ((fmt (%php-stringify format-string))
        (remaining args)
        (flen 0))
    (setf flen (length fmt))
    (with-output-to-string (stream)
      (let ((i 0))
        (loop while (< i flen)
              do (let ((ch (char fmt i)))
                   (incf i)
                   (if (char= ch #\%)
                       (progn
                         ;; parse optional argument position: %1$s
                         (let* ((arg-pos nil)
                                (flags "")
                                (custom-pad nil)
                                (width nil)
                                (precision nil)
                                (j i))
                           ;; check for N$ argument position
                           (when (and (< j flen) (digit-char-p (char fmt j)))
                             (let ((numstart j))
                               (loop while (and (< j flen) (digit-char-p (char fmt j))) do (incf j))
                               (when (and (< j flen) (char= (char fmt j) #\$))
                                 (setf arg-pos (1- (parse-integer fmt :start numstart :end j)))
                                 (incf j)
                                 (setf i j))))
                           ;; flags  ('X sets a custom pad character)
                           (loop while (< i flen)
                                 do (let ((fc (char fmt i)))
                                      (cond
                                        ((char= fc #\')
                                         (incf i)
                                         (when (< i flen)
                                           (setf custom-pad (char fmt i))
                                           (incf i)))
                                        ((member fc '(#\- #\+ #\Space #\0))
                                         (setf flags (concatenate 'string flags (string fc)))
                                         (incf i))
                                        (t (return)))))
                           ;; width
                           (when (and (< i flen) (digit-char-p (char fmt i)))
                             (let ((start i))
                               (loop while (and (< i flen) (digit-char-p (char fmt i))) do (incf i))
                               (setf width (parse-integer fmt :start start :end i))))
                           ;; precision
                           (when (and (< i flen) (char= (char fmt i) #\.))
                             (incf i)
                             (let ((start i))
                               (loop while (and (< i flen) (digit-char-p (char fmt i))) do (incf i))
                               (setf precision (if (= start i) 0 (parse-integer fmt :start start :end i)))))
                           ;; directive
                           (when (< i flen)
                             (let* ((dir (char fmt i))
                                    (val (if arg-pos
                                             (nth arg-pos args)
                                             (pop remaining)))
                                    (left-align (position #\- flags))
                                    (zero-pad  (and (not left-align) (position #\0 flags)))
                                    (pad-char (cond (custom-pad custom-pad) (zero-pad #\0) (t #\Space))))
                               (incf i)
                               (flet ((emit (body &optional (sign ""))
                                        ;; BODY = formatted magnitude (no sign); SIGN = ""/"+"/"-"/" ".
                                        (let* ((full (concatenate 'string sign body))
                                               (pad (if width (max 0 (- width (length full))) 0)))
                                          (cond
                                            (left-align
                                             (write-string full stream)
                                             (dotimes (k pad) (write-char #\Space stream)))
                                            ;; numeric padding goes AFTER the sign
                                            ((and (plusp (length sign)) (or zero-pad custom-pad))
                                             (write-string sign stream)
                                             (dotimes (k pad) (write-char pad-char stream))
                                             (write-string body stream))
                                            (t
                                             (dotimes (k pad) (write-char pad-char stream))
                                             (write-string full stream)))))
                                      (num-sign (n)
                                        (cond ((minusp n) "-")
                                              ((find #\+ flags) "+")
                                              ((find #\Space flags) " ")
                                              (t ""))))
                               (cond
                                 ((char= dir #\%)
                                  (write-char #\% stream))
                                 ((char= dir #\s)
                                  (let* ((sv (%php-stringify val))
                                         (sv (if precision (subseq sv 0 (min precision (length sv))) sv))
                                         (pad (if width (max 0 (- width (length sv))) 0)))
                                    (if left-align
                                        (progn (write-string sv stream)
                                               (dotimes (k pad) (write-char #\Space stream)))
                                        (progn (dotimes (k pad) (write-char (or custom-pad #\Space) stream))
                                               (write-string sv stream)))))
                                 ((member dir '(#\d #\i))
                                  (let ((n (if (numberp val) (truncate val) 0)))
                                    (emit (format nil "~D" (abs n)) (num-sign n))))
                                 ((char= dir #\f)
                                  (let ((x (coerce (or val 0) 'double-float)))
                                    (emit (format nil "~,vF" (or precision 6) (abs x)) (num-sign x))))
                                 ((member dir '(#\e #\E))
                                  (let* ((x (coerce (or val 0) 'double-float))
                                         (raw (format nil "~,vE" (or precision 6) (abs x)))
                                         (marker (if (char= dir #\E) #\E #\e))
                                         ;; CL prints the double-float exponent marker as d/D; PHP uses e/E.
                                         (sv (substitute marker #\D (substitute marker #\d raw))))
                                    (emit sv (num-sign x))))
                                 ((char= dir #\o)
                                  (write-string (format nil "~O" (truncate (or val 0))) stream))
                                 ((char= dir #\x)
                                  (write-string (string-downcase (format nil "~X" (truncate (or val 0)))) stream))
                                 ((char= dir #\X)
                                  (write-string (format nil "~X" (truncate (or val 0))) stream))
                                 ((char= dir #\b)
                                  (write-string (format nil "~B" (truncate (or val 0))) stream))
                                 ((char= dir #\c)
                                  (write-char (code-char (truncate (or val 0))) stream))
                                 ((char= dir #\u)
                                  (let ((n (truncate (or val 0))))
                                    (write-string (format nil "~D" (if (minusp n) (+ n 4294967296) n)) stream)))
                                 (t
                                  (write-char #\% stream)
                                  (write-char dir stream))))))))
                       (write-char ch stream))))))))


(defun %php-htmlspecialchars (string)
  "Escape HTML special characters in STRING."
  ;; (%php-htmlspecialchars "<a&b>") => "&lt;a&amp;b&gt;"
  (let ((text (%php-stringify string)))
    (with-output-to-string (stream)
      (loop for ch across text
            do (case ch
                 (#\& (write-string "&amp;" stream))
                 (#\< (write-string "&lt;" stream))
                 (#\> (write-string "&gt;" stream))
                 (#\" (write-string "&quot;" stream))
                 (#\' (write-string "&#039;" stream))
                 (otherwise (write-char ch stream)))))))
