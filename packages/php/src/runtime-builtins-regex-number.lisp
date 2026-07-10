;;;; packages/php/src/runtime-builtins-regex-number.lisp -- PHP numeric formatting builtins

(in-package :cl-cc/php)

(defun %php-round-half-up (x)
  "Round non-negative double X to the nearest integer with halves rounding UP —
PHP's round-half-away-from-zero applied to a magnitude.  CL ROUND uses banker's
rounding (half-to-even), so number_format(2.5) gave 2 and number_format(1234.5)
gave 1,234 instead of PHP's 3 and 1,235."
  (values (floor (+ x 0.5d0))))

(defun %php-number-format (num &optional (decimals 0) (dec-point ".") (thousands-sep ","))
  "PHP number_format — format a number with decimal and thousands separators."
  (let* ((n (coerce (if (numberp num) num 0) 'double-float))
         (dp (if (and dec-point (not (%php-null-p dec-point))) (%php-stringify dec-point) "."))
         (ts (if (and thousands-sep (not (%php-null-p thousands-sep))) (%php-stringify thousands-sep) ","))
         (abs-n (abs n))
         (rounded (if (> decimals 0)
                      (/ (%php-round-half-up (* abs-n (expt 10 decimals))) (expt 10 decimals))
                      (%php-round-half-up abs-n)))
         (int-part (truncate rounded))
         (frac-part (- rounded int-part))
         ;; Format integer part with thousands separators
         (int-str (format nil "~D" int-part))
         (int-with-sep (with-output-to-string (out)
                         (let ((len (length int-str)))
                           (loop for i from 0 below len
                                 do (write-string int-str out :start i :end (1+ i))
                                    (let ((remaining (- len (1+ i))))
                                      (when (and (> (length ts) 0)
                                                 (> remaining 0)
                                                 (zerop (mod remaining 3)))
                                        (write-string ts out)))))))
         (result (if (> decimals 0)
                     (format nil "~A~A~v,'0D"
                             int-with-sep dp decimals
                             (truncate (* (abs frac-part) (expt 10 decimals))))
                     int-with-sep)))
    (if (< n 0) (concatenate 'string "-" result) result)))
