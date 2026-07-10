;;;; packages/javascript/src/runtime-builtins-intl-number-format.lisp -- Intl.NumberFormat

(in-package :cl-cc/javascript)

(defun %js-number-format-option-integer (options name default)
  (let ((value (%js-intl-option options name +js-undefined+)))
    (if (eq value +js-undefined+)
        default
        (max 0 (truncate (%js-to-number value))))))

(defun %js-number-format-group-integer (digits)
  (let* ((negative-p (and (plusp (length digits)) (char= (char digits 0) #\-)))
         (body (if negative-p (subseq digits 1) digits))
         (len (length body)))
    (with-output-to-string (out)
      (when negative-p (write-char #\- out))
      (loop for i below len
            do (when (and (plusp i) (zerop (mod (- len i) 3)))
                 (write-char #\, out))
               (write-char (char body i) out)))))

(defun %js-number-format-compose (value options)
  (let* ((style (%js-to-string (%js-intl-option options "style" "decimal")))
         (use-grouping (not (null (%js-intl-option options "useGrouping" t))))
         (min-frac (%js-number-format-option-integer options "minimumFractionDigits" 0))
         (max-default (max 3 min-frac))
         (max-frac (max min-frac
                        (%js-number-format-option-integer
                         options "maximumFractionDigits" max-default)))
         (scaled-value (if (string= style "percent")
                           (* (%js-to-number value) 100.0d0)
                           (%js-to-number value)))
         (negative-p (minusp scaled-value))
         (abs-value (abs scaled-value))
         (scale (expt 10 max-frac))
         (rounded-units (floor (+ (* abs-value scale) 0.5d0)))
         (integer-part (floor rounded-units scale))
         (fraction-number (mod rounded-units scale))
         (integer-string (format nil "~D" integer-part))
         (fraction-string (if (zerop max-frac)
                              ""
                              (format nil "~v,'0D" max-frac fraction-number))))
    (loop while (and (> (length fraction-string) min-frac)
                     (char= (char fraction-string (1- (length fraction-string))) #\0))
          do (setf fraction-string
                   (subseq fraction-string 0 (1- (length fraction-string)))))
    (let ((signed-integer (if negative-p
                              (concatenate 'string "-" integer-string)
                              integer-string)))
      (values (with-output-to-string (out)
                (write-string
                 (if use-grouping
                     (%js-number-format-group-integer signed-integer)
                     signed-integer)
                 out)
                (when (plusp (length fraction-string))
                  (write-char #\. out)
                  (write-string fraction-string out))
                (when (string= style "percent")
                  (write-char #\% out)))
              signed-integer
              fraction-string
              style))))

(defun %js-make-intl-number-format (&optional _locale options)
  "Intl.NumberFormat(locale?, options?): format with basic decimal options."
  (declare (ignore _locale))
  (%js-make-object
   "__call__" (lambda (&rest _) (declare (ignore _)) +js-undefined+)
   "format"   (lambda (n)
                (multiple-value-bind (formatted)
                    (%js-number-format-compose n options)
                  formatted))
   "formatToParts" (lambda (n)
                     (multiple-value-bind (formatted integer-string fraction-string style)
                         (%js-number-format-compose n options)
                       (declare (ignore formatted))
                       (let ((parts (list (%js-make-object "type" "integer" "value"
                                                           integer-string))))
                         (when (plusp (length fraction-string))
                           (setf parts (append parts
                                               (list (%js-make-object "type" "decimal" "value" ".")
                                                     (%js-make-object "type" "fraction" "value"
                                                                      fraction-string)))))
                         (when (string= style "percent")
                           (setf parts (append parts
                                               (list (%js-make-object "type" "percentSign"
                                                                      "value" "%")))))
                         (apply #'%js-make-array parts))))))
