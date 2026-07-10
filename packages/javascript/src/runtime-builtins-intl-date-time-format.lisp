;;;; packages/javascript/src/runtime-builtins-intl-date-time-format.lisp -- Intl.DateTimeFormat

(in-package :cl-cc/javascript)

(defparameter +js-date-time-format-month-long-names+
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun %js-date-time-format-option-string (options name default)
  (let ((value (%js-intl-option options name +js-undefined+)))
    (if (eq value +js-undefined+)
        default
        (%js-to-string value))))

(defun %js-date-time-format-input-date (value)
  (cond
    ((js-date-p value) value)
    ((or (numberp value) (stringp value)) (%js-make-date value))
    ((or (eq value +js-undefined+) (eq value +js-null+)) (%js-make-date))
    (t (%js-make-date value))))

(defun %js-date-time-format-two-digit (value)
  (format nil "~2,'0D" (mod (truncate value) 100)))

(defun %js-date-time-format-numeric (value style)
  (if (string= style "2-digit")
      (%js-date-time-format-two-digit value)
      (format nil "~D" value)))

(defun %js-date-time-format-month (month style)
  (cond
    ((string= style "2-digit") (%js-date-time-format-two-digit month))
    ((string= style "short") (aref +js-date-month-names+ (1- month)))
    ((string= style "long") (aref +js-date-time-format-month-long-names+ (1- month)))
    ((string= style "narrow")
     (subseq (aref +js-date-time-format-month-long-names+ (1- month)) 0 1))
    (t (format nil "~D" month))))

(defun %js-date-time-format-style-date (date-style)
  (cond
    ((member date-style '("full" "long") :test #'string=)
     (values "numeric" "long" "numeric"))
    ((string= date-style "medium")
     (values "numeric" "short" "numeric"))
    ((string= date-style "short")
     (values "2-digit" "numeric" "numeric"))
    (t (values nil nil nil))))

(defun %js-date-time-format-style-time (time-style)
  (cond
    ((member time-style '("full" "long" "medium") :test #'string=)
     (values "numeric" "2-digit" "2-digit"))
    ((string= time-style "short")
     (values "numeric" "2-digit" nil))
    (t (values nil nil nil))))

(defun %js-date-time-format-parts (date year-style month-style day-style
                                        hour-style minute-style second-style hour12)
  (%with-date-fields (date :sec sec :min min :hour hour :day day :month month :year year)
    (let ((parts nil)
          (date-p (or year-style month-style day-style))
          (time-p (or hour-style minute-style second-style)))
      (labels ((add (type value)
                 (push (%js-make-object "type" type "value" value) parts))
               (literal (value)
                 (add "literal" value)))
        (when month-style
          (add "month" (%js-date-time-format-month month month-style)))
        (when (and month-style day-style)
          (literal "/"))
        (when day-style
          (add "day" (%js-date-time-format-numeric day day-style)))
        (when (and (or month-style day-style) year-style)
          (literal "/"))
        (when year-style
          (add "year"
               (if (string= year-style "2-digit")
                   (%js-date-time-format-two-digit year)
                   (format nil "~D" year))))
        (when (and date-p time-p)
          (literal ", "))
        (when hour-style
          (let ((display-hour (if hour12
                                  (let ((h (mod hour 12)))
                                    (if (zerop h) 12 h))
                                  hour)))
            (add "hour" (%js-date-time-format-numeric display-hour hour-style))))
        (when (and hour-style minute-style)
          (literal ":"))
        (when minute-style
          (add "minute" (%js-date-time-format-numeric min minute-style)))
        (when (and (or hour-style minute-style) second-style)
          (literal ":"))
        (when second-style
          (add "second" (%js-date-time-format-numeric sec second-style)))
        (when (and hour12 hour-style)
          (literal " ")
          (add "dayPeriod" (if (< hour 12) "AM" "PM")))
        (nreverse parts)))))

(defun %js-date-time-format-render (parts)
  (with-output-to-string (out)
    (loop for part in parts
          do (write-string (gethash "value" part) out))))

(defun %js-date-time-format-resolved-options (date-style time-style year-style
                                                        month-style day-style
                                                        hour-style minute-style
                                                        second-style hour12)
  (%js-make-object
   "locale" "en-US"
   "calendar" "gregory"
   "numberingSystem" "latn"
   "timeZone" "UTC"
   "dateStyle" (or date-style +js-undefined+)
   "timeStyle" (or time-style +js-undefined+)
   "year" (or year-style +js-undefined+)
   "month" (or month-style +js-undefined+)
   "day" (or day-style +js-undefined+)
   "hour" (or hour-style +js-undefined+)
   "minute" (or minute-style +js-undefined+)
   "second" (or second-style +js-undefined+)
   "hour12" hour12))

(defun %js-make-intl-date-time-format (&optional _locale options)
  "Intl.DateTimeFormat(locale?, options?): deterministic UTC English formatter."
  (declare (ignore _locale))
  (let* ((date-style (%js-date-time-format-option-string options "dateStyle" nil))
         (time-style (%js-date-time-format-option-string options "timeStyle" nil))
         (year-style (%js-date-time-format-option-string options "year" nil))
         (month-style (%js-date-time-format-option-string options "month" nil))
         (day-style (%js-date-time-format-option-string options "day" nil))
         (hour-style (%js-date-time-format-option-string options "hour" nil))
         (minute-style (%js-date-time-format-option-string options "minute" nil))
         (second-style (%js-date-time-format-option-string options "second" nil))
         (hour12 (not (null (%js-intl-option options "hour12" nil))))
         (explicit-date-p (or year-style month-style day-style))
         (explicit-time-p (or hour-style minute-style second-style)))
    (unless explicit-date-p
      (multiple-value-bind (style-year style-month style-day)
          (%js-date-time-format-style-date date-style)
        (setf year-style style-year
              month-style style-month
              day-style style-day)))
    (unless explicit-time-p
      (multiple-value-bind (style-hour style-minute style-second)
          (%js-date-time-format-style-time time-style)
        (setf hour-style style-hour
              minute-style style-minute
              second-style style-second)))
    (unless (or date-style time-style explicit-date-p explicit-time-p)
      (setf year-style "numeric"
            month-style "numeric"
            day-style "numeric"))
    (%js-make-object
     "format" (lambda (&optional (date +js-undefined+))
                (let ((parts (%js-date-time-format-parts
                              (%js-date-time-format-input-date date)
                              year-style month-style day-style
                              hour-style minute-style second-style hour12)))
                  (%js-date-time-format-render parts)))
     "formatToParts" (lambda (&optional (date +js-undefined+))
                       (apply #'%js-make-array
                              (%js-date-time-format-parts
                               (%js-date-time-format-input-date date)
                               year-style month-style day-style
                               hour-style minute-style second-style hour12)))
     "resolvedOptions" (lambda ()
                         (%js-date-time-format-resolved-options
                          date-style time-style year-style month-style day-style
                          hour-style minute-style second-style hour12)))))
