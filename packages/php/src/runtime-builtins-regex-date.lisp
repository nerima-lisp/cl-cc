;;;; packages/php/src/runtime-builtins-regex-date.lisp -- PHP date/time and DateTime-compatible builtins

(in-package :cl-cc/php)


;;; -----------------------------------------------------------------------
;;;  Date / Time functions
;;; -----------------------------------------------------------------------

(defconstant +php-epoch-offset+ 2208988800
  "Seconds from CL universal-time epoch (1900) to Unix epoch (1970).")

(defvar *php-default-timezone* "UTC"
  "Current default timezone name used by PHP date/time builtins.")

(defparameter *php-timezone-offsets*
  '(("UTC" . 0)
    ("Etc/UTC" . 0)
    ("GMT" . 0)
    ("Z" . 0)
    ("Asia/Tokyo" . 32400)
    ("Asia/Seoul" . 32400)
    ("Asia/Shanghai" . 28800)
    ("Asia/Hong_Kong" . 28800)
    ("Asia/Singapore" . 28800)
    ("Asia/Kolkata" . 19800)
    ("Asia/Dubai" . 14400)
    ("Europe/London" . 0)
    ("Europe/Berlin" . 3600)
    ("Europe/Paris" . 3600)
    ("Europe/Rome" . 3600)
    ("Europe/Madrid" . 3600)
    ("Europe/Amsterdam" . 3600)
    ("Europe/Zurich" . 3600)
    ("Europe/Stockholm" . 3600)
    ("Europe/Warsaw" . 3600)
    ("America/New_York" . -18000)
    ("America/Chicago" . -21600)
    ("America/Denver" . -25200)
    ("America/Los_Angeles" . -28800)
    ("America/Phoenix" . -25200)
    ("America/Anchorage" . -32400)
    ("America/Toronto" . -18000)
    ("America/Vancouver" . -28800)
    ("America/Sao_Paulo" . -10800)
    ("America/Mexico_City" . -21600)
    ("Australia/Sydney" . 36000)
    ("Australia/Melbourne" . 36000)
    ("Australia/Perth" . 28800)
    ("Pacific/Auckland" . 43200))
  "Fixed UTC offsets in seconds for supported PHP timezone identifiers.
DST transitions are not modelled by this compact runtime table.")

(defun %php-timezone-name (tz)
  (string-trim '(#\Space #\Tab #\Newline #\Return) (%php-stringify tz)))

(defun %php-timezone-offset-seconds (&optional (tz *php-default-timezone*))
  (cdr (assoc (%php-timezone-name tz) *php-timezone-offsets* :test #'string=)))

(defun %php-default-timezone-offset-seconds ()
  (or (%php-timezone-offset-seconds *php-default-timezone*) 0))

(defun %php-time ()
  "PHP time() — current Unix timestamp."
  (- (get-universal-time) +php-epoch-offset+))

(defun %php-microtime (&optional (get-as-float nil))
  "PHP microtime — simplified (no sub-second precision in CL)."
  (let ((ts (%php-time)))
    (if (%php-truthy get-as-float)
        (coerce ts 'double-float)
        (format nil "0.000000 ~D" ts))))

(defun %php-mktime (&optional hour minute second month day year)
  "PHP mktime — create Unix timestamp from date components."
  (let* ((offset (%php-default-timezone-offset-seconds))
         (now (multiple-value-list (decode-universal-time (+ (get-universal-time) offset) 0)))
         (h   (or hour   (nth 2 now)))
         (min (or minute (nth 1 now)))
         (sec (or second (nth 0 now)))
         (m   (or month  (nth 4 now)))
         (d   (or day    (nth 3 now)))
         (y   (or year   (nth 5 now)))
         (ut  (encode-universal-time sec min h d m y 0)))
    (- ut +php-epoch-offset+ offset)))

(defun %php-strtotime (str &optional (base-ts nil))
  "PHP strtotime — simplified: parse 'YYYY-MM-DD' and common relative formats."
  (declare (ignore base-ts))
  (let ((s (string-trim '(#\Space) (%php-stringify str))))
    (handler-case
        (cond
          ;; YYYY-MM-DD or YYYY-MM-DD HH:MM:SS
          ((and (>= (length s) 10)
                (char= (char s 4) #\-)
                (char= (char s 7) #\-))
           (let* ((year  (parse-integer (subseq s 0 4)))
                  (month (parse-integer (subseq s 5 7)))
                  (day   (parse-integer (subseq s 8 10)))
                  (hour  (if (>= (length s) 13) (parse-integer (subseq s 11 13)) 0))
                  (min   (if (>= (length s) 16) (parse-integer (subseq s 14 16)) 0))
                  (sec   (if (>= (length s) 19) (parse-integer (subseq s 17 19)) 0))
                  (ut    (encode-universal-time sec min hour day month year 0)))
             (- ut +php-epoch-offset+ (%php-default-timezone-offset-seconds))))
          (t nil))
      (error () nil))))

(defun %php-date-ordinal-suffix (day)
  "PHP date S: the English ordinal suffix for DAY (st/nd/rd/th)."
  (if (<= 11 (mod day 100) 13)
      "th"
      (case (mod day 10) (1 "st") (2 "nd") (3 "rd") (t "th"))))

(defun %php-date (format &optional (timestamp nil) timezone-offset)
  "PHP date — format a timestamp. Supports d D j N w S m n M F Y y L t z H G g h
i s A a U and \\-escapes."
  (let* ((ts (or timestamp (%php-time)))
         (offset (if timezone-offset timezone-offset (%php-default-timezone-offset-seconds)))
         (ut (+ ts +php-epoch-offset+ offset))
         (decoded (multiple-value-list (decode-universal-time ut 0))))
    (destructuring-bind (sec min hour day month year dow dst tz) decoded
      (declare (ignore dst tz))
      (with-output-to-string (out)
        ;; decode-universal-time's day-of-week is 0=Monday..6=Sunday; PHP's `w'
        ;; is 0=Sunday..6=Saturday and `N' is 1=Monday..7=Sunday.  (Using the CL
        ;; value directly made every weekday off by one — gmdate('D',0) gave Wed
        ;; for 1970-01-01, a Thursday.)
        (let* ((fmt (%php-stringify format))
               (php-w (mod (1+ dow) 7))      ; PHP w: 0=Sunday
               (php-n (1+ dow))              ; PHP N: 1=Monday..7=Sunday
               (leap-p (and (zerop (mod year 4))
                            (or (plusp (mod year 100)) (zerop (mod year 400)))))
               (month-lengths (vector 31 (if leap-p 29 28) 31 30 31 30 31 31 30 31 30 31))
               (days-in-month (aref month-lengths (1- month)))
               (day-of-year (+ (loop for m from 1 below month
                                     sum (aref month-lengths (1- m)))
                               (1- day)))
               (month-names #("January" "February" "March" "April" "May" "June"
                              "July" "August" "September" "October" "November" "December"))
               (day-names #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")))
          (loop for i from 0 below (length fmt)
                for ch = (char fmt i)
                do (write-string
                    (case ch
                      (#\Y (format nil "~4,'0D" year))
                      (#\y (format nil "~2,'0D" (mod year 100)))
                      (#\m (format nil "~2,'0D" month))
                      (#\n (format nil "~D" month))
                      (#\M (subseq (aref month-names (1- month)) 0 3))
                      (#\F (aref month-names (1- month)))
                      (#\d (format nil "~2,'0D" day))
                      (#\j (format nil "~D" day))
                      (#\D (subseq (aref day-names php-w) 0 3))
                      (#\l (aref day-names php-w))
                      (#\N (format nil "~D" php-n))
                      (#\w (format nil "~D" php-w))
                      (#\S (%php-date-ordinal-suffix day))
                      (#\L (if leap-p "1" "0"))
                      (#\t (format nil "~D" days-in-month))
                      (#\z (format nil "~D" day-of-year))
                      (#\H (format nil "~2,'0D" hour))
                      (#\G (format nil "~D" hour))
                      (#\g (format nil "~D" (let ((h (mod hour 12))) (if (= h 0) 12 h))))
                      (#\h (format nil "~2,'0D" (let ((h (mod hour 12))) (if (= h 0) 12 h))))
                      (#\i (format nil "~2,'0D" min))
                      (#\s (format nil "~2,'0D" sec))
                      (#\A (if (< hour 12) "AM" "PM"))
                      (#\a (if (< hour 12) "am" "pm"))
                      (#\U (format nil "~D" ts))
                      (#\\ (if (< (1+ i) (length fmt))
                               (progn (incf i) (string (char fmt i)))
                               "\\"))
                      (t (string ch)))
                    out)))))))

(defun %php-checkdate (month day year)
  "PHP checkdate — validate a date."
  (handler-case
      (progn (encode-universal-time 0 0 0 day month year 0) t)
    (error () nil)))

(defun %php-date-create (&optional (datetime nil))
  "PHP date_create / DateTime::__construct: return a PHP DateTime-like array."
  (let* ((ts (if (or (null datetime) (%php-null-p datetime) (string= (%php-stringify datetime) "now"))
                 (%php-time)
                 (or (%php-strtotime datetime) (%php-time))))
         (obj (%php-make-array)))
    (%php-array-set obj "__class__" "DateTime")
    (%php-array-set obj "timestamp" ts)
    obj))

(defun %php-date-format (obj format)
  "PHP date_format / DateTime::format."
  (let ((ts (%php-array-ref obj "timestamp")))
    (%php-date format (if (%php-null-p ts) (%php-time) ts))))

(defun %php-date-modify (obj modifier)
  "PHP date_modify: modify datetime object."
  (declare (ignore modifier))
  obj)

(defun %php-date-diff (date1 date2 &optional absolute)
  "PHP date_diff: return interval between two DateTime objects."
  (declare (ignore absolute))
  (let* ((ts1 (or (%php-array-ref date1 "timestamp") (%php-time)))
         (ts2 (or (%php-array-ref date2 "timestamp") (%php-time)))
         (diff (abs (- ts2 ts1)))
         (result (%php-make-array)))
    (%php-array-set result "days" (truncate diff 86400))
    (%php-array-set result "h"    (truncate (mod diff 86400) 3600))
    (%php-array-set result "i"    (truncate (mod diff 3600)  60))
    (%php-array-set result "s"    (mod diff 60))
    (%php-array-set result "invert" (if (< ts2 ts1) 1 0))
    result))

(defun %php-date-timestamp (obj)
  "PHP DateTime::getTimestamp."
  (or (%php-array-ref obj "timestamp") (%php-time)))

(defun %php-gmdate (format &optional (timestamp nil))
  "PHP gmdate: same as date() but always UTC."
  (%php-date format timestamp 0))

(defun %php-date-default-timezone-set (tz)
  "PHP date_default_timezone_set: set the default timezone."
  (let ((name (%php-timezone-name tz)))
    (if (%php-timezone-offset-seconds name)
        (progn
          (setf *php-default-timezone* name)
          t)
        nil)))

(defun %php-date-default-timezone-get ()
  "PHP date_default_timezone_get: return the current default timezone."
  *php-default-timezone*)
