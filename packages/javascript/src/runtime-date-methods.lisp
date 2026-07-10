;;;; packages/javascript/src/runtime-date-methods.lisp — JS Date string reps and dispatch table

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Date.prototype string representations
;;; -----------------------------------------------------------------------

(defun %js-date-to-iso-string (date)
  "Date.prototype.toISOString() → 'YYYY-MM-DDTHH:MM:SS.mmmZ'."
  (%with-date-fields (date :sec sec :min min :hour hour :day day :month month :year year)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~3,'0DZ"
            year month day hour min sec (mod (js-date-ms date) 1000))))

(defun %js-date-to-string (date)
  "Date.prototype.toString() — simplified."
  (%js-date-to-iso-string date))

(defun %js-date-to-local-date-string (date)
  "Date.prototype.toLocaleDateString() — simplified YYYY/MM/DD."
  (%with-date-fields (date :day day :month month :year year)
    (format nil "~4,'0D/~2,'0D/~2,'0D" year month day)))

(defun %js-date-to-utc-string (date)
  (%js-date-to-iso-string date))

(defparameter +js-date-weekday-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "CL decode-universal-time day-of-week is 0=Monday..6=Sunday.")

(defparameter +js-date-month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun %js-date-to-date-string (date)
  "Date.prototype.toDateString() → 'Www Mmm DD YYYY'."
  (%with-date-fields (date :day day :month month :year year :dow dow)
    (format nil "~A ~A ~2,'0D ~D"
            (aref +js-date-weekday-names+ dow)
            (aref +js-date-month-names+ (1- month))
            day year)))

(defun %js-date-to-time-string (date)
  "Date.prototype.toTimeString() → 'HH:MM:SS GMT+0000 (UTC)'."
  (%with-date-fields (date :sec sec :min min :hour hour)
    (format nil "~2,'0D:~2,'0D:~2,'0D GMT+0000 (Coordinated Universal Time)"
            hour min sec)))

(defun %js-date-to-json (date &optional key)
  "Date.prototype.toJSON() → ISO string (ignores KEY, per spec)."
  (declare (ignore key))
  (%js-date-to-iso-string date))

(defun %js-date-value-of (date)
  (%js-date-get-time date))

;;; -----------------------------------------------------------------------
;;;  Date method dispatch table
;;; -----------------------------------------------------------------------

(defparameter *js-date-method-table*
  (list
   (cons "getTime"             #'%js-date-get-time)
   (cons "getFullYear"         #'%js-date-get-full-year)
   (cons "getUTCFullYear"      #'%js-date-get-utc-full-year)
   (cons "getMonth"            #'%js-date-get-month)
   (cons "getUTCMonth"         #'%js-date-get-utc-month)
   (cons "getDate"             #'%js-date-get-date)
   (cons "getUTCDate"          #'%js-date-get-utc-date)
   (cons "getDay"              #'%js-date-get-day)
   (cons "getUTCDay"           #'%js-date-get-utc-day)
   (cons "getHours"            #'%js-date-get-hours)
   (cons "getUTCHours"         #'%js-date-get-utc-hours)
   (cons "getMinutes"          #'%js-date-get-minutes)
   (cons "getUTCMinutes"       #'%js-date-get-utc-minutes)
   (cons "getSeconds"          #'%js-date-get-seconds)
   (cons "getUTCSeconds"       #'%js-date-get-utc-seconds)
   (cons "getMilliseconds"     #'%js-date-get-milliseconds)
   (cons "getTimezoneOffset"   #'%js-date-get-timezone-offset)
   (cons "setTime"             #'%js-date-set-time)
   (cons "setFullYear"         #'%js-date-set-full-year)
   (cons "setMonth"            #'%js-date-set-month)
   (cons "setDate"             #'%js-date-set-date)
   (cons "setHours"            #'%js-date-set-hours)
   (cons "setMinutes"          #'%js-date-set-minutes)
   (cons "setSeconds"          #'%js-date-set-seconds)
   (cons "toISOString"         #'%js-date-to-iso-string)
   (cons "toLocaleDateString"  #'%js-date-to-local-date-string)
   (cons "toLocaleTimeString"  #'%js-date-to-time-string)
   (cons "toLocaleString"      #'%js-date-to-string)
   (cons "toUTCString"         #'%js-date-to-utc-string)
   (cons "toDateString"        #'%js-date-to-date-string)
   (cons "toTimeString"        #'%js-date-to-time-string)
   (cons "toJSON"              #'%js-date-to-json)
   (cons "toString"            #'%js-date-to-string)
   (cons "valueOf"             #'%js-date-value-of))
  "Alist of Date.prototype method name -> host function.")
