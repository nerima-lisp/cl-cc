;;;; packages/javascript/src/runtime-temporal.lisp — Temporal API (ES2026 Stage 4)
;;;;
;;;; Temporal replaces the Date API with a comprehensive, immutable datetime library.
;;;; We implement the 8 core types as hash-table objects with the standard methods.
;;;; Arithmetic operations use CL's arithmetic on universal-time seconds.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Shared helpers
;;; -----------------------------------------------------------------------

(defun %temporal-epoch-offset () 2208988800)  ; CL epoch → Unix epoch

(defun %temporal-now-unix-seconds ()
  (- (get-universal-time) (%temporal-epoch-offset)))

(defun %temporal-normalize-number (n &optional (default 0))
  "Normalize Temporal numeric fields before passing them to CL time primitives."
  (if (numberp n)
      (truncate n)
      default))

(defun %temporal-decode (unix-seconds)
  "Decode Unix seconds to (values sec min hour day month year dow)."
  (let ((ut (+ (%temporal-normalize-number unix-seconds) (%temporal-epoch-offset))))
    (multiple-value-bind (s mn h d m y dow) (decode-universal-time ut 0)
      (values s mn h d m y dow))))

(defun %temporal-encode (year month day &optional (hour 0) (min 0) (sec 0))
  (let ((y (%temporal-normalize-number year))
        (mo (%temporal-normalize-number month))
        (d (%temporal-normalize-number day))
        (h (%temporal-normalize-number hour))
        (mi (%temporal-normalize-number min))
        (s (%temporal-normalize-number sec)))
    ;; encode-universal-time raises out-of-range field errors via a hardware
    ;; trap; on macOS 26.5 ARM64 trap delivery hangs the thread (SBCL
    ;; runtime/OS regression), so reject invalid fields with a normal error
    ;; that the parse-fallback handler-cases can catch.
    (unless (and (<= 1900 y) (<= 1 mo 12) (<= 1 d 31)
                 (<= 0 h 23) (<= 0 mi 59) (<= 0 s 59))
      (error "invalid date-time fields: ~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
             y mo d h mi s))
    (- (encode-universal-time s mi h d mo y 0)
       (%temporal-epoch-offset))))

(defun %temporal-pad (n width)
  (format nil "~v,'0D" width n))

(defun %temporal-3way-compare (a b)
  "Return -1.0d0, 0.0d0, or 1.0d0 for ordered comparison of numeric A and B."
  (cond ((< a b) -1.0d0) ((> a b) 1.0d0) (t 0.0d0)))

(defun %temporal-parse-time-fields (s)
  "Parse a time string \"HH:MM:SS\" into (values hour minute second)."
  (flet ((field (start end) (if (>= (length s) end) (parse-integer s :start start :end end) 0)))
    (values (field 0 2) (field 3 5) (field 6 8))))

;;; -----------------------------------------------------------------------
;;;  Temporal.Now
;;; -----------------------------------------------------------------------

(defun %js-temporal-now ()
  (%js-make-object
   "instant"         (lambda () (%js-temporal-instant (%temporal-now-unix-seconds)))
   "plainDateTimeISO" (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-plain-datetime y m d h mn s)))
   "plainDateISO"    (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (declare (ignore s mn h))
                          (%js-temporal-plain-date y m d)))
   "plainTimeISO"    (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-plain-time h mn s)))
   "zonedDateTimeISO" (lambda (&optional _tz)
                        (declare (ignore _tz))
                        (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
                          (%js-temporal-zoned-datetime y m d h mn s "UTC")))
   "timeZoneId"      (lambda () "UTC")))

;;; -----------------------------------------------------------------------
;;;  Temporal.Instant
;;; -----------------------------------------------------------------------

(defun %js-temporal-instant (unix-seconds)
  (let ((ts unix-seconds))
    (%js-make-object
     "__type__"         "Temporal.Instant"
     "epochSeconds"     (coerce ts 'double-float)
     "epochMilliseconds" (coerce (* ts 1000) 'double-float)
     "epochMicroseconds" (coerce (* ts 1000000) 'double-float)
     "epochNanoseconds"  (coerce (* ts 1000000000) 'double-float)
     "toString"          (lambda (&optional _opts) (declare (ignore _opts))
                           (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                             (format nil "~A-~A-~AT~A:~A:~AZ"
                                     (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                                     (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2))))
     "toZonedDateTimeISO" (lambda (&optional _tz) (declare (ignore _tz))
                            (multiple-value-bind (s mn h d m y) (%temporal-decode ts)
                              (%js-temporal-zoned-datetime y m d h mn s "UTC")))
     "add"               (lambda (duration)
                           (%js-temporal-instant (+ ts (%temporal-duration-to-seconds duration))))
     "subtract"          (lambda (duration)
                           (%js-temporal-instant (- ts (%temporal-duration-to-seconds duration))))
     "equals"            (lambda (other)
                           (= ts (or (gethash "epochSeconds" other) 0)))
     "compare"           (lambda (other)
                           (%temporal-3way-compare ts (or (gethash "epochSeconds" other) 0))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainDate
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-date (year month day)
  (let ((y year) (m month) (d day))
    (%js-make-object
     "__type__"   "Temporal.PlainDate"
     "year"       (coerce y 'double-float)
     "month"      (coerce m 'double-float)
     "day"        (coerce d 'double-float)
     "calendarId" "iso8601"
     "dayOfWeek"  (coerce (1+ (nth-value 6 (%temporal-decode (%temporal-encode y m d)))) 'double-float)
     "toString"   (lambda () (format nil "~A-~A-~A"
                                     (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)))
     "toPlainDateTime" (lambda (&optional plain-time)
                         (if (and plain-time (gethash "hour" plain-time))
                             (%js-temporal-plain-datetime y m d
                               (gethash "hour" plain-time) (gethash "minute" plain-time) (gethash "second" plain-time))
                             (%js-temporal-plain-datetime y m d 0 0 0)))
     "add"        (lambda (duration)
                    (let* ((ts (%temporal-encode y m d))
                           (ts2 (+ ts (%temporal-duration-to-seconds duration))))
                      (multiple-value-bind (s mn h nd nm ny) (%temporal-decode ts2)
                        (declare (ignore s mn h))
                        (%js-temporal-plain-date ny nm nd))))
     "subtract"   (lambda (duration)
                    (let* ((ts (%temporal-encode y m d))
                           (ts2 (- ts (%temporal-duration-to-seconds duration))))
                      (multiple-value-bind (s mn h nd nm ny) (%temporal-decode ts2)
                        (declare (ignore s mn h))
                        (%js-temporal-plain-date ny nm nd))))
     "equals"     (lambda (other) (and (= y (gethash "year" other)) (= m (gethash "month" other)) (= d (gethash "day" other))))
     "compare"    (lambda (other)
                    (%temporal-3way-compare
                     (%temporal-encode y m d)
                     (%temporal-encode (gethash "year" other) (gethash "month" other) (gethash "day" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-time (hour minute second &optional (ms 0) (us 0) (ns 0))
  (let ((h hour) (mn minute) (s second))
    (declare (ignore ms us ns))
    (%js-make-object
     "__type__"   "Temporal.PlainTime"
     "hour"       (coerce h 'double-float)
     "minute"     (coerce mn 'double-float)
     "second"     (coerce s 'double-float)
     "millisecond" 0.0d0
     "microsecond" 0.0d0
     "nanosecond"  0.0d0
     "toString"   (lambda () (format nil "~A:~A:~A"
                                     (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2)))
     "add"        (lambda (duration)
                    (let* ((total (+ (* h 3600) (* mn 60) s (%temporal-duration-to-seconds duration)))
                           (new-h (mod (floor total 3600) 24))
                           (new-mn (mod (floor (mod total 3600) 60) 60))
                           (new-s (mod total 60)))
                      (%js-temporal-plain-time new-h new-mn (floor new-s))))
     "equals"     (lambda (other) (and (= h (gethash "hour" other)) (= mn (gethash "minute" other)) (= s (gethash "second" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.PlainDateTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-plain-datetime (year month day hour minute second)
  (let ((y year) (m month) (d day) (h hour) (mn minute) (s second))
    (%js-make-object
     "__type__"   "Temporal.PlainDateTime"
     "year"       (coerce y 'double-float)
     "month"      (coerce m 'double-float)
     "day"        (coerce d 'double-float)
     "hour"       (coerce h 'double-float)
     "minute"     (coerce mn 'double-float)
     "second"     (coerce s 'double-float)
     "millisecond" 0.0d0
     "microsecond" 0.0d0
     "nanosecond"  0.0d0
     "calendarId" "iso8601"
     "toString"   (lambda (&optional _opts) (declare (ignore _opts))
                   (format nil "~A-~A-~AT~A:~A:~A"
                           (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                           (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2)))
     "toPlainDate" (lambda () (%js-temporal-plain-date y m d))
     "toPlainTime" (lambda () (%js-temporal-plain-time h mn s))
     "toInstant"   (lambda (&optional _tz) (declare (ignore _tz))
                    (%js-temporal-instant (%temporal-encode y m d h mn s)))
     "add"         (lambda (duration)
                     (let* ((ts (%temporal-encode y m d h mn s))
                            (ts2 (+ ts (%temporal-duration-to-seconds duration))))
                       (multiple-value-bind (ns nmn nh nd nm ny) (%temporal-decode ts2)
                         (%js-temporal-plain-datetime ny nm nd nh nmn ns))))
     "subtract"    (lambda (duration)
                     (let* ((ts (%temporal-encode y m d h mn s))
                            (ts2 (- ts (%temporal-duration-to-seconds duration))))
                       (multiple-value-bind (ns nmn nh nd nm ny) (%temporal-decode ts2)
                         (%js-temporal-plain-datetime ny nm nd nh nmn ns))))
     "equals"      (lambda (other)
                     (= (%temporal-encode y m d h mn s)
                        (%temporal-encode (gethash "year" other) (gethash "month" other) (gethash "day" other)
                                          (gethash "hour" other) (gethash "minute" other) (gethash "second" other)))))))

;;; -----------------------------------------------------------------------
;;;  Temporal.ZonedDateTime
;;; -----------------------------------------------------------------------

(defun %js-temporal-zoned-datetime (year month day hour minute second tz)
  (let ((y year) (m month) (d day) (h hour) (mn minute) (s second))
    (%js-make-object
     "__type__"     "Temporal.ZonedDateTime"
     "year"         (coerce y 'double-float)
     "month"        (coerce m 'double-float)
     "day"          (coerce d 'double-float)
     "hour"         (coerce h 'double-float)
     "minute"       (coerce mn 'double-float)
     "second"       (coerce s 'double-float)
     "millisecond"  0.0d0
     "microsecond"  0.0d0
     "nanosecond"   0.0d0
     "timeZoneId"   tz
     "calendarId"   "iso8601"
     "epochSeconds" (coerce (%temporal-encode y m d h mn s) 'double-float)
     "toString"     (lambda (&optional _opts) (declare (ignore _opts))
                     (format nil "~A-~A-~AT~A:~A:~A+00:00[~A]"
                             (%temporal-pad y 4) (%temporal-pad m 2) (%temporal-pad d 2)
                             (%temporal-pad h 2) (%temporal-pad mn 2) (%temporal-pad s 2) tz))
     "toPlainDate"  (lambda () (%js-temporal-plain-date y m d))
     "toPlainTime"  (lambda () (%js-temporal-plain-time h mn s))
     "toPlainDateTime" (lambda () (%js-temporal-plain-datetime y m d h mn s))
     "toInstant"    (lambda () (%js-temporal-instant (%temporal-encode y m d h mn s))))))
;;; Temporal.Duration, Temporal.PlainYearMonth / Temporal.PlainMonthDay, and
;;; parse helpers live in runtime-temporal-duration.lisp and
;;; runtime-temporal-parse.lisp.

(defun %js-temporal-plain-year-month (year month)
  (%js-make-object
   "__type__" "Temporal.PlainYearMonth"
   "year"     (coerce year 'double-float)
   "month"    (coerce month 'double-float)
   "toString" (lambda () (format nil "~A-~A" (%temporal-pad year 4) (%temporal-pad month 2)))))

(defun %js-temporal-plain-month-day (month day)
  (%js-make-object
   "__type__" "Temporal.PlainMonthDay"
   "month"    (coerce month 'double-float)
   "day"      (coerce day 'double-float)
   "toString" (lambda () (format nil "--~A-~A" (%temporal-pad month 2) (%temporal-pad day 2)))))

;;; The Temporal global object lives in runtime-temporal-global.lisp.
