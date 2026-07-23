;;;; packages/javascript/tests/js-runtime-date-json-tests.lisp
;;;;
;;;; Temporal helper functions, Date.prototype, JSON stringify, JSON parse.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)

;;; ─── Temporal helper functions ───────────────────────────────────────────────

(it-sequential "js-rt-temporal-pad year-4"
  (destructuring-bind (n width expected) (list 2025 4 "2025")
    (expect (cl-cc/javascript::%temporal-pad n width) :to-equal expected)))

(it-sequential "js-rt-temporal-pad month-2"
  (destructuring-bind (n width expected) (list 3 2 "03")
    (expect (cl-cc/javascript::%temporal-pad n width) :to-equal expected)))

(it-sequential "js-rt-temporal-pad day-2"
  (destructuring-bind (n width expected) (list 15 2 "15")
    (expect (cl-cc/javascript::%temporal-pad n width) :to-equal expected)))

(it-sequential "js-rt-temporal-pad narrow"
  (destructuring-bind (n width expected) (list 9 1 "9")
    (expect (cl-cc/javascript::%temporal-pad n width) :to-equal expected)))

(it-sequential "js-rt-temporal-3way-compare less"
  (destructuring-bind (a b expected) (list 1 2 -1.0d0)
    (expect (= expected (cl-cc/javascript::%temporal-3way-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-temporal-3way-compare equal"
  (destructuring-bind (a b expected) (list 5 5 0.0d0)
    (expect (= expected (cl-cc/javascript::%temporal-3way-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-temporal-3way-compare greater"
  (destructuring-bind (a b expected) (list 9 3 1.0d0)
    (expect (= expected (cl-cc/javascript::%temporal-3way-compare a b)) :to-be-truthy)))

(it-sequential "js-rt-temporal-parse-iso-fields full"
  (destructuring-bind (s exp-y exp-mo exp-d exp-h exp-mi exp-s) (list "2025-06-13T14:30:00" 2025 6 13 14 30 0)
    (multiple-value-bind (y mo d h mi s) (cl-cc/javascript::%temporal-parse-iso-fields s)
    (expect (= exp-y y) :to-be-truthy)
    (expect (= exp-mo mo) :to-be-truthy)
    (expect (= exp-d d) :to-be-truthy)
    (expect (= exp-h h) :to-be-truthy)
    (expect (= exp-mi mi) :to-be-truthy)
    (expect (= exp-s s) :to-be-truthy))))

(it-sequential "js-rt-temporal-parse-iso-fields date-only"
  (destructuring-bind (s exp-y exp-mo exp-d exp-h exp-mi exp-s) (list "2025-01-01" 2025 1 1 0 0 0)
    (multiple-value-bind (y mo d h mi s) (cl-cc/javascript::%temporal-parse-iso-fields s)
    (expect (= exp-y y) :to-be-truthy)
    (expect (= exp-mo mo) :to-be-truthy)
    (expect (= exp-d d) :to-be-truthy)
    (expect (= exp-h h) :to-be-truthy)
    (expect (= exp-mi mi) :to-be-truthy)
    (expect (= exp-s s) :to-be-truthy))))

(it-sequential "js-rt-temporal-duration-to-seconds one-hour"
  (destructuring-bind (unit n expected) (list "hours" 1 3600)
    (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (expect (= expected (cl-cc/javascript::%temporal-duration-to-seconds dur)) :to-be-truthy))))

(it-sequential "js-rt-temporal-duration-to-seconds one-minute"
  (destructuring-bind (unit n expected) (list "minutes" 1 60)
    (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (expect (= expected (cl-cc/javascript::%temporal-duration-to-seconds dur)) :to-be-truthy))))

(it-sequential "js-rt-temporal-duration-to-seconds one-second"
  (destructuring-bind (unit n expected) (list "seconds" 1 1)
    (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (expect (= expected (cl-cc/javascript::%temporal-duration-to-seconds dur)) :to-be-truthy))))

(it-sequential "js-rt-temporal-duration-to-seconds one-day"
  (destructuring-bind (unit n expected) (list "days" 1 86400)
    (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (expect (= expected (cl-cc/javascript::%temporal-duration-to-seconds dur)) :to-be-truthy))))

(it-sequential "js-rt-temporal-parse-time-fields"
  (multiple-value-bind (h m s) (cl-cc/javascript::%temporal-parse-time-fields "14:30:05")
    (expect (= 14 h) :to-be-truthy)
    (expect (= 30 m) :to-be-truthy)
    (expect (= 5 s) :to-be-truthy)))

(it-sequential "js-rt-temporal-now-object"
  (let* ((now (cl-cc/javascript::%js-temporal-now))
         (instant (funcall (gethash "instant" now)))
         (plain-datetime (funcall (gethash "plainDateTimeISO" now)))
         (plain-date (funcall (gethash "plainDateISO" now)))
         (plain-time (funcall (gethash "plainTimeISO" now)))
         (zoned-datetime (funcall (gethash "zonedDateTimeISO" now))))
    (expect (funcall (gethash "timeZoneId" now)) :to-equal "UTC")
    (expect (gethash "__type__" instant) :to-equal "Temporal.Instant")
    (expect (gethash "__type__" plain-datetime) :to-equal "Temporal.PlainDateTime")
    (expect (gethash "__type__" plain-date) :to-equal "Temporal.PlainDate")
    (expect (gethash "__type__" plain-time) :to-equal "Temporal.PlainTime")
    (expect (gethash "__type__" zoned-datetime) :to-equal "Temporal.ZonedDateTime")))

(it-sequential "js-rt-temporal-instant-methods"
  (let* ((instant (cl-cc/javascript::%js-temporal-instant 0))
         (duration (cl-cc/javascript::%js-temporal-duration :hours 1 :minutes 30))
         (added (funcall (gethash "add" instant) duration))
         (subtracted (funcall (gethash "subtract" added) duration))
         (zoned (funcall (gethash "toZonedDateTimeISO" instant))))
    (expect (funcall (gethash "toString" instant)) :to-equal "1970-01-01T00:00:00Z")
    (expect (= 5400.0d0 (gethash "epochSeconds" added)) :to-be-truthy)
    (expect (= 0.0d0 (gethash "epochSeconds" subtracted)) :to-be-truthy)
    (expect (funcall (gethash "equals" instant) subtracted) :to-be-truthy)
    (expect (= -1.0d0 (funcall (gethash "compare" instant) added)) :to-be-truthy)
    (expect (gethash "timeZoneId" zoned) :to-equal "UTC")))

(it-sequential "js-rt-temporal-plain-date-methods"
  (let* ((date (cl-cc/javascript::%js-temporal-plain-date 2025 6 18))
         (datetime (funcall (gethash "toPlainDateTime" date))))
    (expect (funcall (gethash "toString" date)) :to-equal "2025-06-18")
    (expect (= 3.0d0 (gethash "dayOfWeek" date)) :to-be-truthy)
    (expect (funcall (gethash "toString" datetime)) :to-equal "2025-06-18T00:00:00")))

(it-sequential "js-rt-temporal-plain-date-extended"
  (let* ((date (cl-cc/javascript::%js-temporal-plain-date 2025 6 18))
         (time (cl-cc/javascript::%js-temporal-plain-time 7 8 9))
         (duration (cl-cc/javascript::%js-temporal-duration :days 1))
         (datetime (funcall (gethash "toPlainDateTime" date) time))
         (added (funcall (gethash "add" date) duration))
         (subtracted (funcall (gethash "subtract" added) duration)))
    (expect (gethash "__type__" datetime) :to-equal "Temporal.PlainDateTime")
    (expect (= 2025.0d0 (gethash "year" datetime)) :to-be-truthy)
    (expect (= 6.0d0 (gethash "month" datetime)) :to-be-truthy)
    (expect (= 18.0d0 (gethash "day" datetime)) :to-be-truthy)
    (expect (= 7.0d0 (gethash "hour" datetime)) :to-be-truthy)
    (expect (= 8.0d0 (gethash "minute" datetime)) :to-be-truthy)
    (expect (= 9.0d0 (gethash "second" datetime)) :to-be-truthy)
    (expect (funcall (gethash "toString" added)) :to-equal "2025-06-19")
    (expect (funcall (gethash "toString" subtracted)) :to-equal "2025-06-18")
    (expect (= -1.0d0 (funcall (gethash "compare" date) added)) :to-be-truthy)
    (expect (funcall (gethash "equals" date)
                          (cl-cc/javascript::%js-temporal-plain-date 2025 6 18)) :to-be-truthy)))

(it-sequential "js-rt-temporal-plain-time-methods"
  (let* ((time (cl-cc/javascript::%js-temporal-plain-time 23 59 30))
         (duration (cl-cc/javascript::%js-temporal-duration :seconds 90))
         (added (funcall (gethash "add" time) duration)))
    (expect (funcall (gethash "toString" time)) :to-equal "23:59:30")
    (expect (funcall (gethash "toString" added)) :to-equal "00:01:00")
    (expect (funcall (gethash "equals" added)
                          (cl-cc/javascript::%js-temporal-plain-time 0 1 0)) :to-be-truthy)))

(it-sequential "js-rt-temporal-plain-datetime-methods"
  (let* ((datetime (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56))
         (plain-date (funcall (gethash "toPlainDate" datetime)))
         (plain-time (funcall (gethash "toPlainTime" datetime))))
    (expect (funcall (gethash "toString" datetime)) :to-equal "2025-06-18T12:34:56")
    (expect (funcall (gethash "toString" plain-date)) :to-equal "2025-06-18")
    (expect (funcall (gethash "toString" plain-time)) :to-equal "12:34:56")))

(it-sequential "js-rt-temporal-plain-datetime-extended"
  (let* ((datetime (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56))
         (duration (cl-cc/javascript::%js-temporal-duration :seconds 4))
         (added (funcall (gethash "add" datetime) duration))
         (subtracted (funcall (gethash "subtract" added) duration))
         (instant (funcall (gethash "toInstant" datetime))))
    (expect (funcall (gethash "toString" datetime)) :to-equal "2025-06-18T12:34:56")
    (expect (funcall (gethash "toString" added)) :to-equal "2025-06-18T12:35:00")
    (expect (funcall (gethash "toString" subtracted)) :to-equal "2025-06-18T12:34:56")
    (expect (gethash "__type__" instant) :to-equal "Temporal.Instant")
    (expect (funcall (gethash "equals" datetime)
                          (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56)) :to-be-truthy)))

(it-sequential "js-rt-temporal-zoned-datetime-methods"
  (let* ((zoned (cl-cc/javascript::%js-temporal-zoned-datetime 2025 6 18 12 34 56 "UTC"))
         (plain-datetime (funcall (gethash "toPlainDateTime" zoned)))
         (plain-date (funcall (gethash "toPlainDate" zoned)))
         (plain-time (funcall (gethash "toPlainTime" zoned)))
         (instant (funcall (gethash "toInstant" zoned))))
    (expect (funcall (gethash "toString" zoned)) :to-equal "2025-06-18T12:34:56+00:00[UTC]")
    (expect (funcall (gethash "toString" plain-datetime)) :to-equal "2025-06-18T12:34:56")
    (expect (funcall (gethash "toString" plain-date)) :to-equal "2025-06-18")
    (expect (funcall (gethash "toString" plain-time)) :to-equal "12:34:56")
    (expect (gethash "__type__" instant) :to-equal "Temporal.Instant")))

(it-sequential "js-rt-temporal-duration-methods"
  (let* ((duration (cl-cc/javascript::%js-temporal-duration :years 1 :months 2 :weeks 3 :days 4 :hours 5 :minutes 6 :seconds 7))
         (negative (cl-cc/javascript::%js-temporal-duration :hours -2 :minutes -30))
         (abs-duration (funcall (gethash "abs" negative)))
         (negated (funcall (gethash "negated" duration))))
    (expect (funcall (gethash "toString" duration)) :to-equal "P1Y2M3W4DT5H6M7S")
    (expect (= 1.0d0 (gethash "sign" duration)) :to-be-truthy)
    (expect (= -1.0d0 (gethash "sign" negative)) :to-be-truthy)
    (expect (funcall (gethash "toString" abs-duration)) :to-equal "P0Y0M0W0DT2H30M0S")
    (expect (funcall (gethash "toString" negated)) :to-equal "P-1Y-2M-3W-4DT-5H-6M-7S")
    (expect (= 38995567.0d0 (funcall (gethash "total" duration))) :to-be-truthy)))

(it-sequential "js-rt-temporal-normalization-numbers"
  (expect (= 3 (cl-cc/javascript::%temporal-normalize-number 3.9)) :to-be-truthy)
  (expect (= 7 (cl-cc/javascript::%temporal-normalize-number nil 7)) :to-be-truthy))

(it-sequential "js-rt-temporal-normalization-encode-decode"
  (multiple-value-bind (s mn h d m y dow)
      (cl-cc/javascript::%temporal-decode
       (cl-cc/javascript::%temporal-encode 2025 6 18 12 34 56))
    (expect (= 56 s) :to-be-truthy)
    (expect (= 34 mn) :to-be-truthy)
    (expect (= 12 h) :to-be-truthy)
    (expect (= 18 d) :to-be-truthy)
    (expect (= 6 m) :to-be-truthy)
    (expect (= 2025 y) :to-be-truthy)
    (expect (numberp dow) :to-be-truthy)))

(it-sequential "js-rt-temporal-normalization-duration"
  (let ((duration (cl-cc/javascript::%js-temporal-duration :hours -2.7 :minutes nil)))
    (expect (funcall (gethash "toString" duration)) :to-equal "P0Y0M0W0DT-2H0M0S")
    (expect (= -1.0d0 (gethash "sign" duration)) :to-be-truthy)
    (expect (= -7200.0d0 (funcall (gethash "total" duration))) :to-be-truthy)))

(it-sequential "js-rt-temporal-normalization-parse-instant-fallback"
  (let ((instant (cl-cc/javascript::%js-temporal-parse-instant "(")))
    (expect (gethash "__type__" instant) :to-equal "Temporal.Instant")))

(it-sequential "js-rt-temporal-normalization-parse-plain-date-fallback"
  (let ((plain-date (cl-cc/javascript::%js-temporal-parse-plain-date "(")))
    (expect (gethash "__type__" plain-date) :to-equal "Temporal.PlainDate")))

(it-sequential "js-rt-temporal-year-month-and-month-day"
  (let ((year-month (cl-cc/javascript::%js-temporal-plain-year-month 2025 6))
        (month-day (cl-cc/javascript::%js-temporal-plain-month-day 6 18)))
    (expect (funcall (gethash "toString" year-month)) :to-equal "2025-06")
    (expect (funcall (gethash "toString" month-day)) :to-equal "--06-18")))

(it-sequential "js-rt-temporal-global-factories"
  (let* ((temporal cl-cc/javascript::*js-temporal-global*)
         (instant-global (gethash "Instant" temporal))
         (plain-date-global (gethash "PlainDate" temporal))
         (plain-time-global (gethash "PlainTime" temporal))
         (plain-datetime-global (gethash "PlainDateTime" temporal))
         (zoned-global (gethash "ZonedDateTime" temporal))
         (duration-global (gethash "Duration" temporal))
         (plain-year-month-global (gethash "PlainYearMonth" temporal))
         (plain-month-day-global (gethash "PlainMonthDay" temporal))
         (instant-now (funcall (gethash "__call__" instant-global)))
         (instant-from-string (funcall (gethash "from" instant-global) "1970-01-01T00:00:01"))
         (instant-from-ms (funcall (gethash "fromEpochMilliseconds" instant-global) 2000))
         (instant-from-us (funcall (gethash "fromEpochMicroseconds" instant-global) 3000000))
         (instant-from-ns (funcall (gethash "fromEpochNanoseconds" instant-global) 4000000000))
         (plain-date-from-object (funcall (gethash "from" plain-date-global)
                                          (cl-cc/javascript::%js-make-object "year" 2025 "month" 6 "day" 18)))
         (plain-date-from-string (funcall (gethash "from" plain-date-global) "2025-06-19"))
         (plain-time-from-object (funcall (gethash "from" plain-time-global)
                                          (cl-cc/javascript::%js-make-object "hour" 7 "minute" 8 "second" 9)))
         (plain-time-from-string (funcall (gethash "from" plain-time-global) "10:11:12"))
         (plain-datetime-from-object (funcall (gethash "from" plain-datetime-global)
                                              (cl-cc/javascript::%js-make-object
                                               "year" 2025 "month" 6 "day" 18
                                               "hour" 12 "minute" 34 "second" 56)))
         (plain-datetime-from-string (funcall (gethash "from" plain-datetime-global) "2025-06-18T12:34:56"))
         (zoned-from-call (funcall (gethash "__call__" zoned-global) 0 "UTC"))
         (zoned-from-string (funcall (gethash "from" zoned-global) "2025-06-18T12:34:56"))
         (zoned-from-object (funcall (gethash "from" zoned-global)
                                     (cl-cc/javascript::%js-make-object "toString" "2025-06-18T12:34:56")))
         (duration-from-call (funcall (gethash "__call__" duration-global) 1 2 0 3 4 5 6))
         (duration-from-object (funcall (gethash "from" duration-global)
                                        (cl-cc/javascript::%js-make-object
                                         "years" 1.0d0 "months" 2.0d0 "days" 3.0d0
                                         "hours" 4.0d0 "minutes" 5.0d0 "seconds" 6.0d0)))
         (duration-from-fallback (funcall (gethash "from" duration-global) "ignored"))
         (plain-year-month-from-object (funcall (gethash "from" plain-year-month-global)
                                                (cl-cc/javascript::%js-make-object "year" 2025 "month" 6)))
         (plain-year-month-from-fallback (funcall (gethash "from" plain-year-month-global) "ignored"))
         (plain-month-day-from-object (funcall (gethash "from" plain-month-day-global)
                                               (cl-cc/javascript::%js-make-object "month" 6 "day" 18)))
         (plain-month-day-from-fallback (funcall (gethash "from" plain-month-day-global) "ignored")))
    (expect (gethash "__type__" instant-now) :to-equal "Temporal.Instant")
    (expect (= 1.0d0 (gethash "epochSeconds" instant-from-string)) :to-be-truthy)
    (expect (= 2.0d0 (gethash "epochSeconds" instant-from-ms)) :to-be-truthy)
    (expect (= 3.0d0 (gethash "epochSeconds" instant-from-us)) :to-be-truthy)
    (expect (= 4.0d0 (gethash "epochSeconds" instant-from-ns)) :to-be-truthy)
    (expect (= -1.0d0 (funcall (gethash "compare" instant-global) instant-from-string instant-from-ms)) :to-be-truthy)
    (expect (funcall (gethash "toString" plain-date-from-object)) :to-equal "2025-06-18")
    (expect (funcall (gethash "toString" plain-date-from-string)) :to-equal "2025-06-19")
    (expect (funcall (gethash "toString" plain-time-from-object)) :to-equal "07:08:09")
    (expect (funcall (gethash "toString" plain-time-from-string)) :to-equal "10:11:12")
    (expect (funcall (gethash "toString" plain-datetime-from-object)) :to-equal "2025-06-18T12:34:56")
    (expect (funcall (gethash "toString" plain-datetime-from-string)) :to-equal "2025-06-18T12:34:56")
    (expect (funcall (gethash "toString" zoned-from-call)) :to-equal "1970-01-01T00:00:00+00:00[UTC]")
    (expect (funcall (gethash "toString" zoned-from-string)) :to-equal "2025-06-18T12:34:56+00:00[UTC]")
    (expect (funcall (gethash "toString" zoned-from-object)) :to-equal "2025-06-18T12:34:56+00:00[UTC]")
    (expect (funcall (gethash "toString" duration-from-call)) :to-equal "P1Y2M0W3DT4H5M6S")
    (expect (funcall (gethash "toString" duration-from-object)) :to-equal "P1Y2M0W3DT4H5M6S")
    (expect (funcall (gethash "toString" duration-from-fallback)) :to-equal "P0Y0M0W0DT0H0M0S")
    (expect (= -1.0d0 (funcall (gethash "compare" duration-global) duration-from-fallback duration-from-object)) :to-be-truthy)
    (expect (funcall (gethash "toString" plain-year-month-from-object)) :to-equal "2025-06")
    (expect (funcall (gethash "toString" plain-year-month-from-fallback)) :to-equal "2000-01")
    (expect (funcall (gethash "toString" plain-month-day-from-object)) :to-equal "--06-18")
    (expect (funcall (gethash "toString" plain-month-day-from-fallback)) :to-equal "--01-01")))

;;; ─── Date.prototype ──────────────────────────────────────────────────────────

(it-sequential "js-rt-date-now"
  (let ((t1 (cl-cc/javascript::%js-date-now))
        (t2 (cl-cc/javascript::%js-date-now)))
    (expect (integerp t1) :to-be-truthy)
    (expect (>= t2 t1) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-no-args"
  (let ((d (cl-cc/javascript::%js-make-date)))
    (expect (cl-cc/javascript::js-date-p d) :to-be-truthy)
    (expect (integerp (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-from-ms"
  (let ((d (cl-cc/javascript::%js-make-date 1000000.0d0)))
    (expect (= 1000000 (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-copy"
  (let* ((orig (cl-cc/javascript::%js-make-date 42000.0d0))
         (copy (cl-cc/javascript::%js-make-date orig)))
    (expect (= 42000 (cl-cc/javascript::js-date-ms copy)) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-string"
  (let ((d (cl-cc/javascript::%js-make-date "1970-01-01T01:00:00")))
    (expect (cl-cc/javascript::js-date-p d) :to-be-truthy)
    (expect (= 3600000 (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-null"
  (let ((d (cl-cc/javascript::%js-make-date cl-cc/javascript::+js-null+)))
    (expect (cl-cc/javascript::js-date-p d) :to-be-truthy)
    (expect (integerp (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-parse-string-date-only"
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01")))
    (expect (= 0 ms) :to-be-truthy)))

(it-sequential "js-rt-date-parse-string-datetime"
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01T01:00:00")))
    (expect (= 3600000 ms) :to-be-truthy)))

(it-sequential "js-rt-date-parse-string-trims-spaces"
  (let ((ms (cl-cc/javascript::%js-date-parse-string " 1970-01-01T01:02:03 ")))
    (expect (= 3723000 ms) :to-be-truthy)))

(it-sequential "js-rt-date-parse-string-error"
  (let ((result (cl-cc/javascript::%js-date-parse-string "not-a-date")))
    (expect (integerp result) :to-be-truthy)))

(it-sequential "js-rt-date-make-date-fallback"
  (let ((d (cl-cc/javascript::%js-make-date (make-hash-table :test #'equal))))
    (expect (cl-cc/javascript::js-date-p d) :to-be-truthy)
    (expect (integerp (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-setters-with-undefined-optionals"
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-full-year d 1970 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-month d 0 cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-hours d 0 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-minutes d 0 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-seconds d 0 cl-cc/javascript::+js-undefined+)
    (expect (= 0 (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

;;; 97445000 ms = 1970-01-02T03:04:05.000Z
(it-sequential "js-rt-date-getters"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (= 1970 (cl-cc/javascript::%js-date-get-full-year d)) :to-be-truthy)
    (expect (= 1970 (cl-cc/javascript::%js-date-get-utc-full-year d)) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-date-get-month d)) :to-be-truthy)      ; January = 0
    (expect (= 2 (cl-cc/javascript::%js-date-get-date d)) :to-be-truthy)
    (expect (= 3 (cl-cc/javascript::%js-date-get-hours d)) :to-be-truthy)
    (expect (= 4 (cl-cc/javascript::%js-date-get-minutes d)) :to-be-truthy)
    (expect (= 5 (cl-cc/javascript::%js-date-get-seconds d)) :to-be-truthy)
    (expect (= 0 (cl-cc/javascript::%js-date-get-milliseconds d)) :to-be-truthy)))

(it-sequential "js-rt-date-get-time"
  (let ((d (cl-cc/javascript::%js-make-date 12345)))
    (expect (= 12345.0d0 (cl-cc/javascript::%js-date-get-time d)) :to-be-truthy)))

(it-sequential "js-rt-date-to-iso-string"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-iso-string d) :to-equal "1970-01-02T03:04:05.000Z")))

(it-sequential "js-rt-date-to-iso-string-with-ms"
  (let ((d (cl-cc/javascript::%js-make-date 97445123)))
    (expect (cl-cc/javascript::%js-date-to-iso-string d) :to-equal "1970-01-02T03:04:05.123Z")))

(it-sequential "js-rt-date-to-local-date-string"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-local-date-string d) :to-equal "1970/01/02")))

(it-sequential "js-rt-date-to-time-string"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-time-string d) :to-equal "03:04:05 GMT+0000 (Coordinated Universal Time)")))

(it-sequential "js-rt-date-set-time"
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-time d 5000)
    (expect (= 5000 (cl-cc/javascript::js-date-ms d)) :to-be-truthy)))

(it-sequential "js-rt-date-set-full-year-preserves-time"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; 1970-01-02T03:04:05Z
    (cl-cc/javascript::%js-date-set-full-year d 2024.0d0)
    (expect (= 3 (cl-cc/javascript::%js-date-get-hours d)) :to-be-truthy)
    (expect (= 4 (cl-cc/javascript::%js-date-get-minutes d)) :to-be-truthy)
    (expect (= 5 (cl-cc/javascript::%js-date-get-seconds d)) :to-be-truthy)))

(it-sequential "js-rt-date-set-month"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; January
    (cl-cc/javascript::%js-date-set-month d 5.0d0)       ; June (0-based)
    (expect (= 5 (cl-cc/javascript::%js-date-get-month d)) :to-be-truthy)))

(it-sequential "js-rt-date-set-date"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; day 2
    (cl-cc/javascript::%js-date-set-date d 15.0d0)
    (expect (= 15 (cl-cc/javascript::%js-date-get-date d)) :to-be-truthy)))

(it-sequential "js-rt-date-rebuild-preserves-ms"
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))  ; .999 ms
    (cl-cc/javascript::%js-date-rebuild d :sec 10)
    (expect (= 999 (cl-cc/javascript::%js-date-get-milliseconds d)) :to-be-truthy)))

(it-sequential "js-rt-date-timezone-offset-utc"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (= 0.0d0 (cl-cc/javascript::%js-date-get-timezone-offset d)) :to-be-truthy)))

(it-sequential "js-rt-date-to-utc-string"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-utc-string d) :to-equal "1970-01-02T03:04:05.000Z")))

(it-sequential "js-rt-date-to-date-string"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-date-string d) :to-equal "Fri Jan 02 1970")))

(it-sequential "js-rt-date-to-json"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (cl-cc/javascript::%js-date-to-json d) :to-equal "1970-01-02T03:04:05.000Z")))

(it-sequential "js-rt-date-value-of"
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (expect (= (cl-cc/javascript::%js-date-get-time d) (cl-cc/javascript::%js-date-value-of d)) :to-be-truthy)))

(it-sequential "js-rt-date-set-hours-preserves-ms"
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-hours d 6 7 8 1)
    (expect (cl-cc/javascript::%js-date-to-iso-string d) :to-equal "1970-01-02T06:07:08.999Z")))

(it-sequential "js-rt-date-set-minutes-preserves-ms"
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-minutes d 9 10 1)
    (expect (cl-cc/javascript::%js-date-to-iso-string d) :to-equal "1970-01-02T03:09:10.999Z")))

(it-sequential "js-rt-date-set-seconds-preserves-ms"
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-seconds d 11 1)
    (expect (cl-cc/javascript::%js-date-to-iso-string d) :to-equal "1970-01-02T03:04:11.999Z")))

;;; ─── JSON stringify ──────────────────────────────────────────────────────────

(it-sequential "js-rt-json-stringify-primitives null"
  (destructuring-bind (val expected) (list cl-cc/javascript::+js-null+ "null")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives undefined"
  (destructuring-bind (val expected) (list cl-cc/javascript::+js-undefined+ "null")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives true"
  (destructuring-bind (val expected) (list t "true")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives false"
  (destructuring-bind (val expected) (list nil "false")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives integer"
  (destructuring-bind (val expected) (list 42.0d0 "42")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives float"
  (destructuring-bind (val expected) (list 1.5d0 "1.5")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives string"
  (destructuring-bind (val expected) (list "hello" "\"hello\"")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-primitives nan"
  (destructuring-bind (val expected) (list cl-cc/javascript::*js-nan-float* "null")
    (expect (cl-cc/javascript::%js-json-stringify val) :to-equal expected)))

(it-sequential "js-rt-json-stringify-string-escapes"
  (expect (cl-cc/javascript::%js-json-stringify "line1
line2") :to-equal "\"line1\\nline2\"")
  (expect (cl-cc/javascript::%js-json-stringify "a	b") :to-equal "\"a\\tb\"")
  (expect (cl-cc/javascript::%js-json-stringify "say \"hi\"") :to-equal "\"say \\\"hi\\\"\""))

(it-sequential "js-rt-json-stringify-array"
  (let ((arr (cl-cc/javascript::%js-make-array 1.0d0 2.0d0 3.0d0)))
    (expect (cl-cc/javascript::%js-json-stringify arr) :to-equal "[1,2,3]")))

(it-sequential "js-rt-json-stringify-object"
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 1.0d0 "y" 2.0d0))
         (result (cl-cc/javascript::%js-json-stringify obj)))
    (expect (cl-cc/javascript::%js-string-includes result "\"x\":1") :to-be-truthy)
    (expect (cl-cc/javascript::%js-string-includes result "\"y\":2") :to-be-truthy)))

(it-sequential "js-rt-json-stringify-nested"
  (let* ((inner (cl-cc/javascript::%js-make-object "a" 1.0d0))
         (arr   (cl-cc/javascript::%js-make-array inner))
         (result (cl-cc/javascript::%js-json-stringify arr)))
    (expect (cl-cc/javascript::%js-string-includes result "{") :to-be-truthy)
    (expect (cl-cc/javascript::%js-string-includes result "\"a\":1") :to-be-truthy)))

(it-sequential "js-rt-json-raw-json"
  (let* ((raw (cl-cc/javascript::%js-json-raw-json "{\"x\":1}"))
         (obj (cl-cc/javascript::%js-make-object "payload" raw)))
    (expect (cl-cc/javascript::%js-json-is-raw-json raw) :to-be-truthy)
    (expect (not (cl-cc/javascript::%js-json-is-raw-json obj)) :to-be-truthy)
    (expect (cl-cc/javascript::%js-object-get-own-property-descriptor raw "__raw_json__") :to-be cl-cc/javascript::+js-undefined+)
    (expect (cl-cc/javascript::%js-json-stringify obj) :to-equal "{\"payload\":{\"x\":1}}")))

(it-sequential "js-rt-json-raw-json-invalid"
  (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/javascript::%js-json-raw-json "{bad json}")) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy)))

(it-sequential "js-rt-json-raw-json-non-string"
  (let ((%%signaled2 nil)) (handler-case (progn (cl-cc/javascript::%js-json-raw-json 42.0d0)) (error () (setf %%signaled2 t))) (expect %%signaled2 :to-be-truthy)))

;;; ─── JSON parse ──────────────────────────────────────────────────────────────

(it-sequential "js-rt-json-parse-non-undefined null-lit"
  (destructuring-bind (input) (list "null")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (expect (not (eq result cl-cc/javascript::+js-undefined+)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-non-undefined true-lit"
  (destructuring-bind (input) (list "true")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (expect (not (eq result cl-cc/javascript::+js-undefined+)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-non-undefined false-lit"
  (destructuring-bind (input) (list "false")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (expect (not (eq result cl-cc/javascript::+js-undefined+)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-non-undefined number-42"
  (destructuring-bind (input) (list "42")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (expect (not (eq result cl-cc/javascript::+js-undefined+)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-non-undefined str-hello"
  (destructuring-bind (input) (list "\"hello\"")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (expect (not (eq result cl-cc/javascript::+js-undefined+)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-null"
  (expect (eq cl-cc/javascript::+js-null+ (cl-cc/javascript::%js-json-parse "null")) :to-be-truthy))

(it-sequential "js-rt-json-parse-booleans"
  (expect (eq t   (cl-cc/javascript::%js-json-parse "true")) :to-be-truthy)
  (expect (eq nil (cl-cc/javascript::%js-json-parse "false")) :to-be-truthy))

(it-sequential "js-rt-json-parse-number integer"
  (destructuring-bind (str expected) (list "42" 42.0d0)
    (expect (= expected (cl-cc/javascript::%js-json-parse str)) :to-be-truthy)))

(it-sequential "js-rt-json-parse-number float"
  (destructuring-bind (str expected) (list "3.14" 3.14d0)
    (expect (= expected (cl-cc/javascript::%js-json-parse str)) :to-be-truthy)))

(it-sequential "js-rt-json-parse-number negative"
  (destructuring-bind (str expected) (list "-1" -1.0d0)
    (expect (= expected (cl-cc/javascript::%js-json-parse str)) :to-be-truthy)))

(it-sequential "js-rt-json-parse-string"
  (expect (cl-cc/javascript::%js-json-parse "\"hello\"") :to-equal "hello")
  (expect (cl-cc/javascript::%js-json-parse "\"a\\nb\"") :to-equal "a
b"))

(it-sequential "js-rt-json-parse-array"
  (let ((arr (cl-cc/javascript::%js-json-parse "[1,2,3]")))
    (expect (cl-cc/javascript::%js-vec-p arr) :to-be-truthy)
    (expect (= 3 (length arr)) :to-be-truthy)
    (expect (= 1.0d0 (aref arr 0)) :to-be-truthy)))

(it-sequential "js-rt-json-parse-object"
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"x\":1,\"y\":2}")))
    (expect (cl-cc/javascript::%js-ht-p obj) :to-be-truthy)
    (expect (= 1.0d0 (gethash "x" obj)) :to-be-truthy)
    (expect (= 2.0d0 (gethash "y" obj)) :to-be-truthy)))

(it-sequential "js-rt-json-parse-nested"
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"arr\":[1,2]}")))
    (let ((arr (gethash "arr" obj)))
      (expect (cl-cc/javascript::%js-vec-p arr) :to-be-truthy)
      (expect (= 2 (length arr)) :to-be-truthy))))

(it-sequential "js-rt-json-parse-whitespace number"
  (destructuring-bind (input expected) (list "  42  " 42.0d0)
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (if (stringp expected)
        (expect result :to-equal expected)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-json-parse-whitespace string"
  (destructuring-bind (input expected) (list "  \"x\"  " "x")
    (let ((result (cl-cc/javascript::%js-json-parse input)))
    (if (stringp expected)
        (expect result :to-equal expected)
        (expect (= expected result) :to-be-truthy)))))

(it-sequential "js-rt-json-parse-invalid"
  (expect (eq cl-cc/javascript::+js-undefined+ (cl-cc/javascript::%js-json-parse "NOT_JSON")) :to-be-truthy))

(it-sequential "js-rt-json-roundtrip"
  (let* ((orig     (cl-cc/javascript::%js-make-object "name" "Alice" "age" 30.0d0))
         (json     (cl-cc/javascript::%js-json-stringify orig))
         (reparsed (cl-cc/javascript::%js-json-parse json)))
    (expect (gethash "name" reparsed) :to-equal "Alice")
    (expect (= 30.0d0 (gethash "age" reparsed)) :to-be-truthy)))
