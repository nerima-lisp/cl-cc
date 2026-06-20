;;;; packages/javascript/tests/js-runtime-date-json-tests.lisp
;;;;
;;;; Temporal helper functions, Date.prototype, JSON stringify, JSON parse.
;;;;
;;;; Depends on: js-runtime-core-tests.lisp (%jr-arr)

(in-package :cl-cc/test)
(in-suite cl-cc-javascript-suite)

;;; ─── Temporal helper functions ───────────────────────────────────────────────

(deftest-each js-rt-temporal-pad
  "%temporal-pad zero-pads integers to the specified width."
  :cases (("year-4"   2025 4 "2025")
          ("month-2"  3    2 "03")
          ("day-2"    15   2 "15")
          ("narrow"   9    1 "9"))
  (n width expected)
  (assert-string= expected (cl-cc/javascript::%temporal-pad n width)))

(deftest-each js-rt-temporal-3way-compare
  "%temporal-3way-compare returns -1/0/1 for ordered numeric comparison."
  :cases (("less"    1 2 -1.0d0)
          ("equal"   5 5  0.0d0)
          ("greater" 9 3  1.0d0))
  (a b expected)
  (assert-= expected (cl-cc/javascript::%temporal-3way-compare a b)))

(deftest-each js-rt-temporal-parse-iso-fields
  "%temporal-parse-iso-fields decomposes an ISO-8601 datetime string."
  :cases (("full"      "2025-06-13T14:30:00" 2025 6  13 14 30 0)
          ("date-only" "2025-01-01"           2025 1   1  0  0 0))
  (s exp-y exp-mo exp-d exp-h exp-mi exp-s)
  (multiple-value-bind (y mo d h mi s) (cl-cc/javascript::%temporal-parse-iso-fields s)
    (assert-= exp-y  y)
    (assert-= exp-mo mo)
    (assert-= exp-d  d)
    (assert-= exp-h  h)
    (assert-= exp-mi mi)
    (assert-= exp-s  s)))

(deftest-each js-rt-temporal-duration-to-seconds
  "%temporal-duration-to-seconds converts duration hash-tables to total seconds."
  :cases (("one-hour"   "hours"   1 3600)
          ("one-minute" "minutes" 1 60)
          ("one-second" "seconds" 1 1)
          ("one-day"    "days"    1 86400))
  (unit n expected)
  (let ((dur (cl-cc/javascript::%js-make-object unit (coerce n 'double-float))))
    (assert-= expected (cl-cc/javascript::%temporal-duration-to-seconds dur))))

(deftest js-rt-temporal-parse-time-fields
  "%temporal-parse-time-fields decomposes an HH:MM:SS string."
  (multiple-value-bind (h m s) (cl-cc/javascript::%temporal-parse-time-fields "14:30:05")
    (assert-= 14 h)
    (assert-= 30 m)
    (assert-=  5 s)))

(deftest js-rt-temporal-now-object
  "Temporal.Now exposes callable constructors for the simplified runtime types."
  (let* ((now (cl-cc/javascript::%js-temporal-now))
         (instant (funcall (gethash "instant" now)))
         (plain-datetime (funcall (gethash "plainDateTimeISO" now)))
         (plain-date (funcall (gethash "plainDateISO" now)))
         (plain-time (funcall (gethash "plainTimeISO" now)))
         (zoned-datetime (funcall (gethash "zonedDateTimeISO" now))))
    (assert-string= "UTC" (funcall (gethash "timeZoneId" now)))
    (assert-string= "Temporal.Instant" (gethash "__type__" instant))
    (assert-string= "Temporal.PlainDateTime" (gethash "__type__" plain-datetime))
    (assert-string= "Temporal.PlainDate" (gethash "__type__" plain-date))
    (assert-string= "Temporal.PlainTime" (gethash "__type__" plain-time))
    (assert-string= "Temporal.ZonedDateTime" (gethash "__type__" zoned-datetime))))

(deftest js-rt-temporal-instant-methods
  "Temporal.Instant methods cover formatting, arithmetic, comparison, and timezone conversion."
  (let* ((instant (cl-cc/javascript::%js-temporal-instant 0))
         (duration (cl-cc/javascript::%js-temporal-duration :hours 1 :minutes 30))
         (added (funcall (gethash "add" instant) duration))
         (subtracted (funcall (gethash "subtract" added) duration))
         (zoned (funcall (gethash "toZonedDateTimeISO" instant))))
    (assert-string= "1970-01-01T00:00:00Z" (funcall (gethash "toString" instant)))
    (assert-= 5400.0d0 (gethash "epochSeconds" added))
    (assert-= 0.0d0 (gethash "epochSeconds" subtracted))
    (assert-true (funcall (gethash "equals" instant) subtracted))
    (assert-= -1.0d0 (funcall (gethash "compare" instant) added))
    (assert-string= "UTC" (gethash "timeZoneId" zoned))))

(deftest js-rt-temporal-plain-date-methods
  "Temporal.PlainDate methods cover formatting and stable conversion helpers."
  (let* ((date (cl-cc/javascript::%js-temporal-plain-date 2025 6 18))
         (datetime (funcall (gethash "toPlainDateTime" date))))
    (assert-string= "2025-06-18" (funcall (gethash "toString" date)))
    (assert-= 3.0d0 (gethash "dayOfWeek" date))
    (assert-string= "2025-06-18T00:00:00" (funcall (gethash "toString" datetime)))))

(deftest js-rt-temporal-plain-date-extended
  "Temporal.PlainDate covers the optional time argument plus add/subtract/compare."
  (let* ((date (cl-cc/javascript::%js-temporal-plain-date 2025 6 18))
         (time (cl-cc/javascript::%js-temporal-plain-time 7 8 9))
         (duration (cl-cc/javascript::%js-temporal-duration :days 1))
         (datetime (funcall (gethash "toPlainDateTime" date) time))
         (added (funcall (gethash "add" date) duration))
         (subtracted (funcall (gethash "subtract" added) duration)))
    (assert-string= "Temporal.PlainDateTime" (gethash "__type__" datetime))
    (assert-= 2025.0d0 (gethash "year" datetime))
    (assert-= 6.0d0 (gethash "month" datetime))
    (assert-= 18.0d0 (gethash "day" datetime))
    (assert-= 7.0d0 (gethash "hour" datetime))
    (assert-= 8.0d0 (gethash "minute" datetime))
    (assert-= 9.0d0 (gethash "second" datetime))
    (assert-string= "2025-06-19" (funcall (gethash "toString" added)))
    (assert-string= "2025-06-18" (funcall (gethash "toString" subtracted)))
    (assert-= -1.0d0 (funcall (gethash "compare" date) added))
    (assert-true (funcall (gethash "equals" date)
                          (cl-cc/javascript::%js-temporal-plain-date 2025 6 18)))))

(deftest js-rt-temporal-plain-time-methods
  "Temporal.PlainTime methods cover formatting, arithmetic, and equality."
  (let* ((time (cl-cc/javascript::%js-temporal-plain-time 23 59 30))
         (duration (cl-cc/javascript::%js-temporal-duration :seconds 90))
         (added (funcall (gethash "add" time) duration)))
    (assert-string= "23:59:30" (funcall (gethash "toString" time)))
    (assert-string= "00:01:00" (funcall (gethash "toString" added)))
    (assert-true (funcall (gethash "equals" added)
                          (cl-cc/javascript::%js-temporal-plain-time 0 1 0)))))

(deftest js-rt-temporal-plain-datetime-methods
  "Temporal.PlainDateTime methods cover formatting and stable projection helpers."
  (let* ((datetime (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56))
         (plain-date (funcall (gethash "toPlainDate" datetime)))
         (plain-time (funcall (gethash "toPlainTime" datetime))))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" datetime)))
    (assert-string= "2025-06-18" (funcall (gethash "toString" plain-date)))
    (assert-string= "12:34:56" (funcall (gethash "toString" plain-time)))))

(deftest js-rt-temporal-plain-datetime-extended
  "Temporal.PlainDateTime covers add/subtract/toInstant/equals branches."
  (let* ((datetime (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56))
         (duration (cl-cc/javascript::%js-temporal-duration :seconds 4))
         (added (funcall (gethash "add" datetime) duration))
         (subtracted (funcall (gethash "subtract" added) duration))
         (instant (funcall (gethash "toInstant" datetime))))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" datetime)))
    (assert-string= "2025-06-18T12:35:00" (funcall (gethash "toString" added)))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" subtracted)))
    (assert-string= "Temporal.Instant" (gethash "__type__" instant))
    (assert-true (funcall (gethash "equals" datetime)
                          (cl-cc/javascript::%js-temporal-plain-datetime 2025 6 18 12 34 56)))))

(deftest js-rt-temporal-zoned-datetime-methods
  "Temporal.ZonedDateTime methods cover formatting and conversion helpers."
  (let* ((zoned (cl-cc/javascript::%js-temporal-zoned-datetime 2025 6 18 12 34 56 "UTC"))
         (plain-datetime (funcall (gethash "toPlainDateTime" zoned)))
         (plain-date (funcall (gethash "toPlainDate" zoned)))
         (plain-time (funcall (gethash "toPlainTime" zoned)))
         (instant (funcall (gethash "toInstant" zoned))))
    (assert-string= "2025-06-18T12:34:56+00:00[UTC]" (funcall (gethash "toString" zoned)))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" plain-datetime)))
    (assert-string= "2025-06-18" (funcall (gethash "toString" plain-date)))
    (assert-string= "12:34:56" (funcall (gethash "toString" plain-time)))
    (assert-string= "Temporal.Instant" (gethash "__type__" instant))))

(deftest js-rt-temporal-duration-methods
  "Temporal.Duration methods cover formatting, sign, absolute, negation, and total seconds."
  (let* ((duration (cl-cc/javascript::%js-temporal-duration :years 1 :months 2 :weeks 3 :days 4 :hours 5 :minutes 6 :seconds 7))
         (negative (cl-cc/javascript::%js-temporal-duration :hours -2 :minutes -30))
         (abs-duration (funcall (gethash "abs" negative)))
         (negated (funcall (gethash "negated" duration))))
    (assert-string= "P1Y2M3W4DT5H6M7S" (funcall (gethash "toString" duration)))
    (assert-= 1.0d0 (gethash "sign" duration))
    (assert-= -1.0d0 (gethash "sign" negative))
    (assert-string= "P0Y0M0W0DT2H30M0S" (funcall (gethash "toString" abs-duration)))
    (assert-string= "P-1Y-2M-3W-4DT-5H-6M-7S" (funcall (gethash "toString" negated)))
    (assert-= 38995567.0d0 (funcall (gethash "total" duration)))))

(deftest js-rt-temporal-normalization-and-fallbacks
  "Temporal normalization and parse fallback branches cover non-number inputs and invalid ISO strings."
  (let* ((duration (cl-cc/javascript::%js-temporal-duration :hours -2.7 :minutes nil))
         (instant (cl-cc/javascript::%js-temporal-parse-instant "("))
         (plain-date (cl-cc/javascript::%js-temporal-parse-plain-date "(")))
    (assert-= 3 (cl-cc/javascript::%temporal-normalize-number 3.9))
    (assert-= 7 (cl-cc/javascript::%temporal-normalize-number nil 7))
    (multiple-value-bind (s mn h d m y dow)
        (cl-cc/javascript::%temporal-decode
         (cl-cc/javascript::%temporal-encode 2025 6 18 12 34 56))
      (assert-= 56 s)
      (assert-= 34 mn)
      (assert-= 12 h)
      (assert-= 18 d)
      (assert-= 6 m)
      (assert-= 2025 y)
      (assert-true (numberp dow)))
    (assert-string= "P0Y0M0W0DT-2H0M0S" (funcall (gethash "toString" duration)))
    (assert-= -1.0d0 (gethash "sign" duration))
    (assert-= -7200.0d0 (funcall (gethash "total" duration)))
    (assert-string= "Temporal.Instant" (gethash "__type__" instant))
    (assert-string= "Temporal.PlainDate" (gethash "__type__" plain-date))))

(deftest js-rt-temporal-year-month-and-month-day
  "Temporal.PlainYearMonth and Temporal.PlainMonthDay stringify as simplified ISO fragments."
  (let ((year-month (cl-cc/javascript::%js-temporal-plain-year-month 2025 6))
        (month-day (cl-cc/javascript::%js-temporal-plain-month-day 6 18)))
    (assert-string= "2025-06" (funcall (gethash "toString" year-month)))
    (assert-string= "--06-18" (funcall (gethash "toString" month-day)))))

(deftest js-rt-temporal-global-factories
  "The Temporal global object covers constructor, from, and compare branches for simplified factories."
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
    (assert-string= "Temporal.Instant" (gethash "__type__" instant-now))
    (assert-= 1.0d0 (gethash "epochSeconds" instant-from-string))
    (assert-= 2.0d0 (gethash "epochSeconds" instant-from-ms))
    (assert-= 3.0d0 (gethash "epochSeconds" instant-from-us))
    (assert-= 4.0d0 (gethash "epochSeconds" instant-from-ns))
    (assert-= -1.0d0 (funcall (gethash "compare" instant-global) instant-from-string instant-from-ms))
    (assert-string= "2025-06-18" (funcall (gethash "toString" plain-date-from-object)))
    (assert-string= "2025-06-19" (funcall (gethash "toString" plain-date-from-string)))
    (assert-string= "07:08:09" (funcall (gethash "toString" plain-time-from-object)))
    (assert-string= "10:11:12" (funcall (gethash "toString" plain-time-from-string)))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" plain-datetime-from-object)))
    (assert-string= "2025-06-18T12:34:56" (funcall (gethash "toString" plain-datetime-from-string)))
    (assert-string= "1970-01-01T00:00:00+00:00[UTC]" (funcall (gethash "toString" zoned-from-call)))
    (assert-string= "2025-06-18T12:34:56+00:00[UTC]" (funcall (gethash "toString" zoned-from-string)))
    (assert-string= "2025-06-18T12:34:56+00:00[UTC]" (funcall (gethash "toString" zoned-from-object)))
    (assert-string= "P1Y2M0W3DT4H5M6S" (funcall (gethash "toString" duration-from-call)))
    (assert-string= "P1Y2M0W3DT4H5M6S" (funcall (gethash "toString" duration-from-object)))
    (assert-string= "P0Y0M0W0DT0H0M0S" (funcall (gethash "toString" duration-from-fallback)))
    (assert-= -1.0d0 (funcall (gethash "compare" duration-global) duration-from-fallback duration-from-object))
    (assert-string= "2025-06" (funcall (gethash "toString" plain-year-month-from-object)))
    (assert-string= "2000-01" (funcall (gethash "toString" plain-year-month-from-fallback)))
    (assert-string= "--06-18" (funcall (gethash "toString" plain-month-day-from-object)))
    (assert-string= "--01-01" (funcall (gethash "toString" plain-month-day-from-fallback)))))

;;; ─── Date.prototype ──────────────────────────────────────────────────────────

(deftest js-rt-date-now
  "Date.now() returns a positive integer (milliseconds since Unix epoch)."
  (let ((t1 (cl-cc/javascript::%js-date-now))
        (t2 (cl-cc/javascript::%js-date-now)))
    (assert-true (integerp t1))
    (assert-true (>= t2 t1))))

(deftest js-rt-date-make-date-no-args
  "%js-make-date with no args returns a js-date struct."
  (let ((d (cl-cc/javascript::%js-make-date)))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-true (integerp (cl-cc/javascript::js-date-ms d)))))

(deftest js-rt-date-make-date-from-ms
  "%js-make-date from a millisecond value stores the ms directly."
  (let ((d (cl-cc/javascript::%js-make-date 1000000.0d0)))
    (assert-= 1000000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-make-date-copy
  "%js-make-date from another Date copies the ms."
  (let* ((orig (cl-cc/javascript::%js-make-date 42000.0d0))
         (copy (cl-cc/javascript::%js-make-date orig)))
    (assert-= 42000 (cl-cc/javascript::js-date-ms copy))))

(deftest js-rt-date-make-date-string
  "%js-make-date parses string inputs."
  (let ((d (cl-cc/javascript::%js-make-date "1970-01-01T01:00:00")))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-= 3600000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-make-date-null
  "%js-make-date treats null like an omitted argument."
  (let ((d (cl-cc/javascript::%js-make-date cl-cc/javascript::+js-null+)))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-true (integerp (cl-cc/javascript::js-date-ms d)))))

(deftest js-rt-date-parse-string-date-only
  "%js-date-parse-string parses YYYY-MM-DD to ms."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01")))
    (assert-= 0 ms)))

(deftest js-rt-date-parse-string-datetime
  "%js-date-parse-string parses YYYY-MM-DDTHH:MM:SS."
  (let ((ms (cl-cc/javascript::%js-date-parse-string "1970-01-01T01:00:00")))
    (assert-= 3600000 ms)))

(deftest js-rt-date-parse-string-trims-spaces
  "%js-date-parse-string trims surrounding spaces before parsing."
  (let ((ms (cl-cc/javascript::%js-date-parse-string " 1970-01-01T01:02:03 ")))
    (assert-= 3723000 ms)))

(deftest js-rt-date-parse-string-error
  "%js-date-parse-string falls back to now on invalid input."
  (let ((result (cl-cc/javascript::%js-date-parse-string "not-a-date")))
    (assert-true (integerp result))))

(deftest js-rt-date-make-date-fallback
  "%js-make-date falls back to now for non-date, non-number, non-string inputs."
  (let ((d (cl-cc/javascript::%js-make-date (make-hash-table :test #'equal))))
    (assert-true (cl-cc/javascript::js-date-p d))
    (assert-true (integerp (cl-cc/javascript::js-date-ms d)))))

(deftest js-rt-date-setters-with-undefined-optionals
  "Date setters preserve omitted optional args when passed +js-undefined+."
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-full-year d 1970 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-month d 0 cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-hours d 0 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-minutes d 0 cl-cc/javascript::+js-undefined+ cl-cc/javascript::+js-undefined+)
    (cl-cc/javascript::%js-date-set-seconds d 0 cl-cc/javascript::+js-undefined+)
    (assert-= 0 (cl-cc/javascript::js-date-ms d))))

;;; 97445000 ms = 1970-01-02T03:04:05.000Z
(deftest js-rt-date-getters
  "Date.prototype getters return correct decomposed fields for a fixed epoch."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-full-year d))
    (assert-= 1970 (cl-cc/javascript::%js-date-get-utc-full-year d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-month d))      ; January = 0
    (assert-= 2    (cl-cc/javascript::%js-date-get-date d))
    (assert-= 3    (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4    (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5    (cl-cc/javascript::%js-date-get-seconds d))
    (assert-= 0    (cl-cc/javascript::%js-date-get-milliseconds d))))

(deftest js-rt-date-get-time
  "Date.prototype.getTime returns ms as double-float."
  (let ((d (cl-cc/javascript::%js-make-date 12345)))
    (assert-= 12345.0d0 (cl-cc/javascript::%js-date-get-time d))))

(deftest js-rt-date-to-iso-string
  "toISOString formats as YYYY-MM-DDTHH:MM:SS.mmmZ."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970-01-02T03:04:05.000Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-iso-string-with-ms
  "toISOString includes sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445123)))
    (assert-string= "1970-01-02T03:04:05.123Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-to-local-date-string
  "toLocaleDateString returns YYYY/MM/DD."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970/01/02" (cl-cc/javascript::%js-date-to-local-date-string d))))

(deftest js-rt-date-to-time-string
  "toTimeString returns HH:MM:SS GMT+0000 (...)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "03:04:05 GMT+0000 (Coordinated Universal Time)"
                    (cl-cc/javascript::%js-date-to-time-string d))))

(deftest js-rt-date-set-time
  "setTime updates ms and returns the new value."
  (let ((d (cl-cc/javascript::%js-make-date 0)))
    (cl-cc/javascript::%js-date-set-time d 5000)
    (assert-= 5000 (cl-cc/javascript::js-date-ms d))))

(deftest js-rt-date-set-full-year-preserves-time
  "setFullYear preserves the existing time components (was bug: zeroed them)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; 1970-01-02T03:04:05Z
    (cl-cc/javascript::%js-date-set-full-year d 2024.0d0)
    (assert-= 3 (cl-cc/javascript::%js-date-get-hours d))
    (assert-= 4 (cl-cc/javascript::%js-date-get-minutes d))
    (assert-= 5 (cl-cc/javascript::%js-date-get-seconds d))))

(deftest js-rt-date-set-month
  "setMonth changes the month (JS 0-based)."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; January
    (cl-cc/javascript::%js-date-set-month d 5.0d0)       ; June (0-based)
    (assert-= 5 (cl-cc/javascript::%js-date-get-month d))))

(deftest js-rt-date-set-date
  "setDate changes the day of month."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))  ; day 2
    (cl-cc/javascript::%js-date-set-date d 15.0d0)
    (assert-= 15 (cl-cc/javascript::%js-date-get-date d))))

(deftest js-rt-date-rebuild-preserves-ms
  "%js-date-rebuild preserves sub-second milliseconds."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))  ; .999 ms
    (cl-cc/javascript::%js-date-rebuild d :sec 10)
    (assert-= 999 (cl-cc/javascript::%js-date-get-milliseconds d))))

(deftest js-rt-date-timezone-offset-utc
  "getTimezoneOffset returns 0 in the UTC-only date model."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-= 0.0d0 (cl-cc/javascript::%js-date-get-timezone-offset d))))

(deftest js-rt-date-to-utc-string
  "toUTCString mirrors the ISO string rendering."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970-01-02T03:04:05.000Z"
                    (cl-cc/javascript::%js-date-to-utc-string d))))

(deftest js-rt-date-to-date-string
  "toDateString formats the weekday and calendar date."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "Fri Jan 02 1970"
                    (cl-cc/javascript::%js-date-to-date-string d))))

(deftest js-rt-date-to-json
  "toJSON returns the ISO string for valid dates."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-string= "1970-01-02T03:04:05.000Z"
                    (cl-cc/javascript::%js-date-to-json d))))

(deftest js-rt-date-value-of
  "valueOf aliases getTime."
  (let ((d (cl-cc/javascript::%js-make-date 97445000)))
    (assert-= (cl-cc/javascript::%js-date-get-time d)
              (cl-cc/javascript::%js-date-value-of d))))

(deftest js-rt-date-set-hours-preserves-ms
  "setHours preserves sub-second milliseconds and ignores the ms argument."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-hours d 6 7 8 1)
    (assert-string= "1970-01-02T06:07:08.999Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-set-minutes-preserves-ms
  "setMinutes preserves sub-second milliseconds and ignores the ms argument."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-minutes d 9 10 1)
    (assert-string= "1970-01-02T03:09:10.999Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

(deftest js-rt-date-set-seconds-preserves-ms
  "setSeconds preserves sub-second milliseconds and ignores the ms argument."
  (let ((d (cl-cc/javascript::%js-make-date 97445999)))
    (cl-cc/javascript::%js-date-set-seconds d 11 1)
    (assert-string= "1970-01-02T03:04:11.999Z"
                    (cl-cc/javascript::%js-date-to-iso-string d))))

;;; ─── JSON stringify ──────────────────────────────────────────────────────────

(deftest-each js-rt-json-stringify-primitives
  "JSON.stringify handles JS primitive values."
  :cases (("null"        cl-cc/javascript::+js-null+       "null")
          ("undefined"   cl-cc/javascript::+js-undefined+  "null")
          ("true"        t                                  "true")
          ("false"       nil                                "false")
          ("integer"     42.0d0                            "42")
          ("float"       1.5d0                             "1.5")
          ("string"      "hello"                           "\"hello\"")
          ("nan"         cl-cc/javascript::*js-nan-float*  "null"))
  (val expected)
  (assert-string= expected (cl-cc/javascript::%js-json-stringify val)))

(deftest js-rt-json-stringify-string-escapes
  "JSON.stringify escapes special characters in strings."
  (assert-string= "\"line1\\nline2\"" (cl-cc/javascript::%js-json-stringify "line1
line2"))
  (assert-string= "\"a\\tb\"" (cl-cc/javascript::%js-json-stringify "a	b"))
  (assert-string= "\"say \\\"hi\\\"\"" (cl-cc/javascript::%js-json-stringify "say \"hi\"")))

(deftest js-rt-json-stringify-array
  "JSON.stringify serializes JS arrays."
  (let ((arr (cl-cc/javascript::%js-make-array 1.0d0 2.0d0 3.0d0)))
    (assert-string= "[1,2,3]" (cl-cc/javascript::%js-json-stringify arr))))

(deftest js-rt-json-stringify-object
  "JSON.stringify serializes JS objects."
  (let* ((obj    (cl-cc/javascript::%js-make-object "x" 1.0d0 "y" 2.0d0))
         (result (cl-cc/javascript::%js-json-stringify obj)))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"x\":1"))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"y\":2"))))

(deftest js-rt-json-stringify-nested
  "JSON.stringify handles nested objects and arrays."
  (let* ((inner (cl-cc/javascript::%js-make-object "a" 1.0d0))
         (arr   (cl-cc/javascript::%js-make-array inner))
         (result (cl-cc/javascript::%js-json-stringify arr)))
    (assert-true (cl-cc/javascript::%js-string-includes result "{"))
    (assert-true (cl-cc/javascript::%js-string-includes result "\"a\":1"))))

(deftest js-rt-json-raw-json
  "JSON.rawJSON produces a wrapper that stringify emits verbatim."
  (let* ((raw (cl-cc/javascript::%js-json-raw-json "{\"x\":1}"))
         (obj (cl-cc/javascript::%js-make-object "payload" raw)))
    (assert-true (cl-cc/javascript::%js-json-is-raw-json raw))
    (assert-true (not (cl-cc/javascript::%js-json-is-raw-json obj)))
    (assert-eq cl-cc/javascript::+js-undefined+
               (cl-cc/javascript::%js-object-get-own-property-descriptor raw "__raw_json__"))
    (assert-string= "{\"payload\":{\"x\":1}}"
                    (cl-cc/javascript::%js-json-stringify obj))))

(deftest js-rt-json-raw-json-invalid
  "JSON.rawJSON rejects text that is not a complete JSON value."
  (assert-signals error
    (cl-cc/javascript::%js-json-raw-json "{bad json}")))

(deftest js-rt-json-raw-json-non-string
  "JSON.rawJSON rejects non-string input."
  (assert-signals error
    (cl-cc/javascript::%js-json-raw-json 42.0d0)))

;;; ─── JSON parse ──────────────────────────────────────────────────────────────

(deftest-each js-rt-json-parse-non-undefined
  "JSON.parse returns non-undefined for valid JSON literals."
  :cases (("null-lit"  "null")
          ("true-lit"  "true")
          ("false-lit" "false")
          ("number-42" "42")
          ("str-hello" "\"hello\""))
  (input)
  (let ((result (cl-cc/javascript::%js-json-parse input)))
    (assert-true (not (eq result cl-cc/javascript::+js-undefined+)))))

(deftest js-rt-json-parse-null
  "JSON.parse(\"null\") returns the JS null sentinel."
  (assert-true (eq cl-cc/javascript::+js-null+ (cl-cc/javascript::%js-json-parse "null"))))

(deftest js-rt-json-parse-booleans
  "JSON.parse handles true and false."
  (assert-true (eq t   (cl-cc/javascript::%js-json-parse "true")))
  (assert-true (eq nil (cl-cc/javascript::%js-json-parse "false"))))

(deftest-each js-rt-json-parse-number
  "JSON.parse converts numeric strings to double-float."
  :cases (("integer"   "42"   42.0d0)
          ("float"     "3.14" 3.14d0)
          ("negative"  "-1"   -1.0d0))
  (str expected)
  (assert-= expected (cl-cc/javascript::%js-json-parse str)))

(deftest js-rt-json-parse-string
  "JSON.parse converts quoted strings."
  (assert-string= "hello" (cl-cc/javascript::%js-json-parse "\"hello\""))
  (assert-string= "a
b" (cl-cc/javascript::%js-json-parse "\"a\\nb\"")))

(deftest js-rt-json-parse-array
  "JSON.parse builds an adjustable vector for arrays."
  (let ((arr (cl-cc/javascript::%js-json-parse "[1,2,3]")))
    (assert-true (cl-cc/javascript::%js-vec-p arr))
    (assert-= 3 (length arr))
    (assert-= 1.0d0 (aref arr 0))))

(deftest js-rt-json-parse-object
  "JSON.parse builds a hash-table for objects."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"x\":1,\"y\":2}")))
    (assert-true (cl-cc/javascript::%js-ht-p obj))
    (assert-= 1.0d0 (gethash "x" obj))
    (assert-= 2.0d0 (gethash "y" obj))))

(deftest js-rt-json-parse-nested
  "JSON.parse handles nested structures."
  (let ((obj (cl-cc/javascript::%js-json-parse "{\"arr\":[1,2]}")))
    (let ((arr (gethash "arr" obj)))
      (assert-true (cl-cc/javascript::%js-vec-p arr))
      (assert-= 2 (length arr)))))

(deftest-each js-rt-json-parse-whitespace
  "JSON.parse skips leading/trailing whitespace."
  :cases (("number"  "  42  "    42.0d0)
          ("string"  "  \"x\"  "  "x"))
  (input expected)
  (let ((result (cl-cc/javascript::%js-json-parse input)))
    (if (stringp expected)
        (assert-string= expected result)
        (assert-= expected result))))

(deftest js-rt-json-parse-invalid
  "JSON.parse returns +js-undefined+ on invalid input."
  (assert-true (eq cl-cc/javascript::+js-undefined+ (cl-cc/javascript::%js-json-parse "NOT_JSON"))))

(deftest js-rt-json-roundtrip
  "stringify then parse round-trips a JS object."
  (let* ((orig     (cl-cc/javascript::%js-make-object "name" "Alice" "age" 30.0d0))
         (json     (cl-cc/javascript::%js-json-stringify orig))
         (reparsed (cl-cc/javascript::%js-json-parse json)))
    (assert-string= "Alice" (gethash "name" reparsed))
    (assert-= 30.0d0 (gethash "age" reparsed))))
