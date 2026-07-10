;;;; packages/javascript/src/runtime-temporal-duration.lisp — Temporal.Duration
;;;;
;;;; Separated from runtime-temporal.lisp so the core datetime constructors stay
;;;; focused on the immutable Temporal types themselves.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Temporal.Duration
;;; -----------------------------------------------------------------------

;;; Seconds-per-unit table for %temporal-duration-to-seconds.
(defparameter *%temporal-unit-seconds*
  '(("years" . 31557600) ("months" . 2629800) ("weeks" . 604800)
    ("days"  . 86400)    ("hours"  . 3600)    ("minutes" . 60) ("seconds" . 1))
  "Alist mapping Temporal duration field names to their second equivalents.")

(defun %temporal-duration-to-seconds (duration)
  "Convert a Temporal.Duration to total seconds."
  (if (%js-ht-p duration)
      (loop for (key . scale) in *%temporal-unit-seconds*
            sum (* (or (gethash key duration) 0) scale))
      0))

(defun %js-temporal-duration (&key (years 0) (months 0) (weeks 0) (days 0)
                                    (hours 0) (minutes 0) (seconds 0)
                                    (milliseconds 0) (microseconds 0) (nanoseconds 0))
  (let ((y (%temporal-normalize-number years))
        (mo (%temporal-normalize-number months))
        (w (%temporal-normalize-number weeks))
        (d (%temporal-normalize-number days))
        (h (%temporal-normalize-number hours))
        (mn (%temporal-normalize-number minutes))
        (s (%temporal-normalize-number seconds)))
    (declare (ignore milliseconds microseconds nanoseconds))
    (%js-make-object
     "__type__"   "Temporal.Duration"
     "years"      (coerce y 'double-float)
     "months"     (coerce mo 'double-float)
     "weeks"      (coerce w 'double-float)
     "days"       (coerce d 'double-float)
     "hours"      (coerce h 'double-float)
     "minutes"    (coerce mn 'double-float)
     "seconds"    (coerce s 'double-float)
     "milliseconds" 0.0d0
     "microseconds" 0.0d0
     "nanoseconds"  0.0d0
     "sign"       (if (or (plusp y) (plusp mo) (plusp w) (plusp d) (plusp h) (plusp mn) (plusp s)) 1.0d0 -1.0d0)
     "toString"   (lambda ()
                    (format nil "P~AY~AM~AW~ADT~AH~AM~AS"
                            y mo w d h mn s))
     "abs"        (lambda () (%js-temporal-duration :years (abs y) :months (abs mo) :weeks (abs w)
                                                    :days (abs d) :hours (abs h) :minutes (abs mn) :seconds (abs s)))
     "negated"    (lambda () (%js-temporal-duration :years (- y) :months (- mo) :weeks (- w)
                                                    :days (- d) :hours (- h) :minutes (- mn) :seconds (- s)))
     "total"      (lambda (&optional opts)
                    (declare (ignore opts))
                    (coerce (%temporal-duration-to-seconds (%js-make-object
                                                            "years" y "months" mo "weeks" w "days" d
                                                            "hours" h "minutes" mn "seconds" s)) 'double-float)))))

;;; Temporal.PlainYearMonth / Temporal.PlainMonthDay remain in runtime-temporal.lisp.
