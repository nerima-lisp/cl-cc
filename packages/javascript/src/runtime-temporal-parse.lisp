;;;; packages/javascript/src/runtime-temporal-parse.lisp — Temporal parse helpers
;;;;
;;;; Separated from runtime-temporal.lisp so parsing concerns do not inflate the
;;;; core constructor file.

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Parse helpers
;;; -----------------------------------------------------------------------

(defun %temporal-parse-iso-fields (s)
  "Parse an ISO-8601 string S into (values year month day hour minute second).
Fields beyond what S contains default to 0."
  (flet ((field (start end) (if (>= (length s) end) (parse-integer s :start start :end end) 0)))
    (values (field 0 4) (field 5 7) (field 8 10) (field 11 13) (field 14 16) (field 17 19))))

(defun %js-temporal-parse-instant (str)
  (handler-case
      (multiple-value-bind (y mo d h mi s) (%temporal-parse-iso-fields (%js-to-string str))
        (%js-temporal-instant (%temporal-encode y mo d h mi s)))
    (error () (%js-temporal-instant (%temporal-now-unix-seconds)))))

(defun %js-temporal-parse-plain-date (str)
  (handler-case
      (multiple-value-bind (y mo d) (%temporal-parse-iso-fields (%js-to-string str))
        (%js-temporal-plain-date y mo d))
    (error ()
      (multiple-value-bind (s mn h d m y) (%temporal-decode (%temporal-now-unix-seconds))
        (declare (ignore s mn h))
        (%js-temporal-plain-date y m d)))))
