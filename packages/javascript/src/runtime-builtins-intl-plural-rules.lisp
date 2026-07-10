;;;; packages/javascript/src/runtime-builtins-intl-plural-rules.lisp -- Intl.PluralRules

(in-package :cl-cc/javascript)

(defun %js-plural-rules-ordinal-category (n)
  (let* ((integer (truncate (abs n)))
         (mod-10 (mod integer 10))
         (mod-100 (mod integer 100)))
    (cond
      ((and (= mod-10 1) (/= mod-100 11)) "one")
      ((and (= mod-10 2) (/= mod-100 12)) "two")
      ((and (= mod-10 3) (/= mod-100 13)) "few")
      (t "other"))))

(defun %js-plural-rules-cardinal-category (n)
  (let ((abs-n (abs n)))
    (if (= abs-n 1)
        "one"
        "other")))

(defun %js-make-intl-plural-rules (&optional _locale options)
  "Intl.PluralRules(locale?, options?): select basic English plural categories."
  (declare (ignore _locale))
  (let ((type (%js-to-string (%js-intl-option options "type" "cardinal"))))
    (labels ((select-category (n)
               (let ((number (%js-to-number n)))
                 (if (string= type "ordinal")
                     (%js-plural-rules-ordinal-category number)
                     (%js-plural-rules-cardinal-category number)))))
      (%js-make-object
       "select" #'select-category
       "selectRange" (lambda (_start end)
                       (declare (ignore _start))
                       (select-category end))
       "resolvedOptions" (lambda ()
                           (%js-make-object
                            "locale" "en-US"
                            "type" type
                            "pluralCategories"
                            (if (string= type "ordinal")
                                (%js-make-array "one" "two" "few" "other")
                                (%js-make-array "one" "other"))))))))
