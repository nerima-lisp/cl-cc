;;;; packages/javascript/src/runtime-builtins-intl-collator.lisp -- Intl.Collator

(in-package :cl-cc/javascript)

(defun %js-collator-strip-basic-accents (string)
  "Strip the common Latin-1 accents that matter for our lightweight collator."
  (with-output-to-string (out)
    (loop for ch across string
          do (write-char
              (case ch
                ((#\À #\Á #\Â #\Ã #\Ä #\Å #\à #\á #\â #\ã #\ä #\å) #\a)
                ((#\Ç #\ç) #\c)
                ((#\È #\É #\Ê #\Ë #\è #\é #\ê #\ë) #\e)
                ((#\Ì #\Í #\Î #\Ï #\ì #\í #\î #\ï) #\i)
                ((#\Ñ #\ñ) #\n)
                ((#\Ò #\Ó #\Ô #\Õ #\Ö #\Ø #\ò #\ó #\ô #\õ #\ö #\ø) #\o)
                ((#\Ù #\Ú #\Û #\Ü #\ù #\ú #\û #\ü) #\u)
                ((#\Ý #\Ÿ #\ý #\ÿ) #\y)
                (otherwise ch))
              out))))

(defun %js-collator-normalize (string sensitivity)
  (let ((s (%js-to-string string)))
    (cond
      ((string= sensitivity "base")
       (string-downcase (%js-collator-strip-basic-accents s)))
      ((string= sensitivity "accent")
       (string-downcase s))
      ((string= sensitivity "case")
       (%js-collator-strip-basic-accents s))
      (t s))))

(defun %js-collator-read-number (string start)
  (let ((end start)
        (length (length string)))
    (loop while (and (< end length) (digit-char-p (char string end)))
          do (incf end))
    (values (parse-integer string :start start :end end) end)))

(defun %js-collator-string-compare (a b numeric-p)
  (if numeric-p
      (let ((ia 0)
            (ib 0)
            (la (length a))
            (lb (length b)))
        (loop
          (cond
            ((and (= ia la) (= ib lb)) (return 0))
            ((= ia la) (return -1))
            ((= ib lb) (return 1)))
          (let ((ca (char a ia))
                (cb (char b ib)))
            (if (and (digit-char-p ca) (digit-char-p cb))
                (multiple-value-bind (na next-a) (%js-collator-read-number a ia)
                  (multiple-value-bind (nb next-b) (%js-collator-read-number b ib)
                    (cond
                      ((< na nb) (return -1))
                      ((> na nb) (return 1))
                      (t (setf ia next-a
                               ib next-b)))))
                (cond
                  ((char< ca cb) (return -1))
                  ((char> ca cb) (return 1))
                  (t (incf ia) (incf ib)))))))
      (cond ((string< a b) -1)
            ((string> a b) 1)
            (t 0))))

(defun %js-make-intl-collator (&optional _locale options)
  "Intl.Collator(locale?, options?): compare strings with basic options."
  (declare (ignore _locale))
  (let* ((sensitivity (%js-to-string
                       (%js-intl-option options "sensitivity" "variant")))
         (numeric (not (null (%js-intl-option options "numeric" nil)))))
    (%js-make-object
     "compare" (lambda (a b)
                 (%js-collator-string-compare
                  (%js-collator-normalize a sensitivity)
                  (%js-collator-normalize b sensitivity)
                  numeric))
     "resolvedOptions" (lambda ()
                         (%js-make-object
                          "locale" "en-US"
                          "usage" "sort"
                          "sensitivity" sensitivity
                          "numeric" numeric)))))
