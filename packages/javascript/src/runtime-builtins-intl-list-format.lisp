;;;; packages/javascript/src/runtime-builtins-intl-list-format.lisp -- Intl.ListFormat

(in-package :cl-cc/javascript)

(defun %js-list-format-items (list)
  (cond
    ((%js-vec-p list)
     (loop for i below (length list)
           collect (%js-to-string (aref list i))))
    ((listp list)
     (loop for item in list collect (%js-to-string item)))
    (t (list (%js-to-string list)))))

(defun %js-list-format-final-literal (type)
  (cond ((string= type "disjunction") " or ")
        ((string= type "unit") ", ")
        (t " and ")))

(defun %js-list-format-parts (items type)
  (let ((count (length items))
        (parts nil))
    (labels ((push-element (value)
               (push (%js-make-object "type" "element" "value" value) parts))
             (push-literal (value)
               (push (%js-make-object "type" "literal" "value" value) parts)))
      (cond
        ((zerop count))
        ((= count 1) (push-element (first items)))
        ((= count 2)
         (push-element (first items))
         (push-literal (%js-list-format-final-literal type))
         (push-element (second items)))
        (t
         (loop for item in items
               for index from 0
               do (progn
                    (when (plusp index)
                      (push-literal
                       (if (= index (1- count))
                           (if (string= type "unit")
                               ", "
                               (concatenate 'string ","
                                            (%js-list-format-final-literal type)))
                           ", ")))
                    (push-element item)))))
      (nreverse parts))))

(defun %js-list-format-render (parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (write-string (gethash "value" part) out))))

(defun %js-make-intl-list-format (&optional _locale options)
  "Intl.ListFormat(locale?, options?): format English-style conjunction lists."
  (declare (ignore _locale))
  (let ((type (%js-to-string (%js-intl-option options "type" "conjunction")))
        (style (%js-to-string (%js-intl-option options "style" "long"))))
    (%js-make-object
     "format" (lambda (list)
                (%js-list-format-render
                 (%js-list-format-parts (%js-list-format-items list) type)))
     "formatToParts" (lambda (list)
                       (apply #'%js-make-array
                              (%js-list-format-parts
                               (%js-list-format-items list) type)))
     "resolvedOptions" (lambda ()
                         (%js-make-object
                          "locale" "en-US"
                          "type" type
                          "style" style)))))
