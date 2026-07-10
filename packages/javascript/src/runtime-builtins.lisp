;;;; packages/javascript/src/runtime-builtins.lisp — JS built-in dispatch table
;;;;
;;;; *js-builtin-map* maps built-in name strings to CL functions.
;;;; This file contains the misc runtime builtin helpers used by the table.

(in-package :cl-cc/javascript)

(defun %js-make-typed-array-ctor (type-name)
  "Return a constructor lambda for the named TypedArray TYPE-NAME."
  (lambda (&optional arg) (%js-make-typed-array type-name arg)))

(defun %js-make-set-from-iterable (&optional (iter +js-undefined+))
  "Build a new JS Set, optionally seeded from ITER."
  (let ((s (%js-make-set)))
    (when (and (not (eq iter +js-undefined+))
               (not (eq iter +js-null+)))
      (%js-for-of iter (lambda (v) (%js-set-add s v))))
    s))

(defun %js-promise-try (fn &rest args)
  "ES2025 Promise.try: call FN with ARGS, wrapping synchronous throws."
  (handler-case
      (%js-promise-resolve (apply #'%js-funcall fn args))
    (js-exception (c)
      (%js-promise-reject (js-exception-value c)))))

(defun %js-math-sum-precise (iterable)
  "ES2026 Math.sumPrecise: precise sum of a numeric iterable."
  (let ((sum 0.0d0))
    (%js-for-of iterable (lambda (v)
      (incf sum (coerce (%js-to-number v) 'double-float))))
    sum))

(defun %js-error-is-error (val)
  "ES2026 Error.isError: true when VAL is an Error object."
  (and (%js-ht-p val)
       (%js-instanceof val *js-error-class*)))

(defun %js-regexp-ascii-alnum-p (ch)
  (or (and (char>= ch #\0) (char<= ch #\9))
      (and (char>= ch #\A) (char<= ch #\Z))
      (and (char>= ch #\a) (char<= ch #\z))))

(defun %js-regexp-hex2 (code out)
  (format out "\\x~2,'0X" code))

(defun %js-regexp-unicode-escape (code out)
  (format out "\\u~4,'0X" code))

(defun %js-regexp-escape (str)
  "ES2025 RegExp.escape: escape a string for literal use in a RegExp pattern."
  (with-output-to-string (out)
    (loop for ch across (%js-to-string str)
          for first = t then nil
          for code = (char-code ch)
          do (cond
               ((and first (%js-regexp-ascii-alnum-p ch))
                (%js-regexp-hex2 code out))
               ((member ch '(#\^ #\$ #\\ #\. #\* #\+ #\? #\( #\) #\[ #\] #\{ #\} #\| #\/)
                        :test #'char=)
                (write-char #\\ out)
                (write-char ch out))
               ((member ch '(#\, #\- #\= #\< #\> #\# #\& #\! #\% #\: #\; #\@ #\~ #\' #\` #\")
                        :test #'char=)
                (%js-regexp-hex2 code out))
               ((char= ch #\Newline)
                (write-string "\\n" out))
               ((char= ch #\Return)
                (write-string "\\r" out))
               ((char= ch #\Tab)
                (write-string "\\t" out))
               ((char= ch #\Page)
                (write-string "\\f" out))
               ((char= ch #\Space)
                (write-string "\\x20" out))
               ((or (< code #x20)
                    (= code #x2028)
                    (= code #x2029))
                (if (< code #x100)
                    (%js-regexp-hex2 code out)
                    (%js-regexp-unicode-escape code out)))
               (t
                (write-char ch out))))))
