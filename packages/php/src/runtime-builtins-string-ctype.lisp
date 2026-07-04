;;;; packages/php/src/runtime-builtins-string-ctype.lisp -- ctype PHP string predicates

(in-package :cl-cc/php)

;;; ─── ctype_* character classification (PHP ext/ctype) ────────────────────────

(defun %php-ctype-alpha (text)
  "PHP ctype_alpha: check if all chars are alphabetic."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'alpha-char-p s))))

(defun %php-ctype-digit (text)
  "PHP ctype_digit: check if all chars are decimal digits."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'digit-char-p s))))

(defun %php-ctype-alnum (text)
  "PHP ctype_alnum: check if all chars are alphanumeric."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'alphanumericp s))))

(defun %php-ctype-space (text)
  "PHP ctype_space: check if all chars are whitespace."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Null))) s))))

(defun %php-ctype-upper (text)
  "PHP ctype_upper: check if all chars are uppercase."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (alpha-char-p c) (upper-case-p c))) s))))

(defun %php-ctype-lower (text)
  "PHP ctype_lower: check if all chars are lowercase."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (alpha-char-p c) (lower-case-p c))) s))))

(defun %php-ctype-punct (text)
  "PHP ctype_punct: check if all chars are punctuation (visible non-alphanumeric)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (graphic-char-p c) (not (alphanumericp c)) (not (char= c #\Space)))) s))))

(defun %php-ctype-graph (text)
  "PHP ctype_graph: check if all chars are visible (non-space graphic)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (and (graphic-char-p c) (not (char= c #\Space)))) s))))

(defun %php-ctype-print (text)
  "PHP ctype_print: check if all chars are printable (including space)."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every #'graphic-char-p s))))

(defun %php-ctype-xdigit (text)
  "PHP ctype_xdigit: check if all chars are valid hexadecimal."
  (let ((s (%php-stringify text)))
    (and (> (length s) 0)
         (every (lambda (c) (digit-char-p c 16)) s))))
