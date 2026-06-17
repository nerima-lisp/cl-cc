;;;; tests/unit/compile/stdlib-source-tests.lisp — Standard Library Source tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %count-substring (needle haystack)
  (let ((count 0)
        (start 0)
        (step (length needle)))
    (loop for pos = (search needle haystack :start2 start)
          while pos do
            (incf count)
            (setf start (+ pos step)))
    count))

(deftest stdlib-source-string-present
  "The extracted standard library source string exists."
  (assert-true (stringp cl-cc::*standard-library-source*))
  (assert-true (> (length cl-cc::*standard-library-source*) 0)))

(deftest-each stdlib-source-contains-key-defuns
  "The standard library source keeps representative top-level definitions."
  :cases (("mapcar"               "(defun mapcar")
          ("reduce"               "(defun reduce")
          ("class-precedence-list" "(defun class-precedence-list")
          ("proclaim"             "(defun proclaim")
          ("short-site-name"      "(defun short-site-name")
          ("long-site-name"       "(defun long-site-name")
          ("with-compilation-unit" "(defmacro with-compilation-unit")
          ("get-setf-expansion"   "(defun get-setf-expansion")
          ("eval-when"            "(define-expander-for eval-when")
          ("symbol-macrolet"      "(define-expander-for symbol-macrolet")
          ("define-symbol-macro"  "(define-expander-for define-symbol-macro")
          ("string-left-trim"     "(defun string-left-trim")
          ("string-right-trim"    "(defun string-right-trim")
          ("string-upcase"        "(defun string-upcase")
          ("string-downcase"      "(defun string-downcase")
          ("string-capitalize"    "(defun string-capitalize")
          ("nstring-upcase"       "(defun nstring-upcase")
          ("nstring-downcase"     "(defun nstring-downcase")
          ("nstring-capitalize"   "(defun nstring-capitalize")
          ("pathname-match-p"     "(defun pathname-match-p")
          ("translate-pathname"   "(defun translate-pathname")
          ("set"                  "(defun set (sym val) (setf (symbol-value sym) val) val)")
          ("set-fdefinition"      "(defun set-fdefinition"))
  (needle)
  (assert-true (search needle cl-cc::*standard-library-source*)))

(deftest stdlib-source-has-many-defun-forms
  "The source string still contains the bulk of the stdlib as defun forms."
  (assert-true (> (%count-substring "(defun " cl-cc::*standard-library-source*) 20)))
