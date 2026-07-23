(in-package :cl-cc/test)



(it-sequential "syntax-rules-source-scaffold-loads"
  :timeout
  5
  (expect (asdf:find-system :cl-cc-expand nil) :to-be-truthy))

(it-sequential "syntax-rules-define-syntax-basic-expansion"
  :timeout
  10
  (cl-cc/expand:define-syntax sr-when
    (cl-cc/expand:syntax-rules ()
      ((sr-when test body |...|) (if test (progn body |...|)))))
  (multiple-value-bind (expanded expanded-p)
      (cl-cc/expand:our-macroexpand-1 '(sr-when ok (print 1) (print 2)))
    (expect expanded-p :to-be-truthy)
    (expect expanded :to-equal '(if ok (progn (print 1) (print 2))))))

(it-sequential "syntax-rules-hygiene-introduces-uninterned-symbol"
  :timeout
  10
  (cl-cc/expand:define-syntax sr-capture-safe
    (cl-cc/expand:syntax-rules ()
      ((sr-capture-safe expr) (let ((tmp expr)) tmp))))
  (let ((expanded (cl-cc/expand:our-macroexpand-1 '(sr-capture-safe tmp))))
    (expect (car expanded) :to-be 'let)
    (expect (null (symbol-package (caar (second expanded)))) :to-be-truthy)))

(it-sequential "syntax-rules-rejects-bare-ellipsis-pattern"
  :timeout
  10
  (cl-cc/expand:define-syntax sr-no-bare-ellipsis
    (cl-cc/expand:syntax-rules ()
      ((sr-no-bare-ellipsis |...| rest) rest)))
  (multiple-value-bind (expanded expanded-p)
      (cl-cc/expand:our-macroexpand-1 '(sr-no-bare-ellipsis a b c))
    (expect expanded-p :to-be-falsy)
    (expect expanded :to-equal '(sr-no-bare-ellipsis a b c))))

(it-sequential "syntax-case-partial-supports-guarded-pattern"
  :timeout
  10
  (let ((result (cl-cc/expand:syntax-case '(twice 21) ()
                  ((twice x) (numberp x) `(+ ,x ,x)))))
    (expect result :to-equal '(+ 21 21))))
