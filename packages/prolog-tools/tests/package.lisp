;;;; packages/prolog-tools/tests/package.lisp - cl-weave test package
;;;;
;;;; CL-WEAVE:DESCRIBE shadows CL:DESCRIBE, so it must be imported with
;;;; :SHADOWING-IMPORT-FROM rather than :USE — the same convention cl-weave
;;;; uses for its own test package (tests/package.lisp in cl-weave itself).

(defpackage :cl-cc/prolog-tools-tests
  (:use :cl :cl-cc/prolog-tools)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave
   #:it
   #:it-property
   #:expect
   #:gen-list
   #:gen-tuple
   #:gen-member
   #:gen-integer
   #:run-all
   #:run-mutations
   #:mutation-summary
   #:assert-mutation-score
   #:*snapshot-directory*
   #:with-snapshot-updates))
