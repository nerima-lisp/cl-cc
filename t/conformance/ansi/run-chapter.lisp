;; The "nix flake check" CI job runs this file from ansi-test/ for each
;; chapter/mode check, after writing the config file read below.
(defparameter cl-user::*ansi-run-config*
  (with-open-file (stream ".cl-cc-ansi-run-config.sexp" :direction :input)
    (read stream)))

(defparameter cl-user::*ansi-chapter*
  (getf cl-user::*ansi-run-config* :chapter))

(defparameter cl-user::*ansi-mode*
  (getf cl-user::*ansi-run-config* :mode))

;; Load the ANSI regression-test framework and its test registry.
(load "gclload1.lsp")

;; Compile mode compiles each test form before running it; eval mode evaluates it.
(setf regression-test:*compile-tests* (string= cl-user::*ansi-mode* "compile"))

(load (concatenate 'string cl-user::*ansi-chapter* "/load.lsp"))

;; The upstream driver tests require both load specials to be NIL. When this
;; runner inherits either non-NIL load special, remove them from the test registry.
;; The test registry starts with a dummy cell; its CDR contains registered tests.
(defparameter cl-user::*ansi-loaded-test-count*
  (length (cdr regression-test::*entries*)))

(defparameter cl-user::*ansi-filtered-tests*
  nil)

;; rem-test returns the removed test symbol, or NIL when the name is absent.
(setf cl-user::*ansi-filtered-tests*
      (if (or *load-pathname* *load-truename*)
          (remove nil
                  (mapcar (function regression-test:rem-test)
                          (quote (cl-test::load-pathname.1
                                  cl-test::load-truename.1))))
          nil))

;; The ANSI tests use sandbox/ as their default directory for file operations.
(setf cl:*default-pathname-defaults* (truename #P"sandbox/"))

;; do-tests writes its transcript to ansi-suite.log; Nix saves this script's
;; stdout, including the ANSI markers below, as runner.log.
(regression-test:do-tests :out "ansi-suite.log")

;; Tests can carry :notes annotations; do-tests skips entries with disabled notes.
;; Executed is pass plus fail; inactive is the loaded remainder after filtering
;; and execution.
(format t
        "~&ANSI-RESULT chapter=~A mode=~A loaded=~D filtered=~D executed=~D pass=~D fail=~D inactive=~D~%"
        cl-user::*ansi-chapter*
        cl-user::*ansi-mode*
        cl-user::*ansi-loaded-test-count*
        (length cl-user::*ansi-filtered-tests*)
        (+ (length regression-test:*passed-tests*)
           (length regression-test:*failed-tests*))
        (length regression-test:*passed-tests*)
        (length regression-test:*failed-tests*)
        (- cl-user::*ansi-loaded-test-count*
           (length cl-user::*ansi-filtered-tests*)
           (length regression-test:*passed-tests*)
           (length regression-test:*failed-tests*)))

(dolist (test (reverse cl-user::*ansi-filtered-tests*))
  (format t "ANSI-FILTERED-TEST:~A~%" (symbol-name test)))

(dolist (test (reverse regression-test:*passed-tests*))
  (format t "ANSI-PASSED-TEST:~A~%" (symbol-name test)))

(dolist (test (reverse regression-test:*failed-tests*))
  (format t "ANSI-FAILED-TEST:~A~%" (symbol-name test)))
