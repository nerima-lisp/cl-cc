;;;; tests/conformance/format-conformance-tests.lisp
;;;; ANSI CL FORMAT Directive Conformance Tests
;;;;
;;;; Tests FORMAT directives that should work per ANSI CL. These run as
;;;; regular conformance tests; native binary parity is tracked separately.

(in-package :cl-cc/test)



;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun fmt-run (control-string &rest args)
  "Run FORMAT with CONTROL-STRING and ARGS via cl-cc pipeline. Returns output string."
  (let ((out (make-string-output-stream))
        (forms (mapcar (lambda (arg)
                         (if (or (consp arg) (and arg (not (atom arg))))
                             `(quote ,arg)
                             arg))
                       args)))
    (let ((*standard-output* out))
      (cl-cc:run-string (format nil "(format t ~S~{ ~S~})" control-string forms)))
    (get-output-stream-string out)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Basic Output Directives
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-tilde-a-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~A ~A" "hello" 42) :to-equal "hello 42")))

(it-sequential "format-tilde-s-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~S ~S" "hello" 42) :to-equal "\"hello\" 42")))

(it-sequential "format-tilde-percent-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "a~%b") :to-equal (format nil "a~%b"))))

(it-sequential "format-tilde-ampersand-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~& at start should not emit extra newline
    (expect (fmt-run "~&") :to-equal "")))

(it-sequential "format-tilde-tilde-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~~") :to-equal "~")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Numeric Directives
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-tilde-d-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~D" 42) :to-equal "42")
    (expect (fmt-run "~D" -1) :to-equal "-1")))

(it-sequential "format-tilde-b-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~B" 42) :to-equal "101010")))

(it-sequential "format-tilde-o-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~O" 42) :to-equal "52")))

(it-sequential "format-tilde-x-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~X" 42) :to-equal "2A")))

(it-sequential "format-tilde-r-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~R with no arg prints cardinal English
    (expect (fmt-run "~R" 42) :to-equal "forty-two")))

(it-sequential "format-tilde-f-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~F" 3.14) :to-equal "3.14")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Control Flow Directives
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-tilde-asterisk-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~* skips one arg, ~:* backs up
    (expect (fmt-run "~*~D" 1 2) :to-equal "2")))

(it-sequential "format-tilde-question-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~?" "~A" "hello") :to-equal "hello")))

(it-sequential "format-tilde-bracket-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~[zero~;one~;two~]" 0) :to-equal "zero")
    (expect (fmt-run "~[zero~;one~;two~]" 1) :to-equal "one")))

(it-sequential "format-tilde-brace-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~{~A~}" '("a" "b" "c")) :to-equal "abc")))

(it-sequential "format-tilde-caret-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~^ inside ~{ suppresses the trailing separator on the LAST element only:
    ;; ~{~A~^, ~} is the canonical comma-join, so this is "a, b, c" (not "ab" —
    ;; the previous expectation encoded a bug where plain ~^ always terminated,
    ;; dropping every separator and the final element).
    (expect (fmt-run "~{~A~^, ~}" '("a" "b" "c")) :to-equal "a, b, c")))

;;; ──────────────────────────────────────────────────────────────────────
;;; format nil (return as string)
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-nil-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t)
        (result (cl-cc:run-string "(format nil \"hello ~A\" \"world\")")))
    (expect result :to-equal "hello world")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Format Edge Cases
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-tilde-t-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    ;; ~4T should tab to column 4
    (expect (fmt-run "~4Tx") :to-equal "    x")))

(it-sequential "format-tab-after-rendered-directives-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~D~4T!" 12) :to-equal "12  !")
    (expect (fmt-run "~C~4T!" #\A) :to-equal "A   !")
    (expect (fmt-run "~F~6T!" 1.5) :to-equal "1.5   !")))

(it-sequential "format-tilde-p-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~D dog~:P" 1) :to-equal "1 dog")
    (expect (fmt-run "~D dog~:P" 2) :to-equal "2 dogs")))

(it-sequential "format-tilde-c-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~C" #\A) :to-equal "A")))

(it-sequential "format-case-conversion-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~(~A~)" "HELLO WORLD") :to-equal "hello world")
    (expect (fmt-run "~:(~A~)" "hello world") :to-equal "Hello World")
    (expect (fmt-run "~@(~A~)" "hello world") :to-equal "Hello world")
    (expect (fmt-run "~:@(~A~)" "hello world") :to-equal "HELLO WORLD")))

(it-sequential "format-at-modifier-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~@D" 42) :to-equal "+42")
    (expect (fmt-run "~@D" -1) :to-equal "-1")))

(it-sequential "format-colon-modifier-self-host"
  (let ((cl-cc/vm::*vm-self-host-mode* t))
    (expect (fmt-run "~:D" 1000) :to-equal "1,000")))

;;; ──────────────────────────────────────────────────────────────────────
;;; Native Binary FORMAT
;;; ──────────────────────────────────────────────────────────────────────

(it-sequential "format-native-binary-e2e"
  (let ((result (cl-cc:run-string
                 "(let ((out (make-string-output-stream)))
                    (format out \"Hello ~A!\" \"World\")
                    (get-output-stream-string out))")))
    (expect result :to-equal "Hello World!")))
