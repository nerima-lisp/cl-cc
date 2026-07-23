;;;; tests/unit/vm/format-tests.lisp — VM formatted output and reader tests

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun fmt-vm (&optional (out (make-string-output-stream)))
  "Create a vm-io-state with a string output stream for capture."
  (make-instance 'cl-cc/vm::vm-io-state :output-stream out))

(defun fmt-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

(defun fmt-capture (state)
  "Get captured output from the vm-state's output stream."
  (get-output-stream-string (cl-cc/vm::vm-output-stream state)))

;;; ─── write-to-string / princ-to-string ────────────────────────────────────

(it-sequential "fmt-write-to-string number"
  (destructuring-bind (value expected) (list 42 "42")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "fmt-write-to-string symbol"
  (destructuring-bind (value expected) (list :test ":TEST")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "fmt-write-to-string string"
  (destructuring-bind (value expected) (list "hi" "\"hi\"")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-write-to-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "fmt-princ-to-string number"
  (destructuring-bind (value expected) (list 42 "42")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ-to-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "fmt-princ-to-string string"
  (destructuring-bind (value expected) (list "hi" "hi")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ-to-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── princ / prin1 / print / terpri / fresh-line ─────────────────────────

(it-sequential "fmt-princ-values number"
  (destructuring-bind (value expected) (list 42 "42")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ :src :R1) s)
    (expect (fmt-capture s) :to-equal expected))))

(it-sequential "fmt-princ-values string"
  (destructuring-bind (value expected) (list "hello" "hello")
    (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (fmt-exec (cl-cc:make-vm-princ :src :R1) s)
    (expect (fmt-capture s) :to-equal expected))))

(it-sequential "fmt-prin1-prints-strings-with-quotes"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc:make-vm-prin1 :src :R1) s)
    (expect (fmt-capture s) :to-equal "\"hello\"")))

(it-sequential "fmt-print-inst-emits-object-representation"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 42)
    (fmt-exec (cl-cc:make-vm-print-inst :src :R1) s)
    (expect (search "42" (fmt-capture s)) :to-be-truthy)))

(it-sequential "fmt-terpri-outputs-newline"
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-terpri-inst) s)
    (expect (fmt-capture s) :to-equal (string #\Newline))))

(it-sequential "fmt-fresh-line-outputs-newline-after-prior-output"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "x")
    (fmt-exec (cl-cc:make-vm-princ :src :R1) s)
    (fmt-exec (cl-cc:make-vm-fresh-line-inst) s)
    (expect (fmt-capture s) :to-equal (concatenate 'string "x" (string #\Newline)))))

;;; ─── format ───────────────────────────────────────────────────────────────

(it-sequential "fmt-format-no-args-passes-string-through"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "hello world")))

(it-sequential "fmt-format-tilde-a-interpolates-args"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "~A is ~A")
    (cl-cc/vm::vm-reg-set s :R2 "answer")
    (cl-cc/vm::vm-reg-set s :R3 42)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2 :R3)) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "answer is 42")))

(it-sequential "fmt-format-tilde-d-formats-integer"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "count: ~D")
    (cl-cc/vm::vm-reg-set s :R2 99)
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs '(:R2)) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "count: 99")))

;;; ─── String Output Stream ────────────────────────────────────────────────

(it-sequential "fmt-string-output-stream-single-write-roundtrips"
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (expect (streamp stream) :to-be-truthy)
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "hello"))))

(it-sequential "fmt-string-output-stream-multiple-writes-accumulate"
  (let ((s (fmt-vm)))
    (fmt-exec (cl-cc:make-vm-make-string-output-stream-inst :dst :R0) s)
    (let ((stream (cl-cc/vm::vm-reg-get s :R0)))
      (cl-cc/vm::vm-reg-set s :R1 stream)
      (cl-cc/vm::vm-reg-set s :R2 "hello")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (cl-cc/vm::vm-reg-set s :R2 " world")
      (fmt-exec (cl-cc:make-vm-stream-write-string-inst :stream-reg :R1 :src :R2) s)
      (fmt-exec (cl-cc:make-vm-get-output-stream-string-inst :dst :R3 :src :R1) s)
      (expect (cl-cc/vm::vm-reg-get s :R3) :to-equal "hello world"))))

;;; ─── Reader Instructions (use cl-cc's own lexer/parser) ──────────────────

(it-sequential "fmt-read-from-string-number-sets-values-list"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "42")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 42)
    (expect (cl-cc/vm::vm-values-list s) :to-equal '(42 2))))

(it-sequential "fmt-read-from-string-symbol-is-uppercased"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (expect (symbol-name (cl-cc/vm::vm-reg-get s :R0)) :to-equal "HELLO")))

(it-sequential "fmt-read-from-string-list-produces-list-value"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "(1 2 3)")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (listp result) :to-be-truthy)
      (expect (length result) :to-equal 3))))

(it-sequential "fmt-read-from-string-empty-string-yields-nil"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "")
    (fmt-exec (cl-cc:make-vm-read-from-string-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal nil)
    (expect (cl-cc/vm::vm-values-list s) :to-equal '(nil 0))))

(it-sequential "fmt-read-sexp-from-stream"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 (make-string-input-stream "(1 2 3)"))
    (fmt-exec (cl-cc:make-vm-read-sexp-inst :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal '(1 2 3))))

;;; ─── Wave 2: ~_ directive test (partial ANSI, conditional newline) ────────

(it-sequential "fmt-directive-tilde-underscore"
  (let ((s (fmt-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "a~_b")
    (fmt-exec (cl-cc:make-vm-format-inst :dst :R0 :fmt :R1 :arg-regs nil) s)
    (let ((result (cl-cc/vm::vm-reg-get s :R0)))
      (expect (search "a" result) :to-be-truthy)
      (expect (search "b" result) :to-be-truthy))))

;;; ─── ~F / ~E / ~$ floating-point directive parameters ──────────────────────

(it-sequential "fmt-float-directive-params f-2-decimals"
  (destructuring-bind (control arg expected) (list "~,2f" 3.14159 "3.14")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params f-1-rounds"
  (destructuring-bind (control arg expected) (list "~,1f" 2.67 "2.7")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params f-width-pad"
  (destructuring-bind (control arg expected) (list "~6,2f" 3.14159 "  3.14")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params f-integer-arg"
  (destructuring-bind (control arg expected) (list "~,2f" 3 "3.00")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params f-negative"
  (destructuring-bind (control arg expected) (list "~,2f" -3.14159 "-3.14")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params f-no-params"
  (destructuring-bind (control arg expected) (list "~f" 3.14159 "3.14159")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params dollar-default"
  (destructuring-bind (control arg expected) (list "~$" 3.14159 "3.14")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-directive-params dollar-intdig"
  (destructuring-bind (control arg expected) (list "~,3$" 3.14159 "003.14")
    (expect (cl-cc/vm::%vm-format-render-to-string control (list arg)) :to-equal expected)))

(it-sequential "fmt-float-params-in-context"
  (expect (cl-cc/vm::%vm-format-render-to-string "~,2f and ~,2f" (list 1.5 2.555)) :to-equal "1.50 and 2.56"))

;;; ─── ~( ~) case conversion (ANSI) ─────────────────────────────────────────

(it-sequential "fmt-case-conversion-directive"
  (flet ((f (control &rest args)
           (cl-cc/vm::%vm-format-render-to-string control args)))
    (expect (f "~(~A~)" "HELLO WORLD") :to-equal "hello world")
    (expect (f "~:(~A~)" "hello world") :to-equal "Hello World")
    (expect (f "~@(~A~)" "hello world") :to-equal "Hello world")
    (expect (f "~:@(~A~)" "hello world") :to-equal "HELLO WORLD")
    (expect (f "~(ITEM ~D: ~A~)" 1 "ALPHA") :to-equal "item 1: alpha")))

;;; ─── ~T tabulation after rendered directives ──────────────────────────────

(it-sequential "fmt-tab-column-after-rendered-directives"
  (flet ((f (control &rest args)
           (cl-cc/vm::%vm-format-render-to-string control args)))
    (expect (f "~D~4T!" 12) :to-equal "12  !")
    (expect (f "~C~4T!" #\A) :to-equal "A   !")
    (expect (f "~F~6T!" 1.5) :to-equal "1.5   !")
    (expect (f "~R~12T!" 42) :to-equal "forty-two   !")
    (expect (f "~5<~A~>~8T!" "x") :to-equal "    x   !")))

;;; ─── ~{ ~} iteration with ~^ separator (ANSI) ──────────────────────────────

(it-sequential "fmt-iteration-caret-separator"
  (flet ((f (control arg) (cl-cc/vm::%vm-format-render-to-string control (list arg))))
    (expect (f "~{~a~^, ~}"  '(1 2 3)) :to-equal "1, 2, 3")
    (expect (f "~{~a~^, ~}"  '(9)) :to-equal "9")
    (expect (f "~{~a~^, ~}"  '()) :to-equal "")
    (expect (f "~{~a ~}"     '(1 2 3)) :to-equal "1 2 3 ")
    (expect (f "~{~a=~a ~}"  '(1 2 3 4)) :to-equal "1=2 3=4 ")
    (expect (f "[~{~a~^, ~}]" '("a" "b" "c")) :to-equal "[a, b, c]")
    (expect (f "~2{~a ~}"    '(1 2 3 4)) :to-equal "1 2 ")
    ;; ~:{ ~} iterates a list of sublists
    (expect (f "~:{(~a ~a)~}" '((1 2) (3 4))) :to-equal "(1 2)(3 4)")))

(it-sequential "fmt-caret-top-level"
  (expect (cl-cc/vm::%vm-format-render-to-string "~a~^~a" (list 1 2)) :to-equal "12")
  (expect (cl-cc/vm::%vm-format-render-to-string "~a~^~a" (list 1)) :to-equal "1"))
