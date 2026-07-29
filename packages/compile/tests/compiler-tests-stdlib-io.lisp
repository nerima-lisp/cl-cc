;;;; compiler-tests-stdlib-io.lisp — Self-hosting eval, string/char, I/O, HOF, array, sort, coerce tests
(in-package :cl-cc/test)


;;; Self-Hosting Eval Tests — verify macro expansion runs through our-eval

(it-sequential "selfhost-macro-eval-fn-integration"
  (expect (functionp cl-cc:*macro-eval-fn*) :to-be-truthy)
  (expect (funcall cl-cc:*macro-eval-fn* '(+ 1 2)) :to-be 3))

(deftest-compile selfhost-macro-declare-arith-alist
  "Self-hosting macros, declare forms, arithmetic ops, and alist/list ops return expected numeric results."
  :cases (("defmacro-basic"      49 "(progn (defmacro sh-sq (x) (list (quote *) x x)) (sh-sq 7))")
          ("defmacro-with-args"  10 "(progn (defmacro sh-add3 (a b c) (list (quote +) a b c)) (sh-add3 2 3 5))")
          ("defmacro-quasiquote" 42 "(progn (defmacro sh-unless (test &body body) (list (quote if) test nil (cons (quote progn) body))) (sh-unless nil 42))")
          ("macrolet-basic"      42 "(macrolet ((add1 (x) (list (quote +) x 1))) (add1 41))")
          ("macrolet-nested"      8 "(macrolet ((dbl (x) (list (quote +) x x))) (macrolet ((quad (x) (list (quote dbl) (list (quote dbl) x)))) (quad 2)))")
          ("macrolet-body-form"   6 "(macrolet ((triple (x) (list (quote *) x 3))) (triple 2))")
          ("declare-ignore"      42 "(let ((x 42)) (declare (ignore x)) x)")
          ("declare-in-defun"    10 "(progn (defun my-decl-fn (x) (declare (type integer x)) x) (my-decl-fn 10))")
          ("declare-type"         3 "(let ((x 1) (y 2)) (declare (type integer x y)) (+ x y))")
          ("arith-mod"            1 "(mod 7 3)")
          ("arith-rem"            1 "(rem 7 3)")
          ("arith-truncate"       2 "(truncate 7 3)")
          ("arith-floor"          2 "(floor 7 3)")
          ("arith-ceiling"        3 "(ceiling 7 3)")
          ("arith-abs"            5 "(abs (- 0 5))")
          ("arith-min"            2 "(min 5 2)")
          ("arith-max"            5 "(max 5 2)")
          ("alist-assoc-found"    2 "(cdr (assoc 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3))))")
          ("alist-acons"         42 "(cdr (car (acons 'x 42 nil)))")
          ("alist-nconc-len"      4 "(length (nconc (list 1 2) (list 3 4)))")
          ("alist-copy-list-len"  3 "(length (copy-list (list 1 2 3)))")
          ;; LISTP and ATOM answer T/NIL like every other ANSI type predicate.
          ("alist-listp-list"     t "(listp (list 1 2))")
          ("alist-listp-nil"      t "(listp nil)")
          ("alist-atom-true"      t "(atom 42)")
          ("alist-atom-false"     nil "(atom (cons 1 2))"))
  )

(deftest-compile compile-equal-ops
  "equal returns truthy for matching structures, NIL for different structures."
  :cases (("list-match"   t   "(equal (list 1 2 3) (list 1 2 3))")
          ("list-no-match" nil "(equal (list 1 2) (list 1 3))")
          ("subst-match"  t   "(equal (subst 'x 'a (list 'a 'b 'a)) (list 'x 'b 'x))"))
  )

(deftest-compile compile-assoc-and-string-coerce
  "assoc-miss returns nil; string coerces a symbol to its name string."
  :cases (("assoc-miss"    nil     "(assoc 'z (list (cons 'a 1) (cons 'b 2)))")
          ("string-coerce" "HELLO" "(string 'hello)"))
  )

;;; String/Character Builtin Tests

(deftest-compile compile-char-ops
  "char/code-char/char-upcase/char-downcase return the expected character."
  :cases (("char-access"   #\e "(char \"hello\" 1)")
          ("code-char"     #\A "(code-char 65)")
          ("char-upcase"   #\A "(char-upcase #\\a)")
          ("char-downcase" #\a "(char-downcase #\\A)"))
  )

(deftest-compile compile-char-numeric
  "char-code/digit-char-p return numeric values; predicate results use CL booleans."
  :cases (("char-code"      65 "(char-code #\\A)")
          ("char=-true"      1 "(char= #\\a #\\a)")
          ("digit-char-p"    5 "(digit-char-p #\\5)")
          ("alpha-char-p"    t "(alpha-char-p #\\z)")
          ("upper-case-p"    t "(upper-case-p #\\A)")
          ("lower-case-p"    t "(lower-case-p #\\a)")
          ("stringp-true"    t "(stringp \"hello\")")
          ("stringp-false"   nil "(stringp 42)")
          ;; CHAR= keeps its 1: it is a comparison under the :CHAR-CMP
          ;; convention, not a type predicate, so it still answers the VM's
          ;; boolean. DIGIT-CHAR-P keeps its 5 because ANSI returns the weight.
          ("characterp-true" t "(characterp #\\a)"))
  )

(deftest-compile compile-string-char-utils
  "digit-char-p/nil, string-trim, parse-integer, and subseq work correctly."
  :cases (("digit-char-non-digit" nil    "(digit-char-p #\\a)")
          ("string-trim"         "hello" "(string-trim \" \" \"  hello  \")")
          ("parse-integer"        42     "(parse-integer \"42\")")
          ("subseq-with-end"     "ell"   "(subseq \"hello\" 1 4)")
          ("subseq-no-end"       "llo"   "(subseq \"hello\" 2)"))
  )

(deftest-compile compile-search
  "search returns the position of the pattern or nil when not found (ANSI CL)."
  :cases (("found"     2   "(search \"ll\" \"hello\")")
          ("not-found" nil "(search \"xyz\" \"hello\")")
          ("from-end"  3   "(search '(2 3) '(1 2 3 2 3) :from-end t)")
          ("test-not"  0   "(search '(1) '(2 1) :test-not #'=)")
          ("bounds"    3   "(search '(2 3) '(0 2 3 2 3) :start2 2)")
          ("key"       1   "(search '(\"bb\" \"ccc\") '(\"a\" \"bb\" \"ccc\") :key #'length)")
          ("dynamic-nil-key" 1 "(let ((k nil)) (search '(2) '(1 2 3) :key k))")
          ("dynamic-nil-end" 1 "(let ((e nil)) (search '(2 3) '(1 2 3) :end1 e :end2 e))"))
  )

;;; I/O and Format Tests

(deftest-compile io-string-format-equal
  "write-to-string and format nil return the expected string representations."
  :cases (("write-integer"   "42"          "(write-to-string 42)")
          ("write-symbol"    "HELLO"       "(write-to-string 'hello)")
           ("format-string"   "hello world" "(format nil \"hello ~A\" \"world\")")
           ("format-number"   "x=42"        "(format nil \"x=~A\" 42)")
           ("format-no-args"  "hello"       "(format nil \"hello\")")
           ("format-multi"    "1 + 2 = 3"   "(format nil \"~A + ~A = ~A\" 1 2 3)")
           ("format-write"    "\"x\""      "(format nil \"~S\" \"x\")")
           ("format-tilde"    "cost ~ 5"    "(format nil \"cost ~~ ~A\" 5)")
           ("format-newline"  (format nil "line~%next") "(format nil \"line~%next\")"))
  )

(deftest-compile io-format-evaluation-order
  "format evaluates destination/control/arguments left-to-right even when static lowering is used."
  :cases (("extra-args-evaluated" 2 "(let ((x 0)) (format nil \"~A\" (setq x (+ x 1)) (setq x (+ x 1))) x)")
          ("stream-before-static-arg" (list "arg" "dest")
           "(let ((order nil) (s (make-string-output-stream))) (format (progn (setq order (cons \"dest\" order)) s) \"~A\" (progn (setq order (cons \"arg\" order)) \"x\")) order)")
          ("control-before-dynamic-arg" (list "arg" "control")
           "(let ((order nil)) (format nil (progn (setq order (cons \"control\" order)) \"~A\") (progn (setq order (cons \"arg\" order)) \"x\")) order)"))
  )

(it-sequential "io-print-returns-value princ"
  (destructuring-bind (expected form) (list 42 "(princ 42)")
    (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string form) :to-equal expected))))

(it-sequential "io-print-returns-value prin1"
  (destructuring-bind (expected form) (list 42 "(prin1 42)")
    (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string form) :to-equal expected))))

(it-sequential "io-print-returns-value print"
  (destructuring-bind (expected form) (list 42 "(print 42)")
    (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string form) :to-equal expected))))

(it-sequential "io-returns-nil terpri"
  (destructuring-bind (expected form) (list nil "(terpri)")
    (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string form) :to-equal expected))))

(it-sequential "io-returns-nil format-t"
  (destructuring-bind (expected form) (list nil "(format t \"hello\")")
    (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string form) :to-equal expected))))

(deftest-compile io-format-directives
  "format directives for iteration, conditionals, and character output."
  :cases (("iteration"          "1, 2, 3" "(format nil \"~{~A~^, ~}\" (list 1 2 3))")
          ("conditional-index"  "one"     "(format nil \"~[zero~;one~;two~:;many~]\" 1)")
          ("conditional-default" "many"   "(format nil \"~[zero~;one~;two~:;many~]\" 99)"))
  :stdlib t)

(it-sequential "io-write-char-basic"
  (let ((*standard-output* (make-broadcast-stream)))
    (expect (run-string "(write-char #\\A)") :to-equal #\A)))

;;; Higher-Order Function Tests (require stdlib)

(deftest-compile stdlib-hof-list-result
  "HOFs return the correct list result when applied to lists."
  :cases (("mapcar"        '(2 4 6)   "(mapcar (lambda (x) (+ x x)) (list 1 2 3))")
          ("remove-if"     '(1 2)     "(remove-if (lambda (x) (> x 2)) (list 1 2 3 4 5))")
          ("remove-if-not" '(3 4 5)   "(remove-if-not (lambda (x) (> x 2)) (list 1 2 3 4 5))"))
  :stdlib t)

(deftest-compile stdlib-hof-numeric
  "HOFs return numeric results for reduce, find-if, count-if."
  :cases (("reduce"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
           ("find-if"   4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
           ("count-if"  2 "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))"))
  :stdlib t)

(it-sequential "stdlib-hof-truthy every-all"
  (destructuring-bind (form) (list "(every (lambda (x) (> x 0)) (list 1 2 3))")
    (expect (not (null (run-string form :stdlib t))) :to-be-truthy)))

(it-sequential "stdlib-hof-truthy some-found"
  (destructuring-bind (form) (list "(some (lambda (x) (> x 2)) (list 1 2 3))")
    (expect (not (null (run-string form :stdlib t))) :to-be-truthy)))

(deftest-compile stdlib-hof-nil
  "every/some/find-if/mapcar return nil when there is no match or empty input."
  :cases (("mapcar-empty"  nil "(mapcar (lambda (x) x) nil)")
           ("find-if-miss"  nil "(find-if (lambda (x) (> x 10)) (list 1 2 3))")
           ("every-fail"    nil "(every (lambda (x) (> x 2)) (list 1 2 3))")
           ("some-miss"     nil "(some (lambda (x) (> x 10)) (list 1 2 3))"))
  :stdlib t)

;;; With-Output-To-String Tests

(deftest-compile compile-with-output-to-string
  "with-output-to-string, make-string-output-stream, and get-output-stream-string produce the expected string."
  :cases (("empty"              ""            "(with-output-to-string (s))")
          ("format"             "hello world" "(with-output-to-string (s) (format s \"hello ~A\" \"world\"))")
          ("multi-write"        "ab"          "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))")
          ("multi-format"       "x=1 y=2"     "(with-output-to-string (s) (format s \"x=~A\" 1) (format s \" y=~A\" 2))")
          ("string-output-stream" "foo"       "(let ((s (make-string-output-stream))) (write-string \"foo\" s) (get-output-stream-string s))"))
  )

;;; Array/Vector Tests

(it-sequential "compile-make-array-basic"
  (let ((result (run-string "(make-array 5)")))
    (expect (vectorp result) :to-be-truthy)
    (expect (= 5 (length result)) :to-be-truthy)))

(deftest-compile compile-array-numeric
  "aref/setf aref/vector-push-extend return the correct numeric values."
  :cases (("aref-init" 0  "(let ((a (make-array 3))) (aref a 0))")
          ("setf-aref" 42 "(let ((a (make-array 3))) (setf (aref a 1) 42) (aref a 1))")
          ("vec-push"  10 "(let ((v (make-array 0 :fill-pointer t :adjustable t))) (vector-push-extend 10 v) (aref v 0))"))
  )

(deftest-compile compile-vectorp
  "vectorp answers T for a vector and NIL for anything else.

It used to answer the VM's internal 1/0, which read as true to IF but as *false*
to NOT -- so (not (vectorp 42)) was NIL. Type predicates now convert to a
Common Lisp boolean at the call site; see EMIT-BUILTIN-UNARY-PREDICATE."
  :cases (("vector"  t "(vectorp (make-array 3))")
          ("non-vec" nil "(vectorp 42)")))

(deftest-compile compile-type-predicates-compose-with-not
  "Type predicates compose with NOT and NULL through a register, not just folded."
  ;; The predicates answered the VM's internal 1/0, and 0 is false to IF but
  ;; *true* to NOT, which follows ANSI. Every one of these inverted. Constant
  ;; folding hid it: the optimizer folds the instruction with the host's own
  ;; predicate, so the same call answered NIL at top level and 0 through a
  ;; register -- hence the LAMBDA, which keeps the value in a register.
  :cases (("not-consp"   t "((lambda (x) (not (consp x))) 1)")
          ("not-numberp" t "((lambda (x) (not (numberp x))) \"s\")")
          ("not-stringp" nil "((lambda (x) (not (stringp x))) \"s\")")
          ("and-chain"   t "((lambda (x y) (and (not (consp y)) (eql x y))) 1 1)")
          ("tree-equal"  t "(tree-equal '(1 (2 3)) '(1 (2 3)))"))
  )

;;; Sort Tests

(deftest-compile compile-sort
  "sort produces the correctly ordered list."
  :cases (("ascending"  '(1 1 2 3 4 5 6 9) "(sort (list 3 1 4 1 5 9 2 6) (lambda (a b) (< a b)))")
           ("descending" '(5 3 1)            "(sort (list 3 1 5) (lambda (a b) (> a b)))")
           ("single"     '(42)               "(sort (list 42) (lambda (a b) (< a b)))")
           ("empty"      nil                 "(sort nil (lambda (a b) (< a b)))"))
  :stdlib t)

(deftest-compile compile-sort-vector
  "sort and stable-sort handle vectors destructively and return the sorted vector."
  :cases (("ascending"
            '(1 1 3 4 5)
            "(let ((v (vector 3 1 4 1 5)))
               (let ((r (sort v (lambda (a b) (< a b)))))
                 (list (aref r 0) (aref r 1) (aref r 2) (aref r 3) (aref r 4))))")
          ("descending"
            '(5 4 3 1 1)
            "(let ((v (vector 3 1 4 1 5)))
               (sort v (lambda (a b) (> a b)))
               (list (aref v 0) (aref v 1) (aref v 2) (aref v 3) (aref v 4)))")
          ("identity"
            t
            "(let ((v (vector 2 1)))
               (not (eql 0 (eq v (sort v (lambda (a b) (< a b)))))))")
          ("empty"
            0
            "(let ((v (vector)))
               (length (sort v (lambda (a b) (< a b)))))")
          ("stable-key"
            '(:b :d :a :c)
            "(let ((v (vector (cons 2 :a) (cons 1 :b) (cons 2 :c) (cons 1 :d))))
               (stable-sort v (lambda (a b) (< a b)) :key #'car)
               (list (cdr (aref v 0)) (cdr (aref v 1)) (cdr (aref v 2)) (cdr (aref v 3))))"))
  :stdlib t)

;;; Coerce Tests

(it-sequential "compile-coerce"
  (expect (run-string "(coerce (list #\\a #\\b #\\c) 'string)") :to-equal "abc")
  (expect (run-string "(coerce \"hi\" 'list)") :to-equal '(#\h #\i))
  (expect (= 65 (run-string "(char-code (character \"A\"))")) :to-be-truthy)
  (expect (= 66 (run-string "(char-code (coerce \"B\" 'character))")) :to-be-truthy)
  (expect (= 90 (run-string "(let ((ty 'character)) (char-code (coerce \"Z\" ty)))" :stdlib t)) :to-be-truthy)
  (expect (= 90 (run-string "(char-code (character 'z))")) :to-be-truthy)
  (let ((result (run-string "(coerce (list 1 2 3) 'vector)")))
    (expect (vectorp result) :to-be-truthy)
    (expect (= 3 (length result)) :to-be-truthy)))
