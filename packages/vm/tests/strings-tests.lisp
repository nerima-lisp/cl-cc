;;;; tests/unit/vm/strings-tests.lisp — VM String & Character Operations Unit Tests
;;;;
;;;; Tests for string comparison, manipulation, character access, and symbol
;;;; instructions executed via the VM.

(in-package :cl-cc/test)



(it-sequential "strings-cow-copy-set-isolates-shared-backing"
  (let* ((original (cl-cc/vm::vm-string-copy (copy-seq "abc")))
         (copy (cl-cc/vm::vm-string-copy original)))
    (cl-cc/vm::vm-string-set-char copy 0 #\z)
    (expect (cl-cc/vm::vm-string-materialize original) :to-equal "abc")
    (expect (cl-cc/vm::vm-string-materialize copy) :to-equal "zbc")))

(it-sequential
  "strings-cow-subseq-mutation-detaches-displaced-alias"
  (let* ((parent (cl-cc/vm::vm-string-copy (copy-seq "abcdef")))
         (slice (cl-cc/vm::vm-string-subseq parent 2 5))
         (alias (cl-cc/vm::vm-string-copy slice)))
    (expect (cl-cc/vm::vm-string-materialize slice) :to-equal "cde")
    (expect (cl-cc/vm::vm-cow-string-start slice) :to-equal 2)
    (expect
      (eq
        (cl-cc/vm::vm-cow-string-backing parent)
        (cl-cc/vm::vm-cow-string-backing slice))
      :to-be-truthy)
    (expect
      (eq
        (cl-cc/vm::vm-cow-string-backing slice)
        (cl-cc/vm::vm-cow-string-backing alias))
      :to-be-truthy)
    (cl-cc/vm::vm-string-set-char slice 1 #\X)
    (expect (cl-cc/vm::vm-string-materialize parent) :to-equal "abcdef")
    (expect (cl-cc/vm::vm-string-materialize alias) :to-equal "cde")
    (expect (cl-cc/vm::vm-string-materialize slice) :to-equal "cXe")
    (expect (cl-cc/vm::vm-cow-string-start slice) :to-equal 0)
    (expect
      (eq
        (cl-cc/vm::vm-cow-string-backing slice)
        (cl-cc/vm::vm-cow-string-backing alias))
      :to-be-falsy)
    (cl-cc/vm::string-freeze alias)
    (signals error (cl-cc/vm::vm-string-set-char alias 0 #\Y))))

(it-sequential "strings-taint-mark-untaint"
  (let ((s (copy-seq "unsafe")))
    (cl-cc/vm::taint-mark s)
    (expect (cl-cc/vm::tainted-p s) :to-be-truthy)
    (cl-cc/vm::untaint s)
    (expect (cl-cc/vm::tainted-p s) :to-be-falsy)))

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun str-vm ()
  "Create a minimal vm-state for string tests."
  (make-instance 'cl-cc/vm::vm-io-state))

(defun str-exec (inst state)
  "Execute a single instruction against STATE."
  (cl-cc/vm::execute-instruction inst state 0 (make-hash-table :test #'equal)))

;;; ─── String Comparisons (case-sensitive) ──────────────────────────────────

(it-sequential "str-comparison-truthy equal"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string= "hello" "hello")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-comparison-truthy less-than"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string< "abc" "abd")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-comparison-truthy greater-than"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string> "xyz" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-comparison-truthy less-equal"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string<= "abc" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-comparison-truthy greater-equal"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string>= "xyz" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-equal-false"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 "world")
    (str-exec (cl-cc:make-vm-string= :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (null (cl-cc/vm::vm-reg-get s :R0)) :to-be-truthy)))

;;; ─── String Comparisons (case-insensitive) ────────────────────────────────

(it-sequential "str-insensitive-comparison-truthy equal"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-equal "Hello" "hello")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-insensitive-comparison-truthy lessp"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-lessp "ABC" "abd")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-insensitive-comparison-truthy greaterp"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-greaterp "XYZ" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-insensitive-comparison-truthy not-equal"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-not-equal "abc" "xyz")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-insensitive-comparison-truthy not-greaterp"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-not-greaterp "abc" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

(it-sequential "str-insensitive-comparison-truthy not-lessp"
  (destructuring-bind (ctor str1 str2) (list #'cl-cc:make-vm-string-not-lessp "abc" "abc")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 str1)
    (cl-cc/vm::vm-reg-set s :R2 str2)
    (str-exec (funcall ctor :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-be-truthy))))

;;; ─── String Length ────────────────────────────────────────────────────────

(it-sequential "str-length non-empty"
  (destructuring-bind (input expected) (list "hello" 5)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (cl-cc:make-vm-string-length :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-length empty"
  (destructuring-bind (input expected) (list "" 0)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (cl-cc:make-vm-string-length :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Character Access ─────────────────────────────────────────────────────

(it-sequential "str-char-at"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 0)
    (str-exec (cl-cc:make-vm-char :dst :R0 :string :R1 :index :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal #\h)))

(it-sequential "str-char-encoding char-code"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-char-code #\A 65)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-char-encoding code-char"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-code-char 65 #\A)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Character Comparisons ────────────────────────────────────────────────

(it-sequential "char-comparison-returns-1 equal"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char= #\a #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 less-than"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char< #\a #\b)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 greater"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char> #\z #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 le-equal"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char<= #\a #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 ge-greater"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char>= #\z #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 not-equal"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char/= #\a #\b)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

(it-sequential "char-comparison-returns-1 ci-equal"
  (destructuring-bind (ctor char1 char2) (list #'cl-cc:make-vm-char-equal #\A #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 char1)
    (cl-cc/vm::vm-reg-set s :R2 char2)
    (str-exec (funcall ctor :dst :R0 :char1 :R1 :char2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 1))))

;;; ─── String Manipulation ──────────────────────────────────────────────────

(it-sequential "str-case-conversion upcase"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-upcase "hello" "HELLO")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-case-conversion downcase"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-downcase "HELLO" "hello")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-capitalize"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (str-exec (cl-cc:make-vm-string-capitalize :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "Hello World")))

(it-sequential "str-concatenate"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello")
    (cl-cc/vm::vm-reg-set s :R2 " world")
    (str-exec (cl-cc:make-vm-concatenate :dst :R0 :str1 :R1 :str2 :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "hello world")))

(it-sequential "str-subseq"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "hello world")
    (cl-cc/vm::vm-reg-set s :R2 6)
    (cl-cc/vm::vm-reg-set s :R3 11)
    (str-exec (cl-cc:make-vm-subseq :dst :R0 :string :R1 :start :R2 :end :R3) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal "world")))

;;; ─── String Trim ──────────────────────────────────────────────────────────

(it-sequential "str-trim-directions both"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-trim "  hello  " "hello")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 " ")
    (cl-cc/vm::vm-reg-set s :R2 input)
    (str-exec (funcall ctor :dst :R0 :char-bag :R1 :string :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-trim-directions left"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-left-trim "  hello  " "hello  ")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 " ")
    (cl-cc/vm::vm-reg-set s :R2 input)
    (str-exec (funcall ctor :dst :R0 :char-bag :R1 :string :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-trim-directions right"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-string-right-trim "  hello  " "  hello")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 " ")
    (cl-cc/vm::vm-reg-set s :R2 input)
    (str-exec (funcall ctor :dst :R0 :char-bag :R1 :string :R2) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── String Search ────────────────────────────────────────────────────────

(it-sequential "str-search-hit-miss hit"
  (destructuring-bind (pattern string expected) (list "world" "hello world" 6)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 pattern)
    (cl-cc/vm::vm-reg-set s :R2 string)
    (cl-cc/vm::vm-reg-set s :R3 0)
    (str-exec (cl-cc:make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-search-hit-miss miss"
  (destructuring-bind (pattern string expected) (list "xyz" "hello world" -1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 pattern)
    (cl-cc/vm::vm-reg-set s :R2 string)
    (cl-cc/vm::vm-reg-set s :R3 0)
    (str-exec (cl-cc:make-vm-search-string :dst :R0 :pattern :R1 :string :R2 :start :R3) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Make String ──────────────────────────────────────────────────────────

(it-sequential "str-make-string default-space"
  (destructuring-bind (len init-char expected) (list 3 nil "   ")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 len)
    (when init-char
      (cl-cc/vm::vm-reg-set s :R2 init-char))
    (str-exec (cl-cc:make-vm-make-string :dst :R0 :src :R1 :char (if init-char :R2 nil)) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "str-make-string with-char"
  (destructuring-bind (len init-char expected) (list 4 #\x "xxxx")
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 len)
    (when init-char
      (cl-cc/vm::vm-reg-set s :R2 init-char))
    (str-exec (cl-cc:make-vm-make-string :dst :R0 :src :R1 :char (if init-char :R2 nil)) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Character Predicates ─────────────────────────────────────────────────

(it-sequential "char-predicates-true digit"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-digit-char-p #\5 5)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-predicates-true alpha"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-alpha-char-p #\a 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-predicates-true upper-case"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-upper-case-p #\A 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-predicates-true lower-case"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-lower-case-p #\a 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-case-conversion upcase"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-char-upcase #\a #\A)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-case-conversion downcase"
  (destructuring-bind (ctor input expected) (list #'cl-cc:make-vm-char-downcase #\A #\a)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 input)
    (str-exec (funcall ctor :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-alphanumericp alpha"
  (destructuring-bind (ch expected) (list #\a 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 ch)
    (str-exec (cl-cc:make-vm-alphanumericp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "char-alphanumericp non-alnum"
  (destructuring-bind (ch expected) (list #\! 0)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 ch)
    (str-exec (cl-cc:make-vm-alphanumericp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Stringp / Characterp Predicates ──────────────────────────────────────

(it-sequential "stringp string"
  (destructuring-bind (value expected) (list "hello" 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-stringp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "stringp non-string"
  (destructuring-bind (value expected) (list 42 0)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-stringp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "characterp char"
  (destructuring-bind (value expected) (list #\a 1)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-characterp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

(it-sequential "characterp non-char"
  (destructuring-bind (value expected) (list 42 0)
    (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 value)
    (str-exec (cl-cc:make-vm-characterp :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal expected))))

;;; ─── Parse Integer ────────────────────────────────────────────────────────

(it-sequential "parse-integer-basic"
  (let ((s (str-vm)))
    (cl-cc/vm::vm-reg-set s :R1 "42")
    (str-exec (cl-cc:make-vm-parse-integer :dst :R0 :src :R1) s)
    (expect (cl-cc/vm::vm-reg-get s :R0) :to-equal 42)))
