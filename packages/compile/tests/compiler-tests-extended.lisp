(in-package :cl-cc/test)


;;; Self-Hosting Smoke Test: Mini-Optimizer with Labels + HOFs

(it-sequential "self-host-optimizer-pipeline"
  (expect (= 30 (run-string *self-host-optimizer-pipeline-program* :stdlib t)) :to-be-truthy))

;;; Self-Hosting Integration: Macro Expander + Type Checker

(it-sequential "self-host-macro-system-full"
  (expect (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
               (string-downcase (format nil "~S" (run-string *self-host-macro-system-program* :stdlib t)))) :to-equal "(if x (progn (if y (progn z) nil)) nil)"))

(it-sequential "self-host-type-checker"
  (expect (string-downcase
                         (symbol-name (run-string *self-host-type-checker-program*))) :to-equal "ok"))

(it-sequential "self-host-format-error-pipeline"
  (expect (run-string "(handler-case (let ((val 42)) (if (> val 100) val (error (format nil \"bad-value\")))) (error (e) (format nil \"caught: ~A\" e)))") :to-equal "caught: bad-value"))

;;; Prog/With-Slots/Nth-Value Macro Tests

(deftest-compile compile-prog-and-friends
  "prog/prog*/with-slots/nth-value macros work in compiled code."
  :cases (("prog-loop"        10  "(prog ((x 0)) loop (setq x (+ x 1)) (when (= x 10) (return x)) (go loop))")
          ("prog*-sequential"  3  "(prog* ((x 1) (y (+ x 2))) (return y))")
          ("with-slots"       30  "(progn (defclass point () ((x :initarg :x) (y :initarg :y)))
                                          (let ((p (make-instance (quote point) :x 10 :y 20)))
                                            (with-slots (x y) p (+ x y))))")
          ("nth-value"         2  "(nth-value 1 (floor 17 5))")
          ("prog-no-return"   nil "(prog ((x 1)) (setq x 2))"))
  :stdlib t)

;;; ANSI CL FR-400/FR-500 Tests (mismatch, make-string, float literals, string-not-equal)

(deftest-compile stdlib-mismatch-make-string-float
  "mismatch, make-string, and float properties return the expected equal-comparable values."
  :cases (("mismatch-index"     2      "(mismatch (list 1 2 3) (list 1 2 4))")
          ("mismatch-equal"     nil    "(mismatch (list 1 2 3) (list 1 2 3))")
          ("mismatch-prefix"    0      "(mismatch nil (list 1 2))")
          ("make-string-fill"   "xxxx" "(make-string 4 :initial-element #\\x)")
          ("make-string-len"    3      "(length (make-string 3))")
          ("float-literal"      4.0    "(+ 1.5 2.5)"))
  :stdlib t)

(it-sequential "compile-find-package-builtin"
  (expect (run-string "(find-package :cl-user)" :stdlib t) :to-be-truthy))

(it-sequential "compile-find-symbol-builtin"
  (expect (run-string "(multiple-value-bind (sym status) (find-symbol \"CAR\" :cl) (list (symbol-name sym) status))" :stdlib t) :to-equal '("CAR" :external)))

(it-sequential "compile-float-inspection-builtins float-precision"
  (destructuring-bind (form expected) (list "(float-precision 1.0)" (float-precision 1.0))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-float-inspection-builtins float-radix"
  (destructuring-bind (form expected) (list "(float-radix 1.0)" (float-radix 1.0))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-float-inspection-builtins float-sign"
  (destructuring-bind (form expected) (list "(float-sign -2.5)" (float-sign -2.5))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-float-inspection-builtins float-digits"
  (destructuring-bind (form expected) (list "(float-digits 1.0)" (float-digits 1.0))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-float-decode-builtins decode-float"
  (destructuring-bind (form expected) (list "(multiple-value-list (decode-float 1.0))" (multiple-value-list (decode-float 1.0)))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-float-decode-builtins integer-decode-float"
  (destructuring-bind (form expected) (list "(multiple-value-list (integer-decode-float 1.0))" (multiple-value-list (integer-decode-float 1.0)))
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-string-not-equal different"
  (destructuring-bind (form) (list "(string-not-equal \"abc\" \"def\")")
    (expect (run-string form :stdlib t) :to-be-truthy)))

(it-sequential "compile-string-not-equal case-insensitive"
  (destructuring-bind (form) (list "(if (string-not-equal \"abc\" \"ABC\") nil t)")
    (expect (run-string form :stdlib t) :to-be-truthy)))

;;; ─── New stdlib tests (FR-495, FR-496, FR-540, FR-547, FR-582, FR-596, etc.) ──

(it-sequential "compile-list-tree-predicates tailp"
  (destructuring-bind (form) (list "(let* ((x '(1 2 3 4)) (tail (cddr x))) (tailp tail x))")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-list-tree-predicates copy-alist"
  (destructuring-bind (form) (list "(let ((al '((a . 1) (b . 2)))) (equal al (copy-alist al)))")
    (expect (run-string form) :to-be-truthy)))

(it-sequential "compile-list-tree-predicates tree-equal"
  (destructuring-bind (form) (list "(tree-equal '(1 (2 3)) '(1 (2 3)))")
    (expect (run-string form) :to-be-truthy)))

(deftest-compile compile-list-tree-mutators
  "ldiff, get-properties, nunion, nsubst, and nstring-upcase return expected values."
  :cases (("ldiff"          '(1 2)           "(let* ((x '(1 2 3 4)) (tail (cddr x))) (ldiff x tail))")
          ("get-properties"  :b              "(get-properties '(:a 1 :b 2 :c 3) '(:b :c))")
          ("nunion"          '(1 2 3 4)      "(sort (nunion '(1 2 3) '(2 3 4)) #'<)")
          ("nsubst"          '(99 2 (99 3))  "(nsubst 99 1 '(1 2 (1 3)))")
          ("nstring-upcase"  "HELLO"         "(nstring-upcase \"hello\")"))
  )

(deftest-compile compile-array-predicates
  "array-element-type returns T; array-in-bounds-p checks index validity."
  :cases (("element-type"      t   "(array-element-type (make-array 3))")
          ("in-bounds-valid"   t   "(array-in-bounds-p (make-array 5) 3)")
          ("in-bounds-invalid" nil "(array-in-bounds-p (make-array 5) 7)"))
  )

(it-sequential "compile-equalp lists-equal"
  (destructuring-bind (form expected-truthy) (list "(equalp '(1 2) '(1 2))" t)
    (expect (not (null (run-string form))) :to-equal expected-truthy)))

(it-sequential "compile-equalp string-case"
  (destructuring-bind (form expected-truthy) (list "(equalp \"hello\" \"HELLO\")" t)
    (expect (not (null (run-string form))) :to-equal expected-truthy)))

(it-sequential "compile-equalp lists-unequal"
  (destructuring-bind (form expected-truthy) (list "(equalp '(1 2) '(1 3))" nil)
    (expect (not (null (run-string form))) :to-equal expected-truthy)))

(it-sequential "compile-lisp-implementation-type"
  (expect (run-string "(lisp-implementation-type)") :to-equal "cl-cc"))

(deftest-compile compile-last-butlast-count
  "last/butlast with count return the correct sublist."
  :cases (("last"    '(4 5)   "(last '(1 2 3 4 5) 2)")
          ("butlast" '(1 2 3) "(butlast '(1 2 3 4 5) 2)"))
  )

(deftest-compile compile-array-integer-results
  "make-array, setf-bit, and search return integer results."
  :cases (("initial-contents" 20 "(let ((a (make-array 3 :initial-contents '(10 20 30)))) (aref a 1))")
           ("initial-element"   7 "(let ((a (make-array 4 :initial-element 7))) (aref a 3))")
           ("setf-bit"          1 "(let ((bv (make-array 4))) (setf (bit bv 2) 1) (bit bv 2))")
           ("search-vector"     1 "(search #(2 3) #(1 2 3 4))")
           ("search-vector-from-end" 3 "(search #(2) #(1 2 3 2) :from-end t)"))
  )

(deftest-compile compile-make-array-keywords
  "make-array keyword arguments survive codegen and reach vm-make-array."
  :cases (("dynamic-fill-pointer" 2 "(let ((fp 2)) (let ((v (make-array 5 :fill-pointer fp :adjustable t))) (fill-pointer v)))")
          ("dynamic-element-type" 0 "(let ((ty 'character)) (let ((a (make-array 2 :element-type ty))) (char-code (aref a 0))))")
          ("displaced-to"         9 "(let ((base (vector 1 2 3))) (let ((a (make-array 2 :displaced-to base))) (setf (aref a 1) 9) (aref base 1)))"))
  )

(it-sequential "compile-write-to-string-keywords"
  (expect (run-string "(write-to-string 42 :base 10)") :to-equal "42")
  (expect (run-string "(let ((sym 'hello)) (setf (get sym :answer) 42) (list (get sym :answer) (symbol-plist sym)))") :to-equal '(42 (:answer 42))))

;;; ─── FR-635: bit-nor / bit-nand / bit-eqv ────────────────────────────────────

(it-sequential "compile-bit-vector-ops nor"
  (destructuring-bind (form) (list "(bit-nor #*1010 #*1100)")
    (expect (vectorp (run-string form)) :to-be-truthy)))

(it-sequential "compile-bit-vector-ops eqv"
  (destructuring-bind (form) (list "(bit-eqv #*1010 #*0101)")
    (expect (vectorp (run-string form)) :to-be-truthy)))

;;; ─── FR-497: with-hash-table-iterator ────────────────────────────────────────

(it-sequential "compile-with-hash-table-iterator"
  (let ((r (run-string "(let ((h (make-hash-table)) (count 0))
  (setf (gethash :a h) 1 (gethash :b h) 2)
  (with-hash-table-iterator (next h)
    (loop (multiple-value-bind (more k v) (next)
            (unless more (return count))
            (incf count)
            (declare (ignore k v))))))")))
    (expect (= 2 r) :to-be-truthy)))

;;; ─── FR-617/FR-608: stream read forms ────────────────────────────────────────

(deftest-compile compile-stream-read-forms
  "read-from-string returns (val pos); with-input-from-string :start skips prefix."
  :cases (("read-from-string-pos"
           '(42 3)
           "(multiple-value-bind (val pos)
              (read-from-string \"42 rest\")
              (list val pos))")
          ("with-input-from-string-start"
           'cl-cc::world
           "(with-input-from-string (s \"hello world\" :start 6)
              (read s))"))
  )

;;; ─── FR-637: string comparison with keyword bounds ───────────────────────────

(deftest-compile compile-string-comparison-bounds
  "String comparison functions accept :start/:end bounds."
  :cases (("equal-substring" t   "(string= \"hello world\" \"world\" :start1 6)")
          ("less-equal-nil"  nil "(string< \"ab\" \"abcde\" :start2 0 :end2 2)"))
  )

;;; ─── FR-397: compilation local scope forms ───────────────────────────────────

(deftest-compile compile-meta-forms
  "locally evaluates its body and returns the numeric result."
  :cases (("locally" 30 "(locally (+ 10 20))")))

;;; ─── FR-566: pathname host bridges ───────────────────────────────────────────

(deftest-compile compile-pathname-accessors
  "Pathname constructor/accessor bridges compile and agree with host CL results."
  :cases (("make-pathname"
           (namestring (make-pathname :directory '(:absolute "tmp") :name "foo" :type "lisp"))
           "(namestring (make-pathname :directory '(:absolute \"tmp\") :name \"foo\" :type \"lisp\"))")
          ("pathname"
           (namestring (pathname "/tmp/foo.lisp"))
           "(namestring (pathname \"/tmp/foo.lisp\"))")
          ("namestring"
           "/tmp/foo.lisp"
           "(namestring #P\"/tmp/foo.lisp\")")
          ("file-namestring"
           "foo.lisp"
           "(file-namestring #P\"/tmp/foo.lisp\")")
          ("pathname-name-string"
           "foo"
           "(pathname-name \"/tmp/foo.lisp\")")
          ("pathname-type-string"
           "lisp"
           "(pathname-type \"/tmp/foo.lisp\")")
          ("pathname-name-pathname"
           "foo"
           "(pathname-name #P\"/tmp/foo.lisp\")")
          ("pathname-type-pathname"
           "txt"
           "(pathname-type #P\"/tmp/bar.txt\")")
          ("pathname-host"
           (pathname-host #P"/tmp/foo.lisp")
           "(pathname-host #P\"/tmp/foo.lisp\")")
          ("pathname-device"
           (pathname-device #P"/tmp/foo.lisp")
           "(pathname-device #P\"/tmp/foo.lisp\")")
          ("pathname-directory"
           (pathname-directory #P"/tmp/foo.lisp")
           "(pathname-directory #P\"/tmp/foo.lisp\")")
          ("pathname-version"
           (pathname-version #P"/tmp/foo.lisp")
           "(pathname-version #P\"/tmp/foo.lisp\")")
          ("merge-pathnames"
           (namestring (merge-pathnames "foo.lisp" "/tmp/"))
           "(namestring (merge-pathnames \"foo.lisp\" \"/tmp/\"))")
          ("parse-namestring"
           (namestring (parse-namestring "/tmp/foo.lisp"))
           "(namestring (parse-namestring \"/tmp/foo.lisp\"))")
          ("wild-pathname-p"
           t
           "(wild-pathname-p #P\"/tmp/*.lisp\")")
          ("pathname-match-p"
           t
           "(pathname-match-p #P\"/tmp/foo.lisp\" #P\"/tmp/*.lisp\")")
          ("translate-pathname"
           (namestring (translate-pathname #P"/tmp/src/foo.lisp"
                                           #P"/tmp/src/*.lisp"
                                           #P"/tmp/out/*.fasl"))
           "(namestring (translate-pathname #P\"/tmp/src/foo.lisp\" #P\"/tmp/src/*.lisp\" #P\"/tmp/out/*.fasl\"))"))
  )

(it-sequential "compile-pathname-file-host-bridges"
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "cl-cc-path-bridge-~A/" (get-universal-time))
                                 (uiop:temporary-directory))))
         (nested-file (merge-pathnames #P"nested/a/b/output.txt" root))
         (nested-dir (uiop:ensure-directory-pathname (merge-pathnames #P"nested/a/b/" root)))
         (source-file (merge-pathnames #P"source.txt" root))
         (renamed-file (merge-pathnames #P"renamed.txt" root))
         (pattern (merge-pathnames #P"*.txt" root)))
    (unwind-protect
         (progn
           (run-string (format nil "(ensure-directories-exist ~S)" (namestring nested-file)))
           (expect (probe-file nested-dir) :to-be-truthy)
           (with-open-file (out source-file :direction :output :if-exists :supersede :if-does-not-exist :create)
             (write-string "bridge" out))
           (expect (run-string (format nil "(probe-file ~S)" (namestring source-file))) :to-be-truthy)
           (expect (= (length (directory pattern)) (run-string (format nil "(length (directory ~S))" pattern))) :to-be-truthy)
           (expect (run-string (format nil "(namestring (truename ~S))" (namestring source-file))) :to-equal (namestring (truename source-file)))
           (let ((write-date (run-string (format nil "(file-write-date ~S)" (namestring source-file)))))
             (expect (integerp write-date) :to-be-truthy))
           (let ((author (run-string (format nil "(file-author ~S)" (namestring source-file)))))
             (expect (or (null author) (stringp author)) :to-be-truthy))
           (run-string (format nil "(rename-file ~S ~S)"
                               (namestring source-file)
                               (namestring renamed-file)))
           (expect (probe-file source-file) :to-be-falsy)
           (expect (probe-file renamed-file) :to-be-truthy)
           (run-string (format nil "(delete-file ~S)" (namestring renamed-file)))
           (expect (probe-file renamed-file) :to-be-falsy))
      (ignore-errors (delete-file renamed-file))
      (ignore-errors (delete-file source-file)))))

;;; ─── FR-572: #nA multi-dimensional array literal ─────────────────────────────

(it-sequential "compile-hash-na-arrays"
  (let ((r (run-string "#2A((1 2 3) (4 5 6))")))
    (expect (arrayp r) :to-be-truthy)
    (expect (= 2 (array-rank r)) :to-be-truthy)
    (expect (= 2 (array-dimension r 0)) :to-be-truthy)
    (expect (= 3 (array-dimension r 1)) :to-be-truthy)
    (expect (= 1 (aref r 0 0)) :to-be-truthy)
    (expect (= 6 (aref r 1 2)) :to-be-truthy))
  (let ((r (run-string "#1A(10 20 30)")))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 3 (length r)) :to-be-truthy)
    (expect (= 20 (aref r 1)) :to-be-truthy)))

;;; ─── FR-612: read-char / read-line / read with eof args ──────────────────────

(it-sequential "compile-stream-read-eof-args read-char"
  (destructuring-bind (form) (list "(with-input-from-string (s \"\") (read-char s nil :end-of-stream))")
    (let ((r (run-string form)))
    (expect (or (null r) (equal r "") (eq r :eof) (eq r :end-of-stream)) :to-be-truthy))))

(it-sequential "compile-stream-read-eof-args read-line"
  (destructuring-bind (form) (list "(with-input-from-string (s \"\") (read-line s nil nil))")
    (let ((r (run-string form)))
    (expect (or (null r) (equal r "") (eq r :eof) (eq r :end-of-stream)) :to-be-truthy))))

(it-sequential "compile-read-char-stream-arg"
  (let ((r (run-string
              "(with-input-from-string (s \"A\")
                 (read-char s))")))
    (expect r :to-equal #\A)))

(it-sequential "compile-close-abort"
  (let ((r (run-string
              "(let ((s (make-string-output-stream)))
                 (close s :abort t))")))
    (expect r :to-be-truthy)))

;;; Extended stdlib/keyword integration tests are in compiler-tests-extended-stdlib.lisp.
