(in-package :cl-cc/test)

(defun %fr-361-large-body-form ()
  (let ((expr 'x))
    (dotimes (_ 16 expr)
      (declare (ignore _))
      (setf expr `(+ ,expr 1)))))

;;; ─── FR-502/507: fill/replace/copy-seq sequence support ────────────────────

(it-sequential "compile-fill-vector"
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (fill v 0)
                           v)" :stdlib t)))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 0 (aref r 0)) :to-be-truthy)
    (expect (= 0 (aref r 1)) :to-be-truthy)
    (expect (= 0 (aref r 2)) :to-be-truthy))
  (let ((r (run-string "(let ((v (make-array 5 :initial-contents '(0 1 2 3 4))))
                           (fill v 9 :start 1 :end 4)
                           v)" :stdlib t)))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 0 (aref r 0)) :to-be-truthy)
    (expect (= 9 (aref r 1)) :to-be-truthy)
    (expect (= 9 (aref r 2)) :to-be-truthy)
    (expect (= 9 (aref r 3)) :to-be-truthy)
    (expect (= 4 (aref r 4)) :to-be-truthy)))

(it-sequential "compile-replace-and-copy-seq-vector"
  (let ((r (run-string "(let ((d (make-array 3 :initial-contents '(0 0 0)))
                              (s (make-array 3 :initial-contents '(1 2 3))))
                           (replace d s)
                           d)" :stdlib t)))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 1 (aref r 0)) :to-be-truthy)
    (expect (= 2 (aref r 1)) :to-be-truthy)
    (expect (= 3 (aref r 2)) :to-be-truthy))
  (let ((r (run-string "(let ((d (list 0 0 0))
                              (s (make-array 3 :initial-contents '(1 2 3))))
                           (replace d s)
                           d)" :stdlib t)))
    (expect r :to-equal '(1 2 3)))
  (let ((r (run-string "(let ((d (make-array 3 :initial-contents '(0 0 0)))
                              (s '(1 2 3)))
                           (replace d s)
                           d)" :stdlib t)))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 1 (aref r 0)) :to-be-truthy)
    (expect (= 2 (aref r 1)) :to-be-truthy)
    (expect (= 3 (aref r 2)) :to-be-truthy))
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (copy-seq v))" :stdlib t)))
    (expect (vectorp r) :to-be-truthy)
    (expect (= 3 (length r)) :to-be-truthy)
    (expect (= 1 (aref r 0)) :to-be-truthy)))

;;; ─── FR-697: assoc/member with :test/:key keyword args ───────────────────────

(it-sequential "compile-sequence-test-keyword member-test"
  (destructuring-bind (expected form) (list '("b" "c") "(member \"b\" '(\"a\" \"b\" \"c\") :test #'equal)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-sequence-test-keyword assoc-test"
  (destructuring-bind (expected form) (list '("b" . 2) "(assoc \"b\" '((\"a\" . 1) (\"b\" . 2)) :test #'equal)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-member-assoc-key-keyword"
  (let ((r (run-string "(member 2 '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (expect (consp r) :to-be-truthy)
    (expect (= 2 (caar r)) :to-be-truthy))
  (let ((r (run-string "(assoc 4 '((1 . a) (2 . b) (3 . c)) :key (lambda (x) (* x x)))" :stdlib t)))
    (expect (consp r) :to-be-truthy)
    (expect (= 2 (car r)) :to-be-truthy)))

;;; ─── position/count/find-if with keyword args ────────────────────────────────

(it-sequential "compile-sequence-position-count-keywords position-test"
  (destructuring-bind (expected form) (list 1 "(position \"b\" '(\"a\" \"b\" \"c\") :test #'equal)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-sequence-position-count-keywords position-key"
  (destructuring-bind (expected form) (list 1 "(position 2 '((1 . a) (2 . b) (3 . c)) :key #'car)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-sequence-position-count-keywords count-test"
  (destructuring-bind (expected form) (list 2 "(count \"a\" '(\"a\" \"b\" \"a\") :test #'equal)")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-find-if-key-keyword"
  (let ((r (run-string "(find-if #'evenp '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (expect (consp r) :to-be-truthy)
    (expect (= 2 (car r)) :to-be-truthy)))

;;; ─── remove-duplicates and remove with :test keyword ────────────────────────

(it-sequential "compile-remove-test-keywords"
  (expect (= 2 (length (run-string "(remove-duplicates '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t))) :to-be-truthy)
  (expect (run-string "(remove \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t) :to-equal '("b")))

;;; ─── string-upcase/downcase with :start/:end ─────────────────────────────────

(it-sequential "compile-string-case-bounds upcase"
  (destructuring-bind (expected form) (list "hELlo" "(string-upcase \"hello\" :start 1 :end 3)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-case-bounds downcase"
  (destructuring-bind (expected form) (list "HellO" "(string-downcase \"HELLO\" :start 1 :end 4)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-case-bounds capitalize"
  (destructuring-bind (expected form) (list "hEllo" "(string-capitalize \"hELLo\" :start 1 :end 4)")
    (expect (run-string form) :to-equal expected)))

(it-sequential "compile-string-trim-bounds left"
  (destructuring-bind (expected form) (list "hello  " "(string-left-trim \" \" \"  hello  \")")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-string-trim-bounds right"
  (destructuring-bind (expected form) (list "  hello" "(string-right-trim \" \" \"  hello  \")")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-nstring-case-bounds upcase"
  (destructuring-bind (expected form) (list "hELLo" "(nstring-upcase \"hello\" :start 1 :end 4)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-nstring-case-bounds downcase"
  (destructuring-bind (expected form) (list "HellO" "(nstring-downcase \"HELLO\" :start 1 :end 4)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-nstring-case-bounds capitalize"
  (destructuring-bind (expected form) (list "hEllo" "(nstring-capitalize \"hELLo\" :start 1 :end 4)")
    (expect (run-string form :stdlib t) :to-equal expected)))

(it-sequential "compile-empty-let-and-flet"
  (expect (= 42 (run-string "(let () 42)")) :to-be-truthy)
  (expect (= 7 (run-string "(flet () 7)")) :to-be-truthy))

(it-sequential "compile-top-level-defvar-visible-to-following-defun"
  (expect (= 43 (run-string
             "(progn
                (defvar *defvar-cps-visibility-probe-a* 41)
                (defvar *defvar-cps-visibility-probe-b* 2)
                (defun defvar-cps-visibility-probe ()
                  (+ *defvar-cps-visibility-probe-a* *defvar-cps-visibility-probe-b*))
                (defvar-cps-visibility-probe))")) :to-be-truthy))

(it-sequential "compile-type-of-float-and-function"
  (expect (run-string "(type-of 1.0)") :to-be 'single-float)
  (expect (run-string "(type-of (lambda (x) x))") :to-be 'function))

(it-sequential "compile-error-format-control"
  (expect (run-string
    "(handler-case
         (progn (error \"bad ~A\" 42) nil)
       (error (e)
         (not (null (search \"42\" (format nil \"~A\" e))))))") :to-be-truthy))

(it-sequential "compile-warn-format-control"
  (expect (= 42 (run-string "(progn (warn \"warn ~A\" 7) 42)")) :to-be-truthy))

;;; ─── FR-599: #n= / #n# label and reference reader macros ────────────────────

(it-sequential "compile-hash-n-eq"
  (let ((r (run-string "(list #0=(1 2 3) #0#)")))
    (expect (first r) :to-equal '(1 2 3))
    (expect (second r) :to-equal '(1 2 3)))
  (expect (run-string "#0=\"hello\"") :to-equal "hello"))

;;; ─── FR-592: readtable API compatibility ────────────────────────────────────

(it-sequential "compile-readtable-compatibility-api"
  (expect (run-string "(readtablep *readtable*)" :stdlib t) :to-be-truthy)
  (expect (run-string "(let ((rt (copy-readtable)))
                             (setf (readtable-case rt) :downcase)
                             (readtable-case rt))"
                          :stdlib t) :to-be :downcase)
  (expect (run-string "(progn
                                (setf (readtable-case *readtable*) :downcase)
                                (symbol-name (read-from-string \"ABC\")))"
                             :stdlib t) :to-equal "abc")
  (expect (run-string "(let ((rt (copy-readtable)))
                               (set-macro-character #\! (lambda (stream char)
                                                          (declare (ignore stream char))
                                                          :ok)
                                                    t rt)
                               (set-syntax-from-char #\? #\! rt rt)
                               (multiple-value-bind (fn non-terminating-p)
                                   (get-macro-character #\? rt)
                                 (list (functionp fn) non-terminating-p)))"
                            :stdlib t) :to-equal '(t t))
  (expect (run-string "(let ((rt (copy-readtable)))
                  (make-dispatch-macro-character #\# nil rt)
                  (set-dispatch-macro-character #\# #\q
                                                (lambda (stream char arg)
                                                  (declare (ignore stream char arg))
                                                  :q)
                                                rt)
                  (functionp (get-dispatch-macro-character #\# #\q rt)))"
               :stdlib t) :to-be-truthy))

;;; ─── FR-641: union/intersection/set-difference with :test ────────────────────

(it-sequential "compile-set-ops-test-keyword union"
  (destructuring-bind (expected form) (list 3 "(length (union '(\"a\" \"b\") '(\"b\" \"c\") :test #'equal))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-set-ops-test-keyword intersection"
  (destructuring-bind (expected form) (list 2 "(length (intersection '(\"a\" \"b\" \"c\") '(\"b\" \"c\" \"d\") :test #'equal))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

(it-sequential "compile-set-ops-test-keyword set-diff"
  (destructuring-bind (expected form) (list 2 "(length (set-difference '(\"a\" \"b\" \"c\") '(\"b\") :test #'equal))")
    (expect (= expected (run-string form :stdlib t)) :to-be-truthy)))

;;; ─── FR-688: delete/substitute with :test keyword ────────────────────────────

(it-sequential "compile-delete-test-keyword"
  (let ((r (run-string "(delete \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (expect r :to-equal '("b"))))

(it-sequential "compile-substitute-test-keyword"
  (let ((r (run-string "(substitute \"x\" \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (expect (= 3 (length r)) :to-be-truthy)
    (expect (first r) :to-equal "x")
    (expect (third r) :to-equal "x")))

;;; ─── compile-file-pathname host bridge ───────────────────────────────────────

(it-sequential "compile-compile-file-pathname"
  (let ((r (run-string "(compile-file-pathname \"/tmp/foo.lisp\")")))
    (expect (pathnamep r) :to-be-truthy)))

(it-sequential "compile-compile-file-is-fbound"
  (expect (run-string "(fboundp 'compile-file)" :stdlib t) :to-be-truthy))

(it-sequential "compile-safe-debug-bridges"
  (expect (run-string "(disassemble 42)") :to-be-null)
  (expect (= 42 (run-string "(inspect 42)")) :to-be-truthy))

(it-sequential "compile-foreign-funcall-strlen"
  (expect (= 4 (run-string "(foreign-funcall \"strlen\" :string \"abcd\" :int)" :stdlib t)) :to-be-truthy)
  (expect (= 5 (run-string "(cffi:foreign-funcall \"strlen\" :string \"abcde\" :int)" :stdlib t)) :to-be-truthy))

(it-sequential "compile-documentation-defun-docstring"
  (expect (run-string
    "(progn (defun documented-probe () \"doc text\" 42)
       (documentation 'documented-probe 'function))"
    :stdlib t) :to-equal "doc text")
  (expect (run-string "(documentation 'missing-doc-probe 'function)" :stdlib t) :to-be-null))

(it-sequential "compile-subtypep-basic-two-values"
  (expect (run-string "(multiple-value-list (subtypep 'integer 'number))" :stdlib t) :to-equal '(t t))
  (expect (run-string "(multiple-value-list (subtypep 'string 'integer))" :stdlib t) :to-equal '(nil t)))

(it-sequential "compile-string-octets-bridges"
  (let ((octets (run-string "(string-to-octets \"hé\" :encoding :utf-8)")))
    (expect (vectorp octets) :to-be-truthy)
    (expect (= 3 (length octets)) :to-be-truthy)
    (expect (= 104 (aref octets 0)) :to-be-truthy)
    (expect (= 195 (aref octets 1)) :to-be-truthy)
    (expect (= 169 (aref octets 2)) :to-be-truthy))
  (let ((octets (run-string "(string-to-octets \"é\" :external-format :latin-1)")))
    (expect (= 1 (length octets)) :to-be-truthy)
    (expect (= 233 (aref octets 0)) :to-be-truthy))
  (let ((octets (run-string "(string-to-octets \"A\" :encoding :utf-16)")))
    (expect (vectorp octets) :to-be-truthy)
    (expect (= 2 (length octets)) :to-be-truthy)
    (expect (= 65 (aref octets 0)) :to-be-truthy)
    (expect (= 0 (aref octets 1)) :to-be-truthy))
  (let ((octets (run-string "(string-to-octets (string (code-char 128512)) :encoding :utf-16)")))
    (expect (vectorp octets) :to-be-truthy)
    (expect (= 4 (length octets)) :to-be-truthy)
    (expect (= #x3d (aref octets 0)) :to-be-truthy)
    (expect (= #xd8 (aref octets 1)) :to-be-truthy)
    (expect (= #x00 (aref octets 2)) :to-be-truthy)
    (expect (= #xde (aref octets 3)) :to-be-truthy))
  (expect (run-string "(octets-to-string (string-to-octets \"hé\" :external-format :utf-8) :encoding :utf-8)") :to-equal "hé")
  (expect (run-string "(octets-to-string (string-to-octets \"A\" :encoding :utf-16) :encoding :utf-16)") :to-equal "A")
  (expect (= 128512 (run-string "(char-code (aref (octets-to-string (string-to-octets (string (code-char 128512)) :encoding :utf-16) :encoding :utf-16) 0))")) :to-be-truthy))

;;; ─── FR-604: float 2-arg prototype form ──────────────────────────────────────

(it-sequential "compile-float-2arg"
  (expect (floatp (run-string "(float 3 1.0d0)")) :to-be-truthy)
  (expect (= (float 3) (run-string "(float 3 1.0d0)")) :to-be-truthy))

;;; ─── FR-605: bignum predicate helper ────────────────────────────────────────

(it-sequential "compile-bignump"
  (expect (run-string "(bignump (expt 2 100))" :stdlib t) :to-be-truthy)
  (expect (run-string "(bignump 42)" :stdlib t) :to-be-falsy))

;;; ─── FR-361/363/396: declaim inline policy + optimize quality handling ─────

(it-sequential "compile-declaim-optimize-form-records-global-policy"
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (run-string "(declaim (optimize speed (safety 0) (debug 3))) nil" :stdlib t) :to-equal nil)
    (expect (= 3 (gethash 'speed cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 0 (gethash 'safety cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)
    (expect (= 3 (gethash 'debug cl-cc/expand:*declaim-optimize-registry*)) :to-be-truthy)))

(it-sequential "compile-declaim-safety-zero-suppresses-later-defun-type-assertion"
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((declaim (optimize (safety 0)))
                      (defun later-safe (x) (the integer x)))
                    :target :vm))
           (insts (cl-cc/compile:compilation-result-vm-instructions result)))
      (expect (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref))) insts) :to-be-truthy)
      (expect (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-typep)) insts) :to-be-null))))

(it-sequential "compile-declaim-safety-zero-suppresses-top-level-the-type-assertion"
  (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((declaim (optimize (safety 0)))
                      (the integer 42))
                    :target :vm))
           (insts (cl-cc/compile:compilation-result-vm-instructions result)))
      (expect (find-if (lambda (inst) (typep inst 'cl-cc/vm::vm-typep)) insts) :to-be-null))))

(it-sequential "compile-declaim-optimize-inline-policy-applies-globally-across-compilations speed-three"
  (destructuring-bind (declaim-form expected-policy) (list '(declaim (optimize (speed 3))) :inline)
    (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1 declaim-form) :to-equal nil)
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun mapped-inline-policy (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
            (closure (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref)))
                              (cl-cc/compile:compilation-result-vm-instructions result))))
      (expect closure :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy closure) :to-be expected-policy)))))

(it-sequential "compile-declaim-optimize-inline-policy-applies-globally-across-compilations debug-three"
  (destructuring-bind (declaim-form expected-policy) (list '(declaim (optimize (debug 3))) :notinline)
    (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1 declaim-form) :to-equal nil)
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun mapped-inline-policy (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
            (closure (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref)))
                              (cl-cc/compile:compilation-result-vm-instructions result))))
      (expect closure :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy closure) :to-be expected-policy)))))

(it-sequential "compile-declaim-optimize-inline-policy-applies-globally-across-compilations space-two"
  (destructuring-bind (declaim-form expected-policy) (list '(declaim (optimize (space 2))) :notinline)
    (let ((cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1 declaim-form) :to-equal nil)
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun mapped-inline-policy (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
            (closure (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref)))
                              (cl-cc/compile:compilation-result-vm-instructions result))))
      (expect closure :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy closure) :to-be expected-policy)))))

(it-sequential "compile-declaim-inline-applies-globally-across-compilations"
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1 '(declaim (inline big))) :to-equal nil)
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    (list `(defun big (x) ,(%fr-361-large-body-form)))
                    :target :vm
                    :pass-pipeline '(:inline)))
            (closure (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref)))
                              (cl-cc/compile:compilation-result-vm-instructions result))))
      (expect closure :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy closure) :to-be :inline))))

(it-sequential "compile-declaim-notinline-applies-globally-across-compilations"
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq))
        (cl-cc/expand:*declaim-optimize-registry* (make-hash-table :test #'eq)))
    (expect (our-macroexpand-1 '(declaim (optimize (speed 3)))) :to-equal nil)
    (expect (our-macroexpand-1 '(declaim (notinline inc))) :to-equal nil)
    (let* ((result (cl-cc/compile:compile-toplevel-forms
                    '((defun inc (x) (+ x 1)))
                    :target :vm
                    :pass-pipeline '(:inline)))
            (closure (find-if (lambda (inst) (typep inst '(or cl-cc/vm::vm-closure cl-cc/vm::vm-func-ref)))
                              (cl-cc/compile:compilation-result-vm-instructions result))))
      (expect closure :to-be-truthy)
      (expect (cl-cc/vm:vm-closure-inline-policy closure) :to-be :notinline))))

;;; ─── ANSI proclaim / with-compilation-unit runtime support ─────────────────

(it-sequential "compile-proclaim-records-global-declaration"
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (expect (run-string "(proclaim '(inline proclaimed-fn))" :stdlib t) :to-equal '(inline proclaimed-fn))
    (expect (gethash 'proclaimed-fn cl-cc/expand:*declaim-inline-registry*) :to-be :inline)))

(it-sequential "compile-with-compilation-unit-evaluates-body"
  (expect (= 11 (run-string "(with-compilation-unit () (let ((x 11)) x))" :stdlib t)) :to-be-truthy))

;;; ─── FR-598: stream typep ────────────────────────────────────────────────────

(it-sequential "compile-typep-stream-truthy standard-output-stream"
  (destructuring-bind (form stdlib-p) (list "(typep *standard-output* 'stream)" nil)
    (expect (run-string form :stdlib stdlib-p) :to-be-truthy)))

(it-sequential "compile-typep-stream-truthy output-stream-type"
  (destructuring-bind (form stdlib-p) (list "(output-stream-p *standard-output*)" t)
    (expect (run-string form :stdlib stdlib-p) :to-be-truthy)))

(it-sequential "compile-typep-stream-truthy string-output-stream"
  (destructuring-bind (form stdlib-p) (list "(let ((s (make-string-output-stream))) (typep s 'string-stream))" t)
    (expect (run-string form :stdlib stdlib-p) :to-be-truthy)))

(it-sequential "compile-typep-non-stream"
  (expect (= 0 (run-string "(typep 42 'stream)")) :to-be-truthy))

;;; ─── FR-603: (setf (values ...)) ────────────────────────────────────────────

(it-sequential "compile-setf-values-floor"
  (let ((r (run-string "(let (a b) (setf (values a b) (floor 7 3)) (list a b))" :stdlib t)))
    (expect (= 2 (first r)) :to-be-truthy)
    (expect (= 1 (second r)) :to-be-truthy)))

;;; FR-562: Unicode character names via lexer
(it-sequential "compile-unicode-char-code greek-alpha"
  (destructuring-bind (expected form) (list 945 "(char-code #\\Greek_Small_Letter_Alpha)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-unicode-char-code snowman"
  (destructuring-bind (expected form) (list 9731 "(char-code #\\Snowman)")
    (expect (= expected (run-string form)) :to-be-truthy)))

(it-sequential "compile-unicode-char-code emoji"
  (destructuring-bind (expected form) (list 128512 "(char-code (code-char 128512))")
    (expect (= expected (run-string form)) :to-be-truthy)))

;;; FR-687: make-string :element-type with both keywords
(it-sequential "compile-make-string-element-type length"
  (destructuring-bind (form verify) (list "(length (make-string 5 :element-type 'character))" (lambda (result)
             (assert-= 5 result)))
    (funcall verify (run-string form))))

(it-sequential "compile-make-string-element-type fill"
  (destructuring-bind (form verify) (list "(make-string 3 :initial-element #\\x :element-type 'character)" (lambda (result)
             (assert-string= "xxx" result)))
    (funcall verify (run-string form))))

;;; FR-254: with-region macro expansion/compile path
(it-sequential "compile-with-region-runtime-lifetime"
  (expect (= 7 (run-string "(with-region (r) (cl-cc/runtime:rt-region-deref (cl-cc/runtime:rt-region-alloc r 7)))"
                       :stdlib t)) :to-be-truthy)
  (expect (run-string "(handler-case (let (ref) (with-region (r) (setf ref (cl-cc/runtime:rt-region-alloc r 9))) (cl-cc/runtime:rt-region-deref ref) nil) (error () t))"
               :stdlib t) :to-be-truthy))

(it-sequential "compile-copy-hash-table-cow-write-keeps-original"
  (expect (= 1 (run-string "(let* ((h (make-hash-table))
                               (_ (setf (gethash 'a h) 1))
                               (c (copy-hash-table h)))
                          (setf (gethash 'a c) 99)
                          (gethash 'a h))"
                       :stdlib t)) :to-be-truthy))

;;; (run-tests is defined in framework.lisp)
