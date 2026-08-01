(in-package :cl-cc/test)

;;; ─── typed assertions / typed instructions ───────────────────────────────

(it-sequential "pipeline-the-type-assertions fixnum"
  (destructuring-bind (ok-form expected err-form) (list "(the fixnum 42)" 42 "(the fixnum \"oops\")")
    (expect (= expected (run-string ok-form)) :to-be-truthy) (signals type-error (run-string err-form))))

(it-sequential "pipeline-the-type-assertions refinement"
  (destructuring-bind (ok-form expected err-form) (list "(the (refine fixnum plusp) 42)" 42 "(the (refine fixnum plusp) -1)")
    (expect (= expected (run-string ok-form)) :to-be-truthy) (signals type-error (run-string err-form))))

(it-sequential "pipeline-the-type-assertions values"
  (destructuring-bind (ok-form expected err-form) (list "(the (values fixnum string) (values 1 \"ok\"))" 1 "(the (values fixnum string) (values 1 2))")
    (expect (= expected (run-string ok-form)) :to-be-truthy) (signals type-error (run-string err-form))))

(it-sequential "pipeline-the-values-type-error-is-catchable"
  (expect (run-string "(handler-case (the (values fixnum string) (values 1 2)) (type-error (c) (declare (ignore c)) :ok))") :to-be :ok))

(it-sequential "pipeline-the-values-preserves-secondary-values"
  (expect (run-string "(multiple-value-bind (a b) (the (values fixnum string) (values 1 \"ok\")) (list a b))") :to-equal '(1 "ok")))

(it-sequential "pipeline-typed-fixnum-instruction-types add"
  (destructuring-bind (code expected-type) (list "(defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))" 'cl-cc/vm::vm-integer-add)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types sub"
  (destructuring-bind (code expected-type) (list "(defun typed-sub ((x fixnum) (y fixnum)) fixnum (- x y))" 'cl-cc/vm::vm-integer-sub)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types mul"
  (destructuring-bind (code expected-type) (list "(defun typed-mul ((x fixnum) (y fixnum)) fixnum (* x y))" 'cl-cc/vm::vm-integer-mul)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types lt"
  (destructuring-bind (code expected-type) (list "(defun typed-lt  ((x fixnum) (y fixnum)) fixnum (< x y))" 'cl-cc/vm::vm-lt)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types gt"
  (destructuring-bind (code expected-type) (list "(defun typed-gt  ((x fixnum) (y fixnum)) fixnum (> x y))" 'cl-cc/vm::vm-gt)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types eq"
  (destructuring-bind (code expected-type) (list "(defun typed-eq  ((x fixnum) (y fixnum)) fixnum (= x y))" 'cl-cc/vm::vm-num-eq)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types eq-fixnum"
  (destructuring-bind (code expected-type) (list "(defun typed-eq-pred ((x fixnum) (y fixnum)) fixnum (eq x y))" 'cl-cc/vm::vm-num-eq)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types eql-fixnum"
  (destructuring-bind (code expected-type) (list "(defun typed-eql-pred ((x fixnum) (y fixnum)) fixnum (eql x y))" 'cl-cc/vm::vm-num-eq)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types equal-fixnum"
  (destructuring-bind (code expected-type) (list "(defun typed-equal-pred ((x fixnum) (y fixnum)) fixnum (equal x y))" 'cl-cc/vm::vm-num-eq)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-typed-fixnum-instruction-types equal-symbol"
  (destructuring-bind (code expected-type) (list "(defun typed-equal-symbol ((x symbol) (y symbol)) fixnum (equal x y))" 'cl-cc/vm::vm-eq)
    (let ((instrs (vm-program-instructions
                 (compilation-result-program (compile-string code :target :vm)))))
    (expect (some (lambda (i) (typep i expected-type)) instrs) :to-be-truthy))))

(it-sequential "pipeline-equality-predicate-specialization-preserves-generic-equal"
  (let ((typed-instrs
          (vm-program-instructions
           (compilation-result-program
            (compile-string "(defun typed-equal-pred ((x fixnum) (y fixnum)) fixnum (equal x y))" :target :vm))))
        (generic-instrs
          (vm-program-instructions
           (compilation-result-program
            (compile-string "(defun generic-equal-pred (x y) (equal x y))" :target :vm)))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-num-eq)) typed-instrs) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-equal)) typed-instrs) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-equal)) generic-instrs) :to-be-truthy)))

;;; ─── compile-string / run-string ─────────────────────────────────────────

(it-sequential "pipeline-compile-string-returns-result single-form"
  (destructuring-bind (expr) (list "(+ 1 2)")
    (expect (typep (compile-string expr :target :vm) 'cl-cc/compile:compilation-result) :to-be-truthy)))

(it-sequential "pipeline-compile-string-returns-result multiple-forms"
  (destructuring-bind (expr) (list "(defun f (x) x)")
    (expect (typep (compile-string expr :target :vm) 'cl-cc/compile:compilation-result) :to-be-truthy)))

(it-sequential "pipeline-compile-string-custom-pass-pipeline"
  (let* ((baseline (compile-string "(+ 1 2)" :target :vm))
         (result (compile-string "(+ 1 2)" :target :vm :pass-pipeline "fold,dce")))
    (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)
    (expect (listp (cl-cc:compilation-result-optimized-instructions baseline)) :to-be-truthy)
    (expect (listp (cl-cc:compilation-result-optimized-instructions result)) :to-be-truthy)
    (expect (> (length (cl-cc:compilation-result-vm-instructions result)) 0) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms arithmetic"
  (destructuring-bind (expected expr) (list 3 "(+ 1 2)")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms literal"
  (destructuring-bind (expected expr) (list 42 "42")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms nested"
  (destructuring-bind (expected expr) (list 12 "(+ (* 2 3) (- 7 1))")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms let-form"
  (destructuring-bind (expected expr) (list 5 "(let ((x 2) (y 3)) (+ x y))")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms if-true"
  (destructuring-bind (expected expr) (list 1 "(if t 1 2)")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms if-false"
  (destructuring-bind (expected expr) (list 2 "(if nil 1 2)")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-forms lambda"
  (destructuring-bind (expected expr) (list 9 "((lambda (x) (* x x)) 3)")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-list-regressions list"
  (destructuring-bind (expected expr) (list '(1 2 3) "(list 1 2 3)")
    (expect (run-string expr) :to-equal expected)))

(it-sequential "pipeline-run-string-list-regressions cons"
  (destructuring-bind (expected expr) (list '(1 2 3) "(cons 1 (list 2 3))")
    (expect (run-string expr) :to-equal expected)))

(it-sequential "pipeline-run-string-list-regressions dolist-sum"
  (destructuring-bind (expected expr) (list 6 "(let ((acc 0)) (dolist (x (list 1 2 3) acc) (setq acc (+ acc x))))")
    (handler-bind
        ((error
           (lambda (condition)
             (ignore-errors
               (let* ((system (asdf:find-system :cl-cc-optimize))
                      (component (asdf:find-component system "optimizer-flow-block-merge")))
                 (format *error-output* "~&[diagnostic] input: ~A~%" expr)
                 (format *error-output* "[diagnostic] condition: ~A~%" condition)
                 (format *error-output* "[diagnostic] optimize system: ~A~%"
                         (asdf:component-pathname system))
                 (format *error-output* "[diagnostic] block merge source: ~A~%"
                         (asdf:component-pathname component))
                 (format *error-output* "[diagnostic] block merge FASL: ~{~A~^, ~}~%"
                         (asdf:output-files 'asdf:compile-op component))
                 #+sbcl (sb-debug:print-backtrace :stream *error-output*)
                 (finish-output *error-output*))))))
      (expect (run-string expr) :to-equal expected))))

(it-sequential "pipeline-run-string-list-regressions labels-simple"
  (destructuring-bind (expected expr) (list 3 "(labels ((f (x) (if (= x 0) 0 (+ 1 (f (- x 1)))))) (f 3))")
    (expect (run-string expr) :to-equal expected)))

(it-sequential "pipeline-run-string-hash-cons-reuses-flat-pairs"
  (cl-cc/vm:vm-clear-hash-cons-table)
  (expect (eq t (run-string "(let ((a (hash-cons 'x 'y)) (b (hash-cons 'x 'y))) (eq a b))")) :to-be-truthy)
  (expect (eq nil (run-string "(let ((a (hash-cons 'x 'y)) (b (cons 'x 'y))) (eq a b))")) :to-be-truthy))

(it-sequential "pipeline-run-string-stdlib-regressions mapcar"
  (destructuring-bind (expected expr) (list '(2 4 6) "(mapcar (lambda (x) (+ x x)) (list 1 2 3))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-regressions find-if"
  (destructuring-bind (expected expr) (list 4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-regressions count-if"
  (destructuring-bind (expected expr) (list 2 "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-regressions reduce"
  (destructuring-bind (expected expr) (list 10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-regressions set-difference"
  (destructuring-bind (expected expr) (list '(1 3 5) "(set-difference (list 1 2 3 4 5) (list 2 4))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-regressions position-miss"
  (destructuring-bind (expected expr) (list nil "(position 9 (list 1 2 3))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-function-cell-regressions symbol-function"
  (destructuring-bind (expected expr) (list 42 "(progn (defun sf-regression (x) (+ x 1)) (funcall (symbol-function 'sf-regression) 41))")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-function-cell-regressions setf-gethash-fallback"
  (destructuring-bind (expected expr) (list 9 "(let ((h (make-hash-table))) (setf-gethash 'k h 9) (gethash 'k h))")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-function-cell-regressions make-array-numeric-fill-pointer"
  (destructuring-bind (expected expr) (list 1 "(let ((v (make-array 8 :fill-pointer 1))) (fill-pointer v))")
    (expect (= expected (run-string expr)) :to-be-truthy)))

(it-sequential "pipeline-run-string-output-options timings"
  (destructuring-bind (kwarg-prefix verify) (list (list :print-pass-timings t :timing-stream) (lambda (text) (expect (search "OPT-PASS-FOLD" (string-upcase text)) :to-be-truthy)))
    (let* ((stream (make-string-output-stream))
         (kwargs (append kwarg-prefix (list stream))))
    (expect (= 3 (apply #'run-string "(+ 1 2)" :pass-pipeline "fold" kwargs)) :to-be-truthy)
    (funcall verify (get-output-stream-string stream)))))

(it-sequential "pipeline-run-string-output-options stats"
  (destructuring-bind (kwarg-prefix verify) (list (list :print-pass-stats t :stats-stream) (lambda (text)
             (let ((u (string-upcase text)))
               (expect (search "OPT-PASS-FOLD" u) :to-be-truthy)
               (expect (search "BEFORE=" u) :to-be-truthy))))
    (let* ((stream (make-string-output-stream))
         (kwargs (append kwarg-prefix (list stream))))
    (expect (= 3 (apply #'run-string "(+ 1 2)" :pass-pipeline "fold" kwargs)) :to-be-truthy)
    (funcall verify (get-output-stream-string stream)))))

(it-sequential "pipeline-run-string-output-options trace-json"
  (destructuring-bind (kwarg-prefix verify) (list (list :trace-json-stream) (lambda (text)
             (expect (search "\"traceEvents\"" text) :to-be-truthy)
             (expect (search "OPT-PASS-FOLD" text) :to-be-truthy)))
    (let* ((stream (make-string-output-stream))
         (kwargs (append kwarg-prefix (list stream))))
    (expect (= 3 (apply #'run-string "(+ 1 2)" :pass-pipeline "fold" kwargs)) :to-be-truthy)
    (funcall verify (get-output-stream-string stream)))))

;;; ─── prescan / parse / stdlib / our-eval ─────────────────────────────────

(it-sequential "pipeline-prescan-in-package-behavior keyword-form"
  (destructuring-bind (source verify) (list "(in-package :cl-cc)" (lambda (result)
             (expect (string-upcase result) :to-equal "CL-CC")))
    (funcall verify (cl-cc::%prescan-in-package source))))

(it-sequential "pipeline-prescan-in-package-behavior string-form"
  (destructuring-bind (source verify) (list "(in-package \"CL-CC\")" (lambda (result)
             (expect (string-upcase result) :to-equal "CL-CC")))
    (funcall verify (cl-cc::%prescan-in-package source))))

(it-sequential "pipeline-prescan-in-package-behavior non-package"
  (destructuring-bind (source verify) (list "(defun f (x) x)" (lambda (result)
             (expect result :to-be-null)))
    (funcall verify (cl-cc::%prescan-in-package source))))

(it-sequential "pipeline-prescan-in-package-behavior with-comment"
  (destructuring-bind (source verify) (list (format nil ";;; header comment~%(in-package :cl-cc)") (lambda (result)
             (expect (stringp result) :to-be-truthy)))
    (funcall verify (cl-cc::%prescan-in-package source))))

(it-sequential "pipeline-parse-source-for-language single-form"
  (destructuring-bind (source lang expected-count verify) (list "(+ 1 2)" :lisp 1 (lambda (forms)
             (expect (first forms) :to-equal '(+ 1 2))))
    (let ((forms (cl-cc::parse-source-for-language source lang)))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (funcall verify forms))))

(it-sequential "pipeline-parse-source-for-language single-form-elisp"
  (destructuring-bind (source lang expected-count verify) (list "(+ 1 2)" :elisp 1 (lambda (forms)
             (expect (first forms) :to-equal '(+ 1 2))))
    (let ((forms (cl-cc::parse-source-for-language source lang)))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (funcall verify forms))))

(it-sequential "pipeline-parse-source-for-language multiple-forms"
  (destructuring-bind (source lang expected-count verify) (list "(+ 1 2) (* 3 4)" :lisp 2 (lambda (_forms)
             (declare (ignore _forms))))
    (let ((forms (cl-cc::parse-source-for-language source lang)))
    (expect (= expected-count (length forms)) :to-be-truthy)
    (funcall verify forms))))

(it-sequential "pipeline-parse-unknown-language-signals"
  (signals error (cl-cc::parse-source-for-language "(+ 1 2)" :unknown)))

(it-sequential "pipeline-stdlib-forms-content"
  (let ((forms (cl-cc::get-stdlib-forms)))
    (expect (> (length forms) 10) :to-be-truthy)
    (expect (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'mapcar)))
                          forms) :to-be-truthy)
    (expect (cl:some (lambda (f)
                            (and (consp f) (eq (car f) 'defun)
                                 (eq (cadr f) 'reduce)))
                          forms) :to-be-truthy)))

(it-sequential "pipeline-stdlib-forms-return-fresh-tree"
  (let* ((forms-a (cl-cc::get-stdlib-forms))
         (forms-b (cl-cc::get-stdlib-forms))
         (defun-a (cl:find-if (lambda (f) (and (consp f) (eq (car f) 'defun))) forms-a))
         (defun-b (cl:find-if (lambda (f) (and (consp f) (eq (car f) 'defun))) forms-b)))
    (expect (consp defun-a) :to-be-truthy)
    (expect (consp defun-b) :to-be-truthy)
    (expect (eq defun-a defun-b) :to-be-falsy)
    (let ((original-name (second defun-b)))
      (setf (second defun-a) 'mutated-stdlib-name)
      (expect (second defun-b) :to-be original-name))))

(it-sequential "pipeline-our-eval-forms arithmetic"
  (destructuring-bind (expected expr) (list 6 '(* 2 3))
    (expect (cl-cc::our-eval expr) :to-equal expected)))

(it-sequential "pipeline-our-eval-forms quote-data"
  (destructuring-bind (expected expr) (list '(a b c) '(quote (a b c)))
    (expect (cl-cc::our-eval expr) :to-equal expected)))

(it-sequential "pipeline-our-eval-forms if-form"
  (destructuring-bind (expected expr) (list 10 '(if t 10 20))
    (expect (cl-cc::our-eval expr) :to-equal expected)))

(it-sequential "pipeline-our-eval-uses-vm-compile-path-for-simple-expression"
  (let ((orig (symbol-function 'cl-cc::compile-expression)))
    (unwind-protect
         (let ((called nil))
           (setf (symbol-function 'cl-cc::compile-expression)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (setf called t)
                (make-compilation-result
                     :program (make-vm-program :instructions (list (make-vm-const :dst :r0 :value 3)
                                                                    (make-vm-halt :reg :r0)))
                     :assembly ""
                     :cps '(identity 3)
                     :vm-instructions nil
                    :optimized-instructions nil)))
           (expect (= 3 (cl-cc::our-eval '(+ 1 2))) :to-be-truthy)
           (expect called :to-be-truthy))
      (setf (symbol-function 'cl-cc::compile-expression) orig))))

(it-sequential "pipeline-our-eval-falls-back-to-vm-for-definitions"
  (let ((orig (symbol-function 'cl-cc::compile-expression))
        (called nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-cc::compile-expression)
                 (lambda (&rest args)
                   (setf called t)
                   (apply orig args)))
            (expect (cl-cc::our-eval '(defvar *pipeline-cps-fallback* 7)) :to-be '*pipeline-cps-fallback*)
            (expect called :to-be-truthy))
      (ignore-errors (makunbound '*pipeline-cps-fallback*))
      (setf (symbol-function 'cl-cc::compile-expression) orig))))
