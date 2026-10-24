;;;; pipeline-repl-tests.lisp — run-string-repl, reset-repl-state, compile-with-stdlib, whitespace-symbol-p tests
(in-package :cl-cc/test)

;; REPL integration tests intentionally mutate process-global compiler / VM state
;; (`*repl-vm-state*`, label counters, current package, stdlib snapshots). Keep
;; them in a dedicated serial child suite so randomized mixed-mode execution does
;; not reintroduce order-dependent flakes.


;;; ─── run-string-repl (persistent state) ─────────────────────────────────

(it-sequential "pipeline-repl-simple-eval"
  (with-reset-repl-state
    (let ((result (run-string-repl "42")))
      (expect (= 42 result) :to-be-truthy))))

(it-sequential "pipeline-repl-elisp-language-argument"
  (let ((seen-parse-language nil)
        (seen-compile-language nil)
        (orig-parse (symbol-function 'cl-cc/parse:parse-source-for-language))
        (orig-compile (symbol-function 'cl-cc::compile-string))
        (orig-run (symbol-function 'cl-cc/repl::%run-form-repl-impl)))
    (unwind-protect
         (progn
           (sb-ext:without-package-locks
             (setf (symbol-function 'cl-cc/parse:parse-source-for-language)
                   (lambda (source language)
                     (declare (ignore source))
                     (setf seen-parse-language language)
                     '((+ 1 2))))
             (setf (symbol-function 'cl-cc::compile-string)
                   (lambda (source &rest args &key target language &allow-other-keys)
                     (declare (ignore source target args))
                     (setf seen-compile-language language)
                     :compiled))
             (setf (symbol-function 'cl-cc/repl::%run-form-repl-impl)
                   (lambda (&rest args)
                     (declare (ignore args))
                     123)))
           (expect (= 123 (run-string-repl "ignored" :language :elisp)) :to-be-truthy)
           (expect seen-parse-language :to-be :elisp)
           (expect seen-compile-language :to-be :elisp))
      (sb-ext:without-package-locks
        (setf (symbol-function 'cl-cc/parse:parse-source-for-language) orig-parse
              (symbol-function 'cl-cc::compile-string) orig-compile
              (symbol-function 'cl-cc/repl::%run-form-repl-impl) orig-run)))))

(it-sequential "pipeline-repl-persistence defun"
  (destructuring-bind (setup check expected) (list "(defun repl-test-double (x) (* x 2))" "(repl-test-double 21)" 42)
    (with-reset-repl-state
    (run-string-repl setup)
    (expect (= expected (run-string-repl check)) :to-be-truthy))))

(it-sequential "pipeline-repl-persistence defvar"
  (destructuring-bind (setup check expected) (list "(defvar *repl-test-val* 99)" "*repl-test-val*" 99)
    (with-reset-repl-state
    (run-string-repl setup)
    (expect (= expected (run-string-repl check)) :to-be-truthy))))

(it-sequential "pipeline-repl-defvar-without-init-persists-as-global"
  (with-reset-repl-state
    (run-string-repl "(defvar *repl-no-init*)")
    (run-string-repl "(setf *repl-no-init* 3)")
    (expect (= 3 (run-string-repl "*repl-no-init*")) :to-be-truthy)))

(it-sequential "pipeline-run-form-repl-defvar-without-init-persists-as-global"
  (with-reset-repl-state
    (let ((form (first (cl-cc/parse:parse-all-forms "(defvar *run-form-no-init*)"))))
      (cl-cc::run-form-repl form)
      (run-string-repl "(setf *run-form-no-init* 7)")
      (expect (= 7 (run-string-repl "*run-form-no-init*")) :to-be-truthy))))

(it-sequential "pipeline-run-form-repl-registers-top-level-defmacro"
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-defmacro (&body body) `(progn ,@body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (expect (cl-cc::run-form-repl form) :to-be macro-name)
            (let ((expander (gethash macro-name table)))
               (expect expander :to-be-truthy)
               (expect (cl-cc/expand::invoke-registered-expander
                             expander '(pipeline-repl-temp-defmacro (print 1)) nil) :to-equal '(progn (print 1)))))
      (remhash macro-name table))))

(it-sequential "pipeline-run-form-repl-registers-destructuring-defmacro"
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-DESTRUCTURING-DEFMACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(defmacro pipeline-repl-temp-destructuring-defmacro (name (parent) &body body) `(list ',name ',parent ',body))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (expect (cl-cc::run-form-repl form) :to-be macro-name)
             (let ((expander (gethash macro-name table)))
               (expect expander :to-be-truthy)
                (expect (cl-cc/expand::invoke-registered-expander
                             expander
                             '(pipeline-repl-temp-destructuring-defmacro foo (bar) baz quux)
                             nil) :to-equal '(list 'foo 'bar '(baz quux)))))
      (remhash macro-name table))))

(it-sequential "pipeline-repl-defun-not-host-only"
  (multiple-value-bind (result handled-p)
      (cl-cc::%handle-host-only-top-level-form '(defun demo (x) x))
    (expect handled-p :to-be-falsy)
    (expect result :to-be-null)))

(it-sequential "pipeline-run-form-repl-normalizes-register-macro-lambda-body"
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-TEMP-REGISTER-MACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(register-macro 'pipeline-repl-temp-register-macro (lambda (form env) (declare (ignore env)) (let ((x (second form))) `(progn ,x))))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*)))
    (unwind-protect
         (progn
           (expect (cl-cc::run-form-repl form) :to-be macro-name)
             (let ((expander (gethash macro-name table)))
               (expect expander :to-be-truthy)
                (expect (cl-cc/expand::invoke-registered-expander
                             expander '(pipeline-repl-temp-register-macro 42) nil) :to-equal '(progn 42))))
      (remhash macro-name table))))

(it-sequential "pipeline-run-form-repl-register-macro-without-host-compile"
  (let* ((*package* (find-package :cl-cc/compile))
         (macro-name (intern "PIPELINE-REPL-NO-COMPILE-REGISTER-MACRO" *package*))
         (form (first (cl-cc/parse:parse-all-forms
                       "(register-macro 'pipeline-repl-no-compile-register-macro (lambda (form env) (declare (ignore env)) (second form)))")))
         (table (cl-cc/expand::macro-env-table cl-cc/expand::*macro-environment*))
         (orig (symbol-function 'compile)))
    (unwind-protect
         (progn
            (sb-ext:without-package-locks
              (setf (symbol-function 'compile)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (error "host compile should not be called"))))
             (expect (cl-cc::run-form-repl form) :to-be macro-name)
             (let ((expander (gethash macro-name table)))
               (expect expander :to-be-truthy)
               (expect (cl-cc/expand::invoke-registered-expander
                              expander '(pipeline-repl-no-compile-register-macro 42) nil) :to-equal 42)))
       (sb-ext:without-package-locks
         (setf (symbol-function 'compile) orig))
       (remhash macro-name table))))

(it-sequential "pipeline-run-form-repl-rejects-non-lambda-register-macro"
  (let* ((*package* (find-package :cl-cc/compile))
         (form (first (cl-cc/parse:parse-all-forms
                       "(register-macro 'pipeline-repl-bad-register-macro 42)"))))
    (signals error (cl-cc::run-form-repl form))))

(it-sequential "pipeline-run-string-uses-vm-compile-path-for-safe-single-form"
  (expect (cl-cc:run-string "(+ 1 2)") :to-be 3))

(it-sequential "pipeline-run-string-still-handles-definition-forms"
  (expect (cl-cc:run-string "(defun pipeline-cps-fast-path-def () 42)") :to-be-truthy))

;;; ─── reset-repl-state ──────────────────────────────────────────────────

(it-sequential "pipeline-reset-clears-state"
  (with-reset-repl-state
    (run-string-repl "42")
    (expect (not (null cl-cc::*repl-vm-state*)) :to-be-truthy)
    (reset-repl-state)
    (expect cl-cc::*repl-vm-state* :to-be-null)
    (expect cl-cc::*repl-pool-instructions* :to-be-null)
    (expect cl-cc::*repl-pool-labels* :to-be-null)))

;;; ─── compile-string-with-stdlib ─────────────────────────────────────────

(it-sequential "pipeline-compile-with-stdlib"
  (let ((result (cl-cc::compile-string-with-stdlib "(+ 1 2)" :target :vm)))
    (expect (typep result 'cl-cc/compile:compilation-result) :to-be-truthy)))

;;; ─── run-string with :stdlib ────────────────────────────────────────────

(it-sequential "pipeline-run-string-stdlib-forms mapcar-inc"
  (destructuring-bind (expected expr) (list '(2 3 4) "(mapcar (lambda (x) (+ x 1)) '(1 2 3))")
    (expect (run-string expr :stdlib t) :to-equal expected)))

(it-sequential "pipeline-run-string-stdlib-forms reduce-sum"
  (destructuring-bind (expected expr) (list 10 "(reduce (lambda (a b) (+ a b)) '(1 2 3 4) 0 t)")
    (expect (run-string expr :stdlib t) :to-equal expected)))

;;; ─── %whitespace-symbol-p ───────────────────────────────────────────────

(it-sequential "pipeline-whitespace-symbol-p plain-symbol"
  (destructuring-bind (expected form) (list nil 'hello)
    (expect (cl-cc::%whitespace-symbol-p form) :to-equal expected)))

(it-sequential "pipeline-whitespace-symbol-p nil"
  (destructuring-bind (expected form) (list nil nil)
    (expect (cl-cc::%whitespace-symbol-p form) :to-equal expected)))

(it-sequential "pipeline-whitespace-symbol-p keyword"
  (destructuring-bind (expected form) (list nil :foo)
    (expect (cl-cc::%whitespace-symbol-p form) :to-equal expected)))

(it-sequential "pipeline-whitespace-symbol-p number"
  (destructuring-bind (expected form) (list nil 42)
    (expect (cl-cc::%whitespace-symbol-p form) :to-equal expected)))

(it-sequential "pipeline-whitespace-symbol-p empty-string"
  (destructuring-bind (expected form) (list nil "")
    (expect (cl-cc::%whitespace-symbol-p form) :to-equal expected)))

(it-sequential "pipeline-whitespace-symbol-p-space-sym"
  (let ((ws-sym (intern " " (find-package :cl-cc))))
    (expect (cl-cc::%whitespace-symbol-p ws-sym) :to-be-truthy)))

;;; ─── %ensure-repl-state ─────────────────────────────────────────────────

(it-sequential "pipeline-ensure-repl-state-initializes"
  (with-reset-repl-state
    ;; All state vars should be nil after reset
    (expect cl-cc::*repl-vm-state* :to-be-null)
    (expect cl-cc::*repl-pool-instructions* :to-be-null)
    ;; Trigger lazy init
    (cl-cc::%ensure-repl-state)
    ;; All should now be initialized
    (expect (not (null cl-cc::*repl-vm-state*)) :to-be-truthy)
    (expect (not (null cl-cc::*repl-pool-instructions*)) :to-be-truthy)
    (expect (not (null cl-cc::*repl-pool-labels*)) :to-be-truthy)
    (expect (not (null cl-cc::*repl-global-vars-persistent*)) :to-be-truthy)))

(it-sequential "pipeline-ensure-repl-state-idempotent"
  (with-reset-repl-state
    (cl-cc::%ensure-repl-state)
    (let ((first-state cl-cc::*repl-vm-state*))
      (cl-cc::%ensure-repl-state)
      (expect cl-cc::*repl-vm-state* :to-be first-state))))

;;; ─── FR-312: REPL history and completion ────────────────────────────────

(it-sequential "pipeline-repl-history-records-forms"
  (with-reset-repl-state
    (cl-cc::%repl-record-history "  (+ 1 2)  ")
    (cl-cc::%repl-record-history "")
    (cl-cc::%repl-record-history "(list 1 2)")
    (expect (cl-cc:repl-history) :to-equal '("(+ 1 2)" "(list 1 2)"))))

(it-sequential "pipeline-repl-history-arrow-navigation"
  (with-reset-repl-state
    (cl-cc::%repl-record-history "(+ 1 2)")
    (cl-cc::%repl-record-history "(+ 3 4)")
    (multiple-value-bind (line candidates edited-p)
        (cl-cc:repl-edit-input-line (format nil "~C[A" #\Escape))
      (expect line :to-equal "(+ 3 4)")
      (expect candidates :to-be-null)
      (expect edited-p :to-be-truthy))
    (multiple-value-bind (line candidates edited-p)
        (cl-cc:repl-edit-input-line (format nil "~C[A" #\Escape))
      (expect line :to-equal "(+ 1 2)")
      (expect candidates :to-be-null)
      (expect edited-p :to-be-truthy))
    (multiple-value-bind (line candidates edited-p)
        (cl-cc:repl-edit-input-line (format nil "~C[B" #\Escape))
      (expect line :to-equal "(+ 3 4)")
      (expect candidates :to-be-null)
      (expect edited-p :to-be-truthy))))

(it-sequential "pipeline-repl-completion-includes-package-symbols"
  (with-reset-repl-state
    (let ((*package* (find-package :cl-cc/test)))
      (intern "FR312-UNIQUE-PACKAGE-CANDIDATE" *package*)
      (expect (member "FR312-UNIQUE-PACKAGE-CANDIDATE"
                           (cl-cc:repl-completion-candidates "FR312-UNIQUE-PACKAGE")
                           :test #'string=) :to-be-truthy))))

(it-sequential "pipeline-repl-completion-includes-function-registry"
  (with-reset-repl-state
    (run-string-repl "(defun fr312-repl-complete-fn (x) x)")
    (expect (member "FR312-REPL-COMPLETE-FN"
                         (cl-cc:repl-completion-candidates "FR312-REPL-COMPLETE")
                         :test #'string=) :to-be-truthy)))

(it-sequential "pipeline-repl-tab-completes-single-candidate"
  (with-reset-repl-state
    (let ((*package* (find-package :cl-cc/test)))
      (intern "FR312-TAB-ONLY-CANDIDATE" *package*)
      (multiple-value-bind (line candidates edited-p)
          (cl-cc:repl-edit-input-line (concatenate 'string "FR312-TAB-ONLY" (string #\Tab)))
        (expect line :to-equal "FR312-TAB-ONLY-CANDIDATE")
        (expect candidates :to-equal '("FR312-TAB-ONLY-CANDIDATE"))
        (expect edited-p :to-be-truthy)))))
(it-sequential "pipeline-repl-our-load-binds-nested-pathnames"
  (with-reset-repl-state
    (uiop:with-temporary-file (:pathname parent-path :type "lisp")
      (uiop:with-temporary-file (:pathname child-path :type "lisp")
        (cl-cc::%ensure-repl-state)
        (let* ((globals (cl-cc/vm:vm-global-vars cl-cc::*repl-vm-state*))
               (load-pathname-symbol (find-symbol "*LOAD-PATHNAME*" :cl-cc/vm))
               (load-truename-symbol (find-symbol "*LOAD-TRUENAME*" :cl-cc/vm))
               (previous-load-pathname #P"prior-parent.lisp"))
          (with-open-file (out child-path
                               :direction :output
                               :if-exists :supersede)
            (format out
                    "(defvar *pipeline-repl-child-load-pathname*)~%(defvar *pipeline-repl-child-load-truename*)~%(setf *pipeline-repl-child-load-pathname* *load-pathname*)~%(setf *pipeline-repl-child-load-truename* *load-truename*)~%"))
          (with-open-file (out parent-path
                               :direction :output
                               :if-exists :supersede)
            (format out
                    "(defvar *pipeline-repl-parent-load-pathname*)~%(defvar *pipeline-repl-parent-load-truename*)~%(defvar *pipeline-repl-parent-after-load-pathname*)~%(defvar *pipeline-repl-parent-after-load-truename*)~%(setf *pipeline-repl-parent-load-pathname* *load-pathname*)~%(setf *pipeline-repl-parent-load-truename* *load-truename*)~%(load ~S)~%(setf *pipeline-repl-parent-after-load-pathname* *load-pathname*)~%(setf *pipeline-repl-parent-after-load-truename* *load-truename*)~%"
                    (namestring child-path)))
          (setf (gethash load-pathname-symbol globals) previous-load-pathname)
          (remhash load-truename-symbol globals)
          (cl-cc/repl::our-load parent-path)
          (let ((child-load-pathname
                  (gethash (find-symbol "*PIPELINE-REPL-CHILD-LOAD-PATHNAME*" :cl-cc/test) globals))
                (child-load-truename
                  (gethash (find-symbol "*PIPELINE-REPL-CHILD-LOAD-TRUENAME*" :cl-cc/test) globals))
                (parent-load-pathname
                  (gethash (find-symbol "*PIPELINE-REPL-PARENT-LOAD-PATHNAME*" :cl-cc/test) globals))
                (parent-load-truename
                  (gethash (find-symbol "*PIPELINE-REPL-PARENT-LOAD-TRUENAME*" :cl-cc/test) globals))
                (parent-after-load-pathname
                  (gethash (find-symbol "*PIPELINE-REPL-PARENT-AFTER-LOAD-PATHNAME*" :cl-cc/test) globals))
                (parent-after-load-truename
                  (gethash (find-symbol "*PIPELINE-REPL-PARENT-AFTER-LOAD-TRUENAME*" :cl-cc/test) globals)))
            (expect (pathnamep child-load-pathname) :to-be-truthy)
            (expect (namestring child-load-pathname) :to-equal (namestring child-path))
            (expect (namestring child-load-truename) :to-equal (namestring (truename child-path)))
            (expect (namestring parent-load-pathname) :to-equal (namestring parent-path))
            (expect (namestring parent-load-truename) :to-equal (namestring (truename parent-path)))
            (expect (namestring parent-after-load-pathname) :to-equal (namestring parent-path))
            (expect (namestring parent-after-load-truename) :to-equal (namestring (truename parent-path))))
          (expect (eq (gethash load-pathname-symbol globals) previous-load-pathname) :to-be-truthy)
          (expect (nth-value 1 (gethash load-truename-symbol globals)) :to-be-falsy))))))
(it-sequential "pipeline-repl-our-load-binds-common-lisp-pathnames"
  (with-reset-repl-state
    (uiop:with-temporary-file (:pathname load-path :type "lisp")
      (uiop:with-temporary-file (:pathname error-path :type "lisp")
        (cl-cc::%ensure-repl-state)
        (let* ((globals (cl-cc/vm:vm-global-vars cl-cc::*repl-vm-state*))
               (common-load-pathname-symbol (find-symbol "*LOAD-PATHNAME*" :cl))
               (common-load-truename-symbol (find-symbol "*LOAD-TRUENAME*" :cl))
               (previous-common-load-pathname #P"prior-common-load-pathname.lisp")
               (previous-common-load-truename #P"prior-common-load-truename.lisp"))
          (setf (gethash common-load-pathname-symbol globals) previous-common-load-pathname
                (gethash common-load-truename-symbol globals) previous-common-load-truename)
          (with-open-file (out load-path
                               :direction :output
                               :if-exists :supersede)
            (format out
                    "(setf *pipeline-repl-cl-user-load-pathname* *load-pathname*)~%(setf *pipeline-repl-cl-user-load-truename* *load-truename*)~%"))
          (with-open-file (out error-path
                               :direction :output
                               :if-exists :supersede)
            (write-line "(setf *pipeline-repl-cl-user-error-entered* t)" out)
            (write-line "(error \"expected load error\")" out))
          (let ((*package* (find-package :cl-user)))
            (cl-cc/repl::our-load load-path))
          (let ((loaded-pathname
                  (gethash (find-symbol "*PIPELINE-REPL-CL-USER-LOAD-PATHNAME*" :cl-user)
                           globals))
                (loaded-truename
                  (gethash (find-symbol "*PIPELINE-REPL-CL-USER-LOAD-TRUENAME*" :cl-user)
                           globals)))
            (expect (namestring loaded-pathname) :to-equal (namestring load-path))
            (expect (namestring loaded-truename) :to-equal (namestring (truename load-path))))
          (expect (eq (gethash common-load-pathname-symbol globals)
                      previous-common-load-pathname)
                  :to-be-truthy)
          (expect (eq (gethash common-load-truename-symbol globals)
                      previous-common-load-truename)
                  :to-be-truthy)
          (let ((load-failed-p nil))
            (handler-case
                (let ((*package* (find-package :cl-user)))
                  (cl-cc/repl::our-load error-path))
              (error () (setf load-failed-p t)))
            (expect load-failed-p :to-be-truthy)
            (expect (gethash (find-symbol "*PIPELINE-REPL-CL-USER-ERROR-ENTERED*" :cl-user)
                             globals)
                    :to-be-truthy))
          (expect (eq (gethash common-load-pathname-symbol globals)
                      previous-common-load-pathname)
                  :to-be-truthy)
          (expect (eq (gethash common-load-truename-symbol globals)
                      previous-common-load-truename)
                  :to-be-truthy)
          (remhash common-load-pathname-symbol globals)
          (remhash common-load-truename-symbol globals)
          (let ((*package* (find-package :cl-user)))
            (cl-cc/repl::our-load load-path))
          (expect (nth-value 1 (gethash common-load-pathname-symbol globals))
                  :to-be-falsy)
          (expect (nth-value 1 (gethash common-load-truename-symbol globals))
                  :to-be-falsy)
          (let ((load-failed-p nil))
            (handler-case
                (let ((*package* (find-package :cl-user)))
                  (cl-cc/repl::our-load error-path))
              (error () (setf load-failed-p t)))
            (expect load-failed-p :to-be-truthy))
          (expect (nth-value 1 (gethash common-load-pathname-symbol globals))
                  :to-be-falsy)
          (expect (nth-value 1 (gethash common-load-truename-symbol globals))
                  :to-be-falsy))))))
