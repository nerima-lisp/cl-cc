;;;; tests/unit/expand/macros-stdlib-ansi-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib-ansi.lisp
;;;;
;;;; Covers: with-accessors, assert, define-condition,
;;;; with-input-from-string, with-output-to-string,
;;;; with-standard-io-syntax, with-package-iterator, define-compiler-macro.
;;;;
;;;; Note: psetf, shiftf, and define-modify-macro are already covered in
;;;; macro-psetf-tests.lisp, macro-shiftf-tests.lisp,
;;;; and macro-define-modify-macro-tests.lisp.

(in-package :cl-cc/test)



;;; ─── with-accessors ───────────────────────────────────────────────────────

(it-sequential "with-accessors-expansion-structure"
  (expect (car (our-macroexpand-1 '(with-accessors ((x x-val)) inst body))) :to-be 'let)
  (let* ((result (our-macroexpand-1 '(with-accessors ((v slot-v)) inst body)))
         (inner  (caddr result)))
    (expect (car inner) :to-be 'symbol-macrolet))
  (let* ((result   (our-macroexpand-1 '(with-accessors ((v get-v)) obj body)))
         (sm-form  (caddr result))
         (bindings (second sm-form))
         (entry    (first bindings)))
    (expect (car entry) :to-be 'v)
    (expect (car (second entry)) :to-be 'get-v)))

;;; ─── assert ───────────────────────────────────────────────────────────────

(it-sequential "assert-expands-to-unless-guard"
  (let ((result (our-macroexpand-1 '(assert (= x 1)))))
    (expect (car result) :to-be 'unless)
    (expect (second result) :to-equal '(= x 1))))

(it-sequential "assert-failure-body-contains-cerror"
  (let* ((result (our-macroexpand-1 '(assert (zerop n))))
         (body   (cddr result)))
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'cerror))) body) :to-be-truthy)))

(it-sequential "assert-datum-forwarded-to-cerror"
  (let* ((result (our-macroexpand-1 '(assert test nil "msg ~A" x)))
         (body   (cddr result))
         (cerror (find 'cerror body :key #'car)))
    (expect cerror :to-be-truthy)
    (expect (third cerror) :to-equal "msg ~A")))

(it-sequential "assert-with-places-wraps-store-value-restart"
  (let* ((result (our-macroexpand-1 '(assert ok (x y))))
         (loop-form (third result))
         (if-form (third loop-form))
         (restart-form (third if-form))
         (store-value-clause (third restart-form)))
    (expect (car result) :to-be 'let)
    (expect (car loop-form) :to-be 'loop)
    (expect (car if-form) :to-be 'if)
    (expect (car restart-form) :to-be 'restart-case)
    (expect (car store-value-clause) :to-be 'store-value)))

;;; ─── define-condition ─────────────────────────────────────────────────────

(it-sequential "define-condition-basic-structure"
  (let ((result (our-macroexpand-1 '(define-condition my-err (error) ()))))
    (expect (car result) :to-be 'defclass)
    (expect (second result) :to-be 'my-err))
  (let* ((result (our-macroexpand-1 '(define-condition my-err (simple-error) ())))
         (parents (third result)))
    (expect (member 'simple-error parents) :to-be-truthy)))

(it-sequential "define-condition-with-report-wraps-in-progn string"
  (destructuring-bind (form) (list '(define-condition my-err (error) () (:report "something went wrong")))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'progn)
    (expect (car (second result)) :to-be 'defclass)
    (expect (car (third result)) :to-be 'defmethod))))

(it-sequential "define-condition-with-report-wraps-in-progn lambda"
  (destructuring-bind (form) (list '(define-condition my-err (error) ()
                       (:report (lambda (c s) (format s "err: ~A" c)))))
    (let ((result (our-macroexpand-1 form)))
    (expect (car result) :to-be 'progn)
    (expect (car (second result)) :to-be 'defclass)
    (expect (car (third result)) :to-be 'defmethod))))

;;; ─── with-input-from-string ───────────────────────────────────────────────

(it-sequential "with-input-from-string-structure"
  (expect (car (our-macroexpand-1 '(with-input-from-string (s "hello") body))) :to-be 'let*)
  (let* ((result   (our-macroexpand-1 '(with-input-from-string (s "hello") body)))
         (bindings (second result))
         (stream-binding (second bindings)))
    (expect (car (second stream-binding)) :to-be 'make-string-input-stream))
  (let* ((result   (our-macroexpand-1
                    '(with-input-from-string (s "hello" :start 1 :end 3) body)))
         (bindings (second result))
         (str-binding (first bindings)))
    (expect (car (second str-binding)) :to-be 'subseq)))

;;; ─── with-output-to-string ────────────────────────────────────────────────

(it-sequential "with-output-to-string-structure"
  (expect (car (our-macroexpand-1 '(with-output-to-string (s) body))) :to-be 'let)
  (let* ((result   (our-macroexpand-1 '(with-output-to-string (s) body)))
         (bindings (second result))
         (binding  (first bindings)))
    (expect (car (second binding)) :to-be 'make-string-output-stream))
  (let* ((result    (our-macroexpand-1 '(with-output-to-string (s) body)))
         (last-form (car (last result))))
    (expect (car last-form) :to-be 'get-output-stream-string))
  (let* ((result (our-macroexpand-1 '(with-output-to-string (s "prefix") body)))
         (body   (cddr result)))
    (expect (some (lambda (f) (and (consp f) (eq (car f) 'write-string))) body) :to-be-truthy)))

;;; ─── with-standard-io-syntax ──────────────────────────────────────────────

(it-sequential "with-standard-io-syntax-structure"
  (expect (car (our-macroexpand-1 '(with-standard-io-syntax body))) :to-be 'let)
  (let ((bindings (second (our-macroexpand-1 '(with-standard-io-syntax body)))))
    (expect (= 10 (second (assoc '*print-base* bindings))) :to-be-truthy)
    (expect (= 10 (second (assoc '*read-base*  bindings))) :to-be-truthy)
    (expect (second (assoc '*print-readably* bindings)) :to-be 't)
    (expect (assoc '*print-pprint-dispatch* bindings) :to-be-truthy)
    (expect (assoc '*readtable* bindings) :to-be-truthy))
  (let* ((result (our-macroexpand-1 '(with-standard-io-syntax body)))
         (body   (cddr result)))
    (expect (member 'body body) :to-be-truthy)))

;;; ─── with-package-iterator ───────────────────────────────────────────────

(it-sequential "with-package-iterator-honors-requested-symbol-types"
  (let ((cl-cc/runtime:*rt-package-registry* (make-hash-table :test #'equal)))
    (let* ((lib (cl-cc/runtime:rt-make-package "WPI-LIB"))
           (user (cl-cc/runtime:rt-make-package "WPI-USER"))
           (internal-sym (cl-cc/runtime:rt-intern "INTERNAL" user))
           (external-sym (cl-cc/runtime:rt-intern "EXTERNAL" user))
           (inherited-sym (cl-cc/runtime:rt-intern "IMPORTED" lib)))
      (cl-cc/runtime:rt-export external-sym user)
      (cl-cc/runtime:rt-export inherited-sym lib)
      (cl-cc/runtime:rt-use-package lib user)
      (expect (cl-cc/runtime:rt-intern "INTERNAL" user) :to-be internal-sym)
      (let ((expected '(("INTERNAL" :internal "WPI-USER")
                        ("EXTERNAL" :external "WPI-USER")
                        ("IMPORTED" :inherited "WPI-USER")))
            (expanded (our-macroexpand-1
                       '(with-package-iterator (next (list user) :internal :external :inherited)
                          (next)))))
        (expect (%tree-contains-head-p 'cl-cc/expand::%package-iterator-entries expanded) :to-be-truthy)
        (expect (mapcar (lambda (entry)
                   (destructuring-bind (sym type pkg) entry
                     (list (cl-cc/runtime:rt-symbol-name sym)
                           type
                           (cl-cc/runtime:rt-package-name pkg))))
                 (cl-cc/expand::%package-iterator-entries
                  (list user)
                  '(:internal :external :inherited))) :to-equal expected)))))

;;; ─── define-compiler-macro ────────────────────────────────────────────────

(it-sequential "define-compiler-macro-returns-quoted-name"
  (let ((result (our-macroexpand-1
                 '(define-compiler-macro my-fn (x) (1+ x)))))
    (expect (car result) :to-be 'quote)
    (expect (second result) :to-be 'my-fn)))
