(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Shared state-copy helpers
;;; ------------------------------------------------------------

(defmacro with-replaced-function ((name replacement) &body body)
  "Temporarily replace NAME's global function binding while BODY runs."
  (let ((old (gensym "OLD-FN"))
        (had (gensym "HAD-FN")))
    `(let ((,had (fboundp ',name))
           (,old (ignore-errors (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name) ,replacement)
              ,@body)
         (if ,had
             (setf (symbol-function ',name) ,old)
             (fmakunbound ',name))))))

(defmacro with-restored-binding ((place) &body body)
  "Run BODY and restore PLACE to its original value afterward."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved ,place))
       (unwind-protect
            (progn ,@body)
         (setf ,place ,saved)))))

(defmacro with-restored-bindings (bindings &body body)
  "Run BODY while restoring each binding in BINDINGS afterward."
  (labels ((normalize-binding (binding)
             (if (consp binding)
                 binding
                 (list binding))))
    (if bindings
        `(with-restored-binding ,(normalize-binding (first bindings))
           (with-restored-bindings ,(rest bindings)
             ,@body))
        `(progn ,@body))))

(defmacro %with-preserved-env-var ((name) &body body)
  "Run BODY while restoring environment variable NAME afterward."
  (let ((saved (gensym "SAVED-ENV")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn ,@body)
         (if ,saved
             (sb-posix:setenv ,name ,saved 1)
             (sb-posix:unsetenv ,name))))))

(defun %read-file-contents (path)
  "Return PATH as a string."
  (with-open-file (stream path :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun %read-file-lines (path)
  "Return PATH as a list of lines."
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun %write-file-contents (path contents)
  "Write CONTENTS to PATH."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defun %write-file-lines (path lines)
  "Write LINES to PATH, one line per item."
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line stream))))

(defun %copy-macro-environment ()
  "Return a fresh macro-env instance populated from the current global macro table."
  (let* ((copy (make-instance 'cl-cc/expand:macro-env))
         (src  (cl-cc/expand:macro-env-table cl-cc/expand:*macro-environment*))
         (dst  (cl-cc/expand:macro-env-table copy)))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    copy))

(defun %snapshot-hash-table (table &key (copy-value 'identity))
  "Return a fresh snapshot of TABLE, applying COPY-VALUE to every value."
  (let ((snapshot (make-hash-table :test (hash-table-test table)
                                   :size (hash-table-count table))))
    (maphash (lambda (k v)
               (setf (gethash k snapshot) (funcall copy-value v)))
             table)
    snapshot))

(defun %restore-hash-table (target snapshot &key (copy-value 'identity))
  "Replace TARGET contents with SNAPSHOT, applying COPY-VALUE during restore."
  (clrhash target)
  (maphash (lambda (k v)
             (setf (gethash k target) (funcall copy-value v)))
           snapshot)
  target)

(defun %copy-hash-table-shallow (table)
  "Return a shallow copy of TABLE."
  (%snapshot-hash-table table :copy-value #'identity))

(defmacro with-restored-hash-table ((place &key (copy-value 'identity)) &body body)
  "Run BODY while restoring PLACE to its original contents afterward."
  (let ((table (gensym "TABLE"))
        (saved (gensym "SAVED")))
    (if (eq copy-value 'identity)
        `(let* ((,table ,place)
                (,saved (%snapshot-hash-table ,table :copy-value #'identity)))
           (unwind-protect
                (progn ,@body)
             (%restore-hash-table ,table ,saved :copy-value #'identity)))
        `(let* ((,table ,place)
                (,saved (%snapshot-hash-table ,table :copy-value ,copy-value)))
           (unwind-protect
                (progn ,@body)
             (%restore-hash-table ,table ,saved :copy-value ,copy-value))))))

(defmacro with-cleared-hash-table ((place &key (copy-value 'identity)) &body body)
  "Run BODY with PLACE cleared, restoring its prior contents afterward."
  (let ((table (gensym "TABLE")))
    (if (eq copy-value 'identity)
        `(let ((,table ,place))
           (with-restored-hash-table (,table :copy-value #'identity)
             (clrhash ,table)
             ,@body))
        `(let ((,table ,place))
           (with-restored-hash-table (,table :copy-value ,copy-value)
             (clrhash ,table)
             ,@body)))))

;;; ------------------------------------------------------------
;;; Global cache isolation fixtures
;;; ------------------------------------------------------------

;; Global cl-weave before-each hooks (registered into the root suite at load
;; time), replacing the old (defbefore :each (cl-cc-suite) ...) fixtures now that
;; every test lives in cl-weave's root suite rather than the framework's
;; cl-cc-suite. They keep the same cross-test cache isolation.
(before-each
  (cl-cc/vm::vm-clear-hash-cons-table))

(before-each
  (when (boundp 'cl-cc/expand::*macroexpand-step-cache*)
    (clrhash cl-cc/expand::*macroexpand-step-cache*))
  (when (boundp 'cl-cc/expand::*macroexpand-all-cache*)
    (clrhash cl-cc/expand::*macroexpand-all-cache*)))

;;; ------------------------------------------------------------
;;; Test fixtures
;;; ------------------------------------------------------------

(defmacro with-reset-repl-state (&body body)
  "Run BODY with a clean REPL state and restore the caller package afterwards.

REPL-oriented tests reset the persistent VM/compiler state, but some test flows
also switch `*package*` via `(in-package ...)`. Restoring the caller package
here keeps REPL fixtures order-independent without widening the public fixture
surface."
  (let ((saved-package (gensym "SAVED-PACKAGE")))
    `(let ((,saved-package *package*))
       (unwind-protect
            (progn
              (reset-repl-state)
              (setf *package* ,saved-package)
              ,@body)
         (reset-repl-state)
         (setf *package* ,saved-package)))))

(defun make-test-vm ()
  "Create a fresh VM I/O state for instruction-level testing."
  (cl-cc:make-vm-state))

(defmacro with-test-vm ((state &rest bindings) &body body)
  "Create a fresh VM STATE, preload register BINDINGS, then run BODY."
  `(let ((,state (make-test-vm)))
     ,@(mapcar (lambda (binding)
                 (destructuring-bind (register value) binding
                   `(cl-cc:vm-reg-set ,state ,register ,value)))
               bindings)
     ,@body))

(defun vm-exec (inst state &optional (pc 0) (labels (make-hash-table :test #'equal)))
  "Execute one VM instruction and return the next program counter."
  (cl-cc:execute-instruction inst state pc labels))

(defun exec1 (inst state &optional (pc 0))
  "Execute one VM instruction with a fresh label table."
  (vm-exec inst state pc (make-hash-table)))

;;; ------------------------------------------------------------
;;; Invariants
;;; ------------------------------------------------------------

(defmacro definvariant (name &rest args)
  "Register an invariant checked after every test.
   Syntax: (definvariant name [:after-each t] body...)"
  (let ((body args))
    (when (and body (eq (first body) :after-each))
      (setf body (cddr body)))
    `(progn
       (push (cons ',name (lambda () ,@body)) *invariant-registry*)
       ',name)))

(defun %run-invariants ()
  "Run all registered invariants, failing with the invariant name on violation."
  (dolist (inv *invariant-registry*)
    (handler-case
        (funcall (cdr inv))
      (error (c)
        (cl-weave:fail "Invariant ~S violated: ~A" (car inv) c)))))
