;;;; tests/framework-advanced.lisp — CL-CC Test Framework (Advanced Features)
;;;; Parameterized tests and nested sub-cases.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Dynamic Variables for Advanced Features
;;; ------------------------------------------------------------

(defvar *testing-context* nil
  "Stack of nested testing labels, accumulated as a string path.")

;;; ------------------------------------------------------------
;;; FR-015 — testing: Nested Sub-Cases
;;; ------------------------------------------------------------

(defmacro testing (label &body body)
  "Run BODY as a named sub-case within the current test.
Uses *testing-context* to accumulate nesting depth for diagnostics."
  (let ((ctx-var (gensym "CTX")))
    `(let* ((,ctx-var (if *testing-context*
                          (format nil "~A > ~A" *testing-context* ,label)
                          ,label))
            (*testing-context* ,ctx-var))
       ,@body)))

;;; ------------------------------------------------------------
;;; FR-014 — deftest-each: Parameterized Tests
;;; ------------------------------------------------------------

(defun %deftest-each-check-vars (base-name vars)
  "Validate the binding list for a DEFTEST-EACH form."
  (unless (and (listp vars) (every #'symbolp vars))
    (error "DEFTEST-EACH ~A requires a binding list of symbols after :CASES, got ~S"
           base-name vars))
  vars)

(defun %deftest-each-case-values (base-name vars case-entry)
  "Return CASE-ENTRY values after checking label and arity."
  (unless (and (consp case-entry) (stringp (first case-entry)))
    (error "DEFTEST-EACH ~A case must start with a string label, got ~S"
           base-name case-entry))
  (let ((case-vals (rest case-entry)))
    (unless (= (length vars) (length case-vals))
      (error "DEFTEST-EACH ~A case ~S binds ~D vars but supplies ~D values"
             base-name (first case-entry) (length vars) (length case-vals)))
    case-vals))

(defmacro deftest-each (base-name docstring &rest args)
  "Define one test per entry in CASES.
Each case is a list whose first element is the case label string and
whose remaining elements are bound to the variables in the VARS list.

Syntax:
  (deftest-each base-name
    \"docstring\"
    :cases ((\"label\" val ...) ...)
    (var ...)
    body...)

Generates tests named SOURCE/BASE-NAME [label] for each case so
parameterized tests from different files do not silently collide in the global registry."
  (let* ((cases-pos (position :cases args))
         (cases     (if cases-pos (nth (1+ cases-pos) args) nil))
         (body      (if cases-pos (nthcdr (+ 2 cases-pos) args) args)))
    (unless cases-pos
      (error "DEFTEST-EACH ~A requires a :CASES clause" base-name))
    (unless (listp cases)
      (error "DEFTEST-EACH ~A requires :CASES to be a list, got ~S" base-name cases))
    (destructuring-bind (vars &rest body-forms) body
      (unless body-forms
        (error "DEFTEST-EACH ~A requires at least one body form" base-name))
      (let* ((checked-vars (%deftest-each-check-vars base-name vars))
             (source-id  (pathname-name
                          (or *compile-file-pathname*
                              *load-pathname*
                              *default-pathname-defaults*)))
             (expansions
               (loop for case-entry in cases
                     for case-vals = (%deftest-each-case-values base-name checked-vars case-entry)
                     for case-label = (first case-entry)
                     for test-name = (intern
                                      (format nil "~A/~A [~A]"
                                              (string-upcase source-id)
                                              (symbol-name base-name)
                                              case-label))
                     for bindings = (mapcar #'list checked-vars case-vals)
                     ;; The reporter prints the docstring, not the test symbol,
                     ;; so a shared docstring makes every case of one
                     ;; DEFTEST-EACH report under the same line — a failure then
                     ;; names the group without saying which case produced it.
                     for case-docstring = (format nil "~A [~A]" docstring case-label)
                     collect `(deftest ,test-name
                                ,case-docstring
                                (let ,bindings
                                  ,@body-forms)))))
        `(progn ,@expansions)))))
