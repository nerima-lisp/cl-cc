;;;; tests/pbt/macro-pbt-tests.lisp - Suite, generators and helpers for macro PBT
;;;
;;; This file owns the macro-expansion PBT suite plus the domain generators and
;;; predicates that the macro-pbt-*-tests.lisp files share. The properties
;;; themselves live in macro-pbt-props-tests.lisp, macro-pbt-mv-tests.lisp,
;;; macro-pbt-binding-tests.lisp, macro-pbt-hygiene-tests.lisp and
;;; macro-pbt-advanced-tests.lisp.
;;;
;;; The generators are built from cl-weave 1.0.0's native combinators
;;; (gen-one-of / gen-map / gen-tuple / gen-list / gen-integer / gen-boolean /
;;; gen-string) rather than the home-grown cl-cc/pbt framework's. Two
;;; translations are worth noting:
;;;
;;;   - The originals selected among sub-generators with GEN-BIND over an index
;;;     into a list. That is precisely cl-weave's GEN-ONE-OF, which chooses
;;;     among generators (as opposed to GEN-MEMBER, which chooses among values).
;;;
;;;   - GEN-FMAP becomes GEN-MAP, and the one dependent GEN-BIND (gen-cond-clause,
;;;     which fed a generated test form into a closure building the clause) is a
;;;     GEN-MAP over a GEN-TUPLE, since the test and the body were in fact
;;;     independent.

(in-package :cl-cc/pbt)

;;; Test Suite Definition



;;; Custom Generators for Macro Testing

(defun gen-test-form ()
  "Generate a form suitable for use as a test condition."
  (cl-weave:gen-one-of
   (gen-pbt-symbol "TEST")
   (cl-weave:gen-integer :min -100 :max 100)
   (cl-weave:gen-boolean)
   (cl-weave:gen-map (lambda (x) `(= ,x 0))
                     (cl-weave:gen-integer :min -10 :max 10))))

(defun gen-body-form ()
  "Generate a form suitable for use in a macro body."
  (cl-weave:gen-one-of
   (gen-pbt-symbol "BODY")
   (cl-weave:gen-integer :min -100 :max 100)
   (cl-weave:gen-string :max-length 10)
   (cl-weave:gen-map (lambda (x) `(print ,x))
                     (cl-weave:gen-integer :min -10 :max 10))))

(defun gen-binding-pair ()
  "Generate a (symbol value) binding pair."
  (cl-weave:gen-tuple (gen-pbt-symbol "VAR")
                      (cl-weave:gen-integer :min -100 :max 100)))

(defun gen-binding-list (&key (min-length 0) (max-length 5))
  "Generate a list of binding pairs."
  (cl-weave:gen-list (gen-binding-pair)
                     :min-length min-length
                     :max-length max-length))

(defun gen-cond-clause ()
  "Generate a single COND clause: a test form consed onto a list of body forms."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (test body) parts
       (cons test body)))
   (cl-weave:gen-tuple (gen-test-form)
                       (cl-weave:gen-list (gen-body-form)
                                          :min-length 0 :max-length 3))))

(defun gen-cond-clauses (&key (min-length 0) (max-length 5))
  "Generate a list of COND clauses."
  (cl-weave:gen-list (gen-cond-clause)
                     :min-length min-length
                     :max-length max-length))

(defun gen-variable-list (&key (min-length 0) (max-length 5))
  "Generate a list of variable symbols."
  (cl-weave:gen-list (gen-pbt-symbol "VAR")
                     :min-length min-length
                     :max-length max-length))

;;; Helper Functions for Property Testing

(defun form-contains-gensym-p (form)
  "Check if FORM contains any gensym symbols (starting with G or ending with number)."
  (labels ((check-symbol (sym)
             (let ((name (symbol-name sym)))
               (or (and (> (length name) 1)
                        (string= (subseq name 0 1) "G")
                        (some #'digit-char-p name))
                   (some #'digit-char-p name))))
           (check-form (f)
             (typecase f
               (symbol (check-symbol f))
               (cons (or (check-form (car f))
                         (check-form (cdr f))))
               (t nil))))
    (check-form form)))

(defun count-symbols-in-form (symbol form)
  "Count occurrences of SYMBOL in FORM."
  (labels ((count-in (f)
             (typecase f
               (symbol (if (eq f symbol) 1 0))
               (cons (+ (count-in (car f))
                        (count-in (cdr f))))
               (t 0))))
    (count-in form)))

(defun form-contains-symbol-p (symbol form)
  "Check if FORM contains SYMBOL."
  (> (count-symbols-in-form symbol form) 0))

;;; All macro expansion properties (WHEN, UNLESS, COND, AND, OR, LET*, etc.)
;;; are in macro-pbt-props-tests.lisp.
