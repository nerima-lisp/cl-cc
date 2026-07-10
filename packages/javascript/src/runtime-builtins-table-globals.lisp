;;;; packages/javascript/src/runtime-builtins-table-globals.lisp -- JS constructor globals

(in-package :cl-cc/javascript)

;;; -----------------------------------------------------------------------
;;;  Prelude constructor globals for Map and RegExp
;;;
;;;  These must be defparameter (not defun) so (boundp sym) = t and
;;;  seed-js-runtime-globals seeds them into the VM; the compiler then
;;;  emits vm-get-global without "Unbound variable" errors.
;;;
;;;  *js-map-global* is a hash-table (not a bare function) because Map has
;;;  static methods (Map.groupBy) that JS code accesses via Map.groupBy(...).
;;; -----------------------------------------------------------------------

(defparameter *js-map-global*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "__new__"  ht) #'%js-make-map
          (gethash "groupBy"  ht) #'%js-map-group-by)
    ht)
  "JS Map constructor object: __new__ for `new Map()', groupBy static method.")

(defparameter *js-regexp-global*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "__new__"  ht) (lambda (pat &optional flags)
                                    (%js-make-regex (%js-to-string pat)
                                                    (if (eq flags +js-undefined+)
                                                        ""
                                                        (%js-to-string flags))))
          (gethash "escape"   ht) #'%js-regexp-escape)
    ht)
  "JS RegExp constructor object: __new__ for `new RegExp()', escape static method.")

;;; Wire static methods onto class objects after all helpers are defined.
(setf (gethash "isError" *js-error-class*) #'%js-error-is-error)

(defun %js-builtin-ref (name)
  "Return the host function/value registered for builtin NAME in *js-builtin-map*,
or +js-undefined+.  Used to bind standalone global builtins to a runtime VALUE so
`typeof structuredClone' and `const f = structuredClone; f(x)' work; direct calls
`structuredClone(x)' are handled separately by *js-coercion-call-helpers*, which
lowers to the named %js-* helper the direct-call codegen can dispatch."
  (or (gethash name *js-builtin-map*) +js-undefined+))

(defun %js-make-namespace-object (prefix)
  "Build a JS namespace global object (Math, JSON, …) from *js-builtin-specs*
entries whose key is PREFIX + '.' + property. A property whose name is entirely
uppercase (a constant such as Math.PI / Number.MAX_SAFE_INTEGER) has its zero-arg
spec called to materialize the value; method properties keep their function. This
derives the object straight from the existing dispatch table, so it stays complete
and never references a helper that does not exist."
  (let ((plen (length prefix))
        (pairs nil))
    (dolist (spec *js-builtin-specs*)
      (let ((key (car spec)))
        (when (and (> (length key) (1+ plen))
                   (string= prefix key :end2 plen)
                   (char= (char key plen) #\.))
          (let* ((prop (subseq key (1+ plen)))
                 (val (cdr spec))
                 (constant-p (and (plusp (length prop))
                                  (every (lambda (c)
                                           (or (and (alpha-char-p c) (upper-case-p c))
                                               (digit-char-p c) (char= c #\_)))
                                         prop))))
            (push prop pairs)
            (push (if (and constant-p (functionp val)) (funcall val) val) pairs)))))
    (apply #'%js-make-object (nreverse pairs))))

(defun %js-make-math ()
  "Construct the JS Math global object (constants + methods)."
  (%js-make-namespace-object "Math"))

(defun %js-make-json ()
  "Construct the JS JSON global object (stringify / parse)."
  (%js-make-namespace-object "JSON"))

;;; Like *js-map-global*: Date must be a constructor OBJECT (not a bare
;;; function binding) so that `new Date(...)' routes through __new__ and the
;;; statics Date.now / Date.parse / Date.UTC resolve as properties. A bare
;;; :function prelude binding made %js-new fall through to the empty-object
;;; branch, so every Date instance was a plain {} with no methods.
(defparameter *js-date-global*
  (let ((ht (%js-make-namespace-object "Date")))
    (setf (gethash "__new__"  ht) #'%js-make-date
          ;; Date.UTC is set explicitly: the namespace builder treats all-caps
          ;; keys as constants and would call it at build time.
          (gethash "UTC"      ht) #'%js-date-utc
          ;; Date() without `new' returns the current time as a string.
          (gethash "__call__" ht) (lambda (&rest args)
                                    (declare (ignore args))
                                    (%js-date-to-string (%js-make-date))))
    ht)
  "JS Date constructor object: __new__ for `new Date()', now/parse/UTC statics.")
