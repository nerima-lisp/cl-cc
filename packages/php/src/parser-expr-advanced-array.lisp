;;;; frontend/php/parser-expr-advanced-array.lisp -- PHP Array and Collection Expression Helpers
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Array / Collection Helpers ─────────────────────────────────────────────

(defun %php-double-arrow-p (stream)
  "Return true when STREAM begins with PHP double-arrow '=>'."
  (or (eq (php-peek-type stream) :T-DOUBLE-ARROW)
      (and (eq (php-peek-type stream) :T-OP)
           (equal "=>" (php-peek-value stream)))))

(defun %php-array-ref-call (array key)
  "Lower ARRAY[KEY] to the PHP ordered-array reference helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-ref)
                 :args (list array key)))

(defun %php-destructure-ref-call (array key)
  "Lower destructuring ARRAY[KEY] to the PHP 8.5 warning-aware helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-destructure-ref)
                 :args (list array key)))

(defun %php-array-ref-call-p (node)
  "Return true when NODE is a %php-array-ref helper call."
  (%php-helper-call-p node 'cl-cc/php::%php-array-ref 2))

(defun %php-array-literal-call-p (node)
  "Return true when NODE is a %php-array constructor call (an array literal),
i.e. a valid destructuring-assignment target like [$a, $b] or list($a, $b)."
  (%php-helper-call-p node 'cl-cc/php::%php-array))

(defun %php-list-constructor-p (node)
  "True when NODE is a [..] / list(..) constructor (a %php-array call) — i.e. a
nested destructuring target."
  (%php-helper-call-p node 'cl-cc/php::%php-array))

(defun %php-collect-list-bindings (lhs val bindings)
  "Accumulate destructuring bindings for LHS (a %php-array constructor) pulling
from the VAL expression.  Honours `key => $var' entries (access by key) and
recurses into nested [..] / list(..) targets.  Returns the bindings list, with
later entries pushed on the front (caller nreverses)."
  (let ((idx 0))
    (dolist (entry (ast-call-args lhs) bindings)
      ;; Each entry is (make-ast-list :elements (key-present-p key value)).
      (let* ((elts (and (ast-list-p entry) (ast-list-elements entry)))
             (key-present (and elts (ast-quote-p (first elts)) (ast-quote-value (first elts))))
             (key (and elts (second elts)))
             (target (third elts))
             (access (if key-present
                         (%php-destructure-ref-call val key)
                         (%php-destructure-ref-call val (make-ast-int :value idx)))))
        (cond
          ((null target) nil)                     ; [ , $b] hole — skip
          ((ast-var-p target)
           (push (cons (ast-var-name target) access) bindings))
          ((%php-list-constructor-p target)        ; nested [$a,$b] / list(..)
           (setf bindings (%php-collect-list-bindings target access bindings)))))
      (incf idx))
    bindings))

(defun %php-lower-list-assign (lhs val)
  "Lower [$a, $b, ...] = VAL destructuring assignment. LHS is a %php-array
constructor call whose entry values are the assignment targets. Each target
binds to (%php-array-ref VAL <index-or-key>) in a single body-less let
(consistent with how plain $x = v introduces a binding). NIL holes ([, $b]) are
skipped; `key => $var' uses the key; nested [..] targets recurse. Re-references
VAL per element, which is correct for the common variable RHS."
  (make-ast-let
   :bindings (nreverse (%php-collect-list-bindings lhs val nil))
   :body nil))

(defun %php-array-set-call (array key value)
  "Lower ARRAY[KEY] = VALUE to the PHP ordered-array mutation helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-set)
                  :args (list array key value)))

(defun %php-array-append-call (array)
  "Lower ARRAY[] (empty subscript) to an append-target marker. This is only a
valid assignment LHS; the assignment parser turns `$a[] = v' into a push. Using []
to read is a PHP fatal error, represented here by the %php-array-append-target
marker."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-append-target)
                 :args (list array)))

(defun %php-array-append-call-p (node)
  "Return true when NODE is an ARRAY[] append-target marker."
  (%php-helper-call-p node 'cl-cc/php::%php-array-append-target))

(defun %php-array-unset-call (array key)
  "Lower unset(ARRAY[KEY]) to the PHP ordered-array deletion helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-unset)
                 :args (list array key)))

(defun %php-array-entry (key-present-p key value)
  "Build a runtime entry descriptor for %php-array."
  (make-ast-list :elements (list (make-ast-quote :value key-present-p)
                                 key
                                 value)))

(defun %php-array-call (entries)
  "Build the %php-array constructor call for ENTRIES."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array)
                 :args entries))

(defun %php-parse-array-expr (stream known-vars &key (open :T-LBRACKET) (close :T-RBRACKET))
  "Parse PHP array literals written as [..] or array(..)."
  (multiple-value-bind (tok rest) (php-expect open stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) close)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values (%php-array-call nil) rest2 known-vars))
        (let ((entries nil)
              (current rest)
              (kv known-vars))
          (loop
            ;; Spread element: [...$b] — splice another iterable's entries in.
            (if (eq (php-peek-type current) :T-ELLIPSIS)
                (multiple-value-bind (_tok rest-after) (php-consume current)
                  (declare (ignore _tok))
                  (multiple-value-bind (spread-expr rest2 kv2) (php-parse-expr rest-after kv)
                    (push (%php-array-entry nil (make-ast-quote :value nil)
                                            (make-ast-call :func (make-ast-var :name '%php-spread)
                                                           :args (list spread-expr)))
                          entries)
                    (setf current rest2 kv kv2)))
            (multiple-value-bind (first-expr rest2 kv2) (php-parse-expr current kv)
              (if (%php-double-arrow-p rest2)
                  (multiple-value-bind (arrow-tok rest3) (php-consume rest2)
                    (declare (ignore arrow-tok))
                    (multiple-value-bind (value-expr rest4 kv4) (php-parse-expr rest3 kv2)
                      (push (%php-array-entry t first-expr value-expr) entries)
                      (setf current rest4 kv kv4)))
                  (progn
                    (push (%php-array-entry nil (make-ast-quote :value nil) first-expr) entries)
                    (setf current rest2 kv kv2)))))
            (cond
              ((eq (php-peek-type current) :T-COMMA)
               (setf current (cdr current))
               (when (eq (php-peek-type current) close)
                 (return)))
              ((eq (php-peek-type current) close)
               (return))
              (t
               (error "PHP parse error: expected comma or ~S in array literal, got ~S"
                      close (php-peek current)))))
          (multiple-value-bind (tok2 rest2) (php-expect close current)
            (declare (ignore tok2))
            (values (%php-array-call (nreverse entries)) rest2 kv))))))
