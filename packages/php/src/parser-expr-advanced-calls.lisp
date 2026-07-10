;;;; frontend/php/parser-expr-advanced-calls.lisp -- PHP Function Call Expression Lowering
;;;;
;;;; Split from parser-expr-advanced.lisp.

(in-package :cl-cc/php)

;;; ─── Function Call / Builtin Resolution ─────────────────────────────────────

(defun %php-builtin-helper-symbol (qualified-name)
  "Return the PHP runtime helper symbol for simple builtin QUALIFIED-NAME, or NIL.

Consults the central builtin registry (populated by %php-register-builtin) so
every registered builtin — array_*, str*, is_*, math, type predicates, … —
lowers to its cl-cc/php::%php- helper. Previously a hand-coded cond covered only
count/strlen/strtolower/strtoupper/isset/array_key_exists, so the other ~80
registered builtins fell back to an unbridged user symbol and hit `Undefined
function' at runtime (e.g. array_push)."
  (let ((lower (string-downcase qualified-name)))
    (unless (find (code-char 92) lower)
      (%php-lookup-builtin-symbol lower))))

(defun %php-simple-function-spelling (qualified-name)
  "Return QUALIFIED-NAME as a simple global function name, or NIL."
  (let ((name (%php-strip-leading-namespace-separator qualified-name)))
    (unless (find (code-char 92) name)
      name)))

(defun %php-global-builtin-function-p (qualified-name fallback-name)
  "Return true when QUALIFIED-NAME can use PHP global builtin helper lowering."
  (let ((simple-name (%php-simple-function-spelling qualified-name)))
    (and simple-name
         (or (%php-name-absolute-p qualified-name)
             (not (and *php-current-namespace*
                       (not (string= *php-current-namespace* "")))))
         (string= (string-upcase simple-name)
                  (symbol-name fallback-name)))))

(defun %php-fcc-marker-p (node)
  "True when NODE is the first-class-callable marker emitted for f(...)."
  (and (ast-call-p node)
       (let ((f (ast-call-func node)))
         (and (ast-var-p f) (eq (ast-var-name f) '%php-first-class-callable)))))

(defun %php-build-fcc-closure (fn-sym)
  "Build the PHP 8.1 first-class-callable closure for FN-SYM — fn(...$a) =>
FN-SYM(...$a) — a variadic closure that forwards all of its arguments."
  (let* ((args-sym (gensym "PHP-FCC-ARGS-"))
         (body (make-ast-apply
                :func fn-sym
                :args (list (make-ast-call
                             :func (make-ast-var :name 'cl-cc/php::%php-array-values-list)
                             :args (list (make-ast-var :name args-sym)))))))
    (multiple-value-bind (rest-param wrapped-body)
        (%php-variadic-rest-binding args-sym (list body))
      (make-ast-lambda :params nil :optional-params nil
                       :rest-param rest-param :body wrapped-body))))

(defun %php-preg-match-out-param-call-p (qualified-name args)
  "True when this is preg_match / preg_match_all with a $matches variable arg —
the call whose third argument is a by-reference out-parameter."
  (and (>= (length args) 3)
       (typep (third args) 'cl-cc/ast:ast-var)
       (let ((name (string-downcase (%php-strip-leading-namespace-separator qualified-name))))
         (or (string= name "preg_match") (string= name "preg_match_all")))))

(defun %php-lower-preg-match-out-param (qualified-name args)
  "Lower preg_match($p,$s,$m) / preg_match_all($p,$s,$m) so $matches is populated
in the caller's variable: assign $m = the matches array (a helper returns it BY
VALUE — the VM copies host structs across the bridge, so a ref box cannot
propagate), then yield the match count.  Pattern/subject and optional
flags/offset are bound to temps so each is evaluated once and the two helper
calls never share arg AST nodes."
  (let* ((name (string-downcase (%php-strip-leading-namespace-separator qualified-name)))
         (all-p (string= name "preg_match_all"))
         (matches-sym (if all-p 'cl-cc/php::%php-preg-match-all-matches
                          'cl-cc/php::%php-preg-match-matches))
         (count-sym (if all-p 'cl-cc/php::%php-preg-match-all '%php-preg-match))
         (matches-var (cl-cc/ast:ast-var-name (third args)))
         (pat-tmp (gensym "PHP-PREG-PAT-"))
         (subj-tmp (gensym "PHP-PREG-SUBJ-"))
         (flags-p (>= (length args) 4))
         (offset-p (>= (length args) 5))
         (flags-tmp (and flags-p (gensym "PHP-PREG-FLAGS-")))
         (offset-tmp (and offset-p (gensym "PHP-PREG-OFFSET-")))
         (count-tmp (gensym "PHP-PREG-COUNT-"))
         (bindings (append (list (cons pat-tmp (first args))
                                 (cons subj-tmp (second args)))
                           (when flags-p (list (cons flags-tmp (fourth args))))
                           (when offset-p (list (cons offset-tmp (fifth args))))))
         (count-args (cond
                       (offset-p (list (make-ast-var :name pat-tmp)
                                       (make-ast-var :name subj-tmp)
                                       (%php-null-quote)
                                       (if flags-p (make-ast-var :name flags-tmp) (make-ast-int :value 0))
                                       (make-ast-var :name offset-tmp)))
                       (flags-p (list (make-ast-var :name pat-tmp)
                                      (make-ast-var :name subj-tmp)
                                      (%php-null-quote)
                                      (make-ast-var :name flags-tmp)))
                       (t (list (make-ast-var :name pat-tmp)
                                (make-ast-var :name subj-tmp)))))
         (matches-args (cond
                         (offset-p (list (make-ast-var :name pat-tmp)
                                         (make-ast-var :name subj-tmp)
                                         (if flags-p (make-ast-var :name flags-tmp) (make-ast-int :value 0))
                                         (make-ast-var :name offset-tmp)))
                         (flags-p (list (make-ast-var :name pat-tmp)
                                        (make-ast-var :name subj-tmp)
                                        (make-ast-var :name flags-tmp)))
                         (t (list (make-ast-var :name pat-tmp)
                                  (make-ast-var :name subj-tmp))))))
    (make-ast-let
     :bindings bindings
     :body (list
            (make-ast-let
             :bindings (list (cons count-tmp
                                   (make-ast-call :func (make-ast-var :name count-sym)
                                                  :args count-args)))
             :body (list
                    (make-ast-setq
                     :var matches-var
                     :value (make-ast-call :func (make-ast-var :name matches-sym)
                                           :args matches-args))
                    (make-ast-var :name count-tmp)))))))

(defun %php-sscanf-out-param-call-p (qualified-name args)
  "True when this is sscanf($str,$fmt,$a,...) with variable out-params."
  (and (>= (length args) 3)
       (every (lambda (arg) (typep arg 'cl-cc/ast:ast-var)) (cddr args))
       (string= (string-downcase
                 (%php-strip-leading-namespace-separator qualified-name))
                "sscanf")))

(defun %php-scanf-out-param-call-p (qualified-name args)
  "True when this is scanf($fmt,$a,...) with variable out-params."
  (and (>= (length args) 2)
       (every (lambda (arg) (typep arg 'cl-cc/ast:ast-var)) (cdr args))
       (string= (string-downcase
                 (%php-strip-leading-namespace-separator qualified-name))
                "scanf")))

(defun %php-lower-scanf-like-out-param (values-helper helper-args out-args temp-prefix)
  "Lower scanf-family out-params into one parse, assignments, then count."
  (let ((values-tmp (gensym temp-prefix))
        (out-vars (mapcar #'cl-cc/ast:ast-var-name out-args)))
    (make-ast-let
     :bindings (list (cons values-tmp
                           (make-ast-call
                            :func (make-ast-var :name values-helper)
                            :args helper-args)))
     :body (append
            (loop for var in out-vars
                  for idx from 0
                  collect (make-ast-setq
                           :var var
                           :value (make-ast-call
                                   :func (make-ast-var :name 'cl-cc/php::%php-array-ref)
                                        :args (list (make-ast-var :name values-tmp)
                                                    (make-ast-quote :value idx)))))
            (list (make-ast-call
                   :func (make-ast-var :name 'cl-cc/php::%php-count)
                   :args (list (make-ast-var :name values-tmp))))))))

(defun %php-lower-sscanf-out-param (args)
  "Lower sscanf($str,$fmt,$a,...) into caller out-param assignments."
  (%php-lower-scanf-like-out-param 'cl-cc/php::%php-sscanf-values
                                   (list (first args) (second args))
                                   (cddr args)
                                   "PHP-SSCANF-VALUES-"))

(defun %php-lower-scanf-out-param (args)
  "Lower scanf($fmt,$a,...) into caller out-param assignments."
  (%php-lower-scanf-like-out-param 'cl-cc/php::%php-scanf-values
                                   (list (first args))
                                   (cdr args)
                                   "PHP-SCANF-VALUES-"))

(defun %php-by-ref-indices-for-name (name)
  "Return by-reference parameter indices registered for NAME."
  (when name
    (let ((key (princ-to-string name)))
      (or (gethash key *php-by-ref-param-registry*)
          (gethash (string-upcase key) *php-by-ref-param-registry*)
          (gethash (string-downcase key) *php-by-ref-param-registry*)))))

(defun %php-by-ref-indices-for-call (fn-sym)
  "Return by-reference parameter indices registered for the function FN-SYM."
  (%php-by-ref-indices-for-name (symbol-name fn-sym)))

(defun %php-by-ref-indices-for-function (qualified-name fn-sym fallback-name)
  "Return by-reference indices for a function before or after builtin lowering."
  (or (%php-by-ref-indices-for-call fn-sym)
      (%php-by-ref-indices-for-call fallback-name)
      (%php-by-ref-indices-for-name
       (%php-simple-function-spelling qualified-name))))

(defun %php-lower-by-ref-call (func-node args by-ref-indices)
  "Lower a call with by-reference parameters.

Variable actuals are boxed before the call and written back after the callee
returns. Non-variable actuals still receive a temporary ref box so the callee
can run, matching PHP's local mutation behavior for the callee body."
  (let ((box-bindings nil)
        (call-args nil)
        (writebacks nil))
    (loop for arg in args
          for idx from 0
          do (if (member idx by-ref-indices :test #'=)
                 (let ((box-sym (gensym "PHP-BY-REF-ARG-")))
                   (push (cons box-sym
                               (%php-call 'cl-cc/php::%php-make-ref arg))
                         box-bindings)
                   (push (make-ast-var :name box-sym) call-args)
                   (when (ast-var-p arg)
                     (push (make-ast-setq
                            :var (ast-var-name arg)
                            :value (%php-call 'cl-cc/php::%php-deref
                                              (make-ast-var :name box-sym)))
                           writebacks)))
                 (push arg call-args)))
    (let ((call-node (make-ast-call :func func-node
                                    :args (nreverse call-args))))
      (if box-bindings
          (let ((result-sym (gensym "PHP-BY-REF-RESULT-")))
            (make-ast-let
             :bindings (nreverse box-bindings)
             :body (list
                    (make-ast-let
                     :bindings (list (cons result-sym call-node))
                     :body (append (nreverse writebacks)
                                   (list (make-ast-var :name result-sym)))))))
          call-node))))

(defun %php-emit-lowered-function-call (fn-sym args by-ref-indices)
  "Emit a lowered PHP function call from ARGS."
  (let ((func (make-ast-var :name fn-sym)))
    (%php-emit-lowered-call func
                            (if (%php-args-have-named-p args)
                                (%php-reorder-named-args-for-call fn-sym args)
                                (%php-static-call-lowering-result args))
                            :by-ref-indices by-ref-indices)))

(defun %php-compact-static-names-from-node (node)
  "Return static compact() variable names described by NODE.
The second value is true when NODE was fully understood."
  (cond
    ((and (ast-quote-p node)
          (stringp (ast-quote-value node)))
     (values (list (ast-quote-value node)) t))
    ((%php-array-constructor-call-p node)
     (let ((names nil)
           (supported-p t))
       (dolist (entry (ast-call-args node))
         (let* ((elements (and (ast-list-p entry) (ast-list-elements entry)))
                (value (and (= (length elements) 3) (third elements))))
           (if value
               (multiple-value-bind (entry-names entry-supported-p)
                   (%php-compact-static-names-from-node value)
                 (if entry-supported-p
                     (setf names (append names entry-names))
                     (setf supported-p nil)))
               (setf supported-p nil))))
       (values names supported-p)))
    (t
     (values nil nil))))

(defun %php-compact-static-names (args)
  "Return all static variable names requested by a compact() call."
  (let ((names nil)
        (supported-p t))
    (dolist (arg args)
      (multiple-value-bind (arg-names arg-supported-p)
          (%php-compact-static-names-from-node arg)
        (if arg-supported-p
            (setf names (append names arg-names))
            (setf supported-p nil))))
    (values names supported-p)))

(defun %php-lower-static-compact-call (args known-vars)
  "Lower compact('x', ['y']) into an array literal capturing visible variables."
  (multiple-value-bind (names supported-p) (%php-compact-static-names args)
    (when supported-p
      (%php-array-call
       (loop for name in names
             for var-sym = (php-var-sym name)
             when (member var-sym known-vars :test #'eq)
               collect (%php-array-entry t
                                         (make-ast-quote :value name)
                                         (make-ast-var :name var-sym)))))))

(defun %php-lower-isset-call (args fn-sym)
  "Lower isset($x) so the variable operand is not evaluated before boundp."
  (when (= (length args) 1)
    (let ((arg (first args)))
      (when (ast-var-p arg)
        (make-ast-call :func (make-ast-var :name fn-sym)
                       :args (list (make-ast-quote :value (ast-var-name arg))))))))

(defun %php-lower-empty-call (args empty-fn-sym known-vars)
  "Lower empty($x) without evaluating an obviously unbound variable operand."
  (when (= (length args) 1)
    (let ((arg (first args)))
      (when (ast-var-p arg)
        (if (member (ast-var-name arg) known-vars :test #'eq)
            (make-ast-call :func (make-ast-var :name empty-fn-sym)
                           :args (list arg))
            (make-ast-quote :value t))))))

(defun %php-valid-extract-name-p (name)
  "Return true when NAME can become a PHP variable name."
  (and (stringp name)
       (plusp (length name))
       (let ((first (char name 0)))
         (or (alpha-char-p first) (char= first #\_)))
       (loop for ch across name
             always (or (alphanumericp ch) (char= ch #\_)))))

(defun %php-static-extract-bindings-from-node (node)
  "Return variable bindings for extract() when NODE is a static array literal."
  (when (%php-array-constructor-call-p node)
    (let ((bindings nil)
          (supported-p t))
      (dolist (entry (ast-call-args node))
        (let* ((elements (and (ast-list-p entry) (ast-list-elements entry)))
               (key-present-p (and (= (length elements) 3)
                                   (ast-quote-p (first elements))
                                   (ast-quote-value (first elements))))
               (key-node (and key-present-p (second elements)))
               (value-node (and (= (length elements) 3) (third elements))))
          (cond
            ((not (= (length elements) 3))
             (setf supported-p nil))
            ((not key-present-p)
             nil)
            ((and (ast-quote-p key-node)
                  (%php-valid-extract-name-p (ast-quote-value key-node)))
             (let* ((var-sym (php-var-sym (ast-quote-value key-node)))
                    (cell (assoc var-sym bindings :test #'eq)))
               (if cell
                   (setf (cdr cell) value-node)
                   (setf bindings (append bindings (list (cons var-sym value-node)))))))
            (t
             nil))))
      (when supported-p bindings))))

(defun %php-lower-static-extract-call (args)
  "Lower extract(['x' => 1]) into first-class PHP variable bindings."
  (when (= (length args) 1)
    (let ((bindings (%php-static-extract-bindings-from-node (first args))))
      (when bindings
        (make-ast-let :bindings bindings :body nil)))))

(defun %php-static-extract-bound-vars (args)
  "Return the variables introduced by a statically lowered extract() call."
  (let ((bindings (and (= (length args) 1)
                       (%php-static-extract-bindings-from-node (first args)))))
    (mapcar #'car bindings)))

(defun %php-global-builtin-function-name (qualified-name fallback-name)
  "Return the PHP builtin spelling for QUALIFIED-NAME when it is global."
  (when (%php-global-builtin-function-p qualified-name fallback-name)
    (%php-simple-function-spelling qualified-name)))

(defun %php-syntax-like-builtin-kind (builtin-name)
  "Classify syntax-like builtin names that need special lowering."
  (cond
    ((null builtin-name) nil)
    ((string= builtin-name "isset") :isset)
    ((string= builtin-name "empty") :empty)
    ((string= builtin-name "compact") :compact)
    ((string= builtin-name "extract") :extract)
    (t nil)))

(defun %php-parse-function-call (qualified-name fallback-name stream known-vars)
  "Parse a PHP function call and lower known builtins to runtime helpers."
  (multiple-value-bind (args rest kv) (php-parse-arglist stream known-vars)
    (let* ((builtin-name (%php-global-builtin-function-name qualified-name
                                                            fallback-name))
           (builtin-kind (%php-syntax-like-builtin-kind builtin-name))
           (fn-sym (or (and builtin-name
                            (%php-builtin-helper-symbol builtin-name))
                       fallback-name))
           (by-ref-indices
             (%php-by-ref-indices-for-function qualified-name fn-sym fallback-name)))
      (values (cond
                ;; preg_match($p,$s,$m): $matches is a by-reference out-param.
                ((%php-preg-match-out-param-call-p qualified-name args)
                 (%php-lower-preg-match-out-param qualified-name args))
                ;; sscanf($s,$fmt,$a,...): parse once, assign out-params, return count.
                ((%php-sscanf-out-param-call-p qualified-name args)
                 (%php-lower-sscanf-out-param args))
                ;; scanf($fmt,$a,...): read stdin once, assign out-params, return count.
                ((%php-scanf-out-param-call-p qualified-name args)
                 (%php-lower-scanf-out-param args))
                ;; isset($x) is PHP syntax: the variable must not be evaluated
                ;; before the runtime helper checks whether it is bound.
                ((eq builtin-kind :isset)
                 (or (%php-lower-isset-call args fn-sym)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; empty($x) is also syntax-like in PHP: an undefined variable
                ;; is empty and must not be evaluated before that decision.
                ((eq builtin-kind :empty)
                 (or (%php-lower-empty-call args fn-sym kv)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; compact('x', ['y']): requires caller variables, so static
                ;; name lists are lowered before the ordinary builtin path.
                ((eq builtin-kind :compact)
                 (or (%php-lower-static-compact-call args kv)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; extract(['x' => 1]) mutates the caller's variable table. For
                ;; static array literals, lower it into ordinary PHP bindings.
                ((eq builtin-kind :extract)
                 (or (%php-lower-static-extract-call args)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; f(...): first-class callable — a forwarding closure, not a call.
                ((and (= (length args) 1) (%php-fcc-marker-p (first args)))
                 (%php-build-fcc-closure fn-sym))
                (t (%php-emit-lowered-function-call fn-sym
                                                   args
                                                   by-ref-indices)))
              rest
              (if (eq builtin-kind :extract)
                  (append (%php-static-extract-bound-vars args) kv)
                  kv)))))
