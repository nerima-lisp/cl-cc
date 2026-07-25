;;;; tests/pbt/generators-typed-ast.lisp — Typed AST structures and generators
;;;
;;; Built on cl-weave 1.0.0's combinators. Three things had to change beyond
;;; renaming GEN-FMAP to GEN-MAP and GEN-LIST-OF to GEN-LIST:
;;;
;;;   - cl-weave has no GEN-BIND. Most of the originals' uses were not actually
;;;     dependent — "pick a tag, then generate from the matching branch" is
;;;     exactly GEN-ONE-OF over the branch generators, and the ones that fed a
;;;     generated value into a closure only ever used it to *compute* a field
;;;     (a node type), which GEN-MAP over a GEN-TUPLE does directly.
;;;
;;;   - GEN-TYPED-CALL and GEN-TYPED-LET were genuinely dependent: the number of
;;;     arguments/initforms had to equal the arity of the lambda or binding list
;;;     just generated. Both are reformulated by over-generating a fixed-width
;;;     list and truncating it in the mapping function, which needs no dependent
;;;     combinator and keeps the arity invariant exactly.
;;;
;;;   - GEN-TYPED-AST-NODE is now GEN-RECURSIVE. The original chose
;;;     terminal-vs-recursive with a (random 100) call inside the constructor,
;;;     which varied per case only because the home-grown DEFPROPERTY
;;;     re-evaluated the generator expression each iteration; IT-PROPERTY builds
;;;     its generator list once and the shape would have frozen.
;;;
;;; The node-building generators take the sub-expression generator as an
;;; optional argument, defaulting to a terminal. That keeps GEN-TYPED-BINOP and
;;; GEN-TYPED-LAMBDA usable standalone (two properties call them directly) while
;;; letting GEN-TYPED-AST-NODE thread GEN-RECURSIVE's self-reference through
;;; them.

(in-package :cl-cc/pbt)

;;; Typed AST node structures

(defstruct (typed-ast (:constructor make-typed-ast-raw))
  "Base structure for typed AST nodes."
  node-type
  source-node)

(defstruct (typed-ast-int (:include typed-ast)
                          (:constructor make-typed-ast-int-raw))
  "Typed integer literal."
  value)

(defstruct (typed-ast-float (:include typed-ast)
                            (:constructor make-typed-ast-float-raw))
  "Typed float literal."
  value)

(defstruct (typed-ast-string (:include typed-ast)
                             (:constructor make-typed-ast-string-raw))
  "Typed string literal."
  value)

(defstruct (typed-ast-boolean (:include typed-ast)
                              (:constructor make-typed-ast-boolean-raw))
  "Typed boolean literal."
  value)

(defstruct (typed-ast-var (:include typed-ast)
                          (:constructor make-typed-ast-var-raw))
  "Typed variable reference."
  name)

(defstruct (typed-ast-binop (:include typed-ast)
                            (:constructor make-typed-ast-binop-raw))
  "Typed binary operation."
  op
  lhs
  rhs)

(defstruct (typed-ast-if (:include typed-ast)
                         (:constructor make-typed-ast-if-raw))
  "Typed conditional expression."
  cond
  then
  else)

(defstruct (typed-ast-lambda (:include typed-ast)
                             (:constructor make-typed-ast-lambda-raw))
  "Typed lambda expression with typed parameters."
  params      ; List of (name . type) pairs
  body)

(defstruct (typed-ast-call (:include typed-ast)
                           (:constructor make-typed-ast-call-raw))
  "Typed function call."
  func
  func-type   ; Function type
  args)

(defstruct (typed-ast-let (:include typed-ast)
                          (:constructor make-typed-ast-let-raw))
  "Typed let binding."
  bindings    ; List of (name . (type . expr))
  body)

;;; Terminal generators

(defun gen-typed-primitive-value ()
  "Generate a typed primitive literal node paired with its declared type."
  (cl-weave:gen-one-of
   (cl-weave:gen-map (lambda (v) (make-typed-ast-int-raw :node-type 'fixnum :value v))
                     (cl-weave:gen-integer :min -1000 :max 1000))
   (cl-weave:gen-map (lambda (v) (make-typed-ast-float-raw :node-type 'single-float :value v))
                     (gen-pbt-single-float :min -1000.0 :max 1000.0))
   (cl-weave:gen-map (lambda (v) (make-typed-ast-string-raw :node-type 'string :value v))
                     (cl-weave:gen-string :min-length 0 :max-length 20))
   (cl-weave:gen-map (lambda (v) (make-typed-ast-boolean-raw :node-type 'boolean :value v))
                     (cl-weave:gen-boolean))))

(defun gen-typed-var ()
  "Generate a typed variable reference with a primitive declared type."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (name type) parts
       (make-typed-ast-var-raw :node-type type :name name)))
   (cl-weave:gen-tuple (gen-pbt-symbol "VAR") (gen-primitive-type))))

(defun gen-typed-terminal ()
  "Generate typed terminal AST nodes (literals and variables).
Literals are drawn twice as often as variables, as in the original, which
selected among (0 1 2) with both 0 and 2 producing a primitive value."
  (cl-weave:gen-one-of (gen-typed-primitive-value)
                       (gen-typed-var)
                       (gen-typed-primitive-value)))

;;; Compound generators

(defun gen-typed-binop (&optional (subexpr (gen-typed-terminal)))
  "Generate typed binary operation AST nodes."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (op lhs rhs) parts
       (make-typed-ast-binop-raw :node-type 'fixnum :op op :lhs lhs :rhs rhs)))
   (cl-weave:gen-tuple (cl-weave:gen-member '(+ - * /)) subexpr subexpr)))

(defun gen-typed-if (&optional (subexpr (gen-typed-terminal)))
  "Generate typed if expression AST nodes; the node type follows the THEN branch."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (cond-val then else) parts
       (make-typed-ast-if-raw
        :node-type (typed-ast-node-type then)
        :cond (make-typed-ast-boolean-raw :node-type 'boolean :value cond-val)
        :then then
        :else else)))
   (cl-weave:gen-tuple (cl-weave:gen-boolean) subexpr subexpr)))

(defun gen-typed-param ()
  "Generate a typed function parameter (name . type)."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (name type) parts
       (cons name type)))
   (cl-weave:gen-tuple (gen-pbt-symbol "ARG") (gen-terminal-type))))

(defun gen-typed-lambda (&optional (subexpr (gen-typed-terminal)))
  "Generate typed lambda expression AST nodes.
The node type is (function ARG-TYPES RETURN-TYPE), derived from the generated
parameters and body."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (params body) parts
       (make-typed-ast-lambda-raw
        :node-type (list 'function (mapcar #'cdr params) (typed-ast-node-type body))
        :params params
        :body (list body))))
   (cl-weave:gen-tuple (cl-weave:gen-list (gen-typed-param)
                                          :min-length 0 :max-length 3)
                       subexpr)))

(defun gen-typed-call (&optional (subexpr (gen-typed-terminal)))
  "Generate typed function call AST nodes whose arity matches the callee.
The original used a dependent GEN-BIND to size the argument list from the
generated lambda's arity. Here a full-width candidate list is generated
independently and truncated to that arity, which needs no dependent combinator
and preserves the invariant that (length args) equals the callee's arity."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (func candidate-args) parts
       (let* ((fn-type (typed-ast-node-type func))
              (arity (length (second fn-type))))
         (make-typed-ast-call-raw
          :node-type (third fn-type)
          :func func
          :func-type fn-type
          :args (subseq candidate-args 0 arity)))))
   (cl-weave:gen-tuple (gen-typed-lambda subexpr)
                       ;; GEN-TYPED-LAMBDA generates at most 3 parameters, so a
                       ;; fixed-length 3 list always covers the required arity.
                       (cl-weave:gen-list (gen-typed-terminal)
                                          :min-length 3 :max-length 3))))

(defun gen-typed-let (&optional (subexpr (gen-typed-terminal)))
  "Generate typed let binding AST nodes.
Sized the same way as GEN-TYPED-CALL: a fixed-width initform list is generated
independently and truncated to the number of generated bindings."
  (cl-weave:gen-map
   (lambda (parts)
     (destructuring-bind (bindings candidate-values body) parts
       (make-typed-ast-let-raw
        :node-type (typed-ast-node-type body)
        :bindings (loop for (name . type) in bindings
                        for val in candidate-values
                        collect (cons name (cons type val)))
        :body (list body))))
   (cl-weave:gen-tuple (cl-weave:gen-list (gen-typed-param)
                                          :min-length 1 :max-length 3)
                       (cl-weave:gen-list (gen-typed-terminal)
                                          :min-length 3 :max-length 3)
                       subexpr)))

;;; Recursive entry point

(defun gen-typed-ast-node (&key (max-depth *max-type-depth*))
  "Generate AST nodes with type annotations.

   Supports typed literals and variables at the leaves, and typed binops,
   conditionals, lambdas and calls above them. Every node carries a NODE-TYPE."
  (cl-weave:gen-recursive
   (gen-typed-terminal)
   (lambda (self)
     (cl-weave:gen-one-of (gen-typed-binop self)
                          (gen-typed-if self)
                          (gen-typed-lambda self)
                          (gen-typed-call self)
                          (gen-typed-terminal)))
   :max-depth max-depth))
