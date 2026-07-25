;;;; tests/pbt/generator-pbt-tests.lisp — Properties over the type/typed-AST/Mach-O generators
;;;
;;; These nine properties are contracts on the generators themselves — that
;;; GEN-TYPE-EXPR only ever yields well-formed type specifiers, that every
;;; typed-AST node carries a type, that a generated Mach-O header has a real
;;; magic number, and so on. They are what the rest of the type and typed-AST
;;; property tests rest on.
;;;
;;; Formerly generators-typed-ast-utils.lisp, and formerly the last consumer of
;;; the home-grown cl-cc/pbt DEFPROPERTY. Now on cl-weave's native IT-PROPERTY,
;;; which let framework.lisp, framework-dsl.lisp and generators.lisp be deleted.
;;;
;;; Bodies assert through CL-WEAVE:EXPECT: IT-PROPERTY decides pass/fail from a
;;; *signaled* condition and discards the body's return value, so a bare boolean
;;; would report PASS even when false.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;;; Type Expression Properties

(cl-weave:describe "type expression generator properties"

  (cl-weave:it-property "type-expr-is-sexp"
      ((type-expr (gen-type-expr)))
    (cl-weave:expect (or (symbolp type-expr)
                         (and (consp type-expr) (symbolp (car type-expr))))
                     :to-be-truthy))

  (cl-weave:it-property "fn-type-has-function-symbol"
      ((fn-type (gen-fn-type)))
    (cl-weave:expect (car fn-type) :to-be 'function)
    (cl-weave:expect fn-type :to-have-length 3)
    (cl-weave:expect (second fn-type) :to-satisfy #'listp))

  (cl-weave:it-property "type-variables-are-keywords"
      ((type-var (gen-type-variable)))
    (cl-weave:expect type-var :to-satisfy #'keywordp)
    (cl-weave:expect (char (symbol-name type-var) 0) :to-be #\?)))

;;; Mach-O Structure Properties

(cl-weave:describe "Mach-O structure generator properties"

  (cl-weave:it-property "mach-header-has-valid-magic"
      ((header (gen-mach-header)))
    (cl-weave:expect (mach-header-magic header)
                     :to-be-one-of (list +mh-magic+ +mh-magic-64+
                                         +mh-cigam+ +mh-cigam-64+)))

  (cl-weave:it-property "mach-segment-has-valid-permissions"
      ((segment (gen-mach-segment-command)))
    ;; rwx is a three-bit mask, so both protections must land in [0,7].
    (cl-weave:expect (mach-segment-command-maxprot segment)
                     :to-satisfy (lambda (p) (<= 0 p 7)))
    (cl-weave:expect (mach-segment-command-initprot segment)
                     :to-satisfy (lambda (p) (<= 0 p 7))))

  (cl-weave:it-property "mach-section-count-matches"
      ((segment (gen-mach-segment-command)))
    (cl-weave:expect (mach-segment-command-nsects segment)
                     :to-be (length (mach-segment-command-sections segment)))))

;;; Typed AST Properties

(cl-weave:describe "typed AST generator properties"

  (cl-weave:it-property "typed-ast-has-type"
      ((node (gen-typed-ast-node :max-depth 2)))
    (cl-weave:expect (typed-ast-node-type node) :to-be-truthy))

  (cl-weave:it-property "typed-lambda-has-function-type"
      ((node (gen-typed-lambda)))
    (let ((type (typed-ast-node-type node)))
      (cl-weave:expect (car type) :to-be 'function)
      (cl-weave:expect type :to-have-length 3)))

  (cl-weave:it-property "typed-binop-returns-numeric"
      ((node (gen-typed-binop)))
    (cl-weave:expect (typed-ast-node-type node)
                     :to-be-one-of '(fixnum integer single-float double-float number))))
