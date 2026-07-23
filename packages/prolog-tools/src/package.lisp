;;;; packages/prolog-tools/src/package.lisp - CL-CC Prolog Tools Package
;;;;
;;;; Static call-graph analysis over :cl-cc/ast programs, implemented on top
;;;; of the external cl-prolog engine (dynamic assert/retract, recursive
;;;; rules, findall aggregation, FD constraints, DCG grammars).

(defpackage :cl-cc/prolog-tools
  (:use :cl)
  (:export
    ;; call-graph.lisp
    #:call-graph
    #:call-graph-p
    #:call-graph-rulebase
    #:call-graph-defined
    #:call-graph-entry-points
    #:build-call-graph
    #:reachable-p
    #:reachable-from
    #:direct-callees
    #:find-dead-code
    #:find-mutually-recursive-pairs
    ;; edge-dcg.lisp
    #:tokenize-edge-spec
    #:edge-spec-well-formed-p
    #:parse-edge-spec
    ;; graph-coloring.lisp
    #:color-call-graph
    #:valid-coloring-p))
