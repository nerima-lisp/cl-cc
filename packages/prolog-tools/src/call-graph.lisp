;;;; packages/prolog-tools/src/call-graph.lisp - Prolog-backed call-graph analysis
;;;;
;;;; Builds a cl-prolog rulebase from :cl-cc/ast defuns: CALLS/2 facts are
;;;; asserted dynamically (CL-PROLOG:ASSERTZ) so a single hop ("who does F
;;;; call directly?") is answered by a FINDALL query. Multi-hop reachability
;;;; is deliberately NOT a naive recursive Prolog rule
;;;; (REACHABLE(X,Y):-CALLS(X,Y). REACHABLE(X,Y):-CALLS(X,Z),REACHABLE(Z,Y).)
;;;; — that formulation searches forever on a cyclic call graph, since
;;;; nothing prevents it from re-deriving the same targets via ever-longer
;;;; trips around a cycle, and mutual/self recursion is exactly the case a
;;;; compiler call-graph tool must handle. REACHABLE-FROM instead does a
;;;; breadth-first search in Lisp over repeated single-hop DIRECT-CALLEES
;;;; queries with an explicit visited-set, which is trivially safe on
;;;; cycles while still routing every edge lookup through the engine.
;;;;
;;;; The set of defined function names and entry points is tracked as plain
;;;; Lisp slots rather than further Prolog facts, since nothing queries
;;;; them through the engine.
;;;;
;;;; Builtin goal symbols (ASSERTZ, RETRACT, FINDALL, ...) are referenced
;;;; via the CL-PROLOG: package prefix so they resolve to the exact symbols
;;;; the engine's builtin dispatch table was registered under; the user
;;;; predicate CALLS is a plain symbol interned in this package and never
;;;; crosses a package boundary, so its identity stays consistent between
;;;; assertion and query time.

(in-package :cl-cc/prolog-tools)

(defstruct call-graph
  "A cl-prolog rulebase together with the set of function names it describes."
  (rulebase nil)
  (defined nil :type list)
  (entry-points nil :type list))

(defun %assert-fact! (rulebase goal)
  (cl-prolog:query-prolog-first rulebase `(cl-prolog:assertz ,goal)))

(defun %retract-fact! (rulebase goal)
  (cl-prolog:query-prolog-first rulebase `(cl-prolog:retract ,goal)))

(defun %fresh-call-graph-rulebase ()
  (let ((rulebase (cl-prolog:make-rulebase)))
    ;; CALLS/2 must be dispatchable even when the program has no calls at
    ;; all: cl-prolog raises an ISO existence-error for a predicate with no
    ;; clause ever asserted, but tolerates queries against a
    ;; retracted-to-empty one, so seed and immediately retract a throwaway
    ;; fact to register the predicate without leaving a live clause behind.
    (%assert-fact! rulebase '(calls |$seed| |$seed|))
    (%retract-fact! rulebase '(calls |$seed| |$seed|))
    rulebase))

(defun %call-target-name (func)
  "Return the callee symbol for an AST-CALL's FUNC slot, or NIL if unresolvable."
  (cond ((symbolp func) func)
        ((cl-cc/ast:ast-var-p func) (cl-cc/ast:ast-var-name func))
        ((cl-cc/ast:ast-function-p func) (cl-cc/ast:ast-function-name func))
        (t nil)))

(defun %collect-call-targets (node)
  "Return the list of callee symbols for every AST-CALL nested under NODE."
  (let ((targets '()))
    (labels ((walk (n)
               (when (cl-cc/ast:ast-call-p n)
                 (let ((name (%call-target-name (cl-cc/ast:ast-call-func n))))
                   (when name (push name targets))))
               (dolist (child (cl-cc/ast:ast-children n))
                 (when (cl-cc/ast:ast-node-p child) (walk child)))))
      (walk node))
    (nreverse targets)))

(defun build-call-graph (defuns &key entry-points)
  "Build a CALL-GRAPH from a list of AST-DEFUN nodes.

ENTRY-POINTS is a list of function-name symbols treated as always-reachable
roots (e.g. a program's toplevel entry function)."
  (let ((rulebase (%fresh-call-graph-rulebase))
        (names (mapcar #'cl-cc/ast:ast-defun-name defuns)))
    (dolist (defun-node defuns)
      (let ((caller (cl-cc/ast:ast-defun-name defun-node)))
        (dolist (callee (%collect-call-targets defun-node))
          (%assert-fact! rulebase `(calls ,caller ,callee)))))
    (make-call-graph :rulebase rulebase :defined names :entry-points entry-points)))

(defun reachable-from (call-graph function-name)
  "Return every function reachable from FUNCTION-NAME via one or more direct
calls (FUNCTION-NAME itself is included only if reachable via a cycle back
to itself), via breadth-first search over DIRECT-CALLEES."
  (let ((visited '())
        (frontier (direct-callees call-graph function-name)))
    (loop while frontier
          do (let ((next '()))
               (dolist (callee frontier)
                 (unless (member callee visited)
                   (push callee visited)
                   (setf next (nconc (direct-callees call-graph callee) next))))
               (setf frontier next)))
    (nreverse visited)))

(defun reachable-p (call-graph from to)
  "True when TO is reachable from FROM via one or more direct calls."
  (not (null (member to (reachable-from call-graph from)))))

(defun direct-callees (call-graph function-name)
  "Return the functions FUNCTION-NAME calls directly, via a single FINDALL query."
  (let ((solution (cl-prolog:query-prolog-first
                    (call-graph-rulebase call-graph)
                    `(cl-prolog:findall ?callee (calls ,function-name ?callee) ?callees))))
    (when solution (cl-prolog:solution-binding '?callees solution))))

(defun find-dead-code (call-graph)
  "Return the functions defined in CALL-GRAPH that are unreachable from any
entry point and are not themselves entry points."
  (let* ((entry-points (call-graph-entry-points call-graph))
         (reachable (remove-duplicates
                     (mapcan (lambda (entry) (reachable-from call-graph entry))
                             entry-points))))
    (set-difference (call-graph-defined call-graph)
                     (union entry-points reachable))))

(defun find-mutually-recursive-pairs (call-graph)
  "Return (A . B) pairs of distinct functions each reachable from the other."
  (let ((names (call-graph-defined call-graph)))
    (loop for (a . rest) on names
          nconc (loop for b in rest
                      when (and (reachable-p call-graph a b)
                                (reachable-p call-graph b a))
                        collect (cons a b)))))
