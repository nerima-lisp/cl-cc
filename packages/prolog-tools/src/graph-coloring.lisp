;;;; packages/prolog-tools/src/graph-coloring.lisp - FD-constraint graph coloring
;;;;
;;;; Treats direct call edges between two functions as an "interference":
;;;; COLOR-CALL-GRAPH assigns each defined function a color in 1..N such
;;;; that no two functions connected by a direct call share a color, using
;;;; cl-prolog's finite-domain constraint solver (CL-PROLOG:INS domain
;;;; restriction, pairwise CL-PROLOG:|#\=| disequality, CL-PROLOG:LABELING
;;;; search) rather than a hand-rolled greedy-coloring algorithm.

(in-package :cl-cc/prolog-tools)

(defun %call-graph-edges (call-graph)
  "Return the undirected edge set of CALL-GRAPH as (A . B) pairs with A < B
by symbol-name, deduplicated, excluding self-calls."
  (let ((edges '()))
    (dolist (caller (call-graph-defined call-graph))
      (dolist (callee (direct-callees call-graph caller))
        (when (and (member callee (call-graph-defined call-graph)) (not (eq caller callee)))
          (let ((edge (if (string< (symbol-name caller) (symbol-name callee))
                          (cons caller callee)
                          (cons callee caller))))
            (pushnew edge edges :test #'equal)))))
    edges))

(defun color-call-graph (call-graph num-colors)
  "Return an alist of (FUNCTION . COLOR), COLOR in 1..NUM-COLORS, such that
no two functions joined by a direct call share a color; or NIL if no such
coloring exists."
  (let* ((names (call-graph-defined call-graph))
         (var-alist (mapcar (lambda (name)
                               (cons name (intern (format nil "?COLOR-~A" (symbol-name name))
                                                   :cl-cc/prolog-tools)))
                             names))
         (vars (mapcar #'cdr var-alist))
         (edges (%call-graph-edges call-graph))
         (domain-goal `(cl-prolog:ins ,vars (1 cl-prolog:|..| ,num-colors)))
         (disequality-goals
           (mapcar (lambda (edge)
                     `(cl-prolog:|#\\=| ,(cdr (assoc (car edge) var-alist))
                                          ,(cdr (assoc (cdr edge) var-alist))))
                   edges))
         (labeling-goal `(cl-prolog:labeling nil ,vars))
         (goal (reduce (lambda (a b) `(and ,a ,b))
                        (append (list domain-goal) disequality-goals (list labeling-goal)))))
    (when vars
      (let ((solution (cl-prolog:query-prolog-first (call-graph-rulebase call-graph) goal)))
        (when solution
          (mapcar (lambda (pair) (cons (car pair) (cl-prolog:solution-binding (cdr pair) solution)))
                  var-alist))))))

(defun valid-coloring-p (call-graph coloring)
  "True when COLORING assigns distinct colors to every pair of functions
joined by a direct call edge in CALL-GRAPH."
  (every (lambda (edge)
           (not (eql (cdr (assoc (car edge) coloring)) (cdr (assoc (cdr edge) coloring)))))
         (%call-graph-edges call-graph)))
