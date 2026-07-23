;;;; packages/prolog-tools/tests/call-graph-tests.lisp

(in-package :cl-cc/prolog-tools-tests)

;; ASDF may load this file from a FASL cache mirror (e.g.
;; ~/.cache/common-lisp/...) rather than the source tree, so *LOAD-TRUENAME*
;; is not a reliable base for a snapshot directory that should live in and
;; be committed alongside the actual source. ASDF:SYSTEM-RELATIVE-PATHNAME
;; resolves against the .asd file's own location instead.
(defparameter *snapshot-directory*
  (asdf:system-relative-pathname :cl-cc-prolog-tools "tests/__snapshots__/"))

(defun mk-call (callee-name)
  (cl-cc/ast:make-ast-call :func (cl-cc/ast:make-ast-var :name callee-name) :args nil))

(defun mk-defun (name &rest callees)
  (cl-cc/ast:make-ast-defun :name name :body (mapcar #'mk-call callees)))

(defun defuns-from-edges (names edges)
  "Build one AST-DEFUN per NAME whose body calls every callee paired with
that name in EDGES, a list of (CALLER CALLEE) two-element lists."
  (mapcar (lambda (name)
            (apply #'mk-defun name
                   (loop for edge in edges
                         when (eq (first edge) name)
                           collect (second edge))))
          names))

(describe "build-call-graph"
  (it "treats a direct call as reachable"
    (let* ((main (mk-defun 'main 'helper))
           (helper (mk-defun 'helper))
           (cg (build-call-graph (list main helper) :entry-points '(main))))
      (expect (reachable-p cg 'main 'helper) :to-be-truthy)))

  (it "follows transitive calls"
    (let* ((main (mk-defun 'main 'helper))
           (helper (mk-defun 'helper 'leaf))
           (leaf (mk-defun 'leaf))
           (cg (build-call-graph (list main helper leaf) :entry-points '(main))))
      (expect (reachable-p cg 'main 'leaf) :to-be-truthy)
      (expect (reachable-p cg 'leaf 'main) :to-be-falsy)))

  (it "reports direct callees via a single findall query"
    (let* ((main (mk-defun 'main 'a 'b))
           (cg (build-call-graph (list main (mk-defun 'a) (mk-defun 'b)) :entry-points '(main))))
      (expect (sort (copy-list (direct-callees cg 'main)) #'string< :key #'symbol-name)
              :to-equal '(a b))))

  (it "finds functions unreachable from any entry point"
    (let* ((main (mk-defun 'main 'helper))
           (helper (mk-defun 'helper 'leaf))
           (leaf (mk-defun 'leaf))
           (orphan (mk-defun 'orphan 'leaf))
           (cg (build-call-graph (list main helper leaf orphan) :entry-points '(main))))
      (expect (find-dead-code cg) :to-equal '(orphan))))

  (it "matches the recorded snapshot for a small dead-code sample"
    ;; Snapshot the symbol NAMES, not the symbols themselves: a symbol's
    ;; printed form is package-context-dependent (unqualified vs.
    ;; CL-CC/PROLOG-TOOLS-TESTS::-qualified, depending on *PACKAGE* at
    ;; print time), which differs between recording the snapshot
    ;; interactively and running it through ASDF:TEST-SYSTEM.
    (let* ((defuns (defuns-from-edges '(main helper leaf orphan1 orphan2)
                                       '((main helper) (helper leaf))))
           (cg (build-call-graph defuns :entry-points '(main))))
      (expect (sort (mapcar #'symbol-name (find-dead-code cg)) #'string<)
              :to-match-snapshot "prolog-tools/dead-code-sample")))

  (it "finds mutually recursive function pairs"
    (let* ((a (mk-defun 'a 'b))
           (b (mk-defun 'b 'a))
           (cg (build-call-graph (list a b) :entry-points '(a))))
      (expect (find-mutually-recursive-pairs cg) :to-equal '((a . b)))))

  (it-property "reachable-from is closed under composition on random call graphs"
      ((edges (gen-list (gen-tuple (gen-member '(a b c d e)) (gen-member '(a b c d e)))
                         :min-length 0 :max-length 10)))
    (let* ((names '(a b c d e))
           (cg (build-call-graph (defuns-from-edges names edges) :entry-points '(a))))
      (dolist (edge edges)
        (expect (reachable-p cg (first edge) (second edge)) :to-be-truthy))
      (dolist (x names)
        (let ((reachable-from-x (reachable-from cg x)))
          (dolist (y reachable-from-x)
            (dolist (z (reachable-from cg y))
              (expect (member z reachable-from-x) :to-be-truthy))))))))
