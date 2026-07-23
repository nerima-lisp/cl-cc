;;;; packages/prolog-tools/tests/graph-coloring-tests.lisp

(in-package :cl-cc/prolog-tools-tests)

(describe "color-call-graph"
  (it "colors a triangle of mutual calls with 3 colors"
    (let* ((defuns (list (mk-defun 'a 'b 'c) (mk-defun 'b 'c) (mk-defun 'c)))
           (cg (build-call-graph defuns :entry-points '(a)))
           (coloring (color-call-graph cg 3)))
      (expect coloring :to-be-truthy)
      (expect (valid-coloring-p cg coloring) :to-be-truthy)))

  (it "cannot color a triangle with only 2 colors"
    (let* ((defuns (list (mk-defun 'a 'b 'c) (mk-defun 'b 'c) (mk-defun 'c)))
           (cg (build-call-graph defuns :entry-points '(a))))
      (expect (color-call-graph cg 2) :to-be-null)))

  (it "colors a bipartite call graph with 2 colors"
    (let* ((defuns (list (mk-defun 'a 'c 'd) (mk-defun 'b 'c 'd) (mk-defun 'c) (mk-defun 'd)))
           (cg (build-call-graph defuns :entry-points '(a b)))
           (coloring (color-call-graph cg 2)))
      (expect coloring :to-be-truthy)
      (expect (valid-coloring-p cg coloring) :to-be-truthy)))

  (it "kills mutants of the pairwise-disequality invariant"
    (let ((results (run-mutations
                    '(if (eql color-a color-b) nil t)
                    (lambda (form mutation)
                      (declare (ignore mutation))
                      (and (equal (eval `(let ((color-a 1) (color-b 2)) ,form)) t)
                           (equal (eval `(let ((color-a 1) (color-b 1)) ,form)) nil))))))
      (assert-mutation-score results 1.0))))
