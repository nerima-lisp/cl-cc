;;;; tests/unit/cli/main-utils-tests.lisp — utility helper coverage

(in-package :cl-cc/test)


(it-sequential "cli-dump-phase-label-lowercases-keywords"
  (expect (cl-cc/cli::%dump-phase-label :ast) :to-equal "ast")
  (expect (cl-cc/cli::%dump-phase-label :SSA) :to-equal "ssa"))

(it-sequential "cli-parse-ir-phase-supported-values ast"
  (destructuring-bind (expected input) (list :ast "ast")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values CPS"
  (destructuring-bind (expected input) (list :cps "CPS")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values ssa"
  (destructuring-bind (expected input) (list :ssa "ssa")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values vm"
  (destructuring-bind (expected input) (list :vm "vm")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values opt"
  (destructuring-bind (expected input) (list :opt "opt")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values asm"
  (destructuring-bind (expected input) (list :asm "asm")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-parse-ir-phase-supported-values bogus"
  (destructuring-bind (expected input) (list nil "bogus")
    (expect (cl-cc/cli::%parse-ir-phase input) :to-be expected)))

(it-sequential "cli-ensure-list-normalizes-inputs"
  (expect (cl-cc/cli::%ensure-list nil) :to-be-null)
  (expect (cl-cc/cli::%ensure-list '(1 2)) :to-equal '(1 2))
  (expect (cl-cc/cli::%ensure-list 'x) :to-equal '(x)))

(it-sequential "cli-call-with-optional-output-file-passes-nil-when-missing"
  (let ((seen :unset))
    (cl-cc/cli::%call-with-optional-output-file nil
                                                (lambda (stream)
                                                  (setf seen stream)
                                                  :ok))
    (expect seen :to-be-null)))

(it-sequential "cli-call-with-optional-output-file-writes-file-when-path-present"
  (uiop:with-temporary-file (:pathname path :type "txt" :keep t)
    (let ((result (cl-cc/cli::%call-with-optional-output-file
                   path
                   (lambda (stream)
                     (write-string "hello" stream)
                     :written))))
      (expect result :to-be :written)
      (expect (cl-cc/cli::%read-file path) :to-equal "hello")
      (ignore-errors (delete-file path)))))

(it-sequential "cli-get-timeout-defaults-to-30-seconds"
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)"))))
    (expect (= 30 (cl-cc/cli::%get-timeout parsed)) :to-be-truthy)))

(it-sequential "cli-get-timeout-uses-explicit-positive-value"
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--timeout" "9"))))
    (expect (= 9 (cl-cc/cli::%get-timeout parsed)) :to-be-truthy)))

(it-sequential "cli-get-timeout-no-timeout-disables-timeout"
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--no-timeout"))))
    (expect (cl-cc/cli::%get-timeout parsed) :to-be-null)))

(it-sequential "cli-get-timeout-keeps-positive-integer-validation"
  (let ((parsed (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "--timeout" "0"))))
    (signals cl-cc/cli:arg-parse-error (cl-cc/cli::%get-timeout parsed))))

(it-sequential "cli-svg-escape-escapes-special-characters"
  (expect (cl-cc/cli::%svg-escape "<tag attr=\"a&b\">") :to-equal "&lt;tag attr=&quot;a&amp;b&quot;&gt;"))

(it-sequential "cli-flamegraph-color-minor-gc-is-blue"
  (expect (cl-cc/cli::%flamegraph-color "minor-gc") :to-equal "rgb(90,140,255)"))

(it-sequential "cli-flamegraph-color-jit-compile-is-orange"
  (expect (cl-cc/cli::%flamegraph-color "jit-compile") :to-equal "rgb(255,165,0)"))

(it-sequential "cli-flamegraph-color-ordinary-frame-uses-hsl"
  (expect (search "hsl(" (cl-cc/cli::%flamegraph-color "ordinary-frame")) :to-be-truthy))

(it-sequential "cli-flamegraph-build-tree-aggregates-counts"
  (let ((samples (make-hash-table :test #'equal)))
    (setf (gethash "root;alpha" samples) 2)
    (setf (gethash "root;beta" samples) 3)
    (let ((tree (cl-cc/cli::%flamegraph-build-tree samples)))
      (expect (getf tree :name) :to-equal "root")
      (expect (= 5 (getf tree :count)) :to-be-truthy)
      (expect (gethash "root" (getf tree :children)) :to-be-truthy))))

(it-sequential "cli-flamegraph-children-list-sorts-by-name"
  (let* ((a (list :name "zeta" :count 1 :children (make-hash-table :test #'equal)))
         (b (list :name "alpha" :count 1 :children (make-hash-table :test #'equal)))
         (node (list :name "root" :count 2 :children (make-hash-table :test #'equal))))
    (setf (gethash "zeta" (getf node :children)) a)
    (setf (gethash "alpha" (getf node :children)) b)
    (expect (mapcar (lambda (child) (getf child :name))
                          (cl-cc/cli::%flamegraph-children-list node)) :to-equal '("alpha" "zeta"))))

(it-sequential "cli-write-flamegraph-svg-emits-svg-document"
  (uiop:with-temporary-file (:pathname path :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "top;child" samples) 4)
      (expect (cl-cc/cli::%write-flamegraph-svg path samples) :to-equal path)
      (let ((svg (cl-cc/cli::%read-file path)))
        (expect (search "<svg" svg) :to-be-truthy)
        (expect (search "top" svg) :to-be-truthy)
        (expect (search "child" svg) :to-be-truthy))
      (ignore-errors (delete-file path)))))

(it-sequential "fr-702-flamegraph-svg-generation-writes-rectangles-and-sample-titles"
  (uiop:with-temporary-file (:pathname path :type "svg" :keep t)
    (let ((samples (make-hash-table :test #'equal)))
      (setf (gethash "compiler;optimizer;bolt" samples) 5)
      (setf (gethash "compiler;gc" samples) 2)
      (cl-cc/cli::%write-flamegraph-svg path samples)
      (let ((svg (cl-cc/cli::%read-file path)))
        (expect (search "<svg" svg) :to-be-truthy)
        (expect (search "<rect" svg) :to-be-truthy)
        (expect (search "optimizer" svg) :to-be-truthy)
        (expect (search "bolt (5 samples)" svg) :to-be-truthy)
        (expect (search "rgb(90,140,255)" svg) :to-be-truthy))
      (ignore-errors (delete-file path)))))

;;; ─── %flamegraph-depth-of (extracted helper) ────────────────────────────────

(it-sequential "flamegraph-depth-of-single-node"
  (let ((node '(:name "root" :count 1 :children nil))
        (cell (list 0)))
    (cl-cc/cli::%flamegraph-depth-of node 3 cell)
    (expect (= 3 (car cell)) :to-be-truthy)))

(it-sequential "flamegraph-depth-of-finds-max"
  (let* ((child1 '(:name "c1" :count 1 :children nil))
         (child2 '(:name "c2" :count 1 :children nil))
         (children (let ((ht (make-hash-table :test #'equal)))
                     (setf (gethash "c1" ht) child1
                           (gethash "c2" ht) child2)
                     ht))
         (root (list :name "root" :count 2 :children children))
         (cell (list 0)))
    (cl-cc/cli::%flamegraph-depth-of root 0 cell)
    (expect (= 1 (car cell)) :to-be-truthy)))
