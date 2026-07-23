;;;; tests/unit/parse/incremental-tests.lisp — Incremental Parser Tests
;;;;
;;;; Tests for tree-sitter style incremental parsing: edit operations,
;;;; geometric predicates, byte shifting, minimal reparse detection,
;;;; tree rebuilding, parse cache, and CST equality.

(in-package :cl-cc/test)



;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-test-cst-token (kind value start end)
  "Create a CST token with given byte range."
  (cl-cc/parse::make-cst-token :kind kind :value value
                          :start-byte start :end-byte end))

(defun make-test-interior (kind start end children)
  "Create a CST interior node with given byte range and children."
  (cl-cc/parse:make-cst-interior :kind kind :start-byte start :end-byte end
                             :children children))

(defun make-test-edit (start old-end new-end)
  "Create an edit operation."
  (cl-cc/parse:make-edit-operation :start-byte start
                               :old-end-byte old-end
                               :new-end-byte new-end))

;;; ─── edit-operation struct ──────────────────────────────────────────────────

(it-sequential "incr-edit-operation-accessors"
  (let ((e (make-test-edit 10 20 25)))
    (expect (cl-cc/parse::edit-operation-start-byte e) :to-equal 10)
    (expect (cl-cc/parse::edit-operation-old-end-byte e) :to-equal 20)
    (expect (cl-cc/parse::edit-operation-new-end-byte e) :to-equal 25)))

;;; ─── Geometric Predicates ──────────────────────────────────────────────────

(it-sequential "incr-overlaps-edit-cases overlapping"
  (destructuring-bind (node-start node-end expected) (list 5 15 t)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (cl-cc/parse::cst-overlaps-edit-p node edit) :to-equal expected))))

(it-sequential "incr-overlaps-edit-cases non-overlap-1"
  (destructuring-bind (node-start node-end expected) (list 0 5 nil)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (cl-cc/parse::cst-overlaps-edit-p node edit) :to-equal expected))))

(it-sequential "incr-overlaps-edit-cases non-overlap-2"
  (destructuring-bind (node-start node-end expected) (list 25 30 nil)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (cl-cc/parse::cst-overlaps-edit-p node edit) :to-equal expected))))

(it-sequential "incr-before-after-edit-cases before-true"
  (destructuring-bind (predicate node-start node-end expected) (list #'cl-cc/parse::cst-before-edit-p 0 10 t)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (funcall predicate node edit) :to-equal expected))))

(it-sequential "incr-before-after-edit-cases before-false"
  (destructuring-bind (predicate node-start node-end expected) (list #'cl-cc/parse::cst-before-edit-p 0 15 nil)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (funcall predicate node edit) :to-equal expected))))

(it-sequential "incr-before-after-edit-cases after-true"
  (destructuring-bind (predicate node-start node-end expected) (list #'cl-cc/parse::cst-after-edit-p 20 30 t)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (funcall predicate node edit) :to-equal expected))))

(it-sequential "incr-before-after-edit-cases after-false"
  (destructuring-bind (predicate node-start node-end expected) (list #'cl-cc/parse::cst-after-edit-p 15 25 nil)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (funcall predicate node edit) :to-equal expected))))

(it-sequential "incr-reuse-p-cases reuse-true"
  (destructuring-bind (node-start node-end expected) (list 0 5 t)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (cl-cc/parse::cst-reuse-p node edit) :to-equal expected))))

(it-sequential "incr-reuse-p-cases reuse-false"
  (destructuring-bind (node-start node-end expected) (list 5 15 nil)
    (let ((node (make-test-cst-token :T-INT 1 node-start node-end))
         (edit (make-test-edit 10 20 25)))
     (expect (cl-cc/parse::cst-reuse-p node edit) :to-equal expected))))

;;; ─── Byte Shifting ──────────────────────────────────────────────────────────

(it-sequential "incr-edit-byte-delta-cases insertion"
  (destructuring-bind (old-end new-end expected) (list 10 15 5)
    (let ((edit (make-test-edit 10 old-end new-end)))
     (expect (cl-cc/parse::edit-byte-delta edit) :to-equal expected))))

(it-sequential "incr-edit-byte-delta-cases deletion"
  (destructuring-bind (old-end new-end expected) (list 20 15 -5)
    (let ((edit (make-test-edit 10 old-end new-end)))
     (expect (cl-cc/parse::edit-byte-delta edit) :to-equal expected))))

(it-sequential "incr-edit-byte-delta-cases replacement"
  (destructuring-bind (old-end new-end expected) (list 20 20 0)
    (let ((edit (make-test-edit 10 old-end new-end)))
     (expect (cl-cc/parse::edit-byte-delta edit) :to-equal expected))))

(it-sequential "incr-shift-bytes-token"
  (let* ((tok (make-test-cst-token :T-INT 42 10 15))
         (shifted (cl-cc/parse::cst-shift-bytes tok 5)))
    (expect (cl-cc/parse:cst-token-p shifted) :to-be-truthy)
    (expect (cl-cc/parse:cst-node-start-byte shifted) :to-equal 15)
    (expect (cl-cc/parse:cst-node-end-byte shifted) :to-equal 20)
    (expect (cl-cc/parse:cst-token-value shifted) :to-equal 42)))

(it-sequential "incr-shift-bytes-interior"
  (let* ((child (make-test-cst-token :T-INT 1 5 10))
         (parent (make-test-interior :list 0 20 (list child)))
         (shifted (cl-cc/parse::cst-shift-bytes parent 10)))
    (expect (cl-cc/parse:cst-interior-p shifted) :to-be-truthy)
    (expect (cl-cc/parse:cst-node-start-byte shifted) :to-equal 10)
    (expect (cl-cc/parse:cst-node-end-byte shifted) :to-equal 30)
    (let ((shifted-child (first (cl-cc/parse:cst-interior-children shifted))))
      (expect (cl-cc/parse:cst-node-start-byte shifted-child) :to-equal 15)
      (expect (cl-cc/parse:cst-node-end-byte shifted-child) :to-equal 20))))

(it-sequential "incr-shift-bytes-error-node"
  (let* ((err (cl-cc/parse:make-cst-error :kind :error :start-byte 10 :end-byte 20
                                           :message "bad token"))
         (shifted (cl-cc/parse::cst-shift-bytes err 5)))
    (expect (cl-cc/parse:cst-error-p shifted) :to-be-truthy)
    (expect (cl-cc/parse:cst-node-start-byte shifted) :to-equal 15)
    (expect (cl-cc/parse:cst-node-end-byte shifted) :to-equal 25)))

(it-sequential "incr-shift-bytes-negative-delta"
  (let* ((tok (make-test-cst-token :T-INT 1 20 30))
         (shifted (cl-cc/parse::cst-shift-bytes tok -5)))
    (expect (cl-cc/parse:cst-node-start-byte shifted) :to-equal 15)
    (expect (cl-cc/parse:cst-node-end-byte shifted) :to-equal 25)))

;;; ─── Minimal Reparse Detection ──────────────────────────────────────────────

(it-sequential "incr-find-minimal-reparse-leaf"
  (let ((leaf (make-test-cst-token :T-INT 1 0 10))
        (edit (make-test-edit 0 5 5)))
    (expect (cl-cc/parse::find-minimal-reparse-node leaf edit) :to-be leaf)))

(it-sequential "incr-find-minimal-reparse-single-child"
  (let* ((child1 (make-test-cst-token :T-INT 1 0 5))
         (child2 (make-test-interior :list 5 15
                   (list (make-test-cst-token :T-INT 2 5 10)
                         (make-test-cst-token :T-INT 3 10 15))))
         (child3 (make-test-cst-token :T-INT 4 15 20))
         (root (make-test-interior :root 0 20 (list child1 child2 child3)))
         (edit (make-test-edit 7 12 12)))
    ;; Edit overlaps only child2 → should recurse into it
    (let ((result (cl-cc/parse::find-minimal-reparse-node root edit)))
      (expect result :to-be child2))))

(it-sequential "incr-find-minimal-reparse-multiple-children"
  (let* ((child1 (make-test-interior :a 0 10
                   (list (make-test-cst-token :T-INT 1 0 10))))
         (child2 (make-test-interior :b 10 20
                   (list (make-test-cst-token :T-INT 2 10 20))))
         (root (make-test-interior :root 0 20 (list child1 child2)))
         (edit (make-test-edit 5 15 15)))
    ;; Edit spans both children → root is the minimal node
    (expect (cl-cc/parse::find-minimal-reparse-node root edit) :to-be root)))

;;; ─── Rebuild Tree ──────────────────────────────────────────────────────────

(it-sequential "incr-rebuild-tree-basic"
  (let* ((before (make-test-cst-token :T-INT 1 0 5))
         (overlap (make-test-cst-token :T-INT 2 5 15))
         (after (make-test-cst-token :T-INT 3 15 20))
         (edit (make-test-edit 5 15 20))  ; insert 5 bytes
         (new-nodes (list (make-test-cst-token :T-INT 99 5 20)))
         (result (cl-cc/parse::rebuild-tree (list before overlap after) edit 5 new-nodes)))
    ;; before + new + shifted-after
    (expect (length result) :to-equal 3)
    ;; First: before node unchanged
    (expect (cl-cc/parse:cst-node-start-byte (first result)) :to-equal 0)
    ;; Second: new node
    (expect (cl-cc/parse:cst-token-value (second result)) :to-equal 99)
    ;; Third: after node shifted by +5
    (expect (cl-cc/parse:cst-node-start-byte (third result)) :to-equal 20)
    (expect (cl-cc/parse:cst-node-end-byte (third result)) :to-equal 25)))

;;; ─── Parse Cache ───────────────────────────────────────────────────────────

(it-sequential "incr-cache-operations"
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (let ((nodes (list (make-test-cst-token :T-INT 42 0 2))))
      (cl-cc/parse:cache-store "42" nodes)
      (expect (cl-cc/parse:cache-lookup "42") :to-equal nodes)))
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (expect (cl-cc/parse:cache-lookup "unknown") :to-be-null))
  (let ((cl-cc/parse::*parse-cache* (make-hash-table :test 'equal)))
    (cl-cc/parse:cache-store "a" '(1))
    (cl-cc/parse:cache-store "b" '(2))
    (cl-cc/parse:invalidate-parse-cache)
    (expect (cl-cc/parse:cache-lookup "a") :to-be-null)
    (expect (cl-cc/parse:cache-lookup "b") :to-be-null)))


;;; ─── CST Equality ──────────────────────────────────────────────────────────

(it-sequential "incr-cst-equal-token-cases same-kind-value"
  (destructuring-bind (a-kind a-val b-kind b-val expected) (list :T-INT 42 :T-INT 42 t)
    (let ((a (make-test-cst-token a-kind a-val 0 2))
         (b (make-test-cst-token b-kind b-val 0 2)))
     (expect (cl-cc/parse:cst-equal-p a b) :to-equal expected))))

(it-sequential "incr-cst-equal-token-cases diff-value"
  (destructuring-bind (a-kind a-val b-kind b-val expected) (list :T-INT 42 :T-INT 99 nil)
    (let ((a (make-test-cst-token a-kind a-val 0 2))
         (b (make-test-cst-token b-kind b-val 0 2)))
     (expect (cl-cc/parse:cst-equal-p a b) :to-equal expected))))

(it-sequential "incr-cst-equal-token-cases diff-kind"
  (destructuring-bind (a-kind a-val b-kind b-val expected) (list :T-INT 1 :T-IDENT 1 nil)
    (let ((a (make-test-cst-token a-kind a-val 0 2))
         (b (make-test-cst-token b-kind b-val 0 2)))
     (expect (cl-cc/parse:cst-equal-p a b) :to-equal expected))))

(it-sequential "incr-cst-equal-interior"
  (let ((a (make-test-interior :list 0 10
             (list (make-test-cst-token :T-INT 1 0 5)
                   (make-test-cst-token :T-INT 2 5 10))))
        (b (make-test-interior :list 0 10
             (list (make-test-cst-token :T-INT 1 0 5)
                   (make-test-cst-token :T-INT 2 5 10)))))
    (expect (cl-cc/parse:cst-equal-p a b) :to-be-truthy)))

(it-sequential "incr-cst-equal-nil"
  (expect (cl-cc/parse:cst-equal-p nil nil) :to-be-truthy)
  (expect (cl-cc/parse:cst-equal-p nil (make-test-cst-token :T-INT 1 0 1)) :to-be-falsy)
  (expect (cl-cc/parse:cst-equal-p (make-test-cst-token :T-INT 1 0 1) nil) :to-be-falsy))

(it-sequential "incr-cst-equal-error-cases same"
  (destructuring-bind (msg-a msg-b expected) (list "bad" "bad" t)
    (let ((a (cl-cc/parse:make-cst-error :kind :error :start-byte 0 :end-byte 5
                                         :message msg-a))
         (b (cl-cc/parse:make-cst-error :kind :error :start-byte 0 :end-byte 5
                                         :message msg-b)))
     (expect (cl-cc/parse:cst-equal-p a b) :to-equal expected))))

(it-sequential "incr-cst-equal-error-cases different"
  (destructuring-bind (msg-a msg-b expected) (list "bad" "worse" nil)
    (let ((a (cl-cc/parse:make-cst-error :kind :error :start-byte 0 :end-byte 5
                                         :message msg-a))
         (b (cl-cc/parse:make-cst-error :kind :error :start-byte 0 :end-byte 5
                                         :message msg-b)))
     (expect (cl-cc/parse:cst-equal-p a b) :to-equal expected))))
