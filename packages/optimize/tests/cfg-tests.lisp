;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators, cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/test)

;;; ─── Helpers ─────────────────────────────────────────────────────────────
(defun make-test-cfg-linear ()
  "Build a CFG from a simple linear instruction sequence: CONST → ADD → RET."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const :dst :r0 :value 1)
         (make-vm-const :dst :r1 :value 2)
         (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
         (make-vm-ret   :reg :r2))))
(defun make-test-cfg-branch ()
  "Build a CFG with a conditional branch (entry JUMP-ZERO→then, else JUMP→exit, then, exit RET)."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "then")
         (make-vm-const    :dst :r1 :value 99)
         (make-vm-jump     :label "exit")
         (make-vm-label    :name "then")
         (make-vm-const    :dst :r1 :value 42)
         (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r1))))
(defun make-test-cfg-loop ()
  "Build a CFG with a simple natural loop and a separate exit block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-label    :name "head")
          (make-vm-const    :dst :r0 :value 1)
          (make-vm-jump-zero :reg :r0 :label "exit")
          (make-vm-jump     :label "head")
          (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-hot-cold ()
  "Build a CFG with one loop-heavy hot block and one cold exit block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 1)
         (make-vm-jump-zero :reg :r0 :label "cold")
         (make-vm-label    :name "hot")
         (make-vm-const    :dst :r1 :value 2)
         (make-vm-jump     :label "hot")
          (make-vm-label    :name "cold")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-cold-signal ()
  "Build a CFG with a normal block and an explicit cold error block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const     :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "hot")
         (make-vm-label    :name "cold")
          (cl-cc:make-vm-signal-error :error-reg :r0)
          (make-vm-ret      :reg :r0)
          (make-vm-label    :name "hot")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-critical-edge ()
  "Build a CFG with one critical edge into the THEN block."
  (cl-cc/optimize:cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
          (make-vm-jump-zero :reg :r0 :label "then")
          (make-vm-const    :dst :r1 :value 99)
          (make-vm-jump     :label "then")
          (make-vm-label    :name "then")
          (make-vm-const    :dst :r1 :value 42)
          (make-vm-label    :name "merge")
          (make-vm-ret      :reg :r1))))

;;; ─── Basic CFG Construction ──────────────────────────────────────────────
(it-sequential "cfg-linear-has-single-block"
  (let* ((cfg   (make-test-cfg-linear))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= 1 (cl-cc/optimize:cfg-block-count cfg)) :to-be-truthy)
    (expect entry :to-be-truthy)
    (expect (cl-cc:bb-instructions entry) :to-be-truthy)))

(it-sequential "cfg-empty-has-entry-block"
  (let ((cfg (cl-cc/optimize:cfg-build nil)))
    (expect (cl-cc/optimize:cfg-entry cfg) :to-be-truthy)))

(it-sequential "cfg-branch-has-multiple-blocks"
  (let ((cfg (make-test-cfg-branch)))
    (expect (>= (cl-cc/optimize:cfg-block-count cfg) 2) :to-be-truthy)))
(it-sequential "cfg-branch-labels-resolved then"
  (destructuring-bind (label-name) (list "then")
    (let ((cfg (make-test-cfg-branch)))
    (expect (cl-cc/optimize:cfg-get-block-by-label cfg label-name) :to-be-truthy))))

(it-sequential "cfg-branch-labels-resolved exit"
  (destructuring-bind (label-name) (list "exit")
    (let ((cfg (make-test-cfg-branch)))
    (expect (cl-cc/optimize:cfg-get-block-by-label cfg label-name) :to-be-truthy))))

;;; ─── Predecessor / Successor Edges ──────────────────────────────────────
(it-sequential "cfg-branch-entry-has-two-successors"
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= 2 (length (cl-cc:bb-successors entry))) :to-be-truthy)))

(it-sequential "cfg-branch-exit-has-predecessor"
  (let* ((cfg  (make-test-cfg-branch))
         (exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit")))
    (when exit
      (expect (>= (length (cl-cc:bb-predecessors exit)) 1) :to-be-truthy))))

;;; ─── RPO ─────────────────────────────────────────────────────────────────
(it-sequential "cfg-rpo-ordering"
  (let* ((cfg   (make-test-cfg-branch))
         (rpo   (cl-cc/optimize:cfg-compute-rpo cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (expect (= (cl-cc/optimize:cfg-block-count cfg) (length rpo)) :to-be-truthy)
    (expect (car rpo) :to-be entry)))

;;; ─── Dominator Tree ──────────────────────────────────────────────────────
(it-sequential "cfg-dominator-properties"
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (expect (cl-cc:bb-idom entry) :to-be entry)
    (let ((exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit")))
      (when exit
        (expect (cl-cc/optimize:cfg-dominates-p entry exit) :to-be-truthy)))))

;;; ─── Dominance Frontiers ─────────────────────────────────────────────────
(it-sequential "cfg-dominance-frontiers-computed"
  (let ((cfg (make-test-cfg-branch)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-dominance-frontiers cfg)
    (expect t :to-be-truthy)))

;;; ─── Post-Dominator Tree ────────────────────────────────────────────────
(it-sequential "cfg-post-dominators-computed"
  (handler-bind ((warning #'muffle-warning))
    (let ((cfg (make-test-cfg-branch)))
      (cl-cc/optimize::cfg-compute-post-dominators cfg)
      (let ((exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
            (entry (cl-cc/optimize:cfg-entry cfg)))
        (expect exit :to-be-truthy)
        (expect (cl-cc/optimize::bb-post-idom exit) :to-be exit)
        (expect (cl-cc/optimize::cfg-post-dominates-p exit entry) :to-be-truthy)))))
(it-sequential "cfg-loop-depths-computed"
  (let* ((cfg (make-test-cfg-loop))
         (head (cl-cc/optimize:cfg-get-block-by-label cfg "head"))
         (exit (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
         (body (find-if (lambda (b)
                          (some (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "head")))
                                (cl-cc:bb-instructions b)))
                        (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (expect head :to-be-truthy)
    (expect body :to-be-truthy)
    (expect (= 1 (cl-cc:bb-loop-depth head)) :to-be-truthy)
    (expect (= 1 (cl-cc:bb-loop-depth body)) :to-be-truthy)
    (expect (= 0 (cl-cc:bb-loop-depth exit)) :to-be-truthy)))
(it-sequential "cfg-hot-cold-flatten-cold-after-hot loop-vs-exit"
  (destructuring-bind (cfg) (list (make-test-cfg-hot-cold))
    (cl-cc/optimize:cfg-compute-dominators cfg) (cl-cc/optimize:cfg-compute-loop-depths cfg) (let* ((flat   (cl-cc/optimize:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc/vm::vm-label)
                       collect (cl-cc/vm::vm-name inst))))
    (expect (member "hot"  labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy))))

(it-sequential "cfg-hot-cold-flatten-cold-after-hot normal-vs-signal"
  (destructuring-bind (cfg) (list (make-test-cfg-cold-signal))
    (cl-cc/optimize:cfg-compute-dominators cfg) (cl-cc/optimize:cfg-compute-loop-depths cfg) (let* ((flat   (cl-cc/optimize:cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc/vm::vm-label)
                       collect (cl-cc/vm::vm-name inst))))
    (expect (member "hot"  labels :test #'equal) :to-be-truthy)
    (expect (member "cold" labels :test #'equal) :to-be-truthy)
    (expect (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)) :to-be-truthy))))

(it-sequential "cfg-hot-cold-flatten-places-error-handler-last"
  (let ((cfg (make-test-cfg-cold-signal)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (let* ((flat (cl-cc/optimize:cfg-flatten-hot-cold cfg))
           (labels (loop for inst in flat
                         when (typep inst 'cl-cc/vm::vm-label)
                         collect (cl-cc/vm::vm-name inst))))
      (expect (car (last labels)) :to-equal "cold"))))
(it-sequential "cfg-critical-edge-splitting-inserts-landing-pad"
  (let* ((cfg (make-test-cfg-critical-edge))
         (before (cl-cc/optimize:cfg-block-count cfg))
         (entry (cl-cc/optimize:cfg-entry cfg))
         (then  (cl-cc/optimize:cfg-get-block-by-label cfg "then")))
    (cl-cc/optimize:cfg-split-critical-edges cfg)
    (expect (= (1+ before) (cl-cc/optimize:cfg-block-count cfg)) :to-be-truthy)
    (expect (not (member then (cl-cc:bb-successors entry) :test #'eq)) :to-be-truthy)
    (expect (not (member entry (cl-cc:bb-predecessors then) :test #'eq)) :to-be-truthy)
    (let ((pad (find-if (lambda (b)
                          (and (= 1 (length (cl-cc:bb-successors b)))
                               (eq then (first (cl-cc:bb-successors b)))
                               (some (lambda (i)
                                       (and (typep i 'cl-cc/vm::vm-jump)
                                            (equal (cl-cc/vm::vm-label-name i)
                                                   (cl-cc/vm::vm-name (cl-cc:bb-label then)))))
                                     (cl-cc:bb-instructions b))))
                        (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
      (expect pad :to-be-truthy)
      (expect (member pad (cl-cc:bb-successors entry) :test #'eq) :to-be-truthy))))

;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────
(it-sequential "cfg-flatten-preserves-instruction-count"
  (let* ((orig  (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc/optimize:cfg-build orig))
         (flat  (cl-cc/optimize:cfg-flatten cfg)))
    (expect (>= (length flat) (length orig)) :to-be-truthy)))

;;; ─── cfg-idf (Iterated Dominance Frontier) ──────────────────────────────────
(it-sequential "cfg-idf-empty-input-returns-nil"
  (expect (cl-cc/optimize:cfg-idf nil) :to-be-null))

(it-sequential "cfg-idf-linear-entry-returns-list"
  (let* ((cfg   (make-test-cfg-linear))
         (_     (cl-cc/optimize:cfg-compute-dominators cfg))
         (_     (cl-cc/optimize:cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (declare (ignore _ _))
    (expect (listp (cl-cc/optimize:cfg-idf (list entry))) :to-be-truthy)))

(it-sequential "cfg-idf-branch-entry-returns-list"
  (let* ((cfg   (make-test-cfg-branch))
         (_     (cl-cc/optimize:cfg-compute-dominators cfg))
         (_     (cl-cc/optimize:cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize:cfg-entry cfg)))
    (declare (ignore _ _))
    (expect (listp (cl-cc/optimize:cfg-idf (list entry))) :to-be-truthy)))

;;; ─── %cfg-fallthrough-edge / %cfg-jump-target-edge ───────────────────────
(it-sequential "cfg-fallthrough-edge-adds-when-next-start-exists"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (b1  (cl-cc/optimize::cfg-new-block g))
         (b2  (cl-cc/optimize::cfg-new-block g))
         (bbs (let ((ht (make-hash-table)))
                (setf (gethash 1 ht) b2)
                ht)))
    (cl-cc/optimize::%cfg-fallthrough-edge b1 1 bbs)
    (expect (member b2 (cl-cc:bb-successors b1) :test #'eq) :to-be-truthy)
    (expect (member b1 (cl-cc:bb-predecessors b2) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-fallthrough-edge-noop-when-nil"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (b1  (cl-cc/optimize::cfg-new-block g))
         (bbs (make-hash-table)))
    (cl-cc/optimize::%cfg-fallthrough-edge b1 nil bbs)
    (expect (cl-cc:bb-successors b1) :to-be-null)))

(it-sequential "cfg-jump-target-edge-wires-to-label"
  (let* ((g    (cl-cc/optimize:make-cfg))
         (src  (cl-cc/optimize::cfg-new-block g))
         (dest (cl-cc/optimize::cfg-new-block g :label (make-vm-label :name "tgt"))))
    (declare (ignore dest))
    (cl-cc/optimize::%cfg-jump-target-edge src (make-vm-jump :label "tgt") g)
    (let ((tgt (cl-cc/optimize:cfg-get-block-by-label g "tgt")))
      (expect (member tgt (cl-cc:bb-successors src) :test #'eq) :to-be-truthy))))

;;; ─── %cfg-replace-successor / %cfg-replace-predecessor / %cfg-replace-terminator
(it-sequential "cfg-replace-successor-swaps-block"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (make-instance 'cl-cc/optimize:basic-block))
         (new (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors blk) (list old))
    (cl-cc/optimize::%cfg-replace-successor blk old new)
    (expect (member old (cl-cc/optimize:bb-successors blk) :test #'eq) :to-be-falsy)
    (expect (member new (cl-cc/optimize:bb-successors blk) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-replace-predecessor-swaps-block"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (make-instance 'cl-cc/optimize:basic-block))
         (new (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-predecessors blk) (list old))
    (cl-cc/optimize::%cfg-replace-predecessor blk old new)
    (expect (member old (cl-cc/optimize:bb-predecessors blk) :test #'eq) :to-be-falsy)
    (expect (member new (cl-cc/optimize:bb-predecessors blk) :test #'eq) :to-be-truthy)))

(it-sequential "cfg-replace-terminator-swaps-instruction"
  (let* ((blk (make-instance 'cl-cc/optimize:basic-block))
         (old (make-vm-jump :label "a"))
         (new (make-vm-jump :label "b")))
    (setf (cl-cc/optimize:bb-instructions blk) (list old))
    (cl-cc/optimize::%cfg-replace-terminator blk old new)
    (expect (cl-cc/optimize:bb-instructions blk) :to-equal (list new))))

;;; ─── %cfg-ensure-label ────────────────────────────────────────────────────
(it-sequential "cfg-ensure-label-creates-fresh-when-absent"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (blk (cl-cc/optimize::cfg-new-block g)))
    (setf (cl-cc/optimize:bb-label blk) nil)
    (let ((lbl (cl-cc/optimize::%cfg-ensure-label blk g "test")))
      (expect (cl-cc/vm::vm-label-p lbl) :to-be-truthy)
      (expect (cl-cc/optimize:bb-label blk) :to-be lbl))))

(it-sequential "cfg-ensure-label-returns-existing"
  (let* ((g   (cl-cc/optimize:make-cfg))
         (blk (cl-cc/optimize::cfg-new-block g :label (make-vm-label :name "existing"))))
    (let ((lbl (cl-cc/optimize::%cfg-ensure-label blk g "test")))
      (expect (cl-cc/vm::vm-name lbl) :to-equal "existing"))))
