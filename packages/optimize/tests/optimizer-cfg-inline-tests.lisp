;;;; tests/optimizer-cfg-inline-tests.lisp — CFG Reachability and Inlining Pass Tests
;;; Extracted from optimizer-tests.lisp (> 700 lines).

(in-package :cl-cc/test)


;;; ── CFG Reachability / Label Cleanup / Block Merge ───────────────────────

(it-sequential "unreachable-code-after-jump"
  (let* ((j    (make-vm-jump  :label "end"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl  (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list j dead lbl ret))))
    (expect (member dead out) :to-be-falsy)
    (expect (member j    out) :to-be-truthy)
    (expect (member lbl  out) :to-be-truthy)
    (expect (member ret  out) :to-be-truthy)))

(it-sequential "unreachable-code-after-ret"
  (let* ((ret1 (make-vm-ret   :reg :r0))
         (dead (make-vm-const :dst :r1 :value 0))
         (lbl  (make-vm-label :name "after"))
         (ret2 (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list ret1 dead lbl ret2))))
    (expect (member dead out) :to-be-falsy)
    (expect (member ret1 out) :to-be-truthy)
    (expect (member lbl  out) :to-be-truthy)))

(it-sequential "unreachable-label-revives-reachability"
  (let* ((j   (make-vm-jump  :label "lbl"))
         (lbl (make-vm-label :name "lbl"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-unreachable (list j lbl c ret))))
    (expect (member c out) :to-be-truthy)))

(it-sequential "dead-labels-removes-unreferenced-label"
  (let* ((lbl (make-vm-label :name "ghost"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list lbl c ret))))
    (expect (member lbl out) :to-be-falsy)
    (expect (member c   out) :to-be-truthy)
    (expect (member ret out) :to-be-truthy)))

(it-sequential "dead-labels-preserves-referenced-labels jump-target"
  (destructuring-bind (ref-inst lbl-name) (list (make-vm-jump :label "live") "live")
    (let* ((lbl (make-vm-label :name lbl-name))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list ref-inst lbl ret))))
    (expect (member lbl out) :to-be-truthy))))

(it-sequential "dead-labels-preserves-referenced-labels closure-entry"
  (destructuring-bind (ref-inst lbl-name) (list (make-vm-closure :dst :r0 :label "fn" :params nil :captured nil
                            :optional-params nil :rest-param nil :key-params nil) "fn")
    (let* ((lbl (make-vm-label :name lbl-name))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list ref-inst lbl ret))))
    (expect (member lbl out) :to-be-truthy))))

(it-sequential "dead-labels-preserves-referenced-labels handler-label"
  (destructuring-bind (ref-inst lbl-name) (list (cl-cc:make-vm-establish-handler :handler-label "err-handler"
                                              :result-reg :r0 :error-type 'error) "err-handler")
    (let* ((lbl (make-vm-label :name lbl-name))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list ref-inst lbl ret))))
    (expect (member lbl out) :to-be-truthy))))

(it-sequential "dead-basic-blocks-eliminated"
  (let* ((j    (make-vm-jump  :label "exit"))
         (lbl1 (make-vm-label :name "dead"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl2 (make-vm-label :name "exit"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-dead-basic-blocks (list j lbl1 dead lbl2 ret))))
    (expect (member j out) :to-be-truthy)
    (expect (member lbl1 out) :to-be-falsy)
    (expect (member dead out) :to-be-falsy)
    (expect (member lbl2 out) :to-be-truthy)
    (expect (member ret out) :to-be-truthy)))

(it-sequential "block-merge-eliminates-single-pred-label"
  (let* ((start (make-vm-label :name "start"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (jmp   (make-vm-jump :label "mid"))
         (mid   (make-vm-label :name "mid"))
         (c2    (make-vm-const :dst :r2 :value 2))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc/optimize::opt-pass-block-merge (list start c1 jmp mid c2 ret))))
    (expect (member mid out) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out) :to-be-falsy)
    (expect (member c1 out) :to-be-truthy)
    (expect (member c2 out) :to-be-truthy)
    (expect (member ret out) :to-be-truthy)))

(it-sequential "tail-merge-merges-identical-blocks"
  (let* ((entry (make-vm-label :name "entry"))
         (seed  (make-vm-const :dst :r0 :value nil))
         (br    (make-vm-jump-zero :reg :r0 :label "dup2"))
         (dup1  (make-vm-label :name "dup1"))
         (a1    (make-vm-const :dst :r1 :value 1))
         (j1    (make-vm-jump :label "exit"))
         (dup2  (make-vm-label :name "dup2"))
         (a2    (make-vm-const :dst :r1 :value 1))
         (j2    (make-vm-jump :label "exit"))
         (exit  (make-vm-label :name "exit"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-tail-merge (list entry seed br dup1 a1 j1 dup2 a2 j2 exit ret))))
    (expect (member dup1 out) :to-be-truthy)
    (expect (member dup2 out) :to-be-falsy)
    (expect (count-if (lambda (i)
                                (and (typep i 'cl-cc/vm::vm-const)
                                     (eq :r1 (cl-cc/vm::vm-dst i))))
                              out) :to-equal 1)
    (expect (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out) :to-equal 1)))

(it-sequential "constant-hoist-moves-loop-constant-to-preheader"
  (let* ((start (make-vm-label :name "start"))
         (seed  (make-vm-const :dst :r0 :value 0))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (hoist (make-vm-const :dst :r1 :value 99))
         (jmp2  (make-vm-jump :label "body"))
         (body  (make-vm-label :name "body"))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-licm
                 (list start seed jmp1 loop hoist jmp2 body back ret))))
    (expect (member hoist out) :to-be-truthy)
    (expect (member loop out) :to-be-truthy)
    (expect (< (position hoist out :test #'eq)
                    (position loop out :test #'eq)) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership constant-hoist"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-licm)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership global-dce"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-global-dce)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership inline"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-inline-iterative)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership pre"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-pre)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership bswap"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-bswap-recognition)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "opt-convergence-pass-membership rotate"
  (destructuring-bind (pass-fn) (list #'cl-cc/optimize::opt-pass-rotate-recognition)
    (expect (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq) :to-be-truthy)))

(it-sequential "optimizer-batch-concatenate"
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value "a"))
         (i2 (cl-cc:make-vm-const :dst :R1 :value "b"))
         (i3 (cl-cc:make-vm-const :dst :R2 :value "c"))
         (c1 (cl-cc:make-vm-concatenate :dst :R3 :str1 :R0 :str2 :R1))
         (c2 (cl-cc:make-vm-concatenate :dst :R4 :str1 :R3 :str2 :R2))
         (out (cl-cc/optimize::opt-pass-batch-concatenate (list i1 i2 i3 c1 c2)))
         (inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out)))
    (expect (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out) :to-equal 1)
    (expect inst :to-be-truthy)
    (expect (cl-cc/vm::vm-parts inst) :to-equal '(:R0 :R1 :R2))
    (expect (cl-cc/vm::vm-dst inst) :to-equal :R4)))

