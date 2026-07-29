;;;; tests/optimizer-closure-tests.lisp — Closure optimization pass tests
;;; FR-330 closure capture dedup and FR-079 closure thunk sharing.

(in-package :cl-cc/test)


;;; ── FR-330 closure-capture-dedup ──────────────────────────────────────────

(it-sequential "closure-capture-dedup-shares-duplicate-environments"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (expect 2 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (second out)))
    (expect :r1 :to-be (cl-cc/vm::vm-src (second out)))))

(it-sequential "closure-capture-dedup-preserves-non-shareable"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r6))))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (expect 2 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (second out)))))

;;; ── FR-079 closure-thunk-sharing ──────────────────────────────────────────

(it-sequential "closure-thunk-sharing-deduplicates-safe-siblings"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (expect 2 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (second out)))
    (expect :r1 :to-be (cl-cc/vm::vm-src (second out)))))

(it-sequential "closure-thunk-sharing-noops-on-register-overwrite"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (kill (cl-cc/vm:make-vm-const :dst :r1 :value 0))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill c2))))
    (expect 3 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (third out)))))

(it-sequential "closure-thunk-sharing-preserves-different-capture"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r6))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (expect 2 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (second out)))))

(it-sequential "closure-thunk-sharing-noops-on-env-reg-write"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params ()
              :captured (list (cons 'a :r5) (cons 'b :r7))))
         (kill-env (cl-cc/vm:make-vm-const :dst :r5 :value 0))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params ()
              :captured (list (cons 'a :r5) (cons 'b :r7))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill-env c2))))
    (expect 3 :to-be (length out))
    (expect :r2 :to-be (cl-cc/vm::vm-dst (third out)))))

(it-sequential "closure-thunk-sharing-noops-across-cfg-boundary"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (jz  (cl-cc/vm:make-vm-jump-zero :reg :r0 :label "skip"))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 jz c2))))
    (expect 3 :to-be (length out))
    (expect (cl-cc/vm::vm-closure-p (third out)) :to-be-truthy)
    (expect :r2 :to-be (cl-cc/vm::vm-dst (third out)))))

(it-sequential "closure-capture-dedup-noops-across-cfg-boundary"
  (let* ((c1 (cl-cc/vm:make-vm-closure
              :dst :r1 :label "L0" :params () :captured (list (cons 'a :r5))))
         (jmp (cl-cc/vm:make-vm-jump :label "next"))
         (lbl (cl-cc/vm:make-vm-label :name "next"))
         (c2 (cl-cc/vm:make-vm-closure
              :dst :r2 :label "L0" :params () :captured (list (cons 'a :r5))))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 jmp lbl c2))))
    (expect 4 :to-be (length out))
    (expect (cl-cc/vm::vm-closure-p (fourth out)) :to-be-truthy)
    (expect :r2 :to-be (cl-cc/vm::vm-dst (fourth out)))))
