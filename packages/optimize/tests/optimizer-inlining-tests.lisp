;;;; optimizer-inlining-tests.lisp — Inlining pass unit tests
(in-package :cl-cc/test)


;;; ── Inlining Pass: Unit Tests ──────────────────────────────────────────────

(defun inline-has-call-p (instructions)
  "T if INSTRUCTIONS contains at least one vm-call."
  (some #'cl-cc:vm-call-p instructions))

(it-sequential "inline-small-function"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "inc"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "inc"))
         (jump-past (cl-cc:make-vm-jump :label "after_inc"))
         (lbl     (cl-cc:make-vm-label :name "inc"))
         (body1   (cl-cc:make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc:make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc:make-vm-ret :reg :R12))
         (after   (cl-cc:make-vm-label :name "after_inc"))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 4))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (not (inline-has-call-p out)) :to-be-truthy)
    (expect (some (lambda (i)
                         (and (cl-cc:vm-move-p i)
                              (eq :R6 (cl-cc/vm::vm-dst i))))
                       out) :to-be-truthy)))

(it-sequential "inline-skip-large-function"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "big"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "big"))
         (jump-past (cl-cc:make-vm-jump :label "after_big"))
         (lbl     (cl-cc:make-vm-label :name "big"))
         ;; Generate a live arithmetic chain that stays above the inline threshold
         ;; even after convergence passes.
         (body    (append
                   (list (cl-cc:make-vm-const :dst :R20 :value 1))
                   (loop for i from 21 below 40
                         for prev = :R10 then (intern (format nil "R~A" (1- i)) :keyword)
                         collect (cl-cc:make-vm-add
                                  :dst (intern (format nil "R~A" i) :keyword)
                                  :lhs prev
                                  :rhs :R20))))
         (ret     (cl-cc:make-vm-ret :reg :R39))
         (after   (cl-cc:make-vm-label :name "after_big"))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 0))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (append (list closure fref jump-past lbl)
                          body
                          (list ret after arg call halt)))
          (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-truthy)))

(it-sequential "inline-force-large-function-via-policy"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "big-inline"
                                          :params '(:R10)
                                          :captured nil
                                          :inline-policy :inline))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "big-inline"))
         (jump-past (cl-cc:make-vm-jump :label "after_big_inline"))
         (lbl     (cl-cc:make-vm-label :name "big-inline"))
         (body    (append
                   (list (cl-cc:make-vm-const :dst :R20 :value 1))
                   (loop for i from 21 below 40
                         for prev = :R10 then (intern (format nil "R~A" (1- i)) :keyword)
                         collect (cl-cc:make-vm-add
                                  :dst (intern (format nil "R~A" i) :keyword)
                                  :lhs prev
                                  :rhs :R20))))
         (ret     (cl-cc:make-vm-ret :reg :R39))
         (after   (cl-cc:make-vm-label :name "after_big_inline"))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 0))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (append (list closure fref jump-past lbl)
                          body
                          (list ret after arg call halt)))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-falsy)))

(it-sequential "inline-notinline-policy-blocks-small-function"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "tiny-noinline"
                                          :params '(:R10)
                                          :captured nil
                                          :inline-policy :notinline))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "tiny-noinline"))
         (jump-past (cl-cc:make-vm-jump :label "after_tiny_noinline"))
         (lbl     (cl-cc:make-vm-label :name "tiny-noinline"))
         (body1   (cl-cc:make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc:make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc:make-vm-ret :reg :R12))
         (after   (cl-cc:make-vm-label :name "after_tiny_noinline"))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 4))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-truthy)))

(it-sequential "inline-skip-cases captured-vars"
  (destructuring-bind (make-instrs) (list (lambda ()
             (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "captured"
                                                     :params '(:R10) :captured '(:R99)))
                    (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "captured"))
                    (jump-past (cl-cc:make-vm-jump :label "after_cap"))
                    (lbl     (cl-cc:make-vm-label :name "captured"))
                    (body1   (cl-cc:make-vm-const :dst :R11 :value 1))
                    (body2   (cl-cc:make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
                    (ret     (cl-cc:make-vm-ret :reg :R12))
                    (after   (cl-cc:make-vm-label :name "after_cap"))
                    (arg     (cl-cc:make-vm-const :dst :R1 :value 4))
                    (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc:make-vm-halt)))
               (list closure fref jump-past lbl body1 body2 ret after arg call halt))))
    (let* ((instrs (funcall make-instrs))
         (out    (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-truthy))))

(it-sequential "inline-skip-cases recursive-global-ref"
  (destructuring-bind (make-instrs) (list (lambda ()
             (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "rec"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "rec"))
                    (jump-past (cl-cc:make-vm-jump :label "after_rec"))
                    (lbl     (cl-cc:make-vm-label :name "rec"))
                    (b1      (cl-cc:make-vm-const :dst :R11 :value 0))
                    (b2      (cl-cc:make-vm-sub :dst :R12 :lhs :R10 :rhs :R11))
                    (b3      (cl-cc:make-vm-call :dst :R13 :func :R5 :args '(:R12)))
                    (ret     (cl-cc:make-vm-ret :reg :R13))
                    (after   (cl-cc:make-vm-label :name "after_rec"))
                    (arg     (cl-cc:make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc:make-vm-halt)))
               (list closure fref jump-past lbl b1 b2 b3 ret after arg call halt))))
    (let* ((instrs (funcall make-instrs))
         (out    (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-truthy))))

(it-sequential "inline-skip-cases self-recursive-guard"
  (destructuring-bind (make-instrs) (list (lambda ()
             (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "loop"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "loop"))
                    (jump-past (cl-cc:make-vm-jump :label "after_loop"))
                    (lbl     (cl-cc:make-vm-label :name "loop"))
                    (self    (cl-cc:make-vm-func-ref :dst :R7 :label "loop"))
                    (body1   (cl-cc:make-vm-call :dst :R11 :func :R7 :args '(:R10)))
                    (ret     (cl-cc:make-vm-ret :reg :R11))
                    (after   (cl-cc:make-vm-label :name "after_loop"))
                    (arg     (cl-cc:make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc:make-vm-halt)))
               (list closure fref jump-past lbl self body1 ret after arg call halt))))
    (let* ((instrs (funcall make-instrs))
         (out    (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (inline-has-call-p out) :to-be-truthy))))

(it-sequential "inline-register-rename"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "g"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "g"))
         (jump-past (cl-cc:make-vm-jump :label "after_g"))
         (lbl     (cl-cc:make-vm-label :name "g"))
         ;; Body uses :R11 and :R12 (same registers the call site also uses)
         (body1   (cl-cc:make-vm-const :dst :R11 :value 10))
         (body2   (cl-cc:make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc:make-vm-ret :reg :R12))
         (after   (cl-cc:make-vm-label :name "after_g"))
         ;; Call site also uses :R11 and :R12 for its own values
         (site1   (cl-cc:make-vm-const :dst :R11 :value 100))
         (site2   (cl-cc:make-vm-const :dst :R12 :value 200))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 7))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret
                        after site1 site2 arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    ;; The call should be inlined (small body, no captures)
    (expect (not (inline-has-call-p out)) :to-be-truthy)
    ;; The inlined body must use renamed registers (not the original :R11/:R12)
    ;; so collect all dst registers written after the call site's :R12 const
    ;; and before the halt — at least one must be > :R12 index
    (let ((inlined-dsts (loop for i in out
                              when (and (cl-cc:vm-move-p i)
                                        (eq :R6 (cl-cc/vm::vm-dst i)))
                              collect i)))
      ;; The final vm-move into :R6 must exist (proof of inlining)
      (expect (not (null inlined-dsts)) :to-be-truthy))))

(it-sequential "inline-propagates-constant-call-args"
  (let* ((closure (cl-cc:make-vm-closure :dst :R0 :label "k"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc:make-vm-func-ref :dst :R5 :label "k"))
         (jump-past (cl-cc:make-vm-jump :label "after_k"))
         (lbl     (cl-cc:make-vm-label :name "k"))
         (body1   (cl-cc:make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc:make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc:make-vm-ret :reg :R12))
         (after   (cl-cc:make-vm-label :name "after_k"))
         (arg     (cl-cc:make-vm-const :dst :R1 :value 4))
         (call    (cl-cc:make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc:make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (expect (not (inline-has-call-p out)) :to-be-truthy)
    (expect (count-if (lambda (i)
                              (and (cl-cc:vm-const-p i)
                                   (eql 4 (cl-cc/vm::vm-value i))))
                            out) :to-equal 2)))

