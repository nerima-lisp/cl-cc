;;;; optimizer-dataflow-passes-tests.lisp — global-DCE, pipeline, LICM, PRE
(in-package :cl-cc/test)


(it-sequential "global-dce-removes-unreachable-registered-function"
  (let* ((closure (make-vm-closure :dst :r0 :label "dead" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (register (cl-cc:make-vm-register-function :name 'dead :src :r0))
         (label (make-vm-label :name "dead"))
         (body (make-vm-const :dst :r2 :value 7))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc/optimize::opt-pass-global-dce (list closure register label body ret))))
    (expect (member closure out) :to-be-falsy)
    (expect (member register out) :to-be-falsy)
    (expect (member label out) :to-be-falsy)
    (expect (member body out) :to-be-falsy)
    (expect (member ret out) :to-be-falsy)))

(it-sequential "global-dce-preserves-top-level-called-chain"
  (let* ((f-closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                     :captured nil :optional-params nil :rest-param nil :key-params nil))
         (f-register (cl-cc:make-vm-register-function :name 'f :src :r0))
         (f-label (make-vm-label :name "f"))
         (f-ref (make-vm-func-ref :dst :r2 :label "g"))
         (f-call (make-vm-call :dst :r3 :func :r2 :args '(:r1)))
         (f-ret (make-vm-ret :reg :r3))
         (g-closure (make-vm-closure :dst :r4 :label "g" :params '(:r5)
                                     :captured nil :optional-params nil :rest-param nil :key-params nil))
         (g-label (make-vm-label :name "g"))
         (g-body (make-vm-const :dst :r6 :value 9))
         (g-ret (make-vm-ret :reg :r6))
         (top-ref (make-vm-func-ref :dst :r7 :label "f"))
         (top-arg (make-vm-const :dst :r8 :value 1))
         (top-call (make-vm-call :dst :r9 :func :r7 :args '(:r8)))
         (top-ret (make-vm-ret :reg :r9))
         (out (cl-cc/optimize::opt-pass-global-dce
               (list f-closure f-register f-label f-ref f-call f-ret
                     g-closure g-label g-body g-ret
                     top-ref top-arg top-call top-ret))))
    (expect (member f-closure out) :to-be-truthy)
    (expect (member f-label out) :to-be-truthy)
    (expect (member g-closure out) :to-be-truthy)
    (expect (member g-label out) :to-be-truthy)
    (expect (member top-call out) :to-be-truthy)))

(it-sequential "global-dce-removes-unreachable-func-ref-definition"
  (let* ((ref (make-vm-func-ref :dst :r0 :label "dead-ref" :params '(:r1)))
         (register (cl-cc:make-vm-register-function :name 'dead-ref :src :r0))
         (label (make-vm-label :name "dead-ref"))
         (body (make-vm-const :dst :r2 :value 7))
         (ret (make-vm-ret :reg :r2))
         (top (make-vm-const :dst :r3 :value :ok))
         (out (cl-cc/optimize::opt-pass-global-dce
               (list ref register label body ret top))))
    (expect (member ref out) :to-be-falsy)
    (expect (member register out) :to-be-falsy)
    (expect (member label out) :to-be-falsy)
    (expect (member body out) :to-be-falsy)
    (expect (member ret out) :to-be-falsy)
    (expect (member top out) :to-be-truthy)))

(it-sequential "known-callee-labels-track-const-func-ref-and-move direct-closure"
  (destructuring-bind (reg) (list :r0)
    (let* ((closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (regfun  (make-vm-register-function :name 'f :src :r0))
         (const   (make-vm-const :dst :r2 :value 'f))
         (move    (make-vm-move :dst :r3 :src :r2))
         (known   (cl-cc/optimize:opt-known-callee-labels (list closure regfun const move))))
    (expect (gethash reg known) :to-equal "f"))))

(it-sequential "known-callee-labels-track-const-func-ref-and-move const-value"
  (destructuring-bind (reg) (list :r2)
    (let* ((closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (regfun  (make-vm-register-function :name 'f :src :r0))
         (const   (make-vm-const :dst :r2 :value 'f))
         (move    (make-vm-move :dst :r3 :src :r2))
         (known   (cl-cc/optimize:opt-known-callee-labels (list closure regfun const move))))
    (expect (gethash reg known) :to-equal "f"))))

(it-sequential "known-callee-labels-track-const-func-ref-and-move moved-copy"
  (destructuring-bind (reg) (list :r3)
    (let* ((closure (make-vm-closure :dst :r0 :label "f" :params '(:r1)
                                   :captured nil :optional-params nil :rest-param nil :key-params nil))
         (regfun  (make-vm-register-function :name 'f :src :r0))
         (const   (make-vm-const :dst :r2 :value 'f))
         (move    (make-vm-move :dst :r3 :src :r2))
         (known   (cl-cc/optimize:opt-known-callee-labels (list closure regfun const move))))
    (expect (gethash reg known) :to-equal "f"))))

(it-sequential "optimizer-pass-pipeline-forms keyword-list"
  (destructuring-bind (pipeline) (list '(:fold :dce))
    (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (out (cl-cc/optimize:optimize-instructions instrs :pass-pipeline pipeline)))
    (expect (length out) :to-equal 0))))

(it-sequential "optimizer-pass-pipeline-forms string"
  (destructuring-bind (pipeline) (list "fold,dce")
    (let* ((instrs (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (out (cl-cc/optimize:optimize-instructions instrs :pass-pipeline pipeline)))
    (expect (length out) :to-equal 0))))

(it-sequential "optimizer-ir-verify-valid"
  (let ((instrs (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2))))
    (expect (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test") :to-be-truthy)))

(it-sequential "optimizer-ir-verify-rejects-invalid duplicate-label"
  (destructuring-bind (instrs) (list (list (make-vm-label :name "L0")
                 (make-vm-label :name "L0")))
    (expect (handler-case (progn (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test") nil)
     (error () t)) :to-be-truthy)))

(it-sequential "optimizer-ir-verify-rejects-invalid missing-label-target"
  (destructuring-bind (instrs) (list (list (make-vm-jump :label "MISSING")))
    (expect (handler-case (progn (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test") nil)
     (error () t)) :to-be-truthy)))

(it-sequential "optimizer-ir-verify-rejects-invalid use-before-def"
  (destructuring-bind (instrs) (list (list (make-vm-add :dst :r0 :lhs :r1 :rhs :r2)))
    (expect (handler-case (progn (cl-cc/optimize:opt-verify-instructions instrs :pass-name "test") nil)
     (error () t)) :to-be-truthy)))

(it-sequential "optimizer-pass-pipeline-output-modes timings"
  (destructuring-bind (instrs extra-opts expected-strings) (list (list (make-vm-const :dst :r0 :value 1)) (list :print-pass-timings t :timing-stream nil) '("OPT-PASS-FOLD"))
    (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize:optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (expect (search (string-upcase s) text) :to-be-truthy))))))

(it-sequential "optimizer-pass-pipeline-output-modes stats"
  (destructuring-bind (instrs extra-opts expected-strings) (list (list (make-vm-const :dst :r0 :value 1)) (list :print-pass-stats t :stats-stream nil) '("OPT-PASS-FOLD" "BEFORE=" "AFTER="))
    (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize:optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (expect (search (string-upcase s) text) :to-be-truthy))))))

(it-sequential "optimizer-pass-pipeline-output-modes json-trace"
  (destructuring-bind (instrs extra-opts expected-strings) (list (list (make-vm-const :dst :r0 :value 1)) (list :trace-json-stream nil) '("\"traceEvents\"" "OPT-PASS-FOLD" "\"dur\""))
    (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize:optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (expect (search (string-upcase s) text) :to-be-truthy))))))

(it-sequential "optimizer-pass-pipeline-output-modes remarks-changed"
  (destructuring-bind (instrs extra-opts expected-strings) (list (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-const :dst :r1 :value 2)
                 (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)) (list :print-opt-remarks t :opt-remarks-mode :changed :opt-remarks-stream nil) '("OPT-PASS-FOLD" "CHANGED"))
    (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize:optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (expect (search (string-upcase s) text) :to-be-truthy))))))

(it-sequential "optimizer-pass-pipeline-output-modes remarks-missed"
  (destructuring-bind (instrs extra-opts expected-strings) (list (list (make-vm-const :dst :r0 :value 1)) (list :print-opt-remarks t :opt-remarks-mode :missed :opt-remarks-stream nil) '("OPT-PASS-FOLD" "MISSED"))
    (let* ((stream (make-string-output-stream))
         (patched-opts (loop for (k v) on extra-opts by #'cddr
                             nconc (if (null v) (list k stream) (list k v)))))
    (apply #'cl-cc/optimize:optimize-instructions instrs
           :pass-pipeline '(:fold)
           patched-opts)
    (let ((text (string-upcase (get-output-stream-string stream))))
      (dolist (s expected-strings)
        (expect (search (string-upcase s) text) :to-be-truthy))))))

(it-sequential "licm-does-not-hoist-loop-defined-value"
  (let* ((start (make-vm-label :name "start"))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (a1    (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc/optimize::opt-pass-licm (list start jmp1 loop c1 a1 back ret))))
    (expect (member a1 out) :to-be-truthy)
    (expect (> (position a1 out :test #'eq)
                    (position loop out :test #'eq)) :to-be-truthy)))

(it-sequential "pre-hoists-partially-redundant-expression"
  (let* ((entry (make-vm-label :name "entry"))
         (c0    (make-vm-const :dst :r0 :value 1))
         (c2    (make-vm-const :dst :r2 :value 2))
         (br    (make-vm-jump-zero :reg :r0 :label "p2"))
         (p1    (make-vm-label :name "p1"))
         (a1    (make-vm-add :dst :r3 :lhs :r0 :rhs :r2))
         (j1    (make-vm-jump :label "join"))
         (p2    (make-vm-label :name "p2"))
         (x     (make-vm-const :dst :r4 :value 7))
         (j2    (make-vm-jump :label "join"))
         (join  (make-vm-label :name "join"))
         (a2    (make-vm-add :dst :r5 :lhs :r0 :rhs :r2))
         (ret   (make-vm-ret :reg :r5))
         (out   (cl-cc/optimize::opt-pass-pre (list entry c0 c2 br p1 a1 j1 p2 x j2 join a2 ret))))
    (expect (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out) :to-equal 1)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-move)
                               (or (and (eq :r3 (cl-cc/vm::vm-dst i))
                                        (eq :r5 (cl-cc/vm::vm-src i)))
                                   (and (eq :r5 (cl-cc/vm::vm-dst i))
                                        (eq :r3 (cl-cc/vm::vm-src i))))))
                       out) :to-be-truthy)))
