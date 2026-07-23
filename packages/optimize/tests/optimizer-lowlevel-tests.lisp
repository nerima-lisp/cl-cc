(in-package :cl-cc/test)


;;; ── Direct opt-pass-fold Tests ─────────────────────────────────────────

(it-sequential "fold-label-handling-cases"
  (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value 42)
                       (cl-cc:make-vm-jump :label "join")
                       (cl-cc:make-vm-label :name "join")
                        (cl-cc:make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs)))
    (expect (find-if (lambda (i) (typep i 'cl-cc/vm::vm-inc)) out) :to-be-truthy))
  (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value 42)
                       (cl-cc:make-vm-label :name "join")
                       (cl-cc:make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc:vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (expect r1-const :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1-const) :to-equal 43)))

(it-sequential "fold-type-pred number-p"
  (destructuring-bind (const-val pred-inst expected) (list 42 (cl-cc:make-vm-number-p :dst :R1 :src :R0) 1)
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc:vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (expect r1-const :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1-const) :to-equal expected))))

(it-sequential "fold-type-pred symbol-p"
  (destructuring-bind (const-val pred-inst expected) (list 42 (cl-cc:make-vm-symbol-p :dst :R1 :src :R0) 0)
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc:vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (expect r1-const :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1-const) :to-equal expected))))

(it-sequential "fold-type-pred not-nil"
  (destructuring-bind (const-val pred-inst expected) (list nil (cl-cc:make-vm-not       :dst :R1 :src :R0) t)
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc:vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (expect r1-const :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1-const) :to-equal expected))))

(it-sequential "fold-type-pred not-zero"
  (destructuring-bind (const-val pred-inst expected) (list 0 (cl-cc:make-vm-not       :dst :R1 :src :R0) nil)
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc:vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (expect r1-const :to-be-truthy)
    (expect (cl-cc/vm::vm-value r1-const) :to-equal expected))))

(it-sequential "fold-branch-known known-true-no-branch"
  (destructuring-bind (const-val verify) (list 1 (lambda (out)
             (expect (find-if #'cl-cc:vm-jump-p out) :to-be-falsy)))
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val)
                       (cl-cc:make-vm-jump-zero :reg :R0 :label "target")))
          (out (cl-cc/optimize::opt-pass-fold instrs)))
    (funcall verify out)
    (expect (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out) :to-be-falsy))))

(it-sequential "fold-branch-known known-false-jump"
  (destructuring-bind (const-val verify) (list nil (lambda (out)
             (expect (find-if #'cl-cc:vm-jump-p out) :to-be-truthy)))
    (let* ((instrs (list (cl-cc:make-vm-const :dst :R0 :value const-val)
                       (cl-cc:make-vm-jump-zero :reg :R0 :label "target")))
          (out (cl-cc/optimize::opt-pass-fold instrs)))
    (funcall verify out)
    (expect (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out) :to-be-falsy))))

;;; ── Direct opt-pass-copy-prop Tests ────────────────────────────────────

(it-sequential "copy-prop-operand-resolution chain-follows-two-moves"
  (destructuring-bind (instrs expected-lhs) (list (list (cl-cc:make-vm-move :dst :R1 :src :R0)
                 (cl-cc:make-vm-move :dst :R2 :src :R1)
                 (cl-cc:make-vm-add :dst :R3 :lhs :R2 :rhs :R2)) :R0)
    (let* ((out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (expect add-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-lhs add-inst) :to-equal expected-lhs))))

(it-sequential "copy-prop-operand-resolution fallthrough-label-preserves-copies"
  (destructuring-bind (instrs expected-lhs) (list (list (cl-cc:make-vm-move :dst :R1 :src :R0)
                 (cl-cc:make-vm-label :name "join")
                 (cl-cc:make-vm-add :dst :R2 :lhs :R1 :rhs :R1)) :R0)
    (let* ((out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (expect add-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-lhs add-inst) :to-equal expected-lhs))))

(it-sequential "copy-prop-operand-resolution overwrite-kills-alias"
  (destructuring-bind (instrs expected-lhs) (list (list (cl-cc:make-vm-move :dst :R1 :src :R0)
                 (cl-cc:make-vm-const :dst :R0 :value 99)
                 (cl-cc:make-vm-add :dst :R2 :lhs :R1 :rhs :R1)) :R1)
    (let* ((out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (expect add-inst :to-be-truthy)
    (expect (cl-cc/vm::vm-lhs add-inst) :to-equal expected-lhs))))

(it-sequential "copy-prop-kill-and-elim-cases"
  (let* ((copies (make-hash-table :test #'eq))
         (reverse nil))
    (setf (gethash :R1 copies) :R0)
    (setf (gethash :R2 copies) :R0)
    (setf (gethash :R3 copies) :R2)
    (setf reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies))
    (cl-cc/optimize::%opt-copy-prop-kill :R0 copies reverse)
    (expect (gethash :R1 copies) :to-be-falsy)
    (expect (gethash :R2 copies) :to-be-falsy)
    (expect (gethash :R3 copies) :to-be :R2))
  (let* ((instrs (list (cl-cc:make-vm-move :dst :R1 :src :R0)
                       (cl-cc:make-vm-move :dst :R0 :src :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move)) out)))
    (expect (length moves) :to-equal 1)))

(it-sequential "copy-prop-join-point"
  (let* ((instrs (list (cl-cc:make-vm-const :dst :R9 :value nil)
                       (cl-cc:make-vm-jump-zero :reg :R9 :label "else")
                       (cl-cc:make-vm-label :name "then")
                       (cl-cc:make-vm-move :dst :R1 :src :R0)
                       (cl-cc:make-vm-jump :label "join")
                       (cl-cc:make-vm-label :name "else")
                       (cl-cc:make-vm-move :dst :R1 :src :R0)
                       (cl-cc:make-vm-label :name "join")
                       (cl-cc:make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs)))
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
      (expect add-inst :to-be-truthy)
      (expect (cl-cc/vm::vm-lhs add-inst) :to-equal :R0)
      (expect (cl-cc/vm::vm-rhs add-inst) :to-equal :R0))))

(it-sequential "heap-alias-integration-cases"
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list alloc copy))))
    (expect (cl-cc/optimize:opt-must-alias-p :r0 :r3 roots) :to-be-truthy)
    (expect (cl-cc/optimize:opt-must-alias-p :r0 :r9 roots) :to-be-falsy))
  (let* ((alloc-a (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-b (make-vm-make-array :dst :r4 :size-reg :r5))
         (roots   (cl-cc/optimize:opt-compute-heap-aliases (list alloc-a alloc-b))))
    (expect (cl-cc/optimize:opt-may-alias-p :r0 :r4 roots) :to-be-falsy)
    (expect (cl-cc/optimize:opt-may-alias-p :r0 :r9 roots) :to-be-truthy)))

(it-sequential "points-to-helper-tracks-moves-and-kills"
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (kill  (make-vm-const :dst :r3 :value 9))
         (pt1   (cl-cc/optimize:opt-compute-heap-aliases (list alloc copy)))
         (pt2   (cl-cc/optimize:opt-compute-heap-aliases (list alloc copy kill))))
    (expect (gethash :r0 pt1) :to-be :r0)
    (expect (gethash :r3 pt1) :to-be :r0)
    (expect (nth-value 1 (gethash :r3 pt2)) :to-be-falsy)))

(it-sequential "heap-kind-helper-distinguishes-object-classes"
  (let* ((alloc-cons  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-array (make-vm-make-array :dst :r4 :size-reg :r5))
         (points-to   (cl-cc/optimize:opt-compute-heap-aliases (list alloc-cons alloc-array)))
         (heap-kinds  (cl-cc/optimize::opt-compute-heap-kinds (list alloc-cons alloc-array))))
    (expect (gethash :r0 heap-kinds) :to-be :cons)
    (expect (gethash :r4 heap-kinds) :to-be :array)
    (expect (cl-cc/optimize:opt-may-alias-by-type-p :r0 :r4 points-to heap-kinds) :to-be-falsy)
    (expect (cl-cc/optimize:opt-may-alias-by-type-p :r0 :r9 points-to heap-kinds) :to-be-truthy)))

(it-sequential "constant-interval-helper-propagates-basic-arithmetic"
  (let* ((c1 (make-vm-const :dst :r0 :value 3))
         (c2 (make-vm-const :dst :r1 :value 5))
         (a  (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (s  (make-vm-sub :dst :r3 :lhs :r2 :rhs :r0))
         (m  (make-vm-mul :dst :r4 :lhs :r3 :rhs :r1))
         (intervals (cl-cc/optimize::opt-compute-constant-intervals (list c1 c2 a s m))))
    (expect (gethash :r2 intervals) :to-equal '(8 . 8))
    (expect (gethash :r3 intervals) :to-equal '(5 . 5))
    (expect (gethash :r4 intervals) :to-equal '(25 . 25))))

;;; ─── opt-inst-read-regs ──────────────────────────────────────────────────────

(it-sequential "opt-inst-read-regs-cases const"
  (destructuring-bind (inst expected-members) (list (make-vm-const      :dst :r0 :value 42) '())
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases func-ref"
  (destructuring-bind (inst expected-members) (list (make-vm-func-ref   :dst :r0 :label "fn") '())
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases get-global"
  (destructuring-bind (inst expected-members) (list (make-vm-get-global :dst :r0 :name 'x) '())
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases move"
  (destructuring-bind (inst expected-members) (list (make-vm-move       :dst :r0 :src :r1) '(:r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases neg"
  (destructuring-bind (inst expected-members) (list (make-vm-neg        :dst :r0 :src :r1) '(:r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases null-p"
  (destructuring-bind (inst expected-members) (list (make-vm-null-p     :dst :r0 :src :r1) '(:r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases ret"
  (destructuring-bind (inst expected-members) (list (make-vm-ret        :reg :r0) '(:r0))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases set-global"
  (destructuring-bind (inst expected-members) (list (make-vm-set-global :src :r0 :name 'x) '(:r0))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases add"
  (destructuring-bind (inst expected-members) (list (make-vm-add        :dst :r0 :lhs :r1 :rhs :r2) '(:r1 :r2))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases lt"
  (destructuring-bind (inst expected-members) (list (make-vm-lt         :dst :r0 :lhs :r1 :rhs :r2) '(:r1 :r2))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases call"
  (destructuring-bind (inst expected-members) (list (make-vm-call       :dst :r0 :func :r1 :args '(:r2 :r3)) '(:r1 :r2 :r3))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases tail-call"
  (destructuring-bind (inst expected-members) (list (make-vm-tail-call  :dst :out :func :fn :args '(:arg :r1)) '(:fn :arg :r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))

(it-sequential "opt-inst-read-regs-cases trampoline"
  (destructuring-bind (inst expected-members) (list (make-vm-trampoline :dst :out :func :fn :args '(:arg :r1)) '(:fn :arg :r1))
    (let ((regs (cl-cc/optimize:opt-inst-read-regs inst)))
    (expect (length regs) :to-equal (length expected-members))
    (dolist (r expected-members)
      (expect (member r regs) :to-be-truthy)))))
