;;;; tests/unit/optimize/optimizer-inline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline.lisp
;;;;
;;;; Covers: opt-max-reg-index (empty, single, multi-register programs),
;;;;   opt-make-renaming (register discovery + counter assignment),
;;;;   opt-collect-function-defs (linear body detection, jump rejection),
;;;;   opt-build-function-name-map (symbol→label tracking),
;;;;   opt-build-call-graph (direct call edge extraction),
;;;;   opt-call-graph-recursive-labels (SCC detection),
;;;;   opt-known-callee-labels (register tracking through closures/moves).

(in-package :cl-cc/test)

(defmacro assert-inline-boolean-case (expected then-form else-form)
  `(if ,expected
       ,then-form
       ,else-form))

;;; ─── opt-max-reg-index ───────────────────────────────────────────────────────

(it-sequential "opt-max-reg-index-cases empty"
  (destructuring-bind (insts expected) (list nil -1)
    (expect (= expected (cl-cc/optimize::opt-max-reg-index insts)) :to-be-truthy)))

(it-sequential "opt-max-reg-index-cases label"
  (destructuring-bind (insts expected) (list (list (make-vm-label :name "L0")) -1)
    (expect (= expected (cl-cc/optimize::opt-max-reg-index insts)) :to-be-truthy)))

(it-sequential "opt-max-reg-index-cases single"
  (destructuring-bind (insts expected) (list (list (make-vm-const :dst :r0 :value 1)
                            (make-vm-ret   :reg :r0)) 0)
    (expect (= expected (cl-cc/optimize::opt-max-reg-index insts)) :to-be-truthy)))

(it-sequential "opt-max-reg-index-cases multiple"
  (destructuring-bind (insts expected) (list (list (make-vm-const :dst :r0 :value 1)
                            (make-vm-const :dst :r3 :value 2)
                            (make-vm-move  :dst :r7 :src :r3)
                            (make-vm-ret   :reg :r7)) 7)
    (expect (= expected (cl-cc/optimize::opt-max-reg-index insts)) :to-be-truthy)))

;;; ─── opt-make-renaming ───────────────────────────────────────────────────────

(it-sequential "opt-make-renaming-empty-body"
  (let ((ht (cl-cc/optimize::opt-make-renaming nil 0)))
    (expect (= 0 (hash-table-count ht)) :to-be-truthy)))

(it-sequential "opt-make-renaming-assigns-fresh-registers"
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (ht (cl-cc/optimize::opt-make-renaming insts 10)))
    ;; :r0 should be renamed to :r10
    (expect (gethash :r0 ht) :to-be :r10)))

(it-sequential "opt-make-renaming-distinct-per-register"
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-move  :dst :r2 :src :r0)
                      (make-vm-ret   :reg :r2)))
         (ht (cl-cc/optimize::opt-make-renaming insts 5)))
    ;; 3 unique source registers: :r0, :r1, :r2
    (expect (= 3 (hash-table-count ht)) :to-be-truthy)
    ;; All renamed to distinct fresh registers
    (let ((fresh (list (gethash :r0 ht) (gethash :r1 ht) (gethash :r2 ht))))
      (expect (= 3 (length (remove-duplicates fresh))) :to-be-truthy))))

;;; ─── opt-collect-function-defs ───────────────────────────────────────────────

(it-sequential "opt-collect-function-defs-empty-cases empty"
  (destructuring-bind (insts) (list nil)
    (expect (= 0 (hash-table-count (cl-cc/optimize::opt-collect-function-defs insts))) :to-be-truthy)))

(it-sequential "opt-collect-function-defs-empty-cases jump"
  (destructuring-bind (insts) (list (list (make-vm-closure :dst :r0 :label "jmp-fn" :params '(:r0) :captured nil)
                         (make-vm-label :name "jmp-fn")
                         (make-vm-jump  :label "somewhere")
                         (make-vm-ret   :reg :r0)))
    (expect (= 0 (hash-table-count (cl-cc/optimize::opt-collect-function-defs insts))) :to-be-truthy)))

(it-sequential "opt-collect-function-defs-linear-body"
  (let* ((closure (make-vm-closure :dst :r5 :label "fn-label"
                                   :params '(:r0) :captured nil))
         (lbl     (make-vm-label :name "fn-label"))
         (body1   (make-vm-const :dst :r1 :value 42))
         (ret     (make-vm-ret   :reg :r1))
         (insts   (list closure lbl body1 ret))
         (ht      (cl-cc/optimize::opt-collect-function-defs insts)))
    (expect (= 1 (hash-table-count ht)) :to-be-truthy)
    (let ((def (gethash "fn-label" ht)))
      (expect (not (null def)) :to-be-truthy)
      (expect (not (null (getf def :body))) :to-be-truthy))))

;;; ─── opt-build-function-name-map ─────────────────────────────────────────────

(it-sequential "opt-build-function-name-map-empty"
  (let ((ht (cl-cc/optimize::opt-build-function-name-map nil)))
    (expect (= 0 (hash-table-count ht)) :to-be-truthy)))

(it-sequential "opt-build-function-name-map-tracks-registration"
  (let* ((closure  (make-vm-closure :dst :r0 :label "my-label"
                                    :params nil :captured nil))
         (regfn    (cl-cc:make-vm-register-function :src :r0 :name 'my-fn))
         (insts    (list closure regfn))
         (ht       (cl-cc/optimize::opt-build-function-name-map insts)))
    (expect (gethash 'my-fn ht) :to-equal "my-label")))

;;; ─── opt-build-call-graph ────────────────────────────────────────────────────

(it-sequential "opt-build-call-graph-no-calls"
  (let* ((closure (make-vm-closure :dst :r0 :label "pure-fn"
                                   :params '(:r0) :captured nil))
         (lbl     (make-vm-label :name "pure-fn"))
         (body    (make-vm-const :dst :r1 :value 1))
         (ret     (make-vm-ret   :reg :r1))
         (insts   (list closure lbl body ret))
         (fdefs   (cl-cc/optimize::opt-collect-function-defs insts))
         (nmap    (cl-cc/optimize::opt-build-function-name-map insts))
         (graph   (cl-cc/optimize::opt-build-call-graph insts fdefs nmap)))
    (let ((callees (gethash "pure-fn" graph)))
      (expect callees :to-be-null))))

;;; ─── opt-call-graph-recursive-labels ────────────────────────────────────────

(it-sequential "opt-call-graph-recursive-labels-no-recursion"
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (expect (= 0 (hash-table-count rec)) :to-be-truthy))))

(it-sequential "opt-call-graph-recursive-labels-direct-recursion"
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "fact" graph) '("fact"))  ; fact calls itself
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (expect (gethash "fact" rec) :to-be-truthy))))

(it-sequential "opt-call-graph-recursive-labels-mutual-recursion"
  (let ((graph (make-hash-table :test #'equal)))
    (setf (gethash "even" graph) '("odd"))
    (setf (gethash "odd"  graph) '("even"))
    (let ((rec (cl-cc/optimize::opt-call-graph-recursive-labels graph)))
      (expect (gethash "even" rec) :to-be-truthy)
      (expect (gethash "odd"  rec) :to-be-truthy))))

;;; ─── opt-pass-inline ─────────────────────────────────────────────────────────

(it-sequential "opt-pass-inline-skips-recursive-callee"
  (let* ((insts (list (make-vm-closure :dst :r0 :label "loop"
                                       :params '(:r1) :captured nil)
                      (make-vm-label :name "loop")
                      (make-vm-func-ref :dst :r2 :label "loop")
                      (make-vm-call :dst :r3 :func :r2 :args '(:r1))
                      (make-vm-ret :reg :r3)
                      (make-vm-func-ref :dst :r4 :label "loop")
                      (make-vm-call :dst :r5 :func :r4 :args '(:r6))
                      (make-vm-ret :reg :r5)))
         (out (cl-cc/optimize::opt-pass-inline insts :threshold 100)))
    (expect (mapcar #'instruction->sexp out) :to-equal (mapcar #'instruction->sexp insts))
    (expect (= 2 (count-if #'cl-cc:vm-call-p out)) :to-be-truthy)))

;;; ─── opt-known-callee-labels ─────────────────────────────────────────────────

(it-sequential "opt-known-callee-labels-cases closure"
  (destructuring-bind (insts reg expected) (list (list (make-vm-closure :dst :r0 :label "my-fn" :params nil :captured nil)) :r0 "my-fn")
    (let ((table (cl-cc/optimize:opt-known-callee-labels insts)))
    (expect (gethash reg table) :to-equal expected))))

(it-sequential "opt-known-callee-labels-cases propagate"
  (destructuring-bind (insts reg expected) (list (list (make-vm-closure :dst :r0 :label "fn-x" :params nil :captured nil)
                 (make-vm-move :dst :r1 :src :r0)) :r1 "fn-x")
    (let ((table (cl-cc/optimize:opt-known-callee-labels insts)))
    (expect (gethash reg table) :to-equal expected))))

(it-sequential "opt-known-callee-labels-cases cleared"
  (destructuring-bind (insts reg expected) (list (list (make-vm-closure :dst :r0 :label "fn-a" :params nil :captured nil)
                 (make-vm-const :dst :r0 :value 99)) :r0 nil)
    (let ((table (cl-cc/optimize:opt-known-callee-labels insts)))
    (expect (gethash reg table) :to-equal expected))))

(it-sequential "opt-pass-devirtualize-cases direct-closure-call"
  (destructuring-bind (insts expected-count expected-label) (list (list (make-vm-closure :dst :r0 :label "target" :params nil :captured nil)
                 (make-vm-call :dst :r1 :func :r0 :args '(:r2))) 1 "target")
    (let* ((result (cl-cc/optimize:opt-pass-devirtualize insts))
         (refs (remove-if-not (lambda (inst)
                                (typep inst 'cl-cc/vm::vm-func-ref))
                              result)))
    (expect (= expected-count (length refs)) :to-be-truthy)
    (when expected-label
      (expect (cl-cc:vm-label-name (first refs)) :to-equal expected-label)))))

(it-sequential "opt-pass-devirtualize-cases move-propagated-call"
  (destructuring-bind (insts expected-count expected-label) (list (list (make-vm-closure :dst :r0 :label "moved" :params nil :captured nil)
                 (make-vm-move :dst :r3 :src :r0)
                 (make-vm-call :dst :r1 :func :r3 :args nil)) 1 "moved")
    (let* ((result (cl-cc/optimize:opt-pass-devirtualize insts))
         (refs (remove-if-not (lambda (inst)
                                (typep inst 'cl-cc/vm::vm-func-ref))
                              result)))
    (expect (= expected-count (length refs)) :to-be-truthy)
    (when expected-label
      (expect (cl-cc:vm-label-name (first refs)) :to-equal expected-label)))))

(it-sequential "opt-pass-devirtualize-cases overwrite-clears-callee"
  (destructuring-bind (insts expected-count expected-label) (list (list (make-vm-closure :dst :r0 :label "cleared" :params nil :captured nil)
                 (make-vm-const :dst :r0 :value 42)
                 (make-vm-call :dst :r1 :func :r0 :args nil)) 0 nil)
    (let* ((result (cl-cc/optimize:opt-pass-devirtualize insts))
         (refs (remove-if-not (lambda (inst)
                                (typep inst 'cl-cc/vm::vm-func-ref))
                              result)))
    (expect (= expected-count (length refs)) :to-be-truthy)
    (when expected-label
      (expect (cl-cc:vm-label-name (first refs)) :to-equal expected-label)))))

(it-sequential "opt-pass-devirtualize-is-idempotent-for-already-direct-call"
  (let* ((insts (list (make-vm-func-ref :dst :r0 :label "target")
                      (make-vm-call :dst :r1 :func :r0 :args nil)))
         (once (cl-cc/optimize:opt-pass-devirtualize insts))
         (twice (cl-cc/optimize:opt-pass-devirtualize once)))
    (expect (= 1 (count-if (lambda (inst)
                            (typep inst 'cl-cc/vm::vm-func-ref))
                          once)) :to-be-truthy)
    (expect (= 1 (count-if (lambda (inst)
                             (typep inst 'cl-cc/vm::vm-func-ref))
                           twice)) :to-be-truthy)))

(it-sequential "opt-pass-devirtualize-sealed-satiated-single-method-gf"
  (let* (         (cl-cc/optimize:*opt-enable-sealed-gf-devirtualization* t)
         (insts (list
                 (cl-cc:make-vm-class-def :dst :r0 :class-name 'sealed-node
                                           :superclasses nil :slot-names nil
                                           :slot-initargs nil :sealed t)
                 (cl-cc:make-vm-class-def :dst :r1 :class-name 'sealed-area
                                           :superclasses nil :slot-names nil
                                           :slot-initargs nil :sealed nil)
                 (cl-cc:make-vm-set-global :name 'sealed-area :src :r1)
                 (cl-cc:make-vm-get-global :dst :r2 :name 'sealed-area)
                 (cl-cc:make-vm-closure :dst :r3 :label "METHOD_SEALED_AREA_NODE"
                                        :params '(:r10) :captured nil)
                 (cl-cc:make-vm-register-method :gf-reg :r2 :specializer 'sealed-node
                                                :qualifier nil :method-reg :r3)
                 (cl-cc:make-vm-const :dst :r4 :value :__satiated__)
                 (cl-cc:make-vm-const :dst :r5 :value t)
                 (cl-cc:make-vm-sethash :key :r4 :value :r5 :table :r2)
                 (cl-cc:make-vm-make-obj :dst :r6 :class-reg :r0 :initarg-regs nil)
                 (cl-cc:make-vm-generic-call :dst :r7 :gf-reg :r2 :args '(:r6))))
         (out (cl-cc/optimize:opt-pass-devirtualize insts)))
    (expect (find-if (lambda (inst)
                             (typep inst 'cl-cc/vm::vm-generic-call))
                           out) :to-be-falsy)
    (expect (find-if (lambda (inst)
                            (and (typep inst 'cl-cc/vm::vm-func-ref)
                                 (equal "METHOD_SEALED_AREA_NODE"
                                        (cl-cc:vm-label-name inst))))
                          out) :to-be-truthy)
    (expect (find-if #'cl-cc:vm-call-p out) :to-be-truthy)))

(it-sequential "opt-pass-devirtualize-keeps-unsatiated-sealed-gf-dynamic"
  (let* ((insts (list
                 (cl-cc:make-vm-class-def :dst :r0 :class-name 'unsat-node
                                           :superclasses nil :slot-names nil
                                           :slot-initargs nil :sealed t)
                 (cl-cc:make-vm-get-global :dst :r2 :name 'unsat-gf)
                 (cl-cc:make-vm-closure :dst :r3 :label "METHOD_UNSAT"
                                        :params '(:r10) :captured nil)
                 (cl-cc:make-vm-register-method :gf-reg :r2 :specializer 'unsat-node
                                                :qualifier nil :method-reg :r3)
                 (cl-cc:make-vm-make-obj :dst :r6 :class-reg :r0 :initarg-regs nil)
                 (cl-cc:make-vm-generic-call :dst :r7 :gf-reg :r2 :args '(:r6))))
         (out (cl-cc/optimize:opt-pass-devirtualize insts)))
    (expect (find-if (lambda (inst)
                            (typep inst 'cl-cc/vm::vm-generic-call))
                          out) :to-be-truthy)))

(it-sequential "opt-pass-call-site-splitting-duplicates-known-predecessor-call"
  (let* ((insts (list (make-vm-jump-zero :reg :cond :label "else")
                      (make-vm-func-ref :dst :fn :label "then-fn")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-func-ref :dst :fn :label "else-fn")
                      (make-vm-label :name "join")
                      (make-vm-call :dst :out :func :fn :args '(:arg))
                      (make-vm-halt :reg :out)))
         (out (cl-cc/optimize:opt-pass-call-site-splitting insts))
         (calls (remove-if-not #'cl-cc:vm-call-p out))
         (after-jump (find-if (lambda (inst)
                                (and (typep inst 'cl-cc/vm::vm-jump)
                                     (search "CALL-SITE-SPLIT-AFTER-"
                                             (cl-cc:vm-label-name inst))))
                              out))
         (after-label (and after-jump (cl-cc:vm-label-name after-jump))))
    (expect (= 2 (length calls)) :to-be-truthy)
    (expect after-jump :to-be-truthy)
    (expect (find-if (lambda (inst)
                            (and (typep inst 'cl-cc/vm::vm-label)
                                 (equal after-label (cl-cc/vm::vm-name inst))))
                          out) :to-be-truthy)
    (expect (find-if (lambda (inst)
                            (and (typep inst 'cl-cc/vm::vm-func-ref)
                                 (equal "then-fn" (cl-cc:vm-label-name inst))))
                          out) :to-be-truthy)))

(it-sequential "opt-pass-call-site-splitting-noops-without-known-callee"
  (let* ((insts (list (make-vm-jump-zero :reg :cond :label "else")
                      (make-vm-const :dst :fn :value 42)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-label :name "join")
                      (make-vm-call :dst :out :func :fn :args nil)))
         (out (cl-cc/optimize:opt-pass-call-site-splitting insts)))
    (expect (mapcar #'instruction->sexp out) :to-equal (mapcar #'instruction->sexp insts))))

(it-sequential "opt-adaptive-inline-threshold-uses-profile-and-size-hints"
  (let* ((ci (make-vm-closure :dst :r0 :label "profiled"
                              :params '(:r1) :captured nil
                              :optional-params nil :rest-param nil :key-params nil))
         (body (append (loop repeat 10 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (def (list :closure ci :params '(:r1) :body body))
         (cold (cl-cc/optimize::opt-adaptive-inline-threshold def :call-count 0 :loop-depth 0 :max-threshold 100))
         (hot (cl-cc/optimize::opt-adaptive-inline-threshold def :call-count 100 :loop-depth 2 :max-threshold 100))
         (large (cl-cc/optimize::opt-adaptive-inline-threshold def :call-count 0 :loop-depth 0 :function-size 80 :max-threshold 100)))
    (expect (> hot cold) :to-be-truthy)
    (expect (< large cold) :to-be-truthy)))

(it-sequential "opt-pass-call-site-splitting-handles-multi-join-labels"
  (let* ((insts (list (make-vm-func-ref :dst :fn :label "left-fn")
                      (make-vm-jump :label "join-a")
                      (make-vm-label :name "right")
                      (make-vm-func-ref :dst :fn :label "right-fn")
                      (make-vm-jump :label "join-b")
                      (make-vm-label :name "join-a")
                      (make-vm-label :name "join-b")
                      (make-vm-call :dst :out :func :fn :args '(:arg))
                      (make-vm-halt :reg :out)))
         (out (cl-cc/optimize:opt-pass-call-site-splitting insts))
         (calls (remove-if-not #'cl-cc:vm-call-p out)))
    (expect (= 3 (length calls)) :to-be-truthy)))

(it-sequential "opt-pass-call-site-splitting-handles-vm-apply"
  (let* ((insts (list (make-vm-func-ref :dst :fn :label "apply-fn")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (cl-cc:make-vm-apply :dst :out :func :fn :args '(:head :rest))
                      (make-vm-halt :reg :out)))
         (out (cl-cc/optimize:opt-pass-call-site-splitting insts))
         (applies (remove-if-not (lambda (inst) (typep inst 'cl-cc/vm::vm-apply)) out)))
    (expect (= 2 (length applies)) :to-be-truthy)))

(it-sequential "opt-pass-call-site-splitting-handles-vm-tail-call"
  (let* ((insts (list (make-vm-func-ref :dst :fn :label "tail-fn")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (cl-cc:make-vm-tail-call :dst :out :func :fn :args '(:arg))
                      (make-vm-halt :reg :out)))
         (out (cl-cc/optimize:opt-pass-call-site-splitting insts))
         (tail-calls (remove-if-not (lambda (inst)
                                      (typep inst 'cl-cc/vm::vm-tail-call))
                                    out))
         (after-jump (find-if (lambda (inst)
                                (and (typep inst 'cl-cc/vm::vm-jump)
                                     (search "CALL-SITE-SPLIT-AFTER-"
                                             (cl-cc:vm-label-name inst))))
                              out)))
    (expect (= 2 (length tail-calls)) :to-be-truthy)
    (expect after-jump :to-be-truthy)
    (expect (find-if (lambda (inst)
                            (and (typep inst 'cl-cc/vm::vm-label)
                                 (equal (cl-cc:vm-label-name after-jump)
                                        (cl-cc/vm::vm-name inst))))
                          out) :to-be-truthy)))

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

(it-sequential "opt-can-safely-rename-p-cases simple"
  (destructuring-bind (insts) (list (list (make-vm-move :dst :R0 :src :R1) (make-vm-ret :reg :R0)))
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-truthy)))

(it-sequential "opt-can-safely-rename-p-cases empty"
  (destructuring-bind (insts) (list nil)
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-truthy)))

;;; ─── opt-rename-regs-in-inst ─────────────────────────────────────────────────

(it-sequential "opt-rename-regs-in-inst-behavior"
  (let* ((inst     (make-vm-move :dst :R0 :src :R1))
         (renaming (let ((ht (make-hash-table :test #'eq)))
                     (setf (gethash :R0 ht) :R10 (gethash :R1 ht) :R11) ht))
         (renamed  (cl-cc/optimize::opt-rename-regs-in-inst inst renaming)))
    (expect (cl-cc:vm-dst renamed) :to-be :R10)
    (expect (cl-cc:vm-src renamed) :to-be :R11))
  (let* ((renamed (cl-cc/optimize::opt-rename-regs-in-inst
                   (make-vm-move :dst :R0 :src :R1)
                   (make-hash-table :test #'eq))))
    (expect (cl-cc:vm-dst renamed) :to-be :R0)
    (expect (cl-cc:vm-src renamed) :to-be :R1)))

;;; ─── %opt-collect-sexp-regs-into-cell ───────────────────────────────────────

(it-sequential "opt-collect-sexp-regs-cases empty-atom"
  (destructuring-bind (form expected-set) (list 42 nil)
    (let ((cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell form cell)
    (let ((regs (car cell)))
      (expect (= (length expected-set) (length regs)) :to-be-truthy)
      (dolist (r expected-set)
        (expect (member r regs) :to-be-truthy))))))

(it-sequential "opt-collect-sexp-regs-cases register-kw"
  (destructuring-bind (form expected-set) (list :r0 '(:r0))
    (let ((cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell form cell)
    (let ((regs (car cell)))
      (expect (= (length expected-set) (length regs)) :to-be-truthy)
      (dolist (r expected-set)
        (expect (member r regs) :to-be-truthy))))))

(it-sequential "opt-collect-sexp-regs-cases non-reg-kw"
  (destructuring-bind (form expected-set) (list :not-a-reg nil)
    (let ((cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell form cell)
    (let ((regs (car cell)))
      (expect (= (length expected-set) (length regs)) :to-be-truthy)
      (dolist (r expected-set)
        (expect (member r regs) :to-be-truthy))))))

(it-sequential "opt-collect-sexp-regs-cases nested-list"
  (destructuring-bind (form expected-set) (list '(:r0 :r1 (:r2)) '(:r2 :r1 :r0))
    (let ((cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell form cell)
    (let ((regs (car cell)))
      (expect (= (length expected-set) (length regs)) :to-be-truthy)
      (dolist (r expected-set)
        (expect (member r regs) :to-be-truthy))))))

(it-sequential "opt-collect-sexp-regs-registers-from-inst-sexp"
  (let* ((inst (make-vm-move :dst :r0 :src :r1))
         (sexp (cl-cc/optimize::instruction->sexp inst))
         (cell (list nil)))
    (cl-cc/optimize::%opt-collect-sexp-regs-into-cell sexp cell)
    (let ((regs (car cell)))
      (expect (member :r0 regs) :to-be-truthy)
      (expect (member :r1 regs) :to-be-truthy))))

;;; ─── opt-can-safely-rename-p ─────────────────────────────────────────────────

(it-sequential "opt-can-safely-rename-p-cases const-trivially-safe"
  (destructuring-bind (insts expected) (list (list (make-vm-const :dst :r0 :value 42)
                                        (make-vm-ret   :reg :r0)) t)
    (assert-inline-boolean-case expected
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-truthy)
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-falsy))))

(it-sequential "opt-can-safely-rename-p-cases move-safe"
  (destructuring-bind (insts expected) (list (list (make-vm-move :dst :r0 :src :r1)
                                        (make-vm-ret  :reg :r0)) t)
    (assert-inline-boolean-case expected
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-truthy)
    (expect (cl-cc/optimize::opt-can-safely-rename-p insts) :to-be-falsy))))

;;; ─── opt-body-has-global-refs-p ──────────────────────────────────────────────

(it-sequential "opt-body-has-global-refs-p-cases no-globals"
  (destructuring-bind (insts params expected) (list (list (make-vm-add :dst :R2 :lhs :R0 :rhs :R1) (make-vm-ret :reg :R2)) '(:R0 :R1) nil)
    (assert-inline-boolean-case expected
    (expect (cl-cc/optimize::opt-body-has-global-refs-p insts params) :to-be-truthy)
    (expect (cl-cc/optimize::opt-body-has-global-refs-p insts params) :to-be-null))))

(it-sequential "opt-body-has-global-refs-p-cases detects"
  (destructuring-bind (insts params expected) (list (list (make-vm-move :dst :R2 :src :R99) (make-vm-ret :reg :R2)) '(:R0) t)
    (assert-inline-boolean-case expected
    (expect (cl-cc/optimize::opt-body-has-global-refs-p insts params) :to-be-truthy)
    (expect (cl-cc/optimize::opt-body-has-global-refs-p insts params) :to-be-null))))
