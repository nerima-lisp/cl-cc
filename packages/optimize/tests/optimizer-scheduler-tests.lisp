;;;; packages/optimize/tests/optimizer-scheduler-tests.lisp
;;;; Unit tests for the scheduling passes in packages/optimize/src/optimizer-pipeline.lisp
;;;;
;;;; Covers:
;;;;   %opt-scheduler-barrier-p  — returns T for each barrier type, NIL for pure
;;;;   opt-pass-schedule-local   — reorders instructions in a RAW-dependency chain,
;;;;                               preserves barriers in original relative position
;;;;   schedule-pre-ra           — pressure-aware scheduling produces valid orderings

(in-package :cl-cc/test)

;;; ─── Test helpers ─────────────────────────────────────────────────────────

(defun %sched-index (instructions instruction)
  "Return the 0-based position of INSTRUCTION in INSTRUCTIONS by EQ."
  (position instruction instructions :test #'eq))

;;; ─── %opt-scheduler-barrier-p ─────────────────────────────────────────────

(it-sequential "scheduler-barrier-p-returns-true-for-barrier-types vm-call"
  (destructuring-bind (inst) (list (make-vm-call :dst :r0 :func :f :args nil))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-truthy)))

(it-sequential "scheduler-barrier-p-returns-true-for-barrier-types vm-apply"
  (destructuring-bind (inst) (list (cl-cc:make-vm-apply :dst :r0 :func :f :args '(:r1)))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-truthy)))

(it-sequential "scheduler-barrier-p-returns-true-for-barrier-types vm-set-global"
  (destructuring-bind (inst) (list (make-vm-set-global :src :r0 :name 'x))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-truthy)))

(it-sequential "scheduler-barrier-p-returns-true-for-barrier-types vm-signal-error"
  (destructuring-bind (inst) (list (cl-cc:make-vm-signal-error :error-reg :r0))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-truthy)))

(it-sequential "scheduler-barrier-p-returns-true-for-barrier-types vm-slot-write"
  (destructuring-bind (inst) (list (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r0))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-truthy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-const"
  (destructuring-bind (inst) (list (make-vm-const :dst :r0 :value 42))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-move"
  (destructuring-bind (inst) (list (make-vm-move :dst :r1 :src :r0))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-add"
  (destructuring-bind (inst) (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-mul"
  (destructuring-bind (inst) (list (make-vm-mul :dst :r3 :lhs :r0 :rhs :r1))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-sub"
  (destructuring-bind (inst) (list (make-vm-sub :dst :r4 :lhs :r2 :rhs :r1))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

(it-sequential "scheduler-barrier-p-returns-false-for-pure-instructions vm-get-global"
  (destructuring-bind (inst) (list (make-vm-get-global :dst :r5 :name 'x))
    (expect (cl-cc/optimize::%opt-scheduler-barrier-p inst) :to-be-falsy)))

;;; ─── opt-pass-schedule-local: basic reordering ────────────────────────────

(it-sequential "schedule-local-raw-chain-produces-valid-topological-order"
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (c2  (make-vm-const :dst :r1 :value 2))
         (add (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c1 c2 add))))
    ;; ADD must follow both CONSTs
    (let ((idx-c1  (%sched-index result c1))
          (idx-c2  (%sched-index result c2))
          (idx-add (%sched-index result add)))
      (expect (and idx-c1 idx-c2 idx-add) :to-be-truthy)
      (expect (< idx-c1 idx-add) :to-be-truthy)
      (expect (< idx-c2 idx-add) :to-be-truthy))))

(it-sequential "schedule-local-independent-instructions-all-preserved"
  (let* ((c0 (make-vm-const :dst :r0 :value 10))
         (c1 (make-vm-const :dst :r1 :value 20))
         (c2 (make-vm-const :dst :r2 :value 30))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 c1 c2))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (member c0 result :test #'eq) :to-be-truthy)
    (expect (member c1 result :test #'eq) :to-be-truthy)
    (expect (member c2 result :test #'eq) :to-be-truthy)))

(it-sequential "schedule-local-deep-raw-chain-preserves-order"
  (let* ((c0   (make-vm-const :dst :r0 :value 1))
         (add1 (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0))
         (add2 (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 add1 add2))))
    (expect (= 3 (length result)) :to-be-truthy)
    (let ((idx-c0   (%sched-index result c0))
          (idx-add1 (%sched-index result add1))
          (idx-add2 (%sched-index result add2)))
      (expect (< idx-c0 idx-add1) :to-be-truthy)
      (expect (< idx-add1 idx-add2) :to-be-truthy))))

;;; ─── opt-pass-schedule-local: barriers stop scheduling ────────────────────

(it-sequential "schedule-local-call-barrier-splits-run"
  (let* ((c0   (make-vm-const :dst :r0 :value 1))
         (call (make-vm-call  :dst :r1 :func :f :args nil))
         (add  (make-vm-add   :dst :r2 :lhs :r1 :rhs :r0))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 call add))))
    (expect (= 3 (length result)) :to-be-truthy)
    ;; call must appear between c0 and add in the output
    (let ((idx-c0   (%sched-index result c0))
          (idx-call (%sched-index result call))
          (idx-add  (%sched-index result add)))
      (expect (and idx-c0 idx-call idx-add) :to-be-truthy)
      (expect (< idx-c0 idx-call) :to-be-truthy)
      (expect (< idx-call idx-add) :to-be-truthy))))

(it-sequential "schedule-local-slot-write-barrier-preserved"
  (let* ((c0    (make-vm-const :dst :r0 :value 99))
         (write (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r0))
         (c1    (make-vm-const :dst :r1 :value 1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 write c1))))
    (expect (= 3 (length result)) :to-be-truthy)
    (let ((idx-write (%sched-index result write)))
      (expect idx-write :to-be-truthy)
      ;; c0 must stay before the write
      (expect (< (%sched-index result c0) idx-write) :to-be-truthy)
      ;; c1 must stay after the write
      (expect (< idx-write (%sched-index result c1)) :to-be-truthy))))

(it-sequential "schedule-local-multiple-barriers-each-preserved"
  (let* ((call1 (make-vm-call :dst :r0 :func :f1 :args nil))
         (call2 (make-vm-call :dst :r1 :func :f2 :args nil))
         (call3 (make-vm-call :dst :r2 :func :f3 :args nil))
         (result (cl-cc/optimize::opt-pass-schedule-local (list call1 call2 call3))))
    (expect (= 3 (length result)) :to-be-truthy)
    (expect (< (%sched-index result call1) (%sched-index result call2)) :to-be-truthy)
    (expect (< (%sched-index result call2) (%sched-index result call3)) :to-be-truthy)))

(it-sequential "schedule-local-set-global-barrier-preserved"
  (let* ((c0    (make-vm-const :dst :r0 :value 5))
         (store (make-vm-set-global :src :r0 :name 'g))
         (c1    (make-vm-const :dst :r1 :value 6))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 store c1))))
    (expect (= 3 (length result)) :to-be-truthy)
    (let ((idx-store (%sched-index result store)))
      (expect (< (%sched-index result c0) idx-store) :to-be-truthy)
      (expect (< idx-store (%sched-index result c1)) :to-be-truthy))))

;;; ─── schedule-pre-ra: pressure-aware ordering ─────────────────────────────

(it-sequential "schedule-pre-ra-raw-chain-preserves-topological-order"
  (let* ((c0  (make-vm-const :dst :r0 :value 1))
         (c1  (make-vm-const :dst :r1 :value 2))
         (add (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1))
         (result (cl-cc/optimize::schedule-pre-ra (list c0 c1 add))))
    (expect (= 3 (length result)) :to-be-truthy)
    (let ((idx-c0  (%sched-index result c0))
          (idx-c1  (%sched-index result c1))
          (idx-add (%sched-index result add)))
      (expect (< idx-c0 idx-add) :to-be-truthy)
      (expect (< idx-c1 idx-add) :to-be-truthy))))

(it-sequential "schedule-pre-ra-all-instructions-emitted"
  (let* ((c0  (make-vm-const :dst :r0 :value 10))
         (c1  (make-vm-const :dst :r1 :value 20))
         (mul (make-vm-mul   :dst :r2 :lhs :r0 :rhs :r1))
         (add (make-vm-add   :dst :r3 :lhs :r2 :rhs :r0))
         (all (list c0 c1 mul add))
         (result (cl-cc/optimize::schedule-pre-ra all)))
    (expect (= 4 (length result)) :to-be-truthy)
    (dolist (inst all)
      (expect (member inst result :test #'eq) :to-be-truthy))))

(it-sequential "schedule-pre-ra-call-barrier-not-moved"
  (let* ((c0   (make-vm-const :dst :r0 :value 7))
         (call (make-vm-call  :dst :r1 :func :g :args nil))
         (add  (make-vm-add   :dst :r2 :lhs :r1 :rhs :r0))
         (result (cl-cc/optimize::schedule-pre-ra (list c0 call add))))
    (expect (= 3 (length result)) :to-be-truthy)
    (let ((idx-call (%sched-index result call)))
      (expect (< (%sched-index result c0) idx-call) :to-be-truthy)
      (expect (< idx-call (%sched-index result add)) :to-be-truthy))))

(it-sequential "schedule-pre-ra-single-instruction-unchanged"
  (let* ((c0     (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::schedule-pre-ra (list c0))))
    (expect (= 1 (length result)) :to-be-truthy)
    (expect (first result) :to-be c0)))

(it-sequential "schedule-pre-ra-empty-input-returns-empty"
  (let ((result (cl-cc/optimize::schedule-pre-ra nil)))
    (expect (= 0 (length result)) :to-be-truthy)))
