;;;; tests/unit/optimize/optimizer-strength-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength.lisp
;;;;
;;;; Covers: opt-power-of-2-p, opt-pass-strength-reduce,
;;;;   opt-pass-bswap-recognition (pass-through), opt-pass-rotate-recognition.

(in-package :cl-cc/test)

;;; ─── opt-power-of-2-p ────────────────────────────────────────────────────

;;; ─── opt-pass-strength-reduce — multiply by power of 2 ──────────────────

;;; ─── FR-282: Division by constant — targeted tests ─────────────────────────

;;; ─── mod by power of 2 (existing FR-302 coverage) ─────────────────────────

;;; ─── opt-pass-bswap-recognition ──────────────────────────────────────────

;;; ─── opt-pass-rotate-recognition ─────────────────────────────────────────

;;; ─── opt-pass-fill-recognition ───────────────────────────────────────────

(defun make-fill-loop-instructions (&key extra-exit-jump)
  (append (when extra-exit-jump
            (list (make-vm-jump :label "Lexit")))
          (list (make-vm-array-length :dst :rlen :src :rvec)
                (make-vm-const :dst :ri :value 0)
                (make-vm-label :name "Lfill")
                (make-vm-lt :dst :rcond :lhs :ri :rhs :rlen)
                (make-vm-jump-zero :reg :rcond :label "Lexit")
                (make-vm-aset :array-reg :rvec :index-reg :ri :val-reg :rval)
                (make-vm-const :dst :rone :value 1)
                (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
                (make-vm-move :dst :ri :src :rnext)
                (make-vm-jump :label "Lfill")
                (make-vm-label :name "Lexit")
                (make-vm-ret :reg :rvec))))

(defun fill-inst-p (inst)
  (eq (type-of inst) 'cl-cc/vm:vm-fill))

;;; ─── opt-pass-copy-recognition ───────────────────────────────────────────

(defun make-copy-loop-instructions (&key unknown-alias-p same-array-p)
  (let ((src (if same-array-p :rdst :rsrc)))
    (append (unless unknown-alias-p
              (list (make-vm-const :dst :rsize :value 4)
                    (make-vm-make-array :dst :rdst :size-reg :rsize)
                    (make-vm-make-array :dst :rsrc :size-reg :rsize)))
            (list (make-vm-array-length :dst :rlen :src src)
                  (make-vm-const :dst :ri :value 0)
                  (make-vm-label :name "Lcopy")
                  (make-vm-lt :dst :rcond :lhs :ri :rhs :rlen)
                  (make-vm-jump-zero :reg :rcond :label "Lcopy-exit")
                  (cl-cc/vm:make-vm-aref :dst :rtmp :array-reg src :index-reg :ri)
                  (make-vm-aset :array-reg :rdst :index-reg :ri :val-reg :rtmp)
                  (make-vm-const :dst :rone :value 1)
                  (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
                  (make-vm-move :dst :ri :src :rnext)
                  (make-vm-jump :label "Lcopy")
                  (make-vm-label :name "Lcopy-exit")
                  (make-vm-ret :reg :rdst)))))

(defun copy-call-inst-p (inst)
  (and (typep inst 'cl-cc/vm:vm-call)
       (equal (cl-cc/vm:vm-args inst)
              '(:rdst :rsrc :rcond :rlen :rone :rlen))))

