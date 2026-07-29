;;;; tests/unit/optimize/optimizer-strength-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength.lisp
;;;;
;;;; Covers: opt-power-of-2-p, opt-pass-strength-reduce,
;;;;   opt-pass-bswap-recognition (pass-through), opt-pass-rotate-recognition.

(in-package :cl-cc/test)

;;; ─── opt-power-of-2-p ────────────────────────────────────────────────────

(it-sequential "opt-power-of-2-p-true-for-powers-of-two two"
  (destructuring-bind (n) (list 2)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-truthy)))

(it-sequential "opt-power-of-2-p-true-for-powers-of-two four"
  (destructuring-bind (n) (list 4)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-truthy)))

(it-sequential "opt-power-of-2-p-true-for-powers-of-two eight"
  (destructuring-bind (n) (list 8)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-truthy)))

(it-sequential "opt-power-of-2-p-true-for-powers-of-two sixteen"
  (destructuring-bind (n) (list 16)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-truthy)))

(it-sequential "opt-power-of-2-p-true-for-powers-of-two two-fifty-six"
  (destructuring-bind (n) (list 256)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-truthy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers zero"
  (destructuring-bind (n) (list 0)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers one"
  (destructuring-bind (n) (list 1)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers three"
  (destructuring-bind (n) (list 3)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers six"
  (destructuring-bind (n) (list 6)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers seven"
  (destructuring-bind (n) (list 7)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers negative"
  (destructuring-bind (n) (list -4)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

(it-sequential "opt-power-of-2-p-false-for-non-powers float"
  (destructuring-bind (n) (list 4.0)
    (expect (cl-cc/optimize::opt-power-of-2-p n) :to-be-falsy)))

;;; ─── opt-pass-strength-reduce — multiply by power of 2 ──────────────────

(it-sequential "strength-reduce-mul-by-power-of-2-emits-ash power-on-rhs"
  (destructuring-bind (value lhs rhs) (list 4 :x :r0)
    (let* ((const-n (make-vm-const :dst :r0 :value value))
         (mul     (make-vm-mul   :dst :r1 :lhs lhs :rhs rhs))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-n mul))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy))))

(it-sequential "strength-reduce-mul-by-power-of-2-emits-ash power-on-lhs"
  (destructuring-bind (value lhs rhs) (list 8 :r0 :x)
    (let* ((const-n (make-vm-const :dst :r0 :value value))
         (mul     (make-vm-mul   :dst :r1 :lhs lhs :rhs rhs))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-n mul))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy))))

(it-sequential "strength-reduce-div-by-power-of-2-emits-ash"
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-8 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)
    ;; The shift count must be negative (right shift)
    (let ((ash-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result)))
      (when ash-inst
        (let ((shift-dst (cl-cc/vm::vm-rhs ash-inst)))
          (let ((const-inst (find-if (lambda (i)
                                       (and (typep i 'cl-cc/vm::vm-const)
                                            (eq (cl-cc/vm::vm-dst i) shift-dst)))
                                     result)))
            (when const-inst
              (expect (minusp (cl-cc/vm::vm-value const-inst)) :to-be-truthy))))))))

;;; ─── FR-282: Division by constant — targeted tests ─────────────────────────

(it-sequential "fr-282-div-by-2-emits-ash-neg-1"
  (let* ((const-2 (make-vm-const :dst :r0 :value 2))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-2 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)
    ;; Shift count must be -1
    (let ((ash-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result)))
      (when ash-inst
        (let* ((shift-dst (cl-cc/vm::vm-rhs ash-inst))
               (const-inst (find-if (lambda (i)
                                      (and (typep i 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst i) shift-dst)))
                                    result)))
          (when const-inst
            (expect (= -1 (cl-cc/vm::vm-value const-inst)) :to-be-truthy)))))))

(it-sequential "fr-282-div-by-256-emits-ash-neg-8"
  (let* ((const-256 (make-vm-const :dst :r0 :value 256))
         (div       (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result    (cl-cc/optimize::opt-pass-strength-reduce (list const-256 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)
    (let ((ash-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result)))
      (when ash-inst
        (let* ((shift-dst (cl-cc/vm::vm-rhs ash-inst))
               (const-inst (find-if (lambda (i)
                                      (and (typep i 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst i) shift-dst)))
                                    result)))
          (when const-inst
            (expect (= -8 (cl-cc/vm::vm-value const-inst)) :to-be-truthy)))))))

(it-sequential "fr-282-div-by-3-bounded-nonnegative-dividend-emits-reciprocal-seq"
  (let* ((const-4 (make-vm-const :dst :r0 :value 4))
         (const-8 (make-vm-const :dst :r1 :value 8))
         (sum     (make-vm-add   :dst :x :lhs :r0 :rhs :r1))
         (const-3 (make-vm-const :dst :r2 :value 3))
         (div     (make-vm-div   :dst :r3 :lhs :x :rhs :r2))
         (result  (cl-cc/optimize::opt-pass-strength-reduce
                   (list const-4 const-8 sum const-3 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-7-bounded-nonnegative-dividend-emits-reciprocal-seq"
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (const-6 (make-vm-const :dst :r1 :value 6))
         (sum     (make-vm-add   :dst :x :lhs :r0 :rhs :r1))
         (const-7 (make-vm-const :dst :r2 :value 7))
         (div     (make-vm-div   :dst :r3 :lhs :x :rhs :r2))
         (result  (cl-cc/optimize::opt-pass-strength-reduce
                   (list const-8 const-6 sum const-7 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-3-unknown-dividend-not-transformed"
  (let* ((const-3 (make-vm-const :dst :r0 :value 3))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-3 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-falsy)))

(it-sequential "fr-282-div-by-7-unknown-dividend-not-transformed"
  (let* ((const-7 (make-vm-const :dst :r0 :value 7))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-7 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-falsy)))


(it-sequential "fr-282-div-by-3-word64-range-emits-unsigned-mul-high-seq"
  (let* ((mask    (make-vm-const  :dst :mask :value #xffffffffffffffff))
         (masked  (make-vm-logand :dst :x64 :lhs :x :rhs :mask))
         (const-3 (make-vm-const  :dst :d :value 3))
         (div     (make-vm-div    :dst :q :lhs :x64 :rhs :d))
         (result  (cl-cc/optimize::opt-pass-strength-reduce
                   (list mask masked const-3 div))))
    (flet ((has-inst-p (name)
             (some (lambda (i) (typep i (find-class (intern name "CL-CC/VM")))) result)))
      (expect (has-inst-p "VM-DIV") :to-be-falsy)
      (expect (has-inst-p "VM-INTEGER-MUL-HIGH-U") :to-be-truthy)
      (expect (has-inst-p "VM-ASH") :to-be-truthy)
      (expect (has-inst-p "VM-SUB") :to-be-falsy))))

(it-sequential "fr-282-div-by-7-word64-range-emits-add-adjusted-unsigned-mul-high-seq"
  (let* ((mask    (make-vm-const  :dst :mask :value #xffffffffffffffff))
         (masked  (make-vm-logand :dst :x64 :lhs :x :rhs :mask))
         (const-7 (make-vm-const  :dst :d :value 7))
         (div     (make-vm-div    :dst :q :lhs :x64 :rhs :d))
         (result  (cl-cc/optimize::opt-pass-strength-reduce
                   (list mask masked const-7 div))))
    (flet ((has-inst-p (name)
             (some (lambda (i) (typep i (find-class (intern name "CL-CC/VM")))) result)))
      (expect (has-inst-p "VM-DIV") :to-be-falsy)
      (expect (has-inst-p "VM-INTEGER-MUL-HIGH-U") :to-be-truthy)
      (expect (has-inst-p "VM-SUB") :to-be-truthy)
      (expect (has-inst-p "VM-ADD") :to-be-truthy)
      (expect (has-inst-p "VM-ASH") :to-be-truthy))))

(it-sequential "fr-282-div-by-3-negative-dividend-transformed-when-bounded"
  (let* ((const-neg (make-vm-const :dst :x :value -9))
          (const-3   (make-vm-const :dst :r0 :value 3))
          (div       (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
          (result    (cl-cc/optimize::opt-pass-strength-reduce
                      (list const-neg const-3 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-3-bounded-negative-dividend-emits-reciprocal-seq"
  (let* ((const-neg9 (make-vm-const :dst :r0 :value -9))
         (const-neg3 (make-vm-const :dst :r1 :value -3))
         (add        (make-vm-add   :dst :x :lhs :r0 :rhs :r1)) ; x=-12
         (const-3    (make-vm-const :dst :r2 :value 3))
         (div        (make-vm-div   :dst :r3 :lhs :x :rhs :r2))
         (result     (cl-cc/optimize::opt-pass-strength-reduce
                      (list const-neg9 const-neg3 add const-3 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-7-bounded-mixed-sign-dividend-emits-reciprocal-seq"
  (let* ((cneg4  (make-vm-const :dst :r0 :value -4))
         (cpos9  (make-vm-const :dst :r1 :value 9))
         (add    (make-vm-add   :dst :x :lhs :r0 :rhs :r1)) ; x=5 (bounded mixed-source)
         (c7     (make-vm-const :dst :r2 :value 7))
         (div    (make-vm-div   :dst :r3 :lhs :x :rhs :r2))
         (result (cl-cc/optimize::opt-pass-strength-reduce
                  (list cneg4 cpos9 add c7 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-0-not-transformed"
  (let* ((const-0 (make-vm-const :dst :r0 :value 0))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-0 div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)))

(it-sequential "fr-282-div-by-negative-not-transformed"
  (let* ((const-neg (make-vm-const :dst :r0 :value -4))
         (div       (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result    (cl-cc/optimize::opt-pass-strength-reduce (list const-neg div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)))

(it-sequential "fr-282-div-non-constant-rhs-not-transformed"
  (let* ((div    (make-vm-div :dst :r1 :lhs :x :rhs :y))
         (result (cl-cc/optimize::opt-pass-strength-reduce (list div))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result) :to-be-truthy)))

;;; ─── mod by power of 2 (existing FR-302 coverage) ─────────────────────────

(it-sequential "strength-reduce-mod-by-power-of-2-emits-logand"
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (mod     (make-vm-mod   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-8 mod))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mod)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) result) :to-be-truthy)
    ;; The mask constant should be 7 (= 8 - 1)
    (let ((mask-inst (find-if (lambda (i)
                                (and (typep i 'cl-cc/vm::vm-const)
                                     (= (cl-cc/vm::vm-value i) 7)))
                              result)))
      (expect mask-inst :to-be-truthy))))

(it-sequential "strength-reduce-non-power-of-2-mul-passthrough"
  (let* ((const-7 (make-vm-const :dst :r0 :value 7))
         (mul     (make-vm-mul   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-7 mul))))
    ;; 7 has 3 bits set — logcount(7)=3 > 2, so no decomposition
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)))

(it-sequential "strength-reduce-label-clears-constant-env"
  (let* ((const-4 (make-vm-const :dst :r0 :value 4))
         (lbl     (make-vm-label :name "top"))
         (mul     (make-vm-mul   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-4 lbl mul))))
    ;; After label, constant env is cleared so mul should pass through unchanged
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result) :to-be-truthy)))

;;; ─── opt-pass-bswap-recognition ──────────────────────────────────────────

(it-sequential "bswap-recognition-passes-through-non-bswap"
  (let* ((const (make-vm-const :dst :r0 :value 42))
         (move  (make-vm-move  :dst :r1 :src :r0))
         (result (cl-cc/optimize::opt-pass-bswap-recognition (list const move))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (typep (first result)  'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (typep (second result) 'cl-cc/vm::vm-move) :to-be-truthy)))

;;; ─── opt-pass-rotate-recognition ─────────────────────────────────────────

(it-sequential "rotate-recognition-passes-through-non-rotate"
  (let* ((const (make-vm-const :dst :r0 :value 10))
         (add   (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0))
         (result (cl-cc/optimize::opt-pass-rotate-recognition (list const add))))
    (expect (= 2 (length result)) :to-be-truthy)
    (expect (typep (first result)  'cl-cc/vm::vm-const) :to-be-truthy)
    (expect (typep (second result) 'cl-cc/vm::vm-add) :to-be-truthy)))

(it-sequential "rotate-recognition-collapses-rotate-idiom"
  (let* ((k0  (make-vm-const :dst :rc0 :value 8))
         (a0  (make-vm-ash   :dst :ra0 :lhs :x :rhs :rc0))
         (k1  (make-vm-const :dst :rc1 :value -56))
         (a1  (make-vm-ash   :dst :ra1 :lhs :x :rhs :rc1))
         (or0 (make-vm-logior :dst :rout :lhs :ra0 :rhs :ra1))
         (result (cl-cc/optimize::opt-pass-rotate-recognition (list k0 a0 k1 a1 or0))))
    ;; Should collapse to two instructions: a vm-const + vm-rotate
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-rotate)) result) :to-be-truthy)
    ;; vm-logior should not appear in the output
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) result) :to-be-falsy)))

(it-sequential "rotate-recognition-skips-out-of-range-shift-constants"
  (let* ((k0  (make-vm-const :dst :rc0 :value 64))
         (a0  (make-vm-ash   :dst :ra0 :lhs :x :rhs :rc0))
         (k1  (make-vm-const :dst :rc1 :value 0))
         (a1  (make-vm-ash   :dst :ra1 :lhs :x :rhs :rc1))
         (or0 (make-vm-logior :dst :rout :lhs :ra0 :rhs :ra1))
         (result (cl-cc/optimize::opt-pass-rotate-recognition (list k0 a0 k1 a1 or0))))
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-rotate)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) result) :to-be-truthy)))

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
  (eq (type-of inst) 'cl-cc/vm::vm-fill))

(it-sequential "fill-recognition-collapses-canonical-fill-loop"
  (let ((result (cl-cc/optimize::opt-pass-fill-recognition (make-fill-loop-instructions))))
    (expect (= 1 (count-if #'fill-inst-p result)) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) result) :to-be-falsy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-move)
                              (eq (cl-cc/vm::vm-dst i) :ri)
                              (eq (cl-cc/vm::vm-src i) :rlen)))
                       result) :to-be-truthy)))

(it-sequential "fill-recognition-skips-externally-targeted-exit"
  (let ((result (cl-cc/optimize::opt-pass-fill-recognition
                 (make-fill-loop-instructions :extra-exit-jump t))))
    (expect (some #'fill-inst-p result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result) :to-be-truthy)))

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
                  (cl-cc/vm::make-vm-aref :dst :rtmp :array-reg src :index-reg :ri)
                  (make-vm-aset :array-reg :rdst :index-reg :ri :val-reg :rtmp)
                  (make-vm-const :dst :rone :value 1)
                  (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
                  (make-vm-move :dst :ri :src :rnext)
                  (make-vm-jump :label "Lcopy")
                  (make-vm-label :name "Lcopy-exit")
                  (make-vm-ret :reg :rdst)))))

(defun copy-call-inst-p (inst)
  (and (typep inst 'cl-cc/vm::vm-call)
       (equal (cl-cc/vm::vm-args inst)
              '(:rdst :rsrc :rcond :rlen :rone :rlen))))

(it-sequential "copy-recognition-collapses-canonical-copy-loop"
  (let ((result (cl-cc/optimize::opt-pass-copy-recognition (make-copy-loop-instructions))))
    (expect (= 1 (count-if #'copy-call-inst-p result)) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aref)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) result) :to-be-falsy)
    (expect (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-move)
                              (eq (cl-cc/vm::vm-dst i) :ri)
                              (eq (cl-cc/vm::vm-src i) :rlen)))
                       result) :to-be-truthy)))

(it-sequential "copy-recognition-skips-unknown-alias-arrays"
  (let ((result (cl-cc/optimize::opt-pass-copy-recognition
                 (make-copy-loop-instructions :unknown-alias-p t))))
    (expect (some #'copy-call-inst-p result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aref)) result) :to-be-truthy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result) :to-be-truthy)))

(it-sequential "copy-recognition-skips-self-copy-loop"
  (let ((result (cl-cc/optimize::opt-pass-copy-recognition
                 (make-copy-loop-instructions :same-array-p t))))
    (expect (some #'copy-call-inst-p result) :to-be-falsy)
    (expect (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result) :to-be-truthy)))
