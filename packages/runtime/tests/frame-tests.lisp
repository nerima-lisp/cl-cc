;;;; tests/unit/runtime/frame-tests.lisp - vm-frame and Frame Pool Tests
;;;
;;; Tests for the fixed-size register array, frame pool acquire/release,
;;; register read/write, and frame-reset.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------



;;; ------------------------------------------------------------
;;; Frame construction
;;; ------------------------------------------------------------

(it-sequential "frame-pool-acquire-produces-vm-frame"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (expect (cl-cc/runtime:vm-frame-p f) :to-be-truthy)))

(it-sequential "frame-pool-acquire-all-registers-are-nil"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (dotimes (i 256)
      (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f i)) :to-be-truthy))))

(it-sequential "frame-pool-acquire-sp-and-pc-are-zero"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (expect (= 0 (cl-cc/runtime:vm-frame-sp f)) :to-be-truthy)
    (expect (= 0 (cl-cc/runtime:vm-frame-pc f)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; Register read/write
;;; ------------------------------------------------------------

(it-sequential "frame-reg-set-and-get"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 0 (cl-cc/runtime:encode-fixnum 42))
    (expect (= 42 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f 0))) :to-be-truthy)))

(it-sequential "frame-reg-set-all-256"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (dotimes (i 256)
      (cl-cc/runtime:frame-reg-set f i (cl-cc/runtime:encode-fixnum i)))
    (dotimes (i 256)
      (expect (= i (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f i))) :to-be-truthy))))

(it-sequential "frame-reg-set-returns-value-written"
  (cl-cc/runtime:initialize-frame-pool)
  (let* ((f   (cl-cc/runtime:frame-pool-acquire))
         (val (cl-cc/runtime:encode-fixnum 7))
         (ret (cl-cc/runtime:frame-reg-set f 3 val)))
    (expect (= val ret) :to-be-truthy)))

(it-sequential "frame-reg-set-overwrites-correctly"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 5 (cl-cc/runtime:encode-fixnum 100))
    (cl-cc/runtime:frame-reg-set f 5 (cl-cc/runtime:encode-fixnum 200))
    (expect (= 200 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:frame-reg-get f 5))) :to-be-truthy)))

(it-sequential "frame-reg-get-unwritten-is-val-nil"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f 255)) :to-be-truthy)))

;;; ------------------------------------------------------------
;;; frame-reset
;;; ------------------------------------------------------------

(it-sequential "frame-reset-behavior"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    ;; fill registers, then reset
    (dotimes (i 256)
      (cl-cc/runtime:frame-reg-set f i (cl-cc/runtime:encode-fixnum i)))
    (setf (cl-cc/runtime:vm-frame-pc f) 99
          (cl-cc/runtime:vm-frame-sp f) 42)
    (let ((ret (cl-cc/runtime:frame-reset f)))
      ;; returns the frame
      (expect ret :to-equal f)
      ;; all registers cleared
      (dotimes (i 256)
        (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f i)) :to-be-truthy))
      ;; pc and sp zeroed
      (expect (= 0 (cl-cc/runtime:vm-frame-pc f)) :to-be-truthy)
      (expect (= 0 (cl-cc/runtime:vm-frame-sp f)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; frame-pool-release
;;; ------------------------------------------------------------

(it-sequential "frame-pool-release-clears-all-fields"
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (cl-cc/runtime:frame-reg-set f 10 (cl-cc/runtime:encode-fixnum 999))
    (cl-cc/runtime:frame-pool-release f)
    ;; Acquire a frame; it may be the same one just released.
    (let ((f2 (cl-cc/runtime:frame-pool-acquire)))
      (expect (= cl-cc/runtime:+val-nil+ (cl-cc/runtime:frame-reg-get f2 10)) :to-be-truthy)))
  (cl-cc/runtime:initialize-frame-pool)
  (let ((f (cl-cc/runtime:frame-pool-acquire)))
    (setf (cl-cc/runtime:vm-frame-pc f) 5
          (cl-cc/runtime:vm-frame-sp f) 3)
    (cl-cc/runtime:frame-pool-release f)
    (let ((f2 (cl-cc/runtime:frame-pool-acquire)))
      (expect (= 0 (cl-cc/runtime:vm-frame-pc f2)) :to-be-truthy)
      (expect (= 0 (cl-cc/runtime:vm-frame-sp f2)) :to-be-truthy))))

;;; ------------------------------------------------------------
;;; Frame register count constant
;;; ------------------------------------------------------------

(it-sequential "frame-register-count-is-256"
  (expect (= 256 cl-cc/runtime:+frame-register-count+) :to-be-truthy))

(it-sequential "frame-arg-range-within-caller-save"
  (expect (<= cl-cc/runtime:+frame-arg-start+
                   cl-cc/runtime:+frame-arg-end+
                   cl-cc/runtime:+frame-caller-save-end+) :to-be-truthy))

(it-sequential "frame-spill-start-above-callee-save"
  (expect (> cl-cc/runtime:+frame-spill-start+
                  cl-cc/runtime:+frame-callee-save-end+) :to-be-truthy))
