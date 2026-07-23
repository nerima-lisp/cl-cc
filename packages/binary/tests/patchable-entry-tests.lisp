;;;; packages/binary/tests/patchable-entry-tests.lisp — Patchable entry tests
;;;;
;;;; Tests for: emit-nop-sequence, emit-patchable-function-entry,
;;;; and with-patchable-entries (patchable-entry.lisp / FR-584).

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; Helper
;;; ------------------------------------------------------------

(defun %nop-bytes (count)
  "Return emitted NOP bytes for COUNT as a list."
  (coerce (cl-cc/binary::with-output-to-vector (stream)
            (cl-cc/binary::emit-nop-sequence stream count))
          'list))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct byte count
;;; ------------------------------------------------------------

(it-sequential "emit-nop-sequence-correct-total-length count-1"
  (destructuring-bind (count) (list 1)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-2"
  (destructuring-bind (count) (list 2)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-3"
  (destructuring-bind (count) (list 3)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-4"
  (destructuring-bind (count) (list 4)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-5"
  (destructuring-bind (count) (list 5)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-6"
  (destructuring-bind (count) (list 6)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-7"
  (destructuring-bind (count) (list 7)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-8"
  (destructuring-bind (count) (list 8)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-9"
  (destructuring-bind (count) (list 9)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-10"
  (destructuring-bind (count) (list 10)
    (expect (length (%nop-bytes count)) :to-equal count)))

(it-sequential "emit-nop-sequence-correct-total-length count-18"
  (destructuring-bind (count) (list 18)
    (expect (length (%nop-bytes count)) :to-equal count)))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — correct encoding for each single NOP
;;; ------------------------------------------------------------

(it-sequential "emit-nop-sequence-exact-bytes 1"
  (destructuring-bind (count expected) (list 1 '(#x90))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 2"
  (destructuring-bind (count expected) (list 2 '(#x66 #x90))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 3"
  (destructuring-bind (count expected) (list 3 '(#x0F #x1F #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 4"
  (destructuring-bind (count expected) (list 4 '(#x0F #x1F #x40 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 5"
  (destructuring-bind (count expected) (list 5 '(#x0F #x1F #x44 #x00 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 6"
  (destructuring-bind (count expected) (list 6 '(#x66 #x0F #x1F #x44 #x00 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 7"
  (destructuring-bind (count expected) (list 7 '(#x0F #x1F #x80 #x00 #x00 #x00 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 8"
  (destructuring-bind (count expected) (list 8 '(#x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

(it-sequential "emit-nop-sequence-exact-bytes 9"
  (destructuring-bind (count expected) (list 9 '(#x66 #x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00))
    (expect (%nop-bytes count) :to-equal expected)))

;;; ------------------------------------------------------------
;;; emit-nop-sequence — multi-chunk decomposition
;;; ------------------------------------------------------------

(it-sequential "emit-nop-sequence-10-decomposes-9-plus-1"
  (let ((bytes (%nop-bytes 10)))
    (expect (length bytes) :to-equal 10)
    ;; First 9 bytes: the 9-byte NOP prefix
    (expect (subseq bytes 0 9) :to-equal '(#x66 #x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00))
    ;; Last byte: 1-byte NOP
    (expect (subseq bytes 9 10) :to-equal '(#x90))))

(it-sequential "emit-nop-sequence-0-emits-nothing"
  (expect (%nop-bytes 0) :to-equal '()))

;;; ------------------------------------------------------------
;;; emit-patchable-function-entry — non-zero before/after
;;; ------------------------------------------------------------

(it-sequential "emit-patchable-function-entry-before-only"
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 3)
                        (cl-cc/binary::*patchable-entry-after* 0))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (expect (length bytes) :to-equal 3)
    (expect bytes :to-equal '(#x0F #x1F #x00))))

(it-sequential "emit-patchable-function-entry-after-only"
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 0)
                        (cl-cc/binary::*patchable-entry-after* 2))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (expect (length bytes) :to-equal 2)
    (expect bytes :to-equal '(#x66 #x90))))

(it-sequential "emit-patchable-function-entry-before-and-after"
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 5)
                        (cl-cc/binary::*patchable-entry-after* 3))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (expect (length bytes) :to-equal 8)
    ;; First 5 bytes: 5-byte NOP
    (expect (subseq bytes 0 5) :to-equal '(#x0F #x1F #x44 #x00 #x00))
    ;; Next 3 bytes: 3-byte NOP
    (expect (subseq bytes 5 8) :to-equal '(#x0F #x1F #x00))))

(it-sequential "emit-patchable-function-entry-both-zero"
  (let ((bytes (coerce
                (cl-cc/binary::with-output-to-vector (stream)
                  (let ((cl-cc/binary::*patchable-entry-before* 0)
                        (cl-cc/binary::*patchable-entry-after* 0))
                    (cl-cc/binary::emit-patchable-function-entry stream)))
                'list)))
    (expect bytes :to-equal '())))

;;; ------------------------------------------------------------
;;; with-patchable-entries — dynamic binding
;;; ------------------------------------------------------------

(it-sequential "with-patchable-entries-binds-before-and-after"
  (cl-cc/binary::with-patchable-entries (:before 4 :after 2)
    (expect cl-cc/binary::*patchable-entry-before* :to-equal 4)
    (expect cl-cc/binary::*patchable-entry-after* :to-equal 2)))

(it-sequential "with-patchable-entries-restores-after-body"
  (let ((before-saved cl-cc/binary::*patchable-entry-before*)
        (after-saved  cl-cc/binary::*patchable-entry-after*))
    (cl-cc/binary::with-patchable-entries (:before 7 :after 5)
      (expect cl-cc/binary::*patchable-entry-before* :to-equal 7))
    (expect cl-cc/binary::*patchable-entry-before* :to-equal before-saved)
    (expect cl-cc/binary::*patchable-entry-after* :to-equal after-saved)))

;;; ------------------------------------------------------------
;;; patch-function-entry — size guard
;;; ------------------------------------------------------------

(it-sequential "patch-function-entry-rejects-oversized-patch"
  (let ((cl-cc/binary::*patchable-entry-before* 2))
    (let ((%%signaled1 nil)) (handler-case (progn (cl-cc/binary::patch-function-entry 0 #(#x90 #x90 #x90))) (error () (setf %%signaled1 t))) (expect %%signaled1 :to-be-truthy))))
