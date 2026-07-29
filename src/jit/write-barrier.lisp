;;;; src/jit/write-barrier.lisp — FR-552 Write Barrier Optimization
;;;; Card table + remembered set + SATB barrier implementation.
;;;; Generational GC write barrier optimization.
(in-package :cl-cc/jit)

;;; ──── Card Table ────
;; Heap divided into 512-byte cards. Writing to a card dirties it.
;; GC only scans dirty cards during minor collections.
(defconstant +card-size+ 512
  "Size of each card in the card table (bytes).")

(defvar *card-table* nil
  "Byte vector: card-table[i] ≠ 0 means card i is dirty.")

(defvar *heap-base* nil
  "Base address of the managed heap.")

(defvar *heap-size* nil
  "Size in bytes of the managed heap.")

(defvar *young-generation-start* nil)

(defvar *young-generation-end* nil)

(defvar *old-generation-start* nil)

(defvar *old-generation-end* nil)

(defun card-index (address)
  "Compute the card index for ADDRESS, rejecting unconfigured or out-of-heap addresses."
  (unless (and
      (integerp *heap-base*)
      (integerp *heap-size*)
      (plusp *heap-size*)
      *card-table*)
    (error "Card table is not initialized"))
  (unless (integerp address)
    (error "Heap address must be an integer: ~S" address))
  (unless (<= *heap-base* address (1- (+ *heap-base* *heap-size*)))
    (error
      "Address ~S is outside heap [~S, ~S)"
      address
      *heap-base*
      (+ *heap-base* *heap-size*)))
  (floor (- address *heap-base*) +card-size+))

(defun card-table-mark (address)
  "Mark the card containing ADDRESS as dirty."
  (setf (aref *card-table* (card-index address)) 1))

(defun card-table-clear (address)
  "Clear the card containing ADDRESS."
  (setf (aref *card-table* (card-index address)) 0))

(defun card-table-clear-all ()
  "Clear the entire card table (after major GC sweep)."
  (when *card-table*
    (fill *card-table* 0)))

(defun card-table-init (heap-base heap-size &key young-start young-end old-start old-end)
  "Configure heap and generation half-open ranges and initialize the card table."
  (unless (and (integerp heap-base) (integerp heap-size) (plusp heap-size))
    (error "Invalid heap range: base=~S size=~S" heap-base heap-size))
  (let ((heap-end (+ heap-base heap-size)))
    (flet ((valid-range-p (start end)
             (and (integerp start) (integerp end) (<= heap-base start end heap-end))))
      (unless (and (valid-range-p young-start young-end) (valid-range-p old-start old-end))
        (error "Generation ranges must be within heap [~S, ~S)" heap-base heap-end)))
    (setf *heap-base* heap-base
          *heap-size* heap-size
          *young-generation-start* young-start
          *young-generation-end* young-end
          *old-generation-start* old-start
          *old-generation-end* old-end
          *card-table* (make-array
        (ceiling heap-size +card-size+)
        :element-type
        (quote (unsigned-byte 8))
        :initial-element
        0))))

;;; ──── Write Barrier Emission ────
;; Emitted before every slot write (setf slot-value / setf aref).
;; Optimized: card table mark only when slot is in old generation
;; and value being stored is a young-generation reference.
(defvar *barrier-elision-enabled* t
  "When T, skip write barrier when compiler proves it unnecessary.")

(defun emit-write-barrier (stream slot-address-reg)
  "Refuse to emit a barrier until register encoding and code-address relocation are implemented."
  (declare (ignore stream slot-address-reg))
  (error
    "JIT write-barrier machine-code emission is unsupported: register encoding and relocation are unavailable"))

;;; ──── Barrier Elision ────
(defun barrier-elision-p (slot-address value-type)
  "Return T if the write barrier can be safely elided.
Conditions for elision:
- Compiling for young-generation allocation (no old→young references possible)
- Value being stored is definitely not a heap reference (fixnum, char, etc.)
- Slot is in a newly allocated object (still in nursery)"
  (and *barrier-elision-enabled*
       (or (member value-type '(fixnum character float))
           ;; Add more type-based elision rules
           nil)))

;;; ──── SATB (Snapshot-At-The-Beginning) Barrier ────
;; Used for concurrent GC: log old values before overwriting.
(defvar *satb-queue* nil
  "SATB marking queue for concurrent GC.")

(defvar *satb-enabled* nil
  "T when SATB concurrent marking is active.")

(defmacro with-satb-barrier (&body body)
  "Execute BODY with SATB concurrent marking barrier enabled."
  `(let ((*satb-enabled* t))
    ,@body))

(defun satb-enqueue (value)
  "Enqueue VALUE into the SATB marking queue (before overwriting)."
  (when (and *satb-enabled* *satb-queue*)
    (vector-push-extend value *satb-queue*)))

;;; ──── Remembered Set Barrier ────
;; Only track Old→Young references (Young→Old doesn't need tracking).
(defun %write-barrier-address (heap value)
  "Return the raw address designated by VALUE when it belongs to HEAP."
  (when (integerp value)
    (let ((address
          (if (cl-cc/runtime:val-pointer-p value) (cl-cc/runtime:decode-pointer value)
            value)))
      (and (cl-cc/runtime:rt-heap-addr-p heap address) address))))

(defun old-to-young-reference-p (heap slot-address value)
  "Return true if writing VALUE to SLOT-ADDRESS creates an old-to-young reference in HEAP."
  (and (in-old-generation-p heap slot-address) (in-young-generation-p heap value)))

(defun in-old-generation-p (heap value)
  "Return true when VALUE designates an address in the old space of HEAP."
  (let ((address (%write-barrier-address heap value)))
    (and address (cl-cc/runtime:rt-old-addr-p heap address))))

(defun in-young-generation-p (heap value)
  "Return true when VALUE designates an address in the active young space of HEAP."
  (let ((address (%write-barrier-address heap value)))
    (and address (cl-cc/runtime:rt-young-addr-p heap address))))

;;; ──── Helper ────
(defun encode-int32 (value)
  "Encode a 32-bit signed integer as 4 bytes (little-endian)."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref buf 0) (logand value #xFF)
          (aref buf 1) (logand (ash value -8) #xFF)
          (aref buf 2) (logand (ash value -16) #xFF)
          (aref buf 3) (logand (ash value -24) #xFF))
    buf))
