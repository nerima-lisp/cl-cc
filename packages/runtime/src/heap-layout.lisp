(in-package :cl-cc/runtime)

(defparameter *gc-tenuring-threshold* 3
  "Minor GC survival cycles before promotion to old generation.")

(defconstant +gc-card-summary-block-size+ 64
  "Number of cards represented by one card-summary entry.")

(defconstant +gc-card-size-words+ 64
  "Card size in words (512 bytes with 8-byte words).")

(defparameter *rt-free-list-size-class-bytes* #(8 16 32 64 128 256 512 1024)
  "FR-156 old-space segregated free-list size classes, in bytes.")

(defparameter *rt-free-list-size-class-words* #(1 2 4 8 16 32 64 128)
  "FR-156 old-space segregated free-list size classes, in heap words.")

(defparameter *rt-free-list-fallback-class-words* #(256 512 1024 2048 4096 8192 16384 32768)
  "Compatibility fallback classes for blocks larger than the FR-156 buckets.")

(defconstant +rt-free-list-bin-count+ 16
  "Number of old-space free-list bins: 8 FR-156 buckets plus 8 oversized fallbacks.")

(defconstant +rt-slab-page-words+ 256
  "Default slab page size in heap words for fixed-size object classes.")

(defparameter *rt-slab-size-classes*
  '((:cons . 3)
    (:symbol . 4)
    (:closure-small . 2)
    (:closure-4 . 4)
    (:array-min . 4))
  "Named fixed-size slab classes used by the runtime allocator.")

(defstruct (rt-slab (:constructor %make-rt-slab)
                    (:conc-name rt-slab-))
  "Fixed-size slab page metadata.  FREE-LIST contains available object addresses."
  (class-size 0 :type fixnum)
  (free-list nil :type list)
  (slab-base 0 :type fixnum)
  (slab-limit 0 :type fixnum))

;;; ------------------------------------------------------------
;;; Object Header Bit Layout Constants
;;; ------------------------------------------------------------

;;; FR-266 compressed object header (one 64-bit word):
;;;   [ type-tag:8 | gc-bits:4 | shape-id:20 | size:32 ]
;;;
;;; The RT-HEADER-* accessors expose those bitfields directly.
;;;
;;; MARK/GRAY are transient collector flags used by the mark-sweep
;;; implementation.  They live outside the compressed payload so the FR-266
;;; fields retain their exact widths; native runtimes can lower them to side
;;; metadata or use the 4 GC bits according to their collector encoding.

;;; Size field: bits 31..0
(defconstant +header-size-shift+    0)

(defconstant +header-size-mask+     #x00000000FFFFFFFF)

;;; Shape-id field: bits 51..32
(defconstant +header-shape-id-shift+ 32)

(defconstant +header-shape-field-mask+ #x000FFFFF00000000)

(defconstant +header-shape-id-mask+ #x000FFFFF00000000)

(defconstant +header-shape-id-max+ #xFFFFF)

;;; GC-bits field: bits 55..52
(defconstant +header-gc-bits-shift+ 52)

(defconstant +header-gc-bits-mask+  #x00F0000000000000)

;;; Type tag field: bits 63..56
(defconstant +header-tag-shift+     56)

(defconstant +header-tag-mask+      #xFF00000000000000)

;;; Age occupies bits 53..52 (2 bits, 0-3) within the GC-bits field.
;;; Mark (bit 55) and gray (bit 54) are outside the age sub-field.
(defconstant +header-age-shift+     +header-gc-bits-shift+)

(defconstant +header-age-mask+      #x0030000000000000)

;;; Mark/Gray bits live inside the GC-bits field (bits 55..52) so the entire
;;; header word always fits in (unsigned-byte 64).  Bit 55 = mark, bit 54 = gray,
;;; leaving bits 53..52 for the 2-bit age/survival counter.
(defconstant +header-mark-bit+      #x0080000000000000)

(defconstant +header-gray-bit+      #x0040000000000000)

;;; Forwarding pointers are represented as (cons :forwarded dest-addr)
;;; rather than bit-packed integers.  This avoids address-size limitations
;;; and is idiomatic for a Pure CL heap simulation.

;;; ------------------------------------------------------------
;;; Header Construction and Accessors
;;; ------------------------------------------------------------

(defun make-rt-header (size type-tag &key (gc-bits 0) (shape-id 0))
  "Construct a FR-266 compressed runtime object header.

SIZE is the object size in heap words, including the header word.  TYPE-TAG is
an 8-bit runtime type tag.  GC-BITS is the 4-bit collector field.  SHAPE-ID
embeds the FR-214 object-shape id in the header, replacing the need for a
per-object class pointer in compact object layouts."
  (check-type size (integer 0 #xffffffff))
  (check-type type-tag (integer 0 #xff))
  (check-type gc-bits (integer 0 #xf))
  (check-type shape-id (integer 0 #xfffff))
  (logior (ash (logand type-tag #xFF) +header-tag-shift+)
          (ash (logand gc-bits #xF) +header-gc-bits-shift+)
          (ash (logand shape-id +header-shape-id-max+) +header-shape-id-shift+)
          (logand size #xFFFFFFFF)))

(defun rt-header-size (header-word)
  "Extract the 32-bit object size field from compressed header HEADER-WORD."
  (logand header-word #xFFFFFFFF))

(defun rt-header-type-tag (header-word)
  "Extract the 8-bit runtime type tag from compressed header HEADER-WORD."
  (logand (ash header-word (- +header-tag-shift+)) #xFF))

(defun rt-header-gc-bits (header-word)
  "Extract the 4-bit GC field from compressed header HEADER-WORD."
  (logand (ash header-word (- +header-gc-bits-shift+)) #xF))

(defun rt-header-shape-id (header-word)
  "Extract the embedded 20-bit FR-214 shape id from compressed header HEADER-WORD."
  (logand (ash (logand header-word +header-shape-id-mask+) (- +header-shape-id-shift+))
          +header-shape-id-max+))

(defun rt-header-age (header-word)
  "Extract the age (0-3) from compressed header HEADER-WORD."
  (logand (rt-header-gc-bits header-word) #x3))

(defun rt-header-increment-age (header-word)
  "Return a new header with age incremented by 1, capped at 3."
  (let* ((current-age (rt-header-age header-word))
         (new-age (min 3 (1+ current-age))))
    (logior (logand header-word (lognot +header-age-mask+))
            (ash new-age +header-age-shift+))))

(defun header-marked-p (header-word)
  "Return true if the mark bit is set in header HEADER-WORD."
  (not (zerop (logand header-word +header-mark-bit+))))

(defun header-gray-p (header-word)
  "Return true if the gray bit is set in header HEADER-WORD."
  (not (zerop (logand header-word +header-gray-bit+))))

(defun header-forwarding-p (header-word)
  "Return true if HEADER-WORD represents a forwarding pointer (i.e. is a cons :forwarded)."
  (and (consp header-word) (eq (car header-word) :forwarded)))

(defun header-set-mark (header-word)
  "Return a new header with the mark bit set."
  (logior header-word +header-mark-bit+))

(defun header-clear-mark (header-word)
  "Return a new header with the mark bit cleared."
  (logand header-word (lognot +header-mark-bit+)))

(defun header-set-gray (header-word)
  "Return a new header with the gray bit set."
  (logior header-word +header-gray-bit+))

(defun header-clear-gray (header-word)
  "Return a new header with the gray bit cleared."
  (logand header-word (lognot +header-gray-bit+)))

(defun header-make-forwarding-ptr (dest-addr)
  "Create a forwarding pointer value for DEST-ADDR.
   This value is stored in slot 0 of the from-space object.
   Use header-forwarding-p to detect it and header-forwarding-ptr to extract."
  (cons :forwarded dest-addr))

(defun header-forwarding-ptr (header-word)
  "Extract the forwarding destination address from forwarding pointer HEADER-WORD.
   Only valid when (header-forwarding-p header-word) is true."
  (cdr header-word))
