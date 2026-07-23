;;;; tests/unit/emit/macho-tests.lisp — Mach-O Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/macho.lisp:
;;;; Constants, structures, buffer helpers, serialization, builder API.

(in-package :cl-cc/test)



;;; ─── Constants ──────────────────────────────────────────────────────────

(it-sequential "macho-constants"
  (expect cl-cc/binary:+mh-magic-64+ :to-equal #xFEEDFACF)
  (expect cl-cc/binary:+cpu-type-x86-64+ :to-equal #x01000007)
  (expect cl-cc/binary:+cpu-type-arm64+ :to-equal #x0100000C)
  (expect cl-cc/binary:+mh-execute+ :to-equal 2)
  (expect cl-cc/binary:+lc-segment-64+ :to-equal #x19)
  (expect cl-cc/binary:+lc-symtab+ :to-equal #x02))

;;; ─── Structure Defaults ─────────────────────────────────────────────────

(it-sequential "macho-header-defaults"
  (let ((hdr (cl-cc/binary::make-mach-header)))
    (expect (cl-cc/binary:mach-header-magic hdr) :to-equal #xFEEDFACF)
    (expect (cl-cc/binary:mach-header-cputype hdr) :to-equal #x01000007)
    (expect (cl-cc/binary:mach-header-filetype hdr) :to-equal 2)
    (expect (cl-cc/binary:mach-header-ncmds hdr) :to-equal 0)))

(it-sequential "macho-command-defaults"
  (let ((seg (cl-cc/binary::make-segment-command)))
    (expect (cl-cc/binary:segment-command-cmd seg) :to-equal #x19)
    (expect (cl-cc/binary:segment-command-segname seg) :to-equal ""))
  (let ((ep (cl-cc/binary::make-entry-point-command)))
    (expect (cl-cc/binary:entry-point-command-cmd ep) :to-equal cl-cc/binary:+lc-main+)
    (expect (cl-cc/binary:entry-point-command-entryoff ep) :to-equal 0)))

;;; ─── Utilities ──────────────────────────────────────────────────────────

(it-sequential "macho-align-up already-aligned"
  (destructuring-bind (value alignment expected) (list 4096 4096 4096)
    (expect (cl-cc/binary:align-up value alignment) :to-equal expected)))

(it-sequential "macho-align-up needs-rounding"
  (destructuring-bind (value alignment expected) (list 4097 4096 8192)
    (expect (cl-cc/binary:align-up value alignment) :to-equal expected)))

(it-sequential "macho-align-up zero"
  (destructuring-bind (value alignment expected) (list 0 4096 0)
    (expect (cl-cc/binary:align-up value alignment) :to-equal expected)))

(it-sequential "macho-align-up one-to-16"
  (destructuring-bind (value alignment expected) (list 1 16 16)
    (expect (cl-cc/binary:align-up value alignment) :to-equal expected)))

(it-sequential "macho-align-up exact-boundary"
  (destructuring-bind (value alignment expected) (list 32 16 32)
    (expect (cl-cc/binary:align-up value alignment) :to-equal expected)))

(it-sequential "macho-string-to-ascii"
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "ABC")))
    (expect (length bytes) :to-equal 3)
    (expect (aref bytes 0) :to-equal 65)
    (expect (aref bytes 1) :to-equal 66)
    (expect (aref bytes 2) :to-equal 67)))

;;; ─── Buffer ─────────────────────────────────────────────────────────────

(it-sequential "macho-buffer-operations"
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal 0))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::buffer-write-byte buf 42)
    (expect (length (cl-cc/binary::byte-buffer-data buf)) :to-equal 1)
    (expect (aref (cl-cc/binary::byte-buffer-data buf) 0) :to-equal 42))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::buffer-write-byte buf 1)
    (cl-cc/binary::buffer-write-byte buf 2)
    (let ((result (cl-cc/binary::buffer-get-bytes buf)))
      (expect (typep result '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
      (expect (length result) :to-equal 2))))

;;; ─── Serialization ──────────────────────────────────────────────────────

(it-sequential "macho-serialize-uint-le"
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary:serialize-uint32-le #xDEADBEEF buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (length data) :to-equal 4)
      (expect (aref data 0) :to-equal #xEF)
      (expect (aref data 1) :to-equal #xBE)
      (expect (aref data 2) :to-equal #xAD)
      (expect (aref data 3) :to-equal #xDE)))
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary:serialize-uint64-le #x0102030405060708 buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (length data) :to-equal 8)
      (expect (aref data 0) :to-equal #x08)
      (expect (aref data 7) :to-equal #x01))))

(it-sequential "macho-serialize-string-16-pads"
  (let ((buf (cl-cc/binary::make-byte-buffer)))
    (cl-cc/binary::serialize-string-16 "hi" buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (length data) :to-equal 16)
      (expect (aref data 0) :to-equal (char-code #\h))
      (expect (aref data 1) :to-equal (char-code #\i))
      (expect (aref data 2) :to-equal 0)
      (expect (aref data 15) :to-equal 0))))

(it-sequential "macho-serialize-mach-header"
  (let ((buf (cl-cc/binary::make-byte-buffer))
        (hdr (cl-cc/binary::make-mach-header)))
    (cl-cc/binary::serialize-mach-header hdr buf)
    (let ((data (cl-cc/binary::byte-buffer-data buf)))
      (expect (length data) :to-equal 32)
      (expect (aref data 0) :to-equal #xCF)
      (expect (aref data 1) :to-equal #xFA)
      (expect (aref data 2) :to-equal #xED)
      (expect (aref data 3) :to-equal #xFE))))
