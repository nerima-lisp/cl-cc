;;;; packages/binary/tests/binary-buffer-tests.lisp — Binary buffer operation tests
;;;;
;;;; Tests for: binary buffer write/read, alignment, string conversion,
;;;; serialization primitives (macho-buffer.lisp).

(in-package :cl-cc/test)



;;; ------------------------------------------------------------
;;; Binary Buffer — make-binary-buffer, write/read operations
;;; ------------------------------------------------------------

(it-sequential "binary-buffer-create-empty-buffer"
  (let ((buf (cl-cc/binary::make-binary-buffer 256)))
    (expect (typep buf '(array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (array-has-fill-pointer-p buf) :to-be-truthy)
    (expect (length buf) :to-equal 0)))

(it-sequential "binary-buffer-write-u8-roundtrips"
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u8 buf #xAB)
    (expect (length buf) :to-equal 1)
    (expect (aref buf 0) :to-equal #xAB)
    (cl-cc/binary::binary-buffer-write-u8 buf #x00)
    (cl-cc/binary::binary-buffer-write-u8 buf #xFF)
    (expect (length buf) :to-equal 3)
    (expect (aref buf 1) :to-equal #x00)
    (expect (aref buf 2) :to-equal #xFF)))

(it-sequential "binary-buffer-write-u16le-roundtrips"
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u16le buf #xABCD)
    (expect (length buf) :to-equal 2)
    (expect (aref buf 0) :to-equal #xCD)
    (expect (aref buf 1) :to-equal #xAB)))

(it-sequential "binary-buffer-write-u32le-roundtrips"
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u32le buf #xDEADBEEF)
    (expect (length buf) :to-equal 4)
    (expect (aref buf 0) :to-equal #xEF)
    (expect (aref buf 1) :to-equal #xBE)
    (expect (aref buf 2) :to-equal #xAD)
    (expect (aref buf 3) :to-equal #xDE)))

(it-sequential "binary-buffer-write-u64le-roundtrips"
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u64le buf #x0123456789ABCDEF)
    (expect (length buf) :to-equal 8)
    (expect (aref buf 0) :to-equal #xEF)
    (expect (aref buf 1) :to-equal #xCD)
    (expect (aref buf 2) :to-equal #xAB)
    (expect (aref buf 3) :to-equal #x89)
    (expect (aref buf 4) :to-equal #x67)
    (expect (aref buf 5) :to-equal #x45)
    (expect (aref buf 6) :to-equal #x23)
    (expect (aref buf 7) :to-equal #x01)))

(it-sequential "binary-buffer-write-pad-zero-fills"
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (pad-count 5))
    (cl-cc/binary::binary-buffer-write-pad buf pad-count)
    (expect (length buf) :to-equal pad-count)
    (dotimes (i pad-count)
      (expect (aref buf i) :to-equal 0))))

(it-sequential "binary-buffer-write-bytes-vector"
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (data #(1 2 3 4 5)))
    (cl-cc/binary::binary-buffer-write-bytes buf data)
    (expect (length buf) :to-equal 5)
    (dotimes (i 5)
      (expect (aref buf i) :to-equal (aref data i)))))

(it-sequential "binary-buffer-write-bytes-list"
  (let ((buf (cl-cc/binary::make-binary-buffer 16))
        (data '(10 20 30)))
    (cl-cc/binary::binary-buffer-write-bytes buf data)
    (expect (length buf) :to-equal 3)
    (expect (aref buf 0) :to-equal 10)
    (expect (aref buf 1) :to-equal 20)
    (expect (aref buf 2) :to-equal 30)))

(it-sequential "binary-buffer-to-array-preserves-contents"
  (let ((buf (cl-cc/binary::make-binary-buffer 16)))
    (cl-cc/binary::binary-buffer-write-u8 buf 42)
    (cl-cc/binary::binary-buffer-write-u8 buf 99)
    (let ((copy (cl-cc/binary::binary-buffer-to-array buf)))
      (expect (length copy) :to-equal 2)
      (expect (aref copy 0) :to-equal 42)
      (expect (aref copy 1) :to-equal 99))))

;;; ------------------------------------------------------------
;;; byte-buffer — make-byte-buffer, buffer-write-byte
;;; ------------------------------------------------------------

(it-sequential "byte-buffer-create-and-write"
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (cl-cc/binary::buffer-write-byte bb #x7F)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 1)
      (expect (aref data 0) :to-equal #x7F))))

(it-sequential "byte-buffer-multiple-writes"
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (dotimes (i 10)
      (cl-cc/binary::buffer-write-byte bb i))
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 10)
      (dotimes (i 10)
        (expect (aref data i) :to-equal i)))))

(it-sequential "byte-buffer-write-bytes-via-class"
  (let ((bb (cl-cc/binary::make-byte-buffer 64)))
    (cl-cc/binary::buffer-write-bytes bb #(100 200 255))
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 3)
      (expect (aref data 0) :to-equal 100)
      (expect (aref data 1) :to-equal 200)
      (expect (aref data 2) :to-equal 255))))

;;; ------------------------------------------------------------
;;; Utilities — align-up, string-to-ascii-bytes
;;; ------------------------------------------------------------

(it-sequential "align-up-cases already-aligned-16-by-8"
  (destructuring-bind (value alignment expected) (list 16 8 16)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases already-aligned-1024"
  (destructuring-bind (value alignment expected) (list 1024 256 1024)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases 9-rounds-to-16"
  (destructuring-bind (value alignment expected) (list 9 8 16)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases 15-rounds-to-16"
  (destructuring-bind (value alignment expected) (list 15 8 16)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases 129-rounds-to-256"
  (destructuring-bind (value alignment expected) (list 129 128 256)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases zero-stays-zero"
  (destructuring-bind (value alignment expected) (list 0 8 0)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "align-up-cases alignment-one-passthrough"
  (destructuring-bind (value alignment expected) (list 42 1 42)
    (expect (cl-cc/binary::align-up value alignment) :to-equal expected)))

(it-sequential "string-to-ascii-bytes-converts-correctly"
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "ABC")))
    (expect (length bytes) :to-equal 3)
    (expect (aref bytes 0) :to-equal (char-code #\A))
    (expect (aref bytes 1) :to-equal (char-code #\B))
    (expect (aref bytes 2) :to-equal (char-code #\C))))

(it-sequential "string-to-ascii-bytes-empty-string"
  (let ((bytes (cl-cc/binary::string-to-ascii-bytes "")))
    (expect (length bytes) :to-equal 0)))

;;; ------------------------------------------------------------
;;; Serialization — serialize-uint32-le, serialize-uint64-le
;;; ------------------------------------------------------------

(it-sequential "serialize-uint32-le-outputs-little-endian"
  (let ((bb (cl-cc/binary::make-byte-buffer 16)))
    (cl-cc/binary::serialize-uint32-le #x12345678 bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 4)
      (expect (aref data 0) :to-equal #x78)
      (expect (aref data 1) :to-equal #x56)
      (expect (aref data 2) :to-equal #x34)
      (expect (aref data 3) :to-equal #x12))))

(it-sequential "serialize-uint64-le-outputs-little-endian"
  (let ((bb (cl-cc/binary::make-byte-buffer 16)))
    (cl-cc/binary::serialize-uint64-le #xAABBCCDD00112233 bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 8)
      (expect (aref data 0) :to-equal #x33)
      (expect (aref data 1) :to-equal #x22)
      (expect (aref data 2) :to-equal #x11)
      (expect (aref data 3) :to-equal #x00)
      (expect (aref data 4) :to-equal #xDD)
      (expect (aref data 5) :to-equal #xCC)
      (expect (aref data 6) :to-equal #xBB)
      (expect (aref data 7) :to-equal #xAA))))

(it-sequential "serialize-bytes-writes-all"
  (let ((bb (cl-cc/binary::make-byte-buffer 16))
        (input (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 3 5 7))))
    (cl-cc/binary::serialize-bytes input bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 4)
      (dotimes (i 4)
        (expect (aref data i) :to-equal (aref input i))))))

(it-sequential "serialize-string-16-pads-to-16"
  (let ((bb (cl-cc/binary::make-byte-buffer 32)))
    (cl-cc/binary::serialize-string-16 "HI" bb)
    (let ((data (cl-cc/binary::buffer-get-bytes bb)))
      (expect (length data) :to-equal 16)
      (expect (aref data 0) :to-equal (char-code #\H))
      (expect (aref data 1) :to-equal (char-code #\I))
      (dotimes (i 14)
        (expect (aref data (+ i 2)) :to-equal 0)))))
