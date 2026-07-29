;;;; tests/unit/emit/elf-tests.lisp — ELF64 Binary Format Tests
;;;;
;;;; Tests for src/emit/binary/elf.lisp:
;;;; Buffer helpers, strtab builder, ELF64 builder, constants.

(in-package :cl-cc/test)



;;; ─── ELF Constants ──────────────────────────────────────────────────────

(it-sequential "elf-constants magic-0"
  (destructuring-bind (expected actual) (list #x7f cl-cc/binary::+elf-magic-0+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants magic-1"
  (destructuring-bind (expected actual) (list (char-code #\E) cl-cc/binary::+elf-magic-1+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants magic-2"
  (destructuring-bind (expected actual) (list (char-code #\L) cl-cc/binary::+elf-magic-2+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants magic-3"
  (destructuring-bind (expected actual) (list (char-code #\F) cl-cc/binary::+elf-magic-3+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants class-64"
  (destructuring-bind (expected actual) (list 2 cl-cc/binary::+elf-class-64+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants machine-x86"
  (destructuring-bind (expected actual) (list #x3E cl-cc/binary::+elf-machine-x86-64+)
    (expect actual :to-equal expected)))

(it-sequential "elf-constants machine-arm64"
  (destructuring-bind (expected actual) (list #xB7 cl-cc/binary::+elf-machine-aarch64+)
    (expect actual :to-equal expected)))

(it-sequential "elf-structure-sizes ehdr"
  (destructuring-bind (expected actual) (list 64 cl-cc/binary::+elf64-ehdr-size+)
    (expect actual :to-equal expected)))

(it-sequential "elf-structure-sizes shdr"
  (destructuring-bind (expected actual) (list 64 cl-cc/binary::+elf64-shdr-size+)
    (expect actual :to-equal expected)))

(it-sequential "elf-structure-sizes sym"
  (destructuring-bind (expected actual) (list 24 cl-cc/binary::+elf64-sym-size+)
    (expect actual :to-equal expected)))

(it-sequential "elf-structure-sizes rela"
  (destructuring-bind (expected actual) (list 24 cl-cc/binary::+elf64-rela-size+)
    (expect actual :to-equal expected)))

;;; ─── Buffer Helpers ─────────────────────────────────────────────────────

(it-sequential "elf-make-buffer-empty"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (expect (length buf) :to-equal 0)
    (expect (adjustable-array-p buf) :to-be-truthy)))

(it-sequential "elf-buf-u8-behavior"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #xAB)
    (expect (length buf) :to-equal 1)
    (expect (aref buf 0) :to-equal #xAB))
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf #x1FF)
    (expect (aref buf 0) :to-equal #xFF)))

(it-sequential "elf-buf-little-endian-writes u16le"
  (destructuring-bind (writer value expected-len byte-checks) (list #'cl-cc/binary::binary-buffer-write-u16le #x1234 2 '((0 #x34) (1 #x12)))
    (let ((buf (cl-cc/binary::elf-make-buffer)))
    (funcall writer buf value)
    (expect (length buf) :to-equal expected-len)
    (loop for (idx expected) in byte-checks
          do (expect (aref buf idx) :to-equal expected)))))

(it-sequential "elf-buf-little-endian-writes u32le"
  (destructuring-bind (writer value expected-len byte-checks) (list #'cl-cc/binary::binary-buffer-write-u32le #xDEADBEEF 4 '((0 #xEF) (1 #xBE) (2 #xAD) (3 #xDE)))
    (let ((buf (cl-cc/binary::elf-make-buffer)))
    (funcall writer buf value)
    (expect (length buf) :to-equal expected-len)
    (loop for (idx expected) in byte-checks
          do (expect (aref buf idx) :to-equal expected)))))

(it-sequential "elf-buf-little-endian-writes u64le"
  (destructuring-bind (writer value expected-len byte-checks) (list #'cl-cc/binary::binary-buffer-write-u64le #x0102030405060708 8 '((0 #x08) (7 #x01)))
    (let ((buf (cl-cc/binary::elf-make-buffer)))
    (funcall writer buf value)
    (expect (length buf) :to-equal expected-len)
    (loop for (idx expected) in byte-checks
          do (expect (aref buf idx) :to-equal expected)))))

(it-sequential "binary-buffer-write-pad-zeros"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-pad buf 4)
    (expect (length buf) :to-equal 4)
    (expect (every #'zerop (coerce buf 'list)) :to-be-truthy)))

(it-sequential "binary-buffer-write-bytes-input-types vector"
  (destructuring-bind (input) (list (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))
    (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-bytes buf input)
    (expect (length buf) :to-equal 3)
    (expect (aref buf 0) :to-equal (elt input 0))
    (expect (aref buf 2) :to-equal (elt input 2)))))

(it-sequential "binary-buffer-write-bytes-input-types list"
  (destructuring-bind (input) (list '(#x10 #x20 #x30))
    (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-bytes buf input)
    (expect (length buf) :to-equal 3)
    (expect (aref buf 0) :to-equal (elt input 0))
    (expect (aref buf 2) :to-equal (elt input 2)))))

(it-sequential "binary-buffer-to-array-returns-simple-array"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::elf-buf-u8 buf 42)
    (let ((arr (cl-cc/binary::binary-buffer-to-array buf)))
      (expect (typep arr '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
      (expect (length arr) :to-equal 1)
      (expect (aref arr 0) :to-equal 42))))

;;; ─── String Table Builder ───────────────────────────────────────────────

(it-sequential "elf-strtab-initial-null"
  (let ((st (cl-cc/binary::make-strtab)))
    (expect (length (cl-cc/binary::strtab-bytes st)) :to-equal 1)
    (expect (aref (cl-cc/binary::strtab-bytes st) 0) :to-equal 0)))

(it-sequential "elf-strtab-add-behavior"
  (let* ((st (cl-cc/binary::make-strtab))
         (off (cl-cc/binary::strtab-add st "hello")))
    (expect off :to-equal 1))
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "hello"))
         (off2 (cl-cc/binary::strtab-add st "hello")))
    (expect off2 :to-equal off1))
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "foo"))
         (off2 (cl-cc/binary::strtab-add st "bar")))
    (expect (= off1 off2) :to-be-falsy)))

(it-sequential "elf-strtab-bytes-contains-strings"
  (let ((st (cl-cc/binary::make-strtab)))
    (cl-cc/binary::strtab-add st "hi")
    (let ((bytes (cl-cc/binary::strtab-bytes st)))
      ;; bytes[0]=\0, bytes[1]='h', bytes[2]='i', bytes[3]=\0
      (expect (>= (length bytes) 4) :to-be-truthy)
      (expect (aref bytes 0) :to-equal 0)
      (expect (aref bytes 1) :to-equal (char-code #\h))
      (expect (aref bytes 2) :to-equal (char-code #\i))
      (expect (aref bytes 3) :to-equal 0))))

;;; ─── ELF64 Builder ──────────────────────────────────────────────────────

(it-sequential "elf64-builder-fresh"
  (let ((b (cl-cc/binary::make-elf64-object)))
    (expect (cl-cc/binary::elf64-text-size b) :to-equal 0)
    (expect (cl-cc/binary::elf64-bss-size b) :to-equal 0)
    (expect (cl-cc/binary::elf64-symbols b) :to-be-null)
    (expect (cl-cc/binary::elf64-rela-entries b) :to-be-null)))

(it-sequential "elf64-add-bss"
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-bss b 16)
    (cl-cc/binary::elf64-add-bss b 8)
    (expect (cl-cc/binary::elf64-bss-size b) :to-equal 24)))

(it-sequential "elf64-add-text-bytes"
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#x48 #x89 #xC0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (expect (cl-cc/binary::elf64-text-size b) :to-equal 3)))

(it-sequential "elf64-builder-list-accumulation symbol"
  (destructuring-bind (add-fn accessor-fn) (list (lambda (b) (cl-cc/binary::elf64-add-global-symbol b "_main" :section-idx 1 :value 0 :size 10)) #'cl-cc/binary::elf64-symbols)
    (let ((b (cl-cc/binary::make-elf64-object)))
    (funcall add-fn b)
    (expect (length (funcall accessor-fn b)) :to-equal 1))))

(it-sequential "elf64-builder-list-accumulation reloc"
  (destructuring-bind (add-fn accessor-fn) (list (lambda (b) (cl-cc/binary::elf64-add-reloc b 4 "printf")) #'cl-cc/binary::elf64-rela-entries)
    (let ((b (cl-cc/binary::make-elf64-object)))
    (funcall add-fn b)
    (expect (length (funcall accessor-fn b)) :to-equal 1))))

(it-sequential "elf64-finalize-produces-bytes"
  (let ((b (cl-cc/binary::make-elf64-object))
        (code (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 0 0 0))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 4)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (expect (> (length result) 0) :to-be-truthy)
      (expect (typep result '(simple-array (unsigned-byte 8) (*))) :to-be-truthy))))

(it-sequential "elf64-finalize-header-prefix"
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 1)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (expect (aref result 0) :to-equal #x7F)
      (expect (aref result 1) :to-equal (char-code #\E))
      (expect (aref result 2) :to-equal (char-code #\L))
      (expect (aref result 3) :to-equal (char-code #\F))
      (expect (aref result 4) :to-equal 2)
      (expect (aref result 5) :to-equal 1))))

