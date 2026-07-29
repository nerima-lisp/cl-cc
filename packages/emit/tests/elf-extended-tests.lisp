;;;; tests/unit/emit/elf-extended-tests.lisp — ELF64 extended builder and buffer tests

(in-package :cl-cc/test)


;;; ─── Additional Buffer Tests ─────────────────────────────────────────────

(it-sequential "elf-s64le-negative-one-encodes-as-all-ff"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-s64le buf -1)
    (expect (length buf) :to-equal 8)
    (expect (every (lambda (b) (= b #xFF)) (coerce buf 'list)) :to-be-truthy)))

(it-sequential "elf-s64le-positive-value-matches-u64le"
  (let ((buf1 (cl-cc/binary::elf-make-buffer))
        (buf2 (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-s64le buf1 42)
    (cl-cc/binary::binary-buffer-write-u64le buf2 42)
    (expect (coerce buf2 'list) :to-equal (coerce buf1 'list))))

(it-sequential "elf-u64le-writes-eight-bytes-little-endian"
  (let ((buf (cl-cc/binary::elf-make-buffer)))
    (cl-cc/binary::binary-buffer-write-u64le buf #xDEADBEEFCAFEBABE)
    (expect (length buf) :to-equal 8)
    (expect (aref buf 0) :to-equal #xBE)
    (expect (aref buf 1) :to-equal #xBA)
    (expect (aref buf 2) :to-equal #xFE)
    (expect (aref buf 3) :to-equal #xCA)
    (expect (aref buf 4) :to-equal #xEF)
    (expect (aref buf 5) :to-equal #xBE)
    (expect (aref buf 6) :to-equal #xAD)
    (expect (aref buf 7) :to-equal #xDE)))

;;; ─── Additional String Table Tests ──────────────────────────────────────

(it-sequential "elf-strtab-offset-layout"
  (let ((st (cl-cc/binary::make-strtab)))
    (expect (cl-cc/binary::strtab-add st "") :to-equal 0))
  (let* ((st (cl-cc/binary::make-strtab))
         (off1 (cl-cc/binary::strtab-add st "abc"))
         (off2 (cl-cc/binary::strtab-add st "xy")))
    (expect off1 :to-equal 1)
    (expect off2 :to-equal 5))
  (let ((st (cl-cc/binary::make-strtab)))
    (cl-cc/binary::strtab-add st "ab")
    (expect (length (cl-cc/binary::strtab-bytes st)) :to-equal 4)))

;;; ─── Additional ELF64 Builder Tests ──────────────────────────────────────

(it-sequential "elf64-multiple-add-text-bytes-accumulate-total-size"
  (let ((b (cl-cc/binary::make-elf64-object))
        (c1 (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(#x90 #x90)))
        (c2 (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#xC3 #x00 #x00))))
    (cl-cc/binary::elf64-add-text-bytes b c1)
    (cl-cc/binary::elf64-add-text-bytes b c2)
    (expect (cl-cc/binary::elf64-text-size b) :to-equal 5)))

(it-sequential "elf64-multiple-add-global-symbol-all-registered"
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-global-symbol b "_start" :section-idx 1 :value 0 :size 4)
    (cl-cc/binary::elf64-add-global-symbol b "printf" :section-idx 0 :value 0 :size 0)
    (expect (length (cl-cc/binary::elf64-symbols b)) :to-equal 2)))

(it-sequential "elf64-add-reloc-defaults-to-plt32"
  (let ((b (cl-cc/binary::make-elf64-object)))
    (cl-cc/binary::elf64-add-reloc b 1 "puts")
    (let ((entry (first (cl-cc/binary::elf64-rela-entries b))))
      (expect (first entry) :to-equal 1)
      (expect (second entry) :to-equal cl-cc/binary::+r-x86-64-plt32+)
      (expect (third entry) :to-equal "puts")
      (expect (fourth entry) :to-equal -4))))

(it-sequential "elf64-finalize-header-has-correct-fields"
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3))))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (expect (aref result 16) :to-equal 1)
      (expect (aref result 17) :to-equal 0)
      (expect (aref result 18) :to-equal #x3E)
      (expect (aref result 19) :to-equal 0)
      (expect (aref result 60) :to-equal 11)
      (expect (aref result 62) :to-equal 10))))

(it-sequential "elf64-finalize-empty-object-produces-at-least-64-bytes"
  (let ((b (cl-cc/binary::make-elf64-object)))
    (let ((result (cl-cc/binary::elf64-finalize b)))
      (expect (>= (length result) 64) :to-be-truthy))))

(it-sequential "elf64-finalize-bss-section-has-nobits-type-and-size"
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (result nil)
         (shoff nil)
         (bss-shoff nil))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (cl-cc/binary::elf64-add-bss b 32)
    (setf result (cl-cc/binary::elf64-finalize b))
    (setf shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
    (setf bss-shoff (+ shoff (* 3 64)))
    (expect (aref result (+ bss-shoff 4)) :to-equal 8)
    (expect (aref result (+ bss-shoff 32)) :to-equal 32)))

(it-sequential "elf64-finalize-section-offsets-are-properly-aligned"
  (let* ((b (cl-cc/binary::make-elf64-object))
         (code (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(#x90 #x90 #xC3)))
         (result nil)
         (text-offset nil)
         (shoff nil))
    (cl-cc/binary::elf64-add-text-bytes b code)
    (setf result (cl-cc/binary::elf64-finalize b))
    (setf shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
    (setf text-offset (+ (aref result (+ shoff 64 24))
                         (ash (aref result (+ shoff 64 25)) 8)
                         (ash (aref result (+ shoff 64 26)) 16)
                         (ash (aref result (+ shoff 64 27)) 24)))
    (expect (mod text-offset 16) :to-equal 0)
    (expect (mod shoff 8) :to-equal 0)))

(it-sequential "elf64-compile-api-aarch64-machine-value"
  (let* ((code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC0)))
         (result (cl-cc/binary::compile-to-elf64 code nil :arch :arm64)))
    (expect (aref result 18) :to-equal #xB7)
    (expect (aref result 19) :to-equal 0)))

(it-sequential "elf64-compile-api-bss-reservation"
  (let* ((code (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(#xC3)))
         (result (cl-cc/binary::compile-to-elf64 code nil :bss-size 64))
         (shoff (+ (aref result 40)
                   (ash (aref result 41) 8)
                   (ash (aref result 42) 16)
                   (ash (aref result 43) 24)))
          (bss-shoff (+ shoff (* 3 64))))
    (expect (typep result '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (aref result (+ bss-shoff 4)) :to-equal 8)
    (expect (aref result (+ bss-shoff 32)) :to-equal 64)))

(it-sequential "elf64-compile-api-valid-elf-from-code-and-relocs"
  (let* ((code (make-array 5 :element-type '(unsigned-byte 8)
                            :initial-contents '(#xE8 0 0 0 0)))
         (relocs (list (cons 1 "printf")))
         (result (cl-cc/binary::compile-to-elf64 code relocs)))
    (expect (typep result '(simple-array (unsigned-byte 8) (*))) :to-be-truthy)
    (expect (>= (length result) 64) :to-be-truthy)
    (expect (aref result 0) :to-equal #x7F)
    (expect (aref result 1) :to-equal (char-code #\E))))

(it-sequential "elf64-symbol-and-reloc-type-constants stb-local"
  (destructuring-bind (expected actual) (list 0 cl-cc/binary::+stb-local+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants stb-global"
  (destructuring-bind (expected actual) (list 1 cl-cc/binary::+stb-global+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants stb-weak"
  (destructuring-bind (expected actual) (list 2 cl-cc/binary::+stb-weak+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants stt-notype"
  (destructuring-bind (expected actual) (list 0 cl-cc/binary::+stt-notype+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants stt-func"
  (destructuring-bind (expected actual) (list 2 cl-cc/binary::+stt-func+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants r-x86-64-none"
  (destructuring-bind (expected actual) (list 0 cl-cc/binary::+r-x86-64-none+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants r-x86-64-64"
  (destructuring-bind (expected actual) (list 1 cl-cc/binary::+r-x86-64-64+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants r-x86-64-pc32"
  (destructuring-bind (expected actual) (list 2 cl-cc/binary::+r-x86-64-pc32+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants r-x86-64-plt32"
  (destructuring-bind (expected actual) (list 4 cl-cc/binary::+r-x86-64-plt32+)
    (expect actual :to-equal expected)))

(it-sequential "elf64-symbol-and-reloc-type-constants r-x86-64-32"
  (destructuring-bind (expected actual) (list 10 cl-cc/binary::+r-x86-64-32+)
    (expect actual :to-equal expected)))
