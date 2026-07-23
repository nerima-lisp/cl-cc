;;;; packages/binary/tests/binary-constpool-tests.lisp — FR-724 constant pool tests

(in-package :cl-cc/test)



(defun %constpool-ub8 (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(it-sequential "fr-724-elf-rodata-deduplicates-identical-byte-sequences"
  (let* ((builder (cl-cc/binary::make-elf64-object))
         (first (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(1 2 3 4))))
         (second (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(9 8))))
         (third (cl-cc/binary:elf64-add-rodata-bytes builder (%constpool-ub8 '(1 2 3 4)))))
    (expect third :to-equal first)
    (expect first :to-equal 0)
    (expect second :to-equal 4)
    (expect (length (cl-cc/binary::elf64-rodata-buf builder)) :to-equal 6)))

(it-sequential "fr-724-elf-rodata-string-deduplicates-and-sets-merge-flags"
  (let* ((builder (cl-cc/binary::make-elf64-executable))
         (first (cl-cc/binary:elf64-add-rodata-string builder "hello"))
         (second (cl-cc/binary:elf64-add-rodata-string builder "world"))
         (third (cl-cc/binary:elf64-add-rodata-string builder "hello")))
    (cl-cc/binary::elf64-add-text-bytes builder #(195))
    (expect third :to-equal first)
    (expect first :to-equal 0)
    (expect second :to-equal 6)
    (let* ((flags (%elf-section-flags-by-name (cl-cc/binary::elf64-finalize builder)))
           (string-flags (gethash ".rodata.str" flags)))
      (expect string-flags :to-equal (logior cl-cc/binary::+shf-alloc+
                            cl-cc/binary::+shf-merge+
                            cl-cc/binary::+shf-strings+))
      (expect (logand string-flags cl-cc/binary::+shf-write+) :to-equal 0)
      (expect (logand string-flags cl-cc/binary::+shf-execinstr+) :to-equal 0))))

(it-sequential "fr-724-macho-data-const-segment-deduplicates-payloads"
  (let ((builder (cl-cc/binary:make-mach-o-builder :x86-64)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(10 20 30)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(1 2)))
    (cl-cc/binary:add-data-const-segment builder (%constpool-ub8 '(10 20 30)))
    (let* ((segment (find "__DATA_CONST" (cl-cc/binary::mach-o-builder-segments builder)
                          :key #'cl-cc/binary:segment-command-segname
                          :test #'string=))
           (payload (cl-cc/binary::segment-command-payload segment)))
      (expect segment :to-be-truthy)
      (expect (length payload) :to-equal 5)
      (expect (aref payload 0) :to-equal 10)
      (expect (aref payload 1) :to-equal 20)
      (expect (aref payload 2) :to-equal 30)
      (expect (aref payload 3) :to-equal 1)
      (expect (aref payload 4) :to-equal 2))))
