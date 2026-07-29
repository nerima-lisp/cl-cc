;;;; packages/binary/tests/macho-fat-tests.lisp

(in-package :cl-cc/test)



(it-sequential "fr-691-mach-o-universal-binary-fat-magic"
  (let* ((x86 (cl-cc/binary:make-mach-o-fat-slice
               :cputype cl-cc/binary:+fat-cputype-x86-64+
               :cpusubtype cl-cc/binary:+cpu-subtype-x86-64-all+
               :align 2
               :bytes (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(1 2 3 4))))
         (arm (cl-cc/binary:make-mach-o-fat-slice
               :cputype cl-cc/binary:+fat-cputype-arm64+
               :cpusubtype cl-cc/binary:+cpu-subtype-arm64-all+
               :align 2
               :bytes (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(5 6 7 8))))
         (bytes (cl-cc/binary:build-mach-o-fat-binary (list x86 arm))))
    (expect (subseq (coerce bytes 'list) 0 4) :to-equal '(#xCA #xFE #xBA #xBE))
    (expect (elt bytes 7) :to-equal 2)
    (expect (> (length bytes) 48) :to-be-truthy)))
