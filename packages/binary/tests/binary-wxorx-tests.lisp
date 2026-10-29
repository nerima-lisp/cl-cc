;;;; packages/binary/tests/binary-wxorx-tests.lisp — FR-694 ELF W^X tests

(in-package :cl-cc/test)



(defun %elf-u16le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)))

(defun %elf-u32le (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (+ offset 1)) 8)
     (ash (aref bytes (+ offset 2)) 16)
     (ash (aref bytes (+ offset 3)) 24)))

(defun %elf-u64le (bytes offset)
  (loop for i below 8
        sum (ash (aref bytes (+ offset i)) (* 8 i))))

(defun %elf-c-string (bytes offset)
  (with-output-to-string (out)
    (loop for i from offset below (length bytes)
          for byte = (aref bytes i)
          until (zerop byte)
          do (write-char (code-char byte) out))))

(defun %elf-section-flags-by-name (bytes)
  (let* ((shoff (%elf-u64le bytes 40))
         (shentsize (%elf-u16le bytes 58))
         (shnum (%elf-u16le bytes 60))
         (shstrndx (%elf-u16le bytes 62))
         (shstr-header (+ shoff (* shstrndx shentsize)))
         (shstr-offset (%elf-u64le bytes (+ shstr-header 24)))
         (result (make-hash-table :test #'equal)))
    (dotimes (i shnum result)
      (let* ((header (+ shoff (* i shentsize)))
             (name-offset (%elf-u32le bytes header))
             (name (%elf-c-string bytes (+ shstr-offset name-offset)))
             (flags (%elf-u64le bytes (+ header 8))))
        (setf (gethash name result) flags)))))

