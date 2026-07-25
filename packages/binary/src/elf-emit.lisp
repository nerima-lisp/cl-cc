;;;; packages/binary/src/elf-emit.lisp — ELF64 public entry points
;;;
;;; Contains:
;;;   - write-elf64-file — write byte array to file
;;;   - elf64-build-x86-64-start-wrapper — Linux x86-64 _start stub
;;;   - compile-to-elf64 — public entry point: code+relocs → ELF64 object bytes
;;;   - compile-to-elf64-exec — public entry point: code+relocs → ELF64 image
;;;
;;; ELF64 constants, byte-buffer helpers, strtab-builder, elf64-builder struct,
;;; and basic builder API (add-text, add-bss, add-reloc, add-symbol)
;;; are in elf.lisp; section layout and serialization are in elf-serialize.lisp.
;;; Both load before this file.
;;;
;;; Load order: after emit/binary/elf-serialize.lisp.

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; Public API
;;; ------------------------------------------------------------

(defun write-elf64-file (filename bytes)
  "Write ELF64 object file BYTES to FILENAME."
  (with-open-file (out filename
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence bytes out))
  filename)

(defun elf64-build-x86-64-start-wrapper (entry-bytes)
  "Build a Linux x86-64 _start wrapper followed by ENTRY-BYTES.

The wrapper reads argc/argv/envp from the initial process stack, calls the
entry function using the SysV ABI argument registers, and exits with the
entry function's return value via the exit syscall."
  (let ((buf (elf-make-buffer)))
    ;; xor %ebp,%ebp
    (binary-buffer-write-bytes buf '(#x31 #xed))
    ;; mov (%rsp),%rdi             ; argc
    (binary-buffer-write-bytes buf '(#x48 #x8b #x3c #x24))
    ;; lea 8(%rsp),%rsi           ; argv
    (binary-buffer-write-bytes buf '(#x48 #x8d #x74 #x24 #x08))
    ;; lea 8(%rsi,%rdi,8),%rdx    ; envp
    (binary-buffer-write-bytes buf '(#x48 #x8d #x54 #xfe #x08))
    ;; call entry
    (let ((call-offset (length buf)))
      (elf-buf-u8 buf #xe8)
      (let ((entry-offset (+ call-offset 5)))
        (binary-buffer-write-u32le buf (- entry-offset (+ call-offset 5)))))
    ;; mov %rax,%rdi
    (binary-buffer-write-bytes buf '(#x48 #x89 #xc7))
    ;; mov $60,%eax; syscall
    (binary-buffer-write-bytes buf '(#xb8 #x3c #x00 #x00 #x00 #x0f #x05))
    (let* ((wrapper-size (length buf))
           (call-immediate-offset 17)
           (rel32 (- wrapper-size (+ call-immediate-offset 4))))
      (setf (aref buf call-immediate-offset) (logand rel32 #xff)
            (aref buf (+ call-immediate-offset 1)) (logand (ash rel32 -8) #xff)
            (aref buf (+ call-immediate-offset 2)) (logand (ash rel32 -16) #xff)
            (aref buf (+ call-immediate-offset 3)) (logand (ash rel32 -24) #xff)))
    (binary-buffer-write-bytes buf entry-bytes)
    (binary-buffer-to-array buf)))

(defun compile-to-elf64 (code-bytes reloc-entries &key (output-file nil) (arch :x86-64) (bss-size 0)
                                                  compress)
  "Create an ELF64 relocatable object from CODE-BYTES and RELOC-ENTRIES.
   RELOC-ENTRIES is a list of (byte-offset . symbol-name) pairs from the
   x86-64 code generator.
   Returns the byte array; also writes to OUTPUT-FILE if provided."
  (let ((builder (make-elf64-object :machine (ecase arch
                                               (:x86-64 +elf-machine-x86-64+)
                                               (:arm64 +elf-machine-aarch64+)
                                               (:aarch64 +elf-machine-aarch64+)))))
    ;; Add code
    (setf (elf64-compress-text builder) (and compress t))
    (elf64-add-text-bytes builder code-bytes)
    (when (plusp bss-size)
      (elf64-add-bss builder bss-size))
    ;; Add relocations and collect unique symbols
    (let ((seen-syms (make-hash-table :test #'equal)))
      (dolist (reloc reloc-entries)
        (let* ((offset (car reloc))
               (sym-name (cdr reloc)))
          (unless (gethash sym-name seen-syms)
            (elf64-add-global-symbol builder sym-name)
            (setf (gethash sym-name seen-syms) t))
          (elf64-add-reloc builder offset sym-name))))
    ;; Finalize
    (let ((bytes (elf64-finalize builder)))
      (when output-file
        (write-elf64-file output-file bytes))
      bytes)))

(defun compile-to-elf64-exec (code-bytes reloc-entries &key output-file (arch :x86-64)
                                                     (bss-size 0) (type :exec)
                                                     needed-libraries
                                                     (interpreter +elf64-default-interpreter+))
  "Create an ELF64 executable image from CODE-BYTES and RELOC-ENTRIES.

TYPE is either :EXEC for ET_EXEC or :DYN for ET_DYN PIE/shared-object output.
For x86-64 executables, a small _start wrapper is prepended before CODE-BYTES."
  (let* ((machine (ecase arch
                    (:x86-64 +elf-machine-x86-64+)
                    (:arm64 +elf-machine-aarch64+)
                    (:aarch64 +elf-machine-aarch64+)))
         (dynamic-p (ecase type
                      (:exec nil)
                      (:dyn t)
                      (:pie t)
      (:shared t)))
          (builder (if dynamic-p
                       (make-elf64-dynamic :machine machine :interpreter interpreter
                                           :shared-object (eq type :shared))
                       (make-elf64-executable :machine machine)))
         (text-bytes (if (and (not dynamic-p) (eq arch :x86-64))
                         (elf64-build-x86-64-start-wrapper code-bytes)
                         code-bytes)))
    (elf64-add-text-bytes builder text-bytes)
    (elf64-add-global-symbol builder "_start" :section-idx (if dynamic-p 2 1) :value 0
                             :size (length text-bytes))
    (when (plusp bss-size)
      (elf64-add-bss builder bss-size))
    (dolist (library needed-libraries)
      (elf64-add-needed-library builder library))
    (let ((seen-syms (make-hash-table :test #'equal))
          (reloc-offset-delta (if (and (not dynamic-p) (eq arch :x86-64))
                                  (- (length text-bytes) (length code-bytes))
                                  0)))
      (dolist (reloc reloc-entries)
        (let* ((offset (+ (car reloc) reloc-offset-delta))
               (sym-name (cdr reloc)))
          (unless (gethash sym-name seen-syms)
            (elf64-add-global-symbol builder sym-name)
            (setf (gethash sym-name seen-syms) t))
          (elf64-add-reloc builder offset sym-name))))
    (let ((bytes (elf64-finalize builder)))
      (when output-file
        (write-elf64-file output-file bytes))
      bytes)))
